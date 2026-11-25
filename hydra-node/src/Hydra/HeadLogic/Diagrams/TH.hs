{-# LANGUAGE TemplateHaskell #-}

-- | Reflect the topology in a concrete machine's signature. Workflow aliases
-- contain promoted values; owner topologies use Crem's singleton metadata.
module Hydra.HeadLogic.Diagrams.TH (machineDiagrams, stateSources) where

import Crem.BaseMachine (BaseMachineT)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List qualified as List (head, isInfixOf, lookup, tails, (\\))
import Data.Singletons.Base.TH (demote)
import Hydra.HeadLogic.Workflow (Workflow)
import Hydra.Prelude hiding (Type, lift)
import Language.Haskell.TH (
  Dec (TySynD),
  Exp (AppE, AppTypeE, ConE, ListE, VarE),
  Info (TyConI),
  Loc (loc_filename),
  Name,
  Q,
  Type (
    AppKindT,
    AppT,
    ArrowT,
    ConT,
    ForallT,
    PromotedConsT,
    PromotedNilT,
    PromotedT,
    PromotedTupleT,
    SigT
  ),
  listE,
  location,
  nameBase,
  nameModule,
  pprint,
  reify,
  reifyType,
  runIO,
  tupleDataName,
  varE,
 )
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (
  dropExtension,
  takeDirectory,
  takeExtension,
  (</>),
 )
import Prelude qualified as String (lines, unlines, words)

machineDiagrams :: Name -> [(Name, Name)] -> [(String, Name, String)] -> Q Exp
machineDiagrams render annotations entries = do
  checkCoverage [name | (_, name, _) <- entries]
  listE $ flip fmap entries $ \(title, name, description) -> do
    topology <- reifyType name >>= topologyOf . resultType
    value <- case topology of
      ConT alias ->
        reify alias >>= \case
          TyConI (TySynD _ [] body) -> promotedValue body
          _ -> pure $ AppTypeE (VarE 'demote) topology
      _ -> pure $ AppTypeE (VarE 'demote) topology
    (machine, topologySource) <- machineSources name topology
    let sources = "- Machine: " <> machine <> ".\n- Topology: " <> topologySource <> "."
        labels = maybe (ListE []) VarE $ List.lookup name annotations
    [|$(varE render) $(lift title) $(lift sources) $(lift description) $(pure labels) $(pure value)|]

stateSources :: Name -> [Name] -> Q Exp
stateSources coordinator machines = do
  source <- sourceLink coordinator
  rows <- forM machines $ \machine -> do
    topology <- reifyType machine >>= topologyOf . resultType
    (machineSource, topologySource) <- machineSources machine topology
    pure $ "| " <> machineSource <> " | " <> topologySource <> " |"
  lift $ "Updates: " <> source <> ".\n\n" <> String.unlines ("| Machine | Topology |" : "| --- | --- |" : rows)

resultType :: Type -> Type
resultType = \case
  ForallT _ _ body -> resultType body
  AppT (AppT ArrowT _) body -> resultType body
  body -> body

topologyOf :: Type -> Q Type
topologyOf = \case
  AppT (AppT (ConT name) _) topology | name == ''Workflow -> pure topology
  AppT (AppT (AppT (AppT (ConT name) _) topology) _) _ | name == ''BaseMachineT -> pure topology
  unexpected -> fail $ "Expected a concrete Crem machine, got " <> pprint unexpected

machineSources :: Name -> Type -> Q (String, String)
machineSources machine topology = do
  machineSource <- sourceLink machine
  name <- constructor topology
  topologySource <-
    if nameModule name == Just "Crem.Topology"
      -- Single-state owners apply Crem's shared topology directly. Link to the
      -- signature that selects it, rather than inventing a local declaration.
      then (<> " (in the machine signature)") . renderLink (qualifiedName name) <$> sourceLocation machine
      else sourceLink name
  pure (machineSource, topologySource)
 where
  constructor = \case
    ConT name -> pure name
    AppKindT body _ -> constructor body
    SigT body _ -> constructor body
    other -> fail $ "Expected a named topology, got " <> pprint other

sourceLink :: Name -> Q String
sourceLink name = renderLink (qualifiedName name) <$> sourceLocation name

renderLink :: String -> (FilePath, Int) -> String
renderLink name (file, line) =
  "[`" <> name <> "`](../../../../hydra-node/" <> file <> "#L" <> show line <> ")"

-- Source-relative links work on the branch being reviewed. The website resolves
-- them against its build revision. Missing declarations fail generation.
sourceLocation :: Name -> Q (FilePath, Int)
sourceLocation name = do
  moduleName <- maybe (fail $ "Missing source module for " <> show name) pure $ nameModule name
  let file = "src/" <> fmap (\c -> if c == '.' then '/' else c) moduleName <> ".hs"
      symbol = nameBase name
      -- Singleton promotion creates type aliases from value-level topologies.
      promotedSource = case symbol of initial : rest -> toLower initial : rest; [] -> []
  addDependentFile file
  content <- runIO $ readFileBS file
  let ls = zip [1 ..] $ String.words <$> String.lines (toString (decodeUtf8 content :: Text))
      declarations candidate =
        [ line
        | (line, tokens) <- ls
        , case tokens of
            "type" : declared : _ -> declared == candidate
            declared : "::" : _ -> declared == candidate
            _ -> False
        ]
  case declarations symbol <> declarations promotedSource of
    line : _ -> pure (file, line)
    [] -> fail $ "Cannot locate source declaration for " <> qualifiedName name

-- This converts the actual promoted topology, rather than maintaining a second
-- list of edges. Unsupported representations fail the build instead of guessing.
promotedValue :: Type -> Q Exp
promotedValue = \case
  AppT f x -> AppE <$> promotedValue f <*> promotedValue x
  PromotedT name -> pure $ ConE name
  PromotedNilT -> pure $ ListE []
  PromotedConsT -> pure $ ConE '(:)
  PromotedTupleT n -> pure $ ConE $ tupleDataName n
  SigT value _ -> promotedValue value
  other -> fail $ "Unsupported topology representation: " <> pprint other

qualifiedName :: Name -> String
qualifiedName name = maybe "" (<> ".") (nameModule name) <> nameBase name

-- The inventory is checked against concrete factory signatures, not an edge
-- parser. Adding a module to Cabal also invalidates this compile-time check.
checkCoverage :: [Name] -> Q ()
checkCoverage registered = do
  source <- takeDirectory . loc_filename <$> location
  addDependentFile "hydra-node.cabal"
  files <- runIO $ sourceFiles source
  found <- fmap concat $ forM files $ \file -> do
    addDependentFile file
    content <- runIO $ readFileBS file
    let ls = String.lines $ toString (decodeUtf8 content :: Text)
        moduleName = case [m | line <- ls, "module" : m : _ <- [String.words line]] of
          m : _ -> m
          [] -> ""
        signatures = [(takeWhile identifier line, String.unlines (line : takeWhile continuation rest)) | line : rest <- List.tails ls, not (null line), not (isSpace $ List.head line), "::" `List.isInfixOf` line]
    pure [moduleName <> "." <> name | moduleName /= "Hydra.HeadLogic.Workflow", (name, signature) <- signatures, any (`List.isInfixOf` signature) ["BaseMachineT ", "Workflow tx "]]
  let expected = sort $ qualifiedName <$> registered
  unless (sort found == expected) $
    fail $
      "HeadLogic diagram inventory differs from concrete machines. Missing: " <> show (found List.\\ expected) <> "; stale: " <> show (expected List.\\ found)
 where
  identifier :: Char -> Bool
  identifier c = isAlphaNum c || c == '_' || c == '\''
  continuation :: String -> Bool
  continuation line = null line || isSpace (List.head line)
  sourceFiles :: FilePath -> IO [FilePath]
  sourceFiles directory = do
    paths <- fmap (directory </>) <$> listDirectory directory
    fmap concat $ forM paths $ \path -> do
      isDirectory <- doesDirectoryExist path
      if isDirectory then sourceFiles path else pure [path | takeExtension path == ".hs", dropExtension path /= directory </> "Workflow"]
