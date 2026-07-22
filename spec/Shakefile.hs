import Control.Monad (forM_)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles = "_build"} $ do
  want ["_build/hydra-spec" <.> "pdf"]

  phony "clean" $ do
    putInfo "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  "_build/hydra-spec" <.> "pdf" %> \out -> do
    assets <- getDirectoryFiles "src" ["//*.sty", "Hydra/Protocol/Figures/*.svg", "//*.bib", "//*.ttf"]
    need ["_build/latex" </> c | c <- assets]

    srcs <- getDirectoryFiles "src" ["//*.lagda", "//*.tex"]
    need ["_build/latex" </> c -<.> "tex" | c <- srcs]

    cmd_ (Cwd "_build/latex") "latexmk -xelatex -shell-escape -halt-on-error Hydra/Protocol/Main.tex"
    cmd_ "cp _build/latex/Main.pdf _build/hydra-spec.pdf"

  -- Copy assets
  forM_ ["sty", "svg", "bib", "ttf"] $ \ext ->
    ("_build/latex//*." <> ext) %> \out -> do
      let src = "src" </> dropDirectory1 (dropDirectory1 out)
      copyFile' src out

  -- Copy or compile from lagda files
  "_build/latex//*.tex" %> \out -> do
    let src = "src" </> dropDirectory1 (dropDirectory1 out)
    b <- doesFileExist src
    if b
      then do
        need [src]
        copyFile' src out
      else do
        let src = "src" </> dropDirectory1 (dropDirectory1 (out -<.> "lagda"))
        need [src]
        cmd_ $ "agda --transliterate --latex --latex-dir _build/latex " <> src
