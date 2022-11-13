#!/usr/bin/env stack
-- stack runhaskell --package regex-tdfa --package process  --

-- massage sys_times to output graphable data
-- Input format:
-- 1667463778| 48   5  47   0   0|1269M   13G   38M  951M|2544k   36k|   0  1463k|   0     9
-- Output format (tab-separated):
-- timestamp CPU Mem(GB) Net Read (MB/s) Disk Write (MB/s)
-- 1667466538      0.48    1.549   2.465   1.368

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiWayIf #-}

import Debug.Trace(trace)
import System.IO
import Text.Regex.TDFA
import Control.Monad
import Data.List(intersperse, isSuffixOf)

main :: IO ()
main =  do
  hSetBuffering stdout NoBuffering
  forever $ hGetLine stdin >>= processStat stdout
 where
  processStat :: Handle -> String -> IO ()
  processStat file stat =
    case getAllTextMatches (stat =~ ("([0-9]+)(B|k|M|G)?" :: String)) :: [String] of
      -- --epoch--- --total-cpu-usage-- ------memory-usage----- -net/total- -dsk/total- --io/total-
      -- epoch   |usr sys idl wai stl| used  free  buff  cach| recv  send| read  writ| read  writ
      -- 1667980233| 19   1  80   0   0| 348M 4268M   89M   11G|   0     0 |6796M   11G|  51k   57k
      m@[ts , cpu, _, _, _ , _ , mem, _, _, _, netR, _ , diskW, _, _, _] -> do
        let cpuUsage = show ((read cpu :: Double) / 100)
            memUsage = show $ toGiga mem / mAX_MEM
            networkRead = show $ toMega netR
            diskWrite = show $ toMega diskW
        hPutStrLn file $ concat $ intersperse "\t" [ ts, cpuUsage, memUsage, networkRead, diskWrite ]
      -- ignore non matching lines, probably headers or garbage
      m -> pure ()

-- Maximum memory available in GB
mAX_MEM :: Double
mAX_MEM = 16

toGiga :: String -> Double
toGiga num =
  let len = length num
      raw = read $ take (len - 1) num
  in  if
    | "B" `isSuffixOf` num -> raw / 1_000_000_000
    | "k" `isSuffixOf` num -> raw / 1_000_000
    | "M" `isSuffixOf` num -> raw / 1_000
    | "G" `isSuffixOf` num -> raw
    | otherwise -> error $ "Unable to parse bytes dimension: " <>  num

toMega :: String -> Double
toMega num =
  let len = length num
      raw = read $ take (len - 1) num
  in  if
    | "B" `isSuffixOf` num -> raw / 1_000_000
    | "k" `isSuffixOf` num -> raw / 1_000
    | "M" `isSuffixOf` num -> raw
    | "G" `isSuffixOf` num -> raw * 1_000
    | otherwise -> 0
