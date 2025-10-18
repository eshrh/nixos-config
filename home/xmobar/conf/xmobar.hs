{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import XMonad.Util.Run (runProcessWithInput)
import Xmobar
import TokyoWeather (TokyoWeather(..))
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  batteryOut <- outputOf "cat /sys/class/power_supply/BAT0/capacity"
  let hasBattery = not $ null batteryOut

  mpdOut <- outputOf "pidof mpd"
  let hasMPD = not $ null mpdOut

  args <- getArgs
  let args_chunks = zip args (drop 1 args)
  let screen = fromMaybe "0" $ lookup "-x" args_chunks
  let fgcolor = fromMaybe "#ebdbb2" $ lookup "-F" args_chunks
  let bgcolor = fromMaybe "#1d2021" $ lookup "-B" args_chunks

  xmobar $ config fgcolor bgcolor hasBattery hasMPD (read screen)

outputOf :: String -> IO String
outputOf input = runProcessWithInput prog args ""
  where
    prog = (head . words) input
    args = (tail . words) input

config :: String -> String -> Bool -> Bool -> Int -> Config
config fgcolor bgcolor hasBattery hasMPD screen =
  defaultConfig
    { -- appearance
      font = "monospace 16",
      bgColor = bgcolor,
      fgColor = fgcolor,
      position = OnScreen screen (TopH 40),
      border = BottomB,
      borderColor = fgcolor,
      -- layout
      sepChar = "%",
      alignSep = "}{",
      template =
        "%StdinReader% " ++ (if hasBattery then "/ %battery% " else "")
          ++ "/ %multicpu% / %memory% /}{"
          ++ (if hasMPD then "%mpd% / " else "")
          ++ "%tokyo% %date%",
      -- general behavior
      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      persistent = True,
      commands =
        -- weather monitor
        [
          Run TokyoWeather,
          Run $
            MultiCpu
              [ "--template", "cpu: <total>%"]
              10,
          Run $
            Memory
              [ "--template", "mem: <usedratio>%"
              ]
              10,
          Run $ Date "%b-%d %H:%M" "date" 10,
          Run StdinReader
        ]
          ++ [ Run $
                 BatteryP
                   ["BAT0"]
                   [ "-t", "<acstatus> (<left>%)",
                     "--",
                     "-O", "<fc=#8ec07a>Charging</fc>",
                     "-i", "<fc=#83a598>Idle</fc>",
                     "-o", "<fc=#fb4934>Discharging</fc>",
                     "-a", "notify-send -u critical 'battery low'",
                     "-A", "10"
                   ]
                   600
               | hasBattery
             ]
          ++ [ Run $
                 MPD
                   [ "-t", "<fc=#fabd2f> <artist> - <title> (<album>) <statei> </fc>",
                     "-M", "30",
                     "--",
                     "-P", "▶",
                     "-Z", "⏸",
                     "-S", "⏹",
                     "-p", "6600",
                     "-h", "127.0.0.1"
                   ] 10
               | hasMPD
             ]
    }
