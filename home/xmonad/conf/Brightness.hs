module Brightness
  ( setBrightnessPercent,
    adjustBrightnessPercent,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as B
import Data.Char (isSpace)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Ord (clamp)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import XMonad
import XMonad.Util.Run (safeSpawn)

type BrightnessM = ExceptT String IO

backlightRoot :: FilePath
backlightRoot = "/sys/class/backlight"

preferredBacklights :: [String]
preferredBacklights =
  [ "intel_backlight",
    "acpi_video0",
    "amdgpu_bl0"
  ]

data Backlight where
  Backlight :: {backlightName :: String,
                 brightnessPath :: FilePath,
                 actualBrightnessPath :: FilePath,
                 maxBrightnessPath :: FilePath} ->
               Backlight

setBrightnessPercent :: Int -> X ()
setBrightnessPercent percent = runBrightness $ do
  backlight <- discoverBacklight
  maxBrightness <- readInt (> 0) "not positive" (maxBrightnessPath backlight)
  let value = clamp (0, maxBrightness) $ percentStep maxBrightness (clamp (0, 100) percent)
  writeBrightness backlight value

adjustBrightnessPercent :: Int -> X ()
adjustBrightnessPercent percent = runBrightness $ do
  backlight <- discoverBacklight
  maxBrightness <- readInt (> 0) "not positive" (maxBrightnessPath backlight)
  currentBrightness <- readCurrentBrightness backlight
  let delta = percentStep maxBrightness percent
      value = clamp (0, maxBrightness) (currentBrightness + delta)
  writeBrightness backlight value

runBrightness :: BrightnessM () -> X ()
runBrightness action = io (runExceptT action) >>= either warn pure

discoverBacklight :: BrightnessM Backlight
discoverBacklight = do
  names <- ExceptT $ first (showPathError backlightRoot) <$> try (listDirectory backlightRoot)
  let candidates = orderedCandidates names
  valid <- liftIO (filterM hasBrightnessFiles candidates)
  maybe
    (throwE $ "No usable backlight device found under " ++ backlightRoot ++ ". Tried " ++ show candidates ++ ".")
    (return . mkBacklight)
    (listToMaybe valid)

orderedCandidates :: [String] -> [String]
orderedCandidates names =
  preferredBacklights ++ filter (`notElem` preferredBacklights) (sort names)

mkBacklight :: String -> Backlight
mkBacklight name =
  Backlight
    { backlightName = name,
      brightnessPath = devicePath </> "brightness",
      actualBrightnessPath = devicePath </> "actual_brightness",
      maxBrightnessPath = devicePath </> "max_brightness"
    }
  where
    devicePath = backlightRoot </> name

hasBrightnessFiles :: String -> IO Bool
hasBrightnessFiles name = do
  let backlight = mkBacklight name
  brightnessExists <- doesFileExist (brightnessPath backlight)
  maxExists <- doesFileExist (maxBrightnessPath backlight)
  pure (brightnessExists && maxExists)

readCurrentBrightness :: Backlight -> BrightnessM Int
readCurrentBrightness backlight = do
  exists <- liftIO (doesFileExist (actualBrightnessPath backlight))
  readInt (>= 0) "negative" $
    if exists
      then actualBrightnessPath backlight
      else brightnessPath backlight

readInt :: (Int -> Bool) -> String -> FilePath -> BrightnessM Int
readInt valid err path = do
  contents <- ExceptT $ first (showPathError path) <$> try (B.readFile path)
  case B.readInt $ B.dropWhile isSpace contents of
    Nothing -> throwE (path ++ ": invalid integer")
    Just (value, _)
      | valid value -> pure value
      | otherwise -> throwE (path ++ ": " ++ err ++ ": " ++ show value)

writeBrightness :: Backlight -> Int -> BrightnessM ()
writeBrightness backlight value =
  ExceptT $
    first (showPathError (brightnessPath backlight))
      <$> try (writeFile (brightnessPath backlight) (show value ++ "\n"))

showPathError :: FilePath -> IOException -> String
showPathError path err = path ++ ": " ++ show err

percentStep :: Int -> Int -> Int
percentStep maxBrightness percent
  | percent == 0 = 0
  | otherwise = signum percent * max 1 ((maxBrightness * abs percent + 50) `div` 100)

warn :: String -> X ()
warn message = do
  io $ hPutStrLn stderr ("xmonad brightness: " ++ message)
  safeSpawn
    "notify-send"
    [ "-a",
      "xmonad",
      "-u",
      "normal",
      "Brightness",
      message
    ]
