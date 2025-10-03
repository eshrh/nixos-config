{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TokyoWeather (TokyoWeather (..)) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as B
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Data.Time (LocalTime, UTCTime)
import Data.Vector qualified as V
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (simpleHttp)
import Text.Printf (printf)
import Xmobar

data WeatherData = WeatherData
  { time :: UTCTime,
    area :: String,
    code :: Int,
    weather :: String,
    winds :: String,
    waves :: String,
    pops :: Int,
    temps :: (Int, Int)
  }
  deriving (Show, Generic)

withFirstObject :: String -> (Object -> Parser a) -> Value -> Parser a
withFirstObject label f =
  withArray (label ++ "Array") $ \arr ->
    case V.toList arr of
      (x : _) -> withObject label f x
      [] -> fail $ "empty " ++ label ++ " array"

instance FromJSON WeatherData where
  parseJSON = withFirstObject "RootArray" $ \o -> do
    timeSeries <- o .: "timeSeries" :: Parser [Object]

    let wObj = head timeSeries
    wTimes <- (wObj .: "timeDefines") :: Parser [UTCTime]
    wAreas <- wObj .: "areas"
    tokyoW <- findArea "東京" wAreas
    wCode <- tokyoW .: "weatherCodes" >>= parseIndex 0
    wDescr <- tokyoW .: "weathers" >>= parseIndex 0
    wWind <- tokyoW .: "winds" >>= parseIndex 0
    wWave <- tokyoW .: "waves" >>= parseIndex 0

    let pObj = timeSeries !! 1
    pAreas <- pObj .: "areas"
    tokyoP <- findArea "東京" pAreas
    pVals <- tokyoP .: "pops"

    let tObj = timeSeries !! 2
    tAreas <- tObj .: "areas"
    tokyoT <- findArea "東京" tAreas
    tVals <- tokyoT .: "temps"
    let temps' = (read (head tVals), read (tVals !! 1))

    return
      WeatherData
        { time = head wTimes,
          area = "東京地方",
          code = read wCode,
          weather = wDescr,
          winds = wWind,
          waves = wWave,
          pops = read (head pVals),
          temps = temps'
        }

findArea :: T.Text -> [Value] -> Parser Object
findArea name arr = do
  let matches = flip map arr $ \v -> do
        o <- parseJSON v
        areaObj <- o .: "area"
        n <- areaObj .: "name"
        if name `T.isPrefixOf` n then pure o else fail "not match"
  head matches

parseIndex :: Int -> [String] -> Parser String
parseIndex i xs =
  if i < length xs then pure (xs !! i) else fail "index out of range"

getJSON :: IO B.ByteString
getJSON =
  simpleHttp
    "https://www.jma.go.jp/bosai/forecast/data/forecast/130000.json"

getData :: IO String
getData = do
  j <- getJSON
  let d = (decode j :: Maybe WeatherData)
  return $ maybe "Failure" printer d

tempPrinter :: (Int, Int) -> String
tempPrinter (l, h) = show l ++ "-" ++ show h ++ "°c"

addColor :: String -> String -> String
addColor color string = "<fc=" ++ color ++ ">" ++ string ++ "</fc>"

popsPrinter :: Int -> String
popsPrinter p = (if p > 40 then addColor "#6bd7ff" else id) $ show p ++ "%"

printer :: WeatherData -> String
printer w = weather w ++ " / " ++ winds w ++ " / " ++ popsPrinter (pops w) ++ " / " ++ tempPrinter (temps w) ++ " /"

data TokyoWeather = TokyoWeather
  deriving (Read, Show)

instance Exec TokyoWeather where
  alias TokyoWeather = "tokyo"
  run TokyoWeather = getData
  rate TokyoWeather = 36000
