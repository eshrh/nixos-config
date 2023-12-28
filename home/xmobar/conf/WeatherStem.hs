{-# LANGUAGE DeriveGeneric #-}
module WeatherStem (WeatherStem(..)) where

import Xmobar
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Data.Scientific (Scientific, toRealFloat)
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe)

import qualified Data.Map as M
import Data.Map (Map)

import Text.Printf (printf)

data WeatherData = WeatherData
  { value :: Value,
    sensor_name :: T.Text
  }
  deriving (Show, Generic)

newtype Records = Records {records :: [WeatherData]}
  deriving (Show, Generic)

instance FromJSON WeatherData
instance FromJSON Records

getJSON :: IO B.ByteString
getJSON =
  simpleHttp
    "http://cdn.weatherstem.com/dashboard/data/dynamic/model/gatech/stadium/latest.json"

getData :: IO (Map String Double)
getData = do
  j <- getJSON
  return $ transformWeather (decode j :: Maybe Records)


transformWeather' :: Records -> Map String Double
transformWeather' r =
  M.fromList $
    mapMaybe
      ( \x ->
          case value x of
            Number n -> Just ((T.unpack . sensor_name) x, toRealFloat n)
            _ -> Nothing
      )
      (records r)

transformWeather :: Maybe Records -> Map String Double
transformWeather = maybe M.empty transformWeather'

tempPrinter :: Double -> String
tempPrinter f = printf "%.2g°c" ((f - 32) / 1.8)

windSpeedPrinter :: Double -> String
windSpeedPrinter x = printf "%.2g m/s" (0.44704 * x)

dataLookup :: String -> (Double -> String) -> Map String Double -> String
dataLookup k f = maybe "" f . M.lookup k

weatherStemOutput :: IO String
weatherStemOutput = do
  datamap <- getData
  let temp = dataLookup "Thermometer" tempPrinter datamap
  let hum =  dataLookup "Hygrometer" show datamap
  let wind = dataLookup "Anemometer" windSpeedPrinter datamap
  return $ temp ++ " / " ++ hum ++ "% / " ++ wind

data WeatherStem = WeatherStem
  deriving (Read, Show)

instance Exec WeatherStem where
  alias WeatherStem = "ws"
  run WeatherStem = weatherStemOutput
  rate WeatherStem = 36000
