{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Lib
import IrisLib

instance FromNamedRecord Iris where
  parseNamedRecord r =
    Iris
      <$> r .: "sepal_length"
      <*> r .: "sepal_width"
      <*> r .: "petal_length"
      <*> r .: "petal_width"
      <*> r .: "iris_type"

main :: IO ()
main = do
  -- Load file
  csvData <- BS.readFile "./iris/test-iris.csv"
  case decodeByName csvData :: Either String (Header, V.Vector Iris) of
    Left err -> putStrLn err
    -- Call neural network with parsed values
    Right (_, iris) -> do
      network <- newBrain [4, 3, 3]
      -- TODO : mejorar esta forma kbeza de shufflear
      shuffledArray <- shuffle (V.toList iris)
      print (neuralNetwork network (V.fromList shuffledArray) 0.9)
