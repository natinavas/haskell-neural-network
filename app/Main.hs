-- Used for cassava
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Lib
import NeuralLib
import IrisLib
import DigitsLib
import Codec.Compression.GZip (decompress)
import System.Random

main :: IO ()
main = do
  print "Which data set would you like to use?"
  print " 1. Iris"
  print " 2. Digits (MNIST)"
  optn <- getLine
  case optn of
    "1" -> irisMain
    "2" -> digitsMain
    _ -> do
      print "You did not select a valid option, please select 1 or 2."
      main

instance FromNamedRecord Iris where
  parseNamedRecord r =
    Iris
      <$> r .: "sepal_length"
      <*> r .: "sepal_width"
      <*> r .: "petal_length"
      <*> r .: "petal_width"
      <*> r .: "iris_type"

irisMain :: IO ()
irisMain = do
  -- Load file
  csvData <- BS.readFile "./iris/test-iris.csv"
  case decodeByName csvData :: Either String (Header, V.Vector Iris) of
    -- Print error
    Left err -> putStrLn err
    -- Call neural network with parsed values
    Right (_, iris) -> do
      -- TODO : mejorar esta forma kbeza de shufflear
      shuffledArray <- shuffle (V.toList iris)
      irisNeuralNetwork (V.fromList shuffledArray) 0.65

render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

-- https://www.tensorflow.org/versions/r1.0/get_started/mnist/beginners
digitsMain :: IO()
digitsMain = do
  -- http://yann.lecun.com/exdb/mnist/
  digitsNeuralNetwork
