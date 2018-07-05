{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Lib

data Iris = Iris
  { sepal_length  :: !Double
  , sepal_width   :: !Double
  , petal_length  :: !Double
  , petal_width   :: !Double
  , iris_type     :: !String
 } deriving (Show, Eq, Read)

instance FromNamedRecord Iris where
  parseNamedRecord r =
    Iris
      <$> r .: "sepal_length"
      <*> r .: "sepal_width"
      <*> r .: "petal_length"
      <*> r .: "petal_width"
      <*> r .: "iris_type"

-- Print iris attribute values
printIris :: Iris -> IO ()
printIris r  = putStrLn $  show (sepal_length r)  ++ " " ++ show (sepal_width r) ++ " "
   ++ show(petal_length r) ++ " " ++ show(petal_length r) ++ " " ++ show(iris_type r)


main :: IO ()
main = do
  -- Load file
  csvData <- BS.readFile "./iris/test-iris.csv"
  case decodeByName csvData :: Either String (Header, V.Vector Iris) of
    Left err -> putStrLn err
    -- Call neural network with parsed values
    --TODO : mezclar bien los datos de entrenamiento y testeo porque sino no
    -- agarra nunca todos los tipos
    Right (_, iris) -> print (neuralNetwork iris 1.0 4 1 3)
