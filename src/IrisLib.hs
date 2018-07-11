module IrisLib
    (
    Iris (..)
    , printIris
    , getLabel
    , getValues
    , getValuesAux
    , getLabelName
    , getLabelNumber
    , getLabelNumberVec
    , getType
    ) where

import qualified Data.Vector as V

data Iris = Iris
  { sepal_length  :: !Float
  , sepal_width   :: !Float
  , petal_length  :: !Float
  , petal_width   :: !Float
  , iris_type     :: !String
 } deriving (Show, Eq, Read)

-- Print iris attribute values
printIris :: Iris -> IO ()
printIris r  = putStrLn $  show (sepal_length r)  ++ " " ++ show (sepal_width r) ++ " "
    ++ show(petal_length r) ++ " " ++ show(petal_length r) ++ " " ++ show(iris_type r)

-- Transform label in iris data type to int value
getLabelNumber :: Iris -> Int
getLabelNumber (Iris _ _ _ _ iris_type) =
  case iris_type of
    "Iris-setosa" -> 1
    "Iris-versicolor" -> 2
    "Iris-virginica" -> 3
    _ -> error "Not a valid iris type"

getLabelNumberVec :: [Iris] -> [Int]
getLabelNumberVec list = map getLabelNumber list

-- Transform label number into string in order to show
getLabelName :: Int -> String
getLabelName nr =
  case nr of
    1 -> "Iris setosa"
    2 -> "Iris versicolor"
    3 -> "Iris virginica"

-- Get labels as a float array for iris type from a vector accessing through an index
getLabel :: [Iris] -> Int -> [Float]
getLabel iris_vector index = fromIntegral . fromEnum . (getLabelNumber iris ==) <$> [1..3]
  where iris = iris_vector !! index

-- Get float array with iris attributes for one particular iris
getValuesAux :: Iris -> [Float]
getValuesAux (Iris sepal_length sepal_width petal_length petal_width _) =
  [sepal_length, sepal_width, petal_length, petal_width]

-- TODO : NORMALIZAR LOS DATOS OJO CON ESTO!!!!!
-- Get float array with iris attributes for one particular iris for one particular iris
-- from a vector accessing through an index
getValues :: [Iris] -> Int -> [Float]
getValues iris_vector index = getValuesAux iris
  where iris = iris_vector !! index

getType :: Iris -> String
getType (Iris _ _ _ _ iris_type) = iris_type
