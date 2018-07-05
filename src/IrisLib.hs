module IrisLib
    (
    Iris (..)
    , printIris
    , getLabelAux
    , getLabel
    , getValues
    ) where

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

-- transform label in iris data type to int value
getLabelAux :: Iris -> Int
getLabelAux (Iris _ _ _ _ iris_type) =
  case iris_type of
    "Iris-setosa" -> 1
    "Iris-versicolor" -> 2
    "Iris-virginica" -> 3
    _ -> error "Not a valid iris type"

-- returns labels as a float array for iris type
getLabel :: Iris -> [Float]
getLabel iris = fromIntegral . fromEnum . (getLabelAux iris ==) <$> [1..3]

-- get float array with iris attributes
getValues :: Iris -> [Float]
getValues (Iris sepal_length sepal_width petal_length petal_width _) =
  [sepal_length, sepal_width, petal_length, petal_width]
