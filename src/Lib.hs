module Lib
    ( neuralNetwork
    ) where
import Control.Monad
import System.Random
import Data.List
import qualified Data.Vector as V

data Iris = Iris
  { sepal_length  :: !Double
  , sepal_width   :: !Double
  , petal_length  :: !Double
  , petal_width   :: !Double
  , iris_type     :: !String
 } deriving (Show, Eq, Read)

gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

-- create a new neural network, parameters represent input neurons, middle
-- layers and output neurons and it returns the weights of the in a two dimensional
-- float array and the biases in a list of floats
-- biases are initialized to 1
newBrain :: [Int] -> IO [([Float], [[Float]])]
-- @ : As-patterns allow you to break up an item according to a pattern, while
--still keeping a reference to the entire original item.
newBrain szs@(_:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts
newBrain _ = error "Neural network couldn't be created: An empty list was provided"

-- hyperbolic tangent will be used as activation function
activationFunction :: Float -> Float
activationFunction x = tanh x

-- square function used in activationFunction derivative
square :: Float -> Float
square x = x*x

-- derivative of the activation activationFunction
-- https://theclevermachine.wordpress.com/2014/09/08/derivation-derivatives-for-common-neural-network-activation-functions/
activationFunction' :: Float -> Float
activationFunction' x = 1 - square (tanh x)

-- save previous weights, used for backpropagation
zLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
zLayer as (bias, weight_vec) = zipWith (+) bias $ sum . zipWith (*) as <$> weight_vec

-- feedforward algorithm, information moves to next layers
feed :: [Float] -> [([Float], [[Float]])] -> [Float]
feed = foldl' (((activationFunction <$>) . ) . zLayer)

-- backpropagation algorithm needs the weights of every neuron, in reverse order
-- this method gives the values (weighted inputs, activations) in reverse order
-- (from last to first layer)
reverseValues :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
reverseValues xv = foldl' (\(avs@(av:_), zs) (bs, wms) -> let
  zs' = zLayer av (bs, wms) in (((activationFunction <$> zs'):avs), (zs':zs))) ([xv], [])

-- any value greater than 1 is the same as 1
dCost a y | y == 1 && a >= y = 0 | otherwise = a - y

-- xv: vector of inputs
-- yv: correct outputs
-- Returns list of (activations, deltas) of each layer in order.
deltas :: [Float] -> [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
deltas xv yv layers = let
  (avs@(av:_), zv:zvs) = reverseValues xv layers
  delta0 = zipWith (*) (zipWith dCost av yv) (activationFunction' <$> zv)
  in (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
      zipWith (*) [(sum $ zipWith (*) row dv) | row <- wm] (activationFunction' <$> zv)

eta = 0.002

descend av dv = zipWith (-) av ((eta *) <$> dv)

-- xv : inputs
-- yv: correct outputs
learn :: [Float] -> [Float] -> [([Float], [[Float]])] -> [([Float], [[Float]])]
learn xv yv layers = let (avs, dvs) = deltas xv yv layers
  in zip (zipWith descend (fst <$> layers) dvs) $
    zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
    (snd <$> layers) avs dvs

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
  [toFloating sepal_length, toFloating sepal_width, toFloating petal_length, toFloating petal_width]

-- Neural network recieves values and the amount of variables, middle layers
-- and output
-- values -> training_percentage -> input_quant -> middle_layers -> output_quantity
-- TODO: obvio que no va a devolver int, ver bien qeu retorna
neuralNetwork :: V.Vector a -> Double -> Int -> Int -> Int -> Int
neuralNetwork values training_percentage input_quant middle_layers output_quantity  = let
  network = newBrain [input_quant, middle_layers, output_quantity]
  trainingSetSize = round (fromIntegral (length values) * training_percentage)
  (testSamples, trainSamples) = V.splitAt trainingSetSize values
  -- bs is a list of trained networks, ordered by amount of training ascending
  -- bs = scanl (foldl' (\b n -> learn (getValues testSamples) (getLabel testSample) network)) network
  -- [[   0.. 999],
  --    [1000..2999],
  --    [3000..5999],
  --    [6000..9999]]
  -- bs = foldl (foldl' (\b n -> learn (getValues testSamples) (getLabel testSample) network)) network [0.. 999999]
  -- trained_brain = last bs
  --
  -- example = getValues head trainSamples
  -- putStrLn $ "best guess: " ++ show (bestOf $ feed example trained_brain)
  in length testSamples