module Lib
    ( neuralNetwork
    , newBrain
    , shuffle
    ) where
import Control.Monad
import System.Random
import Data.List
import Data.Ord
import IrisLib
import qualified Data.Vector as V

-- Box Muller Transformation
-- Pseudo-random number sampling method for generating pairs of independent,
-- standard, normally distributed (zero expectation, unit variance) random numbers
gauss stdev = do
  x1 <- randomIO
  x2 <- randomIO
  return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)

-- Create a new neural network, parameters represent input neurons, middle
-- layers and output neurons
-- [Float] = biases (initialized to 1), [[Float]] = weights (initialized with
-- gauss distribution)
-- initialNetwork :: Int -> Int -> Int -> IO [([Float], [[Float]])]
-- initialNetwork ini mid out =

---
newBrain :: [Int] -> IO [([Float], [[Float]])]
newBrain szs@(_:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts
newBrain _ = error "Neural network couldn't be created: An empty list of parameters was provided"
---

-- initialBiases :: Int -> [Float]
--
--
-- initialLayers :: Int -> [[Float]]

-- Hyperbolic tangent will be used as activation function
activationFunction :: Float -> Float
activationFunction x = tanh x

-- Square function used in activationFunction derivative
square :: Float -> Float
square x = x*x

-- Derivative of the activation activationFunction
-- https://theclevermachine.wordpress.com/2014/09/08/derivation-derivatives-for-common-neural-network-activation-functions/
activationFunction' :: Float -> Float
activationFunction' x = 1 - square (tanh x)

-- Save previous weights, used for backpropagation
zLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
zLayer as (bias, weight_vec) = zipWith (+) bias $ sum . zipWith (*) as <$> weight_vec

-- Feedforward algorithm, information moves to next layers
feed :: [Float] -> [([Float], [[Float]])] -> [Float]
feed = foldl' (((activationFunction <$>) . ) . zLayer)

-- Backpropagation algorithm needs the weights of every neuron, in reverse order
-- this method gives the values (weighted inputs, activations) in reverse order
-- (from last to first layer)
reverseValues :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
reverseValues xv = foldl' (\(avs@(av:_), zs) (bs, wms) -> let
  zs' = zLayer av (bs, wms) in (((activationFunction <$> zs'):avs), (zs':zs))) ([xv], [])

-- any value greater than 1 is the same as 1
dCost a y | y == 1 && a >= y = 0 | otherwise = a - y

-- xv: vector of inputs
-- yv: correct outputs
-- layers: network
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

-- xv: inputs
-- yv: correct outputs
-- layers: network
learn :: [Float] -> [Float] -> [([Float], [[Float]])] -> [([Float], [[Float]])]
learn xv yv layers = let (avs, dvs) = deltas xv yv layers
  in zip (zipWith descend (fst <$> layers) dvs) $
    zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
    (snd <$> layers) avs dvs

--- https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuffle rest
    where
    getIx = getStdRandom $ randomR (1, length lst)
    pickElem n = case splitAt n lst of
        ([], s) -> error $ "failed at index " ++ show n -- should never match
        (r, s)  -> (last r, init r ++ s)

-- TODO: chequear que los parametros esten bien (ej: % entrenamiento no sea <=0 >=1)
neuralNetwork :: [([Float], [[Float]])] -> V.Vector Iris -> Double -> String
neuralNetwork network values training_percentage = let
  trainingSetSize = round (fromIntegral (length values) * training_percentage)
  (trainSamples, testSamples) = V.splitAt trainingSetSize values
  trained_network = (foldl' (\b n -> learn (getValues trainSamples n) (getLabel trainSamples n) network)) network [0.. trainingSetSize - 1]
  test_index = 23
  example = getValues trainSamples 56
  bestOf = fst . maximumBy (comparing snd) . zip [1..3]
  in "best guess: " ++ getLabelName(bestOf $ feed example trained_network) ++ show example ++ " " ++  (getType (trainSamples V.! 56))
