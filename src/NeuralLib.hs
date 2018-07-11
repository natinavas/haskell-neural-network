module NeuralLib
    ( irisNeuralNetwork
    , digitsNeuralNetwork
    , initializeNeuralNetwork
    ) where
import Control.Monad
import System.Random
import Data.List
import Data.Ord
import IrisLib
import DigitsLib
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip (decompress)

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
-- TODO: https://www.quora.com/What-are-good-initial-weights-in-a-neural-network
-- TODO: http://andyljones.tumblr.com/post/110998971763/an-explanation-of-xavier-initialization
initializeNeuralNetwork :: [Int] -> IO [([Float], [[Float]])]
initializeNeuralNetwork szs@(_:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts
initializeNeuralNetwork _ = error "Neural network couldn't be created: An empty list of parameters was provided"

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
previousWeights :: [Float] -> ([Float], [[Float]]) -> [Float]
previousWeights as (bias, weight_vec) = zipWith (+) bias $ sum . zipWith (*) as <$> weight_vec

-- feedForwardforward algorithm, information moves to next layers
feedForward :: [Float] -> [([Float], [[Float]])] -> [Float]
feedForward = foldl' (((activationFunction <$>) . ) . previousWeights)

-- Backpropagation algorithm needs the weights of every neuron, in reverse order
-- this method gives the values (weighted inputs, activations) in reverse order
-- (from last to first layer)
reverseValues :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
reverseValues input = foldl' (\(avs@(av:_), zs) (bs, wms) -> let
  zs' = previousWeights av (bs, wms) in (((activationFunction <$> zs'):avs), (zs':zs))) ([input], [])

-- any value greater than 1 is the same as 1
dCost a y | y == 1 && a >= y = 0 | otherwise = a - y

-- input: vector of inputs
-- output: correct outputs
-- layers: network
-- Returns list of (activations, deltas) of each layer in order.
deltas :: [Float] -> [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
deltas input output layers = let
  (avs@(av:_), zv:zvs) = reverseValues input layers
  delta0 = zipWith (*) (zipWith dCost av output) (activationFunction' <$> zv)
  in (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
      zipWith (*) [(sum $ zipWith (*) row dv) | row <- wm] (activationFunction' <$> zv)

-- Learning rate
eta = 0.07

descend av dv = zipWith (-) av ((eta *) <$> dv)

-- input: inputs
-- output: correct outputs
-- layers: network
learn :: [Float] -> [Float] -> [([Float], [[Float]])] -> [([Float], [[Float]])]
learn input output layers = let (avs, dvs) = deltas input output layers
  in zip (zipWith descend (fst <$> layers) dvs) $
    zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
    (snd <$> layers) avs dvs

-- Run several iterations to train network with the same training values
train_iris_network :: Int -> V.Vector Iris -> [[([Float], [[Float]])]] -> Int -> [[([Float], [[Float]])]]
train_iris_network iteration train_samples network_seq training_set_size
  | iteration == 0  = network_seq
  | otherwise = train_iris_network it train_samples bs training_set_size
  where
    network = last network_seq
    it = iteration - 1
    bs = scanl (foldl' (\b n -> learn (getValues train_samples n) (getLabel train_samples n) b)) network [
     [0..20],
     [20..30],
     [30..50],
     [50..training_set_size-1]] :: [[([Float], [[Float]])]]

-- TODO: METER TODO EN UN NEURAL NETWORK ESTO ES MUY KBEZA
-- TODO : ver de arreglar la cantidad de dos y lets que estan mezclados cualquieramente
irisNeuralNetwork :: V.Vector Iris -> Double -> IO()
irisNeuralNetwork values training_percentage = do
  network <- initializeNeuralNetwork [4, 5, 3]
  let
    training_set_size = round (fromIntegral (length values) * training_percentage)
    testing_set_size = (V.length values) - training_set_size
    (train_samples, test_samples) = V.splitAt training_set_size values
    -- bestOf gets the output neuron with the biggest value
    bestOf = fst . maximumBy (comparing snd) . zip [1..]
  -- Select a random example out of the test samples
  random_sample_index <- (`mod` testing_set_size) <$> randomIO
  putStr "The chosen example is of type: "
  print (getLabelName(getLabelNumber (test_samples V.! random_sample_index)))
  print (getLabelNumber (test_samples V.! random_sample_index))
  let
    network_seq = train_iris_network 100 train_samples [network] training_set_size
    trained_network = last network_seq
    cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
    example = getValues test_samples random_sample_index
  forM_ network_seq $ putStrLn . unlines . zipWith cute [1..3] . feedForward example
  putStr "Best guess: "
  print (getLabelName(bestOf $ feedForward example trained_network))
  let
   guesses = bestOf . (\n -> feedForward (getValues test_samples n) trained_network) <$> [0..(testing_set_size - 1)]
   answers = (getLabelNumberVec test_samples) -- <$> [0..((length test_samples) - 1)]
  print (guesses)
  print (answers)
  putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++ " / " ++ show(testing_set_size)

digitsNeuralNetwork :: IO()
digitsNeuralNetwork = do
  [train_images, train_labels, test_images, test_labels] <- mapM ((decompress  <$>) . BS.readFile)
    [ "./digits/train-images-idx3-ubyte.gz"
    , "./digits/train-labels-idx1-ubyte.gz"
    , "./digits/t10k-images-idx3-ubyte.gz"
    , "./digits/t10k-labels-idx1-ubyte.gz"
    ]
  initial_network <- initializeNeuralNetwork [mnistFeatures, 30, mnistLabels] -- 30 neurons in middle layer
  random_image_index <- (`mod` 10000) <$> randomIO -- chooses a random image of the test images (10000 test images)
  putStr . unlines $ -- draws chosen image
    take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage test_images random_image_index)
  print(getY test_labels random_image_index)
  putStr "The actual digit is: "
  print(getLabelDig test_labels random_image_index)
  let
    -- Train neural network
    -- use scanl to save all the middle networks and show progress
    bs = scanl (foldl' (\b n -> learn (getNormalizedImage train_images n) (getY train_labels n) b)) initial_network [
     [0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..20000]] -- Change last parameter, the larger (<60000) the more precise, but might take too long
    -- Select the most trained brain
    trained_brain = last bs
    -- Test the randomly selected image with the neural network
    example = getNormalizedImage test_images random_image_index
    cute d score = show d ++ ": " ++ replicate (round $ 100 * min 1 score) '+'
    bestOf = fst . maximumBy (comparing snd) . zip [0..]
  -- Draw progress of each neural network
  forM_ bs $ putStrLn . unlines . zipWith cute [0..9] . feedForward example
  putStrLn $ "best guess: " ++ show (bestOf $ feedForward example trained_brain)
  -- Test all the test images to find porcentage of correctly classified images
  let guesses = bestOf . (\n -> feedForward (getNormalizedImage test_images n) trained_brain) <$> [0..9999]
  let answers = getLabelDig test_labels <$> [0..9999]
  putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++ " / 10000"
