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
import qualified Data.ByteString.Lazy as BS
import Codec.Compression.GZip (decompress)

-- Box Muller Transformation
-- Pseudo-random number sampling method for generating pairs of independent,
-- standard, normally distributed (zero expectation, unit variance) random numbers
gauss :: Float -> IO Float
gauss stdev = do
  x1 <- randomIO
  x2 <- randomIO
  return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)

-- Create a new neural network, parameters represent input neurons, middle
-- layers and output neurons
-- [Float] = biases (initialized to 1), [[Float]] = weights (initialized with
-- gauss distribution)
initializeNeuralNetwork :: [Int] -> IO [([Float], [[Float]])]
initializeNeuralNetwork szs@(_:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) szs ts
initializeNeuralNetwork _ = error "Neural network couldn't be created: An empty list of parameters was provided"

-- Hyperbolic tangent will be used actual_value activation function
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
-- receives a list of actual values, a neural network with weights and biases and returns new values of nodes
-- Adds biases with products of weights and actual values, uses map and sum composition to go layer through layer
previousWeights :: [Float] -> ([Float], [[Float]]) -> [Float]
previousWeights actual_value (bias, weights) = zipWith (+) bias (map (sum.(zipWith (*) actual_value)) weights)

-- FeedForwardforward algorithm, information moves to next layers
-- Use foldl to go through all layers of neural network, applies composition of previous weights with activation function
-- https://stackoverflow.com/questions/20279306/what-does-f-g-mean-in-haskell
feedForward :: [Float] -> [([Float], [[Float]])] -> [Float]
feedForward = foldl (((map activationFunction) . ) . previousWeights)

-- Backpropagation algorithm needs the weights of every neuron, in reverse order
-- this method gives the values (activations, weighted inputs) in reverse order
-- (from last to first layer)
reverseValues :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
reverseValues input = foldl (\(activation_values@(av:_), prevW) (bias, weights) -> let
  prevW' = previousWeights av (bias, weights) in (((map activationFunction prevW'):activation_values), (prevW':prevW))) ([input], [])

-- any value greater than 1 is the same as 1
-- Basically returns if a neuron has been activated or not
perceptronActivation :: Float -> Float -> Float
perceptronActivation a y | y == 1 && a >= y = 0 | otherwise = a - y

-- input: vector of inputs
-- output: correct outputs
-- layers: network
-- Returns list of (activations, deltas) of each layer in order.
deltas :: [Float] -> [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
deltas input output layers = let
  (activation_values@(av:_), zv:zvs) = reverseValues input layers
  delta0 = zipWith (*) (zipWith perceptronActivation av output) (map activationFunction' zv)
  in (reverse activation_values, f (fmap (transpose . snd)(reverse layers)) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs ((:dvs)
      (zipWith (*) [(sum (zipWith (*) row dv)) | row <- wm] (map activationFunction' zv)))

-- Learning rate
eta = 0.07

-- Recieves a list of activation values and delta values and returns the substraction of the activation
-- values with the product of the delta_values multiplied the learning rate
descend :: [Float] -> [Float] -> [Float]
descend activation_values delta_value = zipWith (-) activation_values (map (eta *) delta_value)

-- This function trains a method with one sample, it receives one set of values and the corresponding label,
-- the starting neural network and returns the trained network
train :: [Float] -> [Float] -> [([Float], [[Float]])] -> [([Float], [[Float]])]
train input output layers = let
  (activation_values, delta_values) = deltas input output layers :: ([[Float]], [[Float]])
    in zip (zipWith descend (map fst layers) delta_values)
      (zipWith3 (\weight_values activation_value delta_value ->
        zipWith (\wv d -> descend wv (map (d*) activation_value)) weight_values delta_value)
        (map snd layers) activation_values delta_values)

-- bestOf gets the output neuron with the biggest value
bestOf = fst . maximumBy (comparing snd) . zip [0..]

-- Return a string with the score of each category represented by +
score_representation :: Int -> Float -> String
score_representation category_number score = show category_number ++ ": " ++ replicate (round (70 * min 1 score)) '+'

-- Run several iterations to train network with the same training values
train_iris_network :: Int -> [Iris] -> [[([Float], [[Float]])]] -> Int -> [[([Float], [[Float]])]]
train_iris_network iteration train_samples network_seq training_set_size
  | iteration == 0  = network_seq
  | otherwise = train_iris_network it train_samples bs training_set_size
  where
    network = last network_seq
    it = iteration - 1
    -- use scanl to save all the middle networks and show progress
    bs = scanl (foldl' (\b n -> train (getValues train_samples n) (getLabel train_samples n) b)) network [
     [0..20],
     [20..30],
     [30..50],
     [50..training_set_size-1]] :: [[([Float], [[Float]])]]

-- Main method for iris neural network
irisNeuralNetwork :: [Iris] -> Double -> Int -> IO()
irisNeuralNetwork values training_percentage iterations = do
  let
    training_set_size = round (fromIntegral (length values) * training_percentage)
    testing_set_size = (length values) - training_set_size
    (train_samples, test_samples) = splitAt training_set_size values
  network <- initializeNeuralNetwork [4, 5, 3]
  -- Select a random example out of the test samples and print the real type
  random_sample_index <- (`mod` testing_set_size) <$> randomIO
  putStr "The chosen example is of type: "
  print (getLabelName(getLabelNumber (test_samples !! random_sample_index)))
  let
    -- train the network for iterations
    network_seq = train_iris_network iterations train_samples [network] training_set_size
    -- pick the most trained network
    trained_network = last network_seq
    example = getValues test_samples random_sample_index
  forM_ network_seq (putStrLn . unlines . zipWith score_representation [0..2] . feedForward example)
  putStr "Best guess: "
  print (getLabelName(bestOf(feedForward example trained_network)))
  let
    -- Test all the test images to find percentage of correctly classified images
   guesses = bestOf . (\n -> feedForward (getValues test_samples n) trained_network) <$> [0..(testing_set_size - 1)]
   answers = (getLabelNumberVec test_samples)
  putStrLn (show (sum (fromEnum <$> zipWith (==) guesses answers)) ++ " / " ++ show(testing_set_size))

-- Main method for digits neural network
digitsNeuralNetwork :: IO()
digitsNeuralNetwork = do
  [train_images, train_labels, test_images, test_labels] <- mapM ((decompress  <$>) . BS.readFile)
    [ "./digits/train-images-idx3-ubyte.gz"
    , "./digits/train-labels-idx1-ubyte.gz"
    , "./digits/t10k-images-idx3-ubyte.gz"
    , "./digits/t10k-labels-idx1-ubyte.gz"
    ]
  initial_network <- initializeNeuralNetwork [mnistFeatures, 15, mnistLabels] -- 15 neurons in middle layer
  random_image_index <- (`mod` 10000) <$> randomIO -- chooses a random image of the test images (10000 test images)
  -- draws render of chosen image and prints real digit value
  putStr . unlines $
    take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage test_images random_image_index)
  putStr "The actual digit is: "
  print(getLabelDig test_labels random_image_index)
  let
    -- Train neural network
    -- use scanl to save all the middle networks and show progress
    bs = scanl (foldl' (\b n -> train (getNormalizedImage train_images n) (getY train_labels n) b)) initial_network [
     [0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..10000]] -- Change last parameter, the larger (<60000) the more precise, but might take too long
    -- Select the most trained brain
    trained_brain = last bs
    -- Test the randomly selected image with the neural network
    example = getNormalizedImage test_images random_image_index
  -- Draw progress of each neural network
  forM_ bs $ putStrLn . unlines . zipWith score_representation [0..9] . feedForward example
  putStrLn $ "best guess: " ++ show (bestOf $ feedForward example trained_brain)
  -- Test all the test images to find percentage of correctly classified images
  let guesses = bestOf . (\n -> feedForward (getNormalizedImage test_images n) trained_brain) <$> [0..9999]
  let answers = getLabelDig test_labels <$> [0..9999]
  putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++ " / 10000"
