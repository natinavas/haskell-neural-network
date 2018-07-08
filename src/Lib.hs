module Lib
    ( irisNeuralNetwork
    , digitsNeuralNetwork
    , initializeNeuralNetwork
    , shuffle
    ) where
import Control.Monad
import System.Random
import Data.List
import Data.Ord
import IrisLib
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
reverseValues xv = foldl' (\(avs@(av:_), zs) (bs, wms) -> let
  zs' = previousWeights av (bs, wms) in (((activationFunction <$> zs'):avs), (zs':zs))) ([xv], [])

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

-- TODO: METER TODO EN UN NEURAL NETWORK ESTO ES MUY KBEZA

-- TODO: chequear que los parametros esten bien (ej: % entrenamiento no sea <=0 >=1)
irisNeuralNetwork :: [([Float], [[Float]])] -> V.Vector Iris -> Double -> String
irisNeuralNetwork network values training_percentage = let
  trainingSetSize = round (fromIntegral (length values) * training_percentage)
  (trainSamples, testSamples) = V.splitAt trainingSetSize values
  trained_network = (foldl' (\b n -> learn (getValues trainSamples n) (getLabel trainSamples n) network)) network [0.. trainingSetSize - 1]
  test_index = 2
  example = getValues trainSamples 56
  -- bestOf gets the output neuron with the biggest value
  bestOf = fst . maximumBy (comparing snd) . zip [1..]

  guesses = bestOf . (\n -> feedForward (getValues testSamples n) trained_network) <$> [0..((length testSamples) - 1)]
  answers = getLabelNumberVec testSamples -- <$> [0..((length testSamples) - 1)]
  in show(trained_network) ++ show(network) ++ show(guesses)
--  in show (sum $ fromEnum <$> zipWith (==) guesses answers)
--  in "best guess: " ++ getLabelName(bestOf $ feedForward example trained_network) ++ show example ++ " " ++  (getType (trainSamples V.! 56))

getImage s n = fromIntegral . BS.index s . (n*28^2 + 16 +) <$> [0..28^2 - 1]
getX     s n = (/ 256) <$> getImage s n
getLabelDig s n = fromIntegral $ BS.index s (n + 8)
getY     s n = fromIntegral . fromEnum . (getLabelDig s n ==) <$> [0..9]

render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)


digitsNeuralNetwork :: IO()
digitsNeuralNetwork = do
  [trainI, trainL, testI, testL] <- mapM ((decompress  <$>) . BS.readFile)
    [ "digits/train-images-idx3-ubyte.gz"
    , "digits/train-labels-idx1-ubyte.gz"
    , "digits/t10k-images-idx3-ubyte.gz"
    , "digits/t10k-labels-idx1-ubyte.gz"
    ]
  b <- initializeNeuralNetwork [784, 30, 10]
  n <- (`mod` 10000) <$> randomIO
  putStr . unlines $
    take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage testI n)

  let
    example = getX testI n
    bs = scanl (foldl' (\b n -> learn (getX trainI n) (getY trainL n) b)) b [
     [   0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..9999]]
    smart = last bs
    cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
    bestOf = fst . maximumBy (comparing snd) . zip [0..]

  forM_ bs $ putStrLn . unlines . zipWith cute [0..9] . feedForward example

  putStrLn $ "best guess: " ++ show (bestOf $ feedForward example smart)

  let guesses = bestOf . (\n -> feedForward (getX testI n) smart) <$> [0..9999]
  let answers = getLabelDig testL <$> [0..9999]
  putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++
    " / 10000"
