module DigitsLib
    ( mnistFeatures
    , mnistLabels
    , getImage
    , getNormalizedImage
    , getLabelDig
    , getY
    , render
    ) where

import qualified Data.ByteString.Lazy as BS

-- Each image is 28x28 pixels = 784, input values
mnistFeatures :: Int
mnistFeatures = 784

-- Labels correspond to one digit, from 0 to 9 = 10 digits = 10 labels
mnistLabels :: Int
mnistLabels = 10

mnistTestSamples :: Int
mnistTestSamples = 10000

---- http://www-cs-students.stanford.edu/~blynn/haskell/brain.html

-- Gets image from images file with image_index index
getImage images image_index  = fromIntegral . BS.index images . (image_index*28^2 + 16 +) <$> [0..28^2 - 1]

-- Gets the image and normalizes it to a value between 0 and 1
getNormalizedImage images image_index  = (/ 256) <$> getImage images image_index

-- Gets the label for the image with image_index in images
getLabelDig image_labels image_index  = fromIntegral $ BS.index image_labels (image_index + 8) -- +8

-- Gets label
getY images image_index  = fromIntegral . fromEnum . (getLabelDig images image_index  ==) <$> [0..9]

-- Draws corresponding image (image_index) in ascii art
render image_index  = let images = " .:oO@" in images !! (fromIntegral image_index  * length images `div` 256)
