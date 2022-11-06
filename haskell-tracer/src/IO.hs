{-# LANGUAGE RecordWildCards #-} -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html
                                 -- record inference for fields, instead of {a=1, b=b, c=c} do {a=1, ..}      
{-# LANGUAGE TypeOperators   #-} -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_operators.html
                                 -- define types with operators names   

module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R -- for Repa



main :: IO ()
main = do
  [path] <- getArgs
  savePngImage path generateImg

generateImg :: DynamicImage
generateImg = ImageRGB8 (generateImage originalFnc 1200 1200)

originalFnc :: Int -> Int -> PixelRGB8
originalFnc x y =
  let (q, r) = x `quotRem` max 10 y
      s      = fromIntegral . min 0xff
  in PixelRGB8 (s q) (s r) (s (q + r + 30))