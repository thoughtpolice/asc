--------------------------------------------------------------------------------

-- |
-- Module      : Main
-- Copyright   : (c) Austin Seipp 2019
--               (c) Max Bolingbroke 2011-2019
-- License     : BSD3
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Application entry point.
--
module Main
  ( main -- :: IO ()
  ) where

--------------------------------------------------------------------------------

-- base
import           System.Console.GetOpt

-- lang
import qualified Language ( main )

--------------------------------------------------------------------------------

mainWith
  :: [OptDescr (Either String a)]
  -> ( a -> [String] -> IO () )
  -> IO ()
mainWith _ _ = pure ()

--------------------------------------------------------------------------------

main :: IO ()
main = mainWith [] (\_ _ -> Language.main)
