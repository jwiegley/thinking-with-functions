{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State
import           Data.Char
import           Data.Data
import           Data.Data.Lens
import           Data.Function
import           Data.List
import           Data.Maybe (isJust)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Numeric.Natural
import           Test.Hspec

{------------------------------------------------------------------------}

infixr 0 ==>
(==>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(==>) = shouldBe

infixr 0 /=>
(/=>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(/=>) = shouldNotBe

infixr 0 !!>
(!!>) :: (HasCallStack, Exception e) => a -> Selector e -> Expectation
v !!> s = pure v `shouldThrow` s

main :: IO ()
main = hspec $ parallel $
  describe "Part" $
    describe "Chapter" $
      describe "Section" $
        it "Test" $
          True
            ==> True

-- Local Variables:
-- haskell-indent-spaces: 2
-- haskell-indentation-ifte-offset: 2
-- haskell-indentation-layout-offset: 2
-- haskell-indentation-left-offset: 2
-- haskell-indentation-starter-offset: 2
-- haskell-indentation-where-post-offset: 2
-- haskell-indentation-where-pre-offset: 2
-- End:
