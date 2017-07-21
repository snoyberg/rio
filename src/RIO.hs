{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RIO
  ( module ForEach.Prelude
  , module Prelude -- FIXME! better prelude
  , module Control.Applicative
  , module Control.Monad
  , module Data.Word
  , module Data.Int
  , module Say
  , module System.Environment -- FIXME do better
  , module Data.Maybe
  , module Data.Monoid
  , module Control.Monad.RIO
  , MonadReader (..)
  , Text
  , LText
  , ByteString
  , LByteString
  , Map
  , Set
  , HashMap
  , HashSet
  , Hashable
  , Vector
  , UVector
  , VU.Unbox
  , SVector
  , VS.Storable
  , encodeUtf8
  , decodeUtf8
  ) where

import Prelude hiding (lines, unlines, readFile, writeFile)
import Control.Monad.RIO
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import System.Environment
import Data.Word
import Data.Int
import ForEach.Prelude
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Vector (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Hashable (Hashable)
import qualified Data.ByteString.Internal as BI
import Say
import Control.Monad.Reader
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

type LText = TL.Text
type LByteString = BL.ByteString
type UVector = VU.Vector
type SVector = VS.Vector

toByteVector :: SVector Word8 -> ByteString
toByteVector v =
    let (fptr, off, len) = VS.unsafeToForeignPtr v
     in BI.fromForeignPtr fptr off len

fromByteVector :: ByteString -> SVector Word8
fromByteVector bs =
    let (fptr, off, len) = BI.toForeignPtr bs
     in VS.unsafeFromForeignPtr fptr off len

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
