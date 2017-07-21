{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A RIO-targeted prelude
--
-- @since 0.1.0.0
module RIO
  ( module X
  , LText
  , LByteString
  , UVector
  , VU.Unbox
  , SVector
  , VS.Storable
  , decodeUtf8
  , toByteVector
  , fromByteVector
  ) where

import           Control.Applicative      as X (Alternative, Applicative (..),
                                                liftA, liftA2, liftA3, many,
                                                optional, some, (<|>))
import           Control.Arrow            as X (first, second, (&&&), (***))
import           Control.DeepSeq          as X (NFData (..), force, ($!!))
import           Control.Monad            as X (Monad (..), MonadPlus (..),
                                                filterM, foldM, foldM_, forever,
                                                guard, join, liftM, liftM2,
                                                replicateM_, unless, when,
                                                zipWithM, zipWithM_, (<$!>),
                                                (<=<), (=<<), (>=>))
import           Control.Monad.Catch      as X (MonadThrow (..))
import           Control.Monad.Logger     as X (Loc, LogLevel (..), LogSource,
                                                LogStr, MonadLogger (..),
                                                MonadLoggerIO (..), liftLoc,
                                                logDebug, logError, logInfo,
                                                logOther, logWarn, toLogStr)
import           Control.Monad.Reader     as X (MonadReader, MonadTrans (..),
                                                ReaderT (..), ask, asks)
import           Control.Monad.RIO        as X
import           Data.Bool                as X (Bool (..), not, otherwise, (&&),
                                                (||))
import           Data.ByteString          as X (ByteString)
import           Data.Char                as X (Char)
import           Data.Data                as X (Data (..))
import           Data.Either              as X (Either (..), either, isLeft,
                                                isRight, lefts,
                                                partitionEithers, rights)
import           Data.Eq                  as X (Eq (..))
import           Data.Foldable            as X (Foldable, all, and, any, asum,
                                                concat, concatMap, elem, fold,
                                                foldMap, foldl', foldr, forM_,
                                                for_, length, mapM_, msum,
                                                notElem, null, or, product,
                                                sequenceA_, sequence_, sum,
                                                toList, traverse_)
import           Data.Function            as X (const, fix, flip, id, on, ($),
                                                (&), (.))
import           Data.Functor             as X (Functor (..), void, ($>), (<$),
                                                (<$>))
import           Data.Hashable            as X (Hashable)
import           Data.HashMap.Strict      as X (HashMap)
import           Data.HashSet             as X (HashSet)
import           Data.Int                 as X
import           Data.IntMap.Strict       as X (IntMap)
import           Data.IntSet              as X (IntSet)
import           Data.List                as X (break, drop, dropWhile, filter,
                                                lines, lookup, map, replicate,
                                                reverse, span, take, takeWhile,
                                                unlines, unwords, words, zip,
                                                (++))
import           Data.Map.Strict          as X (Map)
import           Data.Maybe               as X (Maybe (..), catMaybes,
                                                fromMaybe, isJust, isNothing,
                                                listToMaybe, mapMaybe, maybe,
                                                maybeToList)
import           Data.Monoid              as X (All (..), Any (..), Endo (..),
                                                First (..), Last (..),
                                                Monoid (..), Product (..),
                                                Sum (..), (<>))
import           Data.Ord                 as X (Ord (..), Ordering (..),
                                                comparing)
import           Data.Set                 as X (Set)
import           Data.String              as X (IsString (..))
import           Data.Text                as X (Text)
import           Data.Text.Encoding       as X (encodeUtf8)
import           Data.Traversable         as X (Traversable (..), for, forM)
import           Data.Vector              as X (Vector)
import           Data.Void                as X (Void, absurd)
import           Data.Word                as X
import           GHC.Generics             as X (Generic)
import           Prelude                  as X (Bounded (..), Double, Enum,
                                                FilePath, Float, Floating (..),
                                                Fractional (..), IO, Integer,
                                                Integral (..), Num (..),
                                                Rational, Real (..),
                                                RealFloat (..), RealFrac (..),
                                                Show, String, asTypeOf, curry,
                                                error, even, fromIntegral, fst,
                                                gcd, lcm, odd, realToFrac, seq,
                                                show, snd, subtract, uncurry,
                                                undefined, ($!), (^), (^^))
import           Say                      as X
import           Text.Read                as X (Read, readMaybe)
import           UnliftIO                 as X

import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy     as BL
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy           as TL
import qualified Data.Vector.Storable     as VS
import qualified Data.Vector.Unboxed      as VU

-- | Lazy text
--
-- @since 0.1.0.0
type LText = TL.Text

-- | Lazy bytestring
--
-- @since 0.1.0.0
type LByteString = BL.ByteString

-- | Unboxed vector
--
-- @since 0.1.0.0
type UVector = VU.Vector

-- | Storable vector
--
-- @since 0.1.0.0
type SVector = VS.Vector

-- | Convert a storable vector to a @ByteString@
--
-- @since 0.1.0.0
fromByteVector :: SVector Word8 -> ByteString
fromByteVector v =
    let (fptr, off, len) = VS.unsafeToForeignPtr v
     in BI.fromForeignPtr fptr off len

-- | Convert a @ByteString@ to a storable vector
--
-- @since 0.1.0.0
toByteVector :: ByteString -> SVector Word8
toByteVector bs =
    let (fptr, off, len) = BI.toForeignPtr bs
     in VS.unsafeFromForeignPtr fptr off len

-- | UTF8 decode a @ByteString@ with 'lenientDecode'. Unlike the
-- @decodeUtf8@ in the text package, this is a total function.
--
-- @since 0.1.0.0
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

-- TODO add a MonadThrow or MonadError variant of decodeUtf8?
