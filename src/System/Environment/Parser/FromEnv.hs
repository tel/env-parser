{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  System.Environment.Parser.FromEnv
-- Copyright   :  (C) 2014 Joseph Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  non-portable

module System.Environment.Parser.FromEnv (

  FromEnv (..)
  
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as At
import           Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as Sl
import qualified Data.CaseInsensitive as Ci
import           Data.Char (isSpace)
import           Data.Int
import           Data.Ratio
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import qualified Data.Text.Lazy as Tl
import qualified Data.Text.Lazy.Encoding as Tle
import           Data.Word
import qualified Foreign.C.Types as Cty
import qualified System.Posix.Types as Posix

-- | A type which can be converted from some representation in the
-- environment. The environment is typically a very shallow syntactic
-- space so these parsers are not necessarily intended to be
-- unambiguous---see notes on the instance for lists.
class FromEnv a where
  fromEnv     :: Parser a

  fromEnvList :: Parser [a]
  fromEnvList = At.sepBy fromEnv (At.takeWhile1 isSpace)

  parseEnv :: T.Text -> Either String a
  parseEnv = At.parseOnly fromEnv

instance FromEnv T.Text where
  fromEnv = At.takeText

instance FromEnv Tl.Text where
  fromEnv = At.takeLazyText

instance FromEnv S.ByteString where
  fromEnv = fmap Te.encodeUtf8 fromEnv

instance FromEnv Sl.ByteString where
  fromEnv = fmap Tle.encodeUtf8 fromEnv

instance (Ci.FoldCase s, FromEnv s) => FromEnv (Ci.CI s) where
  fromEnv = fmap Ci.mk fromEnv

instance FromEnv Char where
  fromEnv = At.anyChar
  fromEnvList = fmap T.unpack fromEnv

-- | Follows Bash conventions and assumes space delimited lists. Not
-- exactly flexible, but if you need more than this there's always
-- JSON.
instance FromEnv a => FromEnv [a] where
  fromEnv = fromEnvList

instance FromEnv Int        where fromEnv = At.signed At.decimal
instance FromEnv Integer    where fromEnv = At.signed At.decimal
instance FromEnv Int8       where fromEnv = At.signed At.decimal
instance FromEnv Int16      where fromEnv = At.signed At.decimal
instance FromEnv Int32      where fromEnv = At.signed At.decimal
instance FromEnv Int64      where fromEnv = At.signed At.decimal
instance FromEnv Double     where fromEnv = At.signed At.rational
instance FromEnv Float      where fromEnv = At.signed At.rational
instance FromEnv Scientific where fromEnv = At.signed At.rational

byteWidth :: (Integral a, Bits a) => Int -> At.Parser a
byteWidth n = do
  feed <- At.take n
  case At.parseOnly At.hexadecimal feed of
    Left err -> fail err
    Right a  -> return a

instance FromEnv Word8      where fromEnv = byteWidth 2
instance FromEnv Word16     where fromEnv = byteWidth 4
instance FromEnv Word32     where fromEnv = byteWidth 8
instance FromEnv Word64     where fromEnv = byteWidth 16

-- | Parses a syntax like
--
-- > 16/3
-- > 254/13
-- > -12/7
--
-- Note that spaces are not allowed between the numerator, the @'/'@, and
-- the denominator.
instance Integral a => FromEnv (Ratio a) where
  fromEnv = (%) <$> At.signed At.decimal
                <*> ( (At.char '/' *> At.decimal)
                      <|>
                      pure 1 )

-- | Consumes the entire input and ignores it. Note that this forms a big
-- distinction between the parse and the lookup---a lookup will still fail
-- for this type if the variable is not set... but if it is set then the
-- parse will always succeed.
instance FromEnv () where
  fromEnv = At.takeText *> pure ()

-- | The syntactic space here includes @yes@, @no@, @true@, @false@,
-- @1@, and @0@ and is case insensitive.
instance FromEnv Bool where
  fromEnv =
    At.choice . map (\(n, v) -> At.asciiCI n *> pure v)
      $ [ ("true",  True)
        , ("false", False)
        , ("yes",   True)
        , ("no",    False)
        , ("1",     True)
        , ("0",     False)
        ]

-- 
-- I'm not sure these should be added. They're kind of conventionally
-- useful---for instance, if you'd like to casually take ENV
-- parameters directly to C libraries being wrapped---but it's a fair
-- amount of overhead in the library.
--
-- So I'm almost universally against their inclusion, but the code is
-- here for historical interest.
--
-- ~ Joseph Abrahamson / 2014 Sept 19
--
-- instance FromEnv Cty.CChar      where fromEnv = flip fmap fromEnv Cty.CChar
-- instance FromEnv Cty.CClock     where fromEnv = flip fmap fromEnv Cty.CClock
-- instance FromEnv Cty.CDouble    where fromEnv = flip fmap fromEnv Cty.CDouble
-- instance FromEnv Cty.CFloat     where fromEnv = flip fmap fromEnv Cty.CFloat
-- instance FromEnv Cty.CInt       where fromEnv = flip fmap fromEnv Cty.CInt
-- instance FromEnv Cty.CIntPtr    where fromEnv = flip fmap fromEnv Cty.CIntPtr
-- instance FromEnv Cty.CLLong     where fromEnv = flip fmap fromEnv Cty.CLLong
-- instance FromEnv Cty.CLong      where fromEnv = flip fmap fromEnv Cty.CLong
-- instance FromEnv Cty.CSChar     where fromEnv = flip fmap fromEnv Cty.CSChar
-- instance FromEnv Cty.CSUSeconds where fromEnv = flip fmap fromEnv Cty.CSUSeconds
-- instance FromEnv Cty.CShort     where fromEnv = flip fmap fromEnv Cty.CShort
-- instance FromEnv Cty.CSize      where fromEnv = flip fmap fromEnv Cty.CSize
-- instance FromEnv Cty.CTime      where fromEnv = flip fmap fromEnv Cty.CTime
-- instance FromEnv Cty.CUChar     where fromEnv = flip fmap fromEnv Cty.CUChar
-- instance FromEnv Cty.CUInt      where fromEnv = flip fmap fromEnv Cty.CUInt
-- instance FromEnv Cty.CUIntPtr   where fromEnv = flip fmap fromEnv Cty.CUIntPtr
-- instance FromEnv Cty.CULong     where fromEnv = flip fmap fromEnv Cty.CULong
-- instance FromEnv Cty.CUSeconds  where fromEnv = flip fmap fromEnv Cty.CUSeconds
-- instance FromEnv Cty.CUShort    where fromEnv = flip fmap fromEnv Cty.CUShort
-- instance FromEnv Cty.CWchar     where fromEnv = flip fmap fromEnv Cty.CWchar
-- instance FromEnv Posix.CDev     where fromEnv = flip fmap fromEnv Posix.CDev
-- instance FromEnv Posix.CGid     where fromEnv = flip fmap fromEnv Posix.CGid
-- instance FromEnv Posix.CIno     where fromEnv = flip fmap fromEnv Posix.CIno
-- instance FromEnv Posix.CMode    where fromEnv = flip fmap fromEnv Posix.CMode
-- instance FromEnv Posix.CNlink   where fromEnv = flip fmap fromEnv Posix.CNlink
-- instance FromEnv Posix.COff     where fromEnv = flip fmap fromEnv Posix.COff
-- instance FromEnv Posix.CPid     where fromEnv = flip fmap fromEnv Posix.CPid
-- instance FromEnv Posix.CUid     where fromEnv = flip fmap fromEnv Posix.CUid

-- | List lists, tuples are space-delimited
instance 
  ( FromEnv a
  , FromEnv b 
  ) => FromEnv (a, b) 
    where 
  fromEnv = (,) <$> fromEnv <*> (At.takeWhile1 isSpace *> fromEnv)

instance
  ( FromEnv a
  , FromEnv b 
  , FromEnv c 
  ) => FromEnv (a, b, c) 
    where 
  fromEnv = (,,) 
    <$> fromEnv <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)

instance
  ( FromEnv a
  , FromEnv b 
  , FromEnv c 
  , FromEnv d
  ) => FromEnv (a, b, c, d) 
    where 
  fromEnv = (,,,)
    <$> fromEnv <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)

instance
  ( FromEnv a
  , FromEnv b 
  , FromEnv c 
  , FromEnv d
  , FromEnv e
  ) => FromEnv (a, b, c, d, e) 
    where 
  fromEnv = (,,,,)
    <$> fromEnv <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)

instance
  ( FromEnv a
  , FromEnv b 
  , FromEnv c 
  , FromEnv d
  , FromEnv e
  , FromEnv f
  ) => FromEnv (a, b, c, d, e, f) 
    where 
  fromEnv = (,,,,,) 
    <$> fromEnv <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)
                <*> (At.takeWhile1 isSpace *> fromEnv)
