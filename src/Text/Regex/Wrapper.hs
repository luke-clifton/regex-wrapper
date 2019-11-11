{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Text.Regex.Wrapper
    ( Matched()
    , parseMatched
    , parseMatchedEither
    , parseMatchedM
    , asStr
    , asText
    , asString
    , asByteString
    , compile
    , parseMatchedWith
    , parseMatchedEitherWith
    , RegexPat()
    ) where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Proxy
import GHC.TypeLits
import Text.Regex.TDFA as TDFA
import Data.Hashable
import Data.Aeson
import Data.CaseInsensitive as CI

data RegexError str = NoMatch (MatchError str) | CompileError String
    deriving Show

prettyRegexError :: Show str => RegexError str -> String
prettyRegexError (NoMatch err) = prettyMatchError err
prettyRegexError (CompileError err) = "Invalid regular expression: " ++ err

data MatchError str = MatchError
    { matchErrorPattern :: String
    , matchErrorInput :: str
    } deriving Show

prettyMatchError :: Show str => MatchError str -> String
prettyMatchError err
    = "The input "
    ++ show (matchErrorInput err)
    ++ " did not match the pattern "
    ++ matchErrorPattern err

-- | A compiled regular expression that can produce @`Matched` str pat@
-- values.
newtype RegexPat (pat :: Symbol) = RegexPat { unwrap :: TDFA.Regex }

-- | Create a @`Regex`@ that can be used to construct @`Matched`@ values
-- when used with @`parseMatchedWith`@. This allows you to compile
-- the pattern once, and use it multiple times.
compile :: forall pat m. (KnownSymbol pat) => Either String (RegexPat pat)
compile =
    let
        pat = symbolVal (Proxy :: Proxy pat)
    in
        case RegexPat <$> makeRegexM pat of
            Success a -> Right a
            Error s -> Left s

-- | Use a precompiled @`Regex`@ to create @`Matched`@ values.
parseMatchedWith
    :: RegexLike Regex str
    => RegexPat pat -> str -> Maybe (Matched str pat)
parseMatchedWith (RegexPat pat) str = do
    guard $ matchTest pat str
    pure (Matched str)

-- | Like @`parseMatchedWith`@, but calls returns a useful error message.
parseMatchedEitherWith
    :: forall pat str m.
    ( RegexLike Regex str
    , Show str
    , KnownSymbol pat
    ) => RegexPat pat -> str -> Either (MatchError str) (Matched str pat)
parseMatchedEitherWith reg str = do
    let pat = symbolVal (Proxy :: Proxy pat)
    case parseMatchedWith reg str of
        Nothing -> Left MatchError
            { matchErrorPattern = pat
            , matchErrorInput = str
            }
        Just x -> Right x

-- | A wrapper type that can only be constructed if the underlying string
-- type matches the regular expression in @pat@.
newtype Matched str (pat :: Symbol) = Matched str
    deriving newtype (Show, Eq, Ord, Hashable, FoldCase)

-- | Extract the wrapped @str@ type.
asStr :: Matched str pat -> str
asStr (Matched str) = str

instance (KnownSymbol pat, RegexLike Regex str, Read str)
    => Read (Matched str pat) where
    readsPrec p s = do
        r@(a,s) <- readsPrec p s
        Just m <- pure (parseMatched a)
        pure (m, s)

instance ToJSON str => ToJSON (Matched str pat) where
    toJSON = toJSON . asStr
    toEncoding = toEncoding . asStr

instance (KnownSymbol pat, Show str, FromJSON str, RegexLike Regex str)
    => FromJSON (Matched str pat) where
    parseJSON v = parseJSON v >>= either (fail . prettyRegexError) pure . parseMatchedEither

-- | Same as @`asStr`@ but with a fixed type.
asText :: Matched Text p -> Text
asText = asStr

-- | Same as @`asStr`@ but with a fixed type.
asString :: Matched String p -> String
asString = asStr

-- | Same as @`asStr`@ but with a fixed type.
asByteString :: Matched ByteString p -> ByteString
asByteString = asStr

-- | Convert a @str@ into a @`Matched` str pat@ if possible.
parseMatched
    :: forall str pat. (KnownSymbol pat, RegexLike Regex str)
    => str -> Maybe (Matched str pat)
parseMatched str = case compile of
    Left _ -> Nothing
    Right r -> parseMatchedWith r str

-- | Same as @`parseMatched`@ but provides a slightly helpful error
-- message on failure. This requires that your @str@ type is an instance
-- of @`Show`@.
parseMatchedEither
    :: forall pat str. (Show str, KnownSymbol pat, RegexLike Regex str)
    => str -> Either (RegexError str) (Matched str pat)
parseMatchedEither str = do
    case compile of
        Left e -> Left $ CompileError e
        Right r -> case parseMatchedEitherWith r str of
            Left e -> Left $ NoMatch e
            Right r -> pure r

-- | Like @`parseMatchedEither`@ except that it calls @`fail`@ instead.
parseMatchedM
    :: forall pat str m.
    ( KnownSymbol pat, RegexLike Regex str, MonadFail m, Show str)
    => str -> m (Matched str pat)
parseMatchedM str = case parseMatchedEither str of
    Left e -> fail $ prettyRegexError e
    Right r -> pure r
