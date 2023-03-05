{- 
 From missingh: Data-Either
-}
module Utils.Either
(
     maybeToEither,
     forceEither,
     forceLeft, forceRight,
) where
import Control.Monad.Except

{- | Converts a Maybe value to an Either value, using the supplied parameter
as the Left value if the Maybe is Nothing.

This function can be interpreted as:

@maybeToEither :: e -> Maybe a -> Either e a@

Its definition is given as it is so that it can be used in the Error and related monads.

-}
maybeToEither :: MonadError e m =>
                 e                      -- ^ (Left e) will be returned if the Maybe value is Nothing
              -> Maybe a                -- ^ (Right a) will be returned if this is (Just a)
              -> m a
maybeToEither errorval Nothing   = throwError errorval
maybeToEither _ (Just normalval) = return normalval

{- | Pulls a "Right" value out of an Either value.  If the Either value is
Left, raises an exception with "error". -}
forceEither :: Show e => Either e a -> a
forceEither (Left x)  = error (show x)
forceEither (Right x) = x

-- | Take a Left to a value, crashes on a Right. Renamed to avoid Data.Either clashes.
forceLeft :: Either a b -> a
forceLeft (Left a) = a
forceLeft _        = error "forceLeft: Right"

-- | Take a Right to a value, crashes on a Left. Renamed to avoid Data.Either clashes.
forceRight :: Either a b -> b
forceRight (Right a) = a
forceRight _         = error "forceRight: Left"
