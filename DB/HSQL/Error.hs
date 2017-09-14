{-# LANGUAGE CPP,DeriveDataTypeable #-}
{-| `SqlError' type for a variety of DB specific error conditions,
with appropriate `Show', `Typeable', and `Exception' instances.
-}
module DB.HSQL.Error(SqlError(..)) where

import Control.Exception(Exception(..),SomeException(..))
import Data.Typeable(Typeable,cast)
import DB.HSQL.Type(SqlType)

-- |   
data SqlError
    = SqlError 
      { seState       :: String
      , seNativeError :: Int
      , seErrorMsg    :: String 
      } -- ^ generic error condition, with further specification
    | SqlNoMoreData -- ^ no more data was available
    | SqlInvalidHandle -- ^ requested handle is invalid
    | SqlStillExecuting -- ^ connection is blocked by running transaction
    | SqlNeedMoreData -- ^ more data is needed, e.g. additional connection
                      -- specs
    | SqlBadTypeCast 
      { seFieldName :: String
      , seFieldType :: SqlType 
      } -- ^ requested field can't be converted to requested type
    | SqlFetchNull
      { seFieldName :: String } -- ^ requested field returns NULL
    | SqlUnknownField 
      { seFieldName :: String } -- ^ requested field isn't known
    | SqlUnsupportedOperation -- ^ requested operation isn't supported
    | SqlClosedHandle -- ^ referenced handle is already closed
#ifdef __GLASGOW_HASKELL__
   deriving (Eq,Ord,Typeable)
#else
   deriving (Eq,Ord)

instance Typeable SqlError where
	typeOf _ = mkAppTy sqlErrorTc []

-- | The `TyCon' of `SqlError'.
sqlErrorTc :: TyCon
sqlErrorTc = mkTyCon3 "DB.HSQL" "Error" "SqlError"

#endif

-- |
instance Show SqlError where
    showsPrec _ (SqlError{seErrorMsg=msg}) = showString msg
    showsPrec _ SqlNoMoreData = 
      showString "No more data was available"
    showsPrec _ SqlInvalidHandle           = showString "Invalid handle"
    showsPrec _ SqlStillExecuting          = showString "Still executing"
    showsPrec _ SqlNeedMoreData = 
      showString "More data is needed, e.g. additional connection specs"
    showsPrec _ (SqlBadTypeCast name tp)   = 
        showString ("The type of "++name++" field can't be converted to " 
                    ++show tp++" type")
    showsPrec _ (SqlFetchNull name)        = 
        showString ("The value of "++name++" field is null")
    showsPrec _ (SqlUnknownField name)     = 
        showString ("Unknown field name: "++name)
    showsPrec _ SqlUnsupportedOperation    = showString "Unsupported operation"
    showsPrec _ SqlClosedHandle = 
        showString "The referenced handle is already closed"

-- |
instance Exception SqlError where
    toException = SomeException
    fromException (SomeException exception) =
        cast exception

