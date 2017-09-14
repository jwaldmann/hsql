{-# LANGUAGE CPP,ForeignFunctionInterface #-}
{-| `SqlBind' instances for `Int', `Int64', `Integer', `Double', and `Float'.
-}
module DB.HSQL.Type.Numeric() where

import Control.Exception(throw)
import Data.Int(Int64)
import Foreign(Ptr,nullPtr)
import Foreign.C(CString)

import DB.HSQL.Type
    (SqlType(SqlReal,SqlFloat,SqlDouble,SqlNumeric,SqlDecimal
            ,SqlBigInt,SqlTinyInt,SqlInteger,SqlMedInt,SqlSmallInt
            ,SqlText))
import DB.HSQL.Error(SqlError(SqlFetchNull,SqlBadTypeCast))
import Database.HSQL.Types(SqlBind(..))

-- |
foreign import ccall "stdlib.h atoi" 
  c_atoi :: CString -> IO Int

#ifdef mingw32_TARGET_OS
foreign import ccall "stdlib.h _atoi64" 
  c_atoi64 :: CString -> IO Int64
#else
foreign import ccall "stdlib.h strtoll" 
  c_strtoll :: CString -> Ptr CString -> Int -> IO Int64
#endif


-- |
instance SqlBind Int where
  fromSqlCStringLen (name,sqlType,_) cstr cstrLen
    | cstr == nullPtr        = throw (SqlFetchNull name)
    | sqlType==SqlInteger || 
      sqlType==SqlMedInt  ||
      sqlType==SqlTinyInt ||
      sqlType==SqlSmallInt||
      sqlType==SqlBigInt     = c_atoi cstr
    | otherwise = throw (SqlBadTypeCast name sqlType)

  fromSqlValue SqlInteger  s = Just (read s)
  fromSqlValue SqlMedInt   s = Just (read s)
  fromSqlValue SqlTinyInt  s = Just (read s)
  fromSqlValue SqlSmallInt s = Just (read s)
  fromSqlValue SqlBigInt   s = Just (read s)
  fromSqlValue SqlDouble   s = Just (truncate (read s :: Double))
  fromSqlValue SqlText     s = Just (read s)
  fromSqlValue _ _           = Nothing

  toSqlValue s = show s


-- |
instance SqlBind Int64 where
  fromSqlCStringLen (name,sqlType,_) cstr cstrLen
    | cstr == nullPtr        = throw (SqlFetchNull name)
    | sqlType==SqlInteger ||
      sqlType==SqlMedInt  ||
      sqlType==SqlTinyInt ||
      sqlType==SqlSmallInt||
      sqlType==SqlBigInt     =

#ifdef mingw32_TARGET_OS
       c_atoi64 cstr
#else
       c_strtoll cstr nullPtr 10
#endif

    | otherwise = throw (SqlBadTypeCast name sqlType)

  fromSqlValue SqlInteger s = Just (read s)
  fromSqlValue SqlMedInt s   = Just (read s)
  fromSqlValue SqlTinyInt s  = Just (read s)
  fromSqlValue SqlSmallInt s = Just (read s)
  fromSqlValue SqlBigInt s = Just (read s)
  fromSqlValue SqlDouble s = Just (truncate (read s :: Double))
  fromSqlValue SqlText   s = Just (read s)
  fromSqlValue _ s = Nothing

  toSqlValue val = show val


-- |
instance SqlBind Integer where
    fromSqlValue SqlInteger  s = Just (read s)
    fromSqlValue SqlMedInt s   = Just (read s)
    fromSqlValue SqlTinyInt s  = Just (read s)
    fromSqlValue SqlSmallInt s = Just (read s)
    fromSqlValue SqlBigInt   s = Just (read s)
    fromSqlValue SqlDouble s = Just (truncate (read s :: Double))
    fromSqlValue SqlText   s = Just (read s)
    fromSqlValue _ _           = Nothing

    toSqlValue s = show s


-- |
instance SqlBind Double where
    fromSqlValue (SqlDecimal _ _) s = Just (read s)
    fromSqlValue (SqlNumeric _ _) s = Just (read s)
    fromSqlValue SqlDouble  s = Just (read s)
    fromSqlValue SqlReal s = Just (read s)
    fromSqlValue SqlFloat s = Just (read s)
    fromSqlValue SqlText   s = Just (read s)
    fromSqlValue _ _ = Nothing

    toSqlValue d = show d


-- |
instance SqlBind Float where
    fromSqlValue (SqlDecimal _ _) s = Just (read s)
    fromSqlValue (SqlNumeric _ _) s = Just (read s)
    fromSqlValue SqlDouble  s = Just (read s)
    fromSqlValue SqlReal s = Just (read s)
    fromSqlValue SqlFloat s = Just (read s)
    fromSqlValue SqlText   s = Just (read s)
    fromSqlValue _ _ = Nothing

    toSqlValue d = show d
