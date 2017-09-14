{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-| `SqlBind' instances for `String', `Bool' and `Maybe'.
-}
module DB.HSQL.Type.Diverse where
import Foreign(nullPtr)
import DB.HSQL.Type(SqlType(SqlBit))
import Database.HSQL.Types(SqlBind(..))

-- |
instance SqlBind String where
	fromSqlValue _ = Just

	toSqlValue s = '\'' : foldr mapChar "'" s
		where
			mapChar '\\'   s = '\\':'\\':s
			mapChar '\''   s = '\\':'\'':s
			mapChar '\n'   s = '\\':'n' :s
			mapChar '\r'   s = '\\':'r' :s
			mapChar '\t'   s = '\\':'t' :s
			mapChar '\NUL' s = '\\':'0' :s
			mapChar c      s = c        :s


-- |
instance SqlBind Bool where
    fromSqlValue SqlBit s = Just (s == "t")
    fromSqlValue _ _ = Nothing

    toSqlValue True  = "'t'"
    toSqlValue False = "'f'"


-- |
instance SqlBind a => SqlBind (Maybe a) where
  fromSqlCStringLen fieldDef cstr cstrLen
    | cstr == nullPtr = return Nothing
    | otherwise       = do v <- fromSqlCStringLen fieldDef cstr cstrLen
                           return (Just v)
                           
  fromSqlValue tp s   = Just (fromSqlValue tp s)

  toSqlValue (Just v) = toSqlValue v
  toSqlValue Nothing  = "null"
