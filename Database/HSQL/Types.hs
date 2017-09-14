{-# LANGUAGE RankNTypes #-}
-- #hide
{-| Basic type class & type definitions for DB interfacing.
-}
module Database.HSQL.Types
    (SQL,TableId,Connection(..)
    ,ColId,Nullability,ColDef,FieldReader,FieldReading,Statement(..)
    ,SqlBind(..),SqlType(..),SqlError(..)) where

import Control.Concurrent.MVar(MVar)
import Control.Exception(throw)
import Foreign(nullPtr)
import Foreign.C(CString,peekCStringLen)

import DB.HSQL.Type(SqlType(..))
import DB.HSQL.Error(SqlError(..))

-- | A table column ID.
type ColId = String

-- | Whether fields of a table col may be NULL.
type Nullability = Bool

-- | Description of the properties of a table column.
type ColDef = (ColId, SqlType, Nullability)

-- | An SQL Query.
type SQL = String

-- | A table ID.
type TableId = String

-- | A 'Connection' type represents a connection to a database,
-- through which you can operate on the it.
-- In order to create the connection you need to use the @connect@ function
-- from the module for your prefered backend.
data Connection
  = Connection {
      -- | disconnect action
      connDisconnect :: IO (),
      -- | query execution action (without return value)
      connExecute :: SQL -> IO (),
      -- | query action with return value
      connQuery :: SQL -> IO Statement,
      -- | retrieval of the names of the tables in reach
      connTables :: IO [TableId],
      -- | retrieval of the field defs of a table
      connDescribe :: TableId -> IO [ColDef],
      -- | begin of a transaction
      connBeginTransaction :: IO (),
      -- | commit of a pending transaction
      connCommitTransaction :: IO (),
      -- | rollback of a pending transaction
      connRollbackTransaction :: IO (),
      -- | closing state of the connection
      connClosed :: MVar Bool }

-- | A DB generic field extraction function, specifiable by 
-- field definition, receiving the content code and its length.
type FieldReader t = ColDef -- ^ field type spec 
                  -> CString -- ^ field content code
                  -> Int -- ^ field content length
                  -> IO t -- ^ field read action

-- | An extraction of a field of type to be specified by requester,
-- from a row index with source `ColDef', applying an appropriate 
-- `FieldReader'.
type FieldReading = forall t 
                  . Int -- ^ field (column) index
                 -> ColDef -- ^ source field type spec
                 -> FieldReader t -- ^ field reader to be applied
                 -> IO t -- ^ field read action

-- | The 'Statement' type represents a result from the execution of given
-- SQL query.
data Statement
    = Statement { 
        -- | DB connection the statement depends on
        stmtConn :: Connection,
        -- | close action
        stmtClose :: IO (),
        -- | incrementation of the row pointer and indication
        -- whether this is still in range of available rows
        stmtFetch :: IO Bool,
        -- | a `FieldReading' function applicable for each row
        stmtGetCol :: FieldReading,
        -- | field descriptors for each result table column
        stmtFields :: [ColDef],
        -- | check whether the statement is closed
        stmtClosed :: MVar Bool }


-- | Equivalent to Show and Read adapted to SQL expressions.
class SqlBind a where
        -- | show as an SQL expression
	toSqlValue:: a-> SQL
        -- | read from an SQL expression in text representation, 
        -- by support of its `SqlType'
	fromSqlValue:: SqlType-> SQL-> Maybe a
	-- | read from SQL expression in binary representation,
        -- by support of its `ColDef' and code size info.
        -- This allows for faster conversion for e.g. integral numeric types,
        -- etc.
	fromSqlCStringLen:: ColDef 
                         -> CString -- ^ binary content of SQL expression 
                         -> Int -- ^ size of binary content
                         -> IO a
	fromSqlCStringLen (name,sqlType,_) cstr cstrLen
	  | cstr == nullPtr = throw (SqlFetchNull name)
	  | otherwise = do 
	      str <- peekCStringLen (cstr, cstrLen)
	      case fromSqlValue sqlType str of
	        Nothing -> throw (SqlBadTypeCast name sqlType)
	        Just v  -> return v
