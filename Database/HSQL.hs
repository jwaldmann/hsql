{-# LANGUAGE ScopedTypeVariables #-}
{-| Module      :  Database.HSQL
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module provides an abstract database interface
-}
module Database.HSQL
    (
    -- * Connect\/Disconnect
    Connection
    , disconnect        -- :: Connection -> IO ()
    
    -- * Metadata
    , tables            -- :: Connection -> IO [String]
    , ColDef, describe          -- :: Connection -> String -> IO [ColDef]
      
    -- * Command Execution Functions
    , SQL
    -- | Once a connection to a database has been successfully established, 
    -- the functions described here are used to perform
    -- SQL queries and commands.
    , execute           -- :: Connection -> String -> IO ()
    , Statement
    , query             -- :: Connection -> String -> IO Statement
    , closeStatement    -- :: Statement -> IO ()
    , fetch             -- :: Statement -> IO Bool
    
    -- * Retrieving Statement values and types
    , SqlBind(..)
    , getFieldValue     -- :: SqlBind a => Statement -> String -> IO a
    , getFieldValueMB   
    , getFieldValue'    -- :: SqlBind a => Statement -> String -> a -> IO a
    , getFieldValueType -- :: Statement -> String -> (SqlType, Bool)
    , getFieldsTypes    -- :: Statement -> [(String, SqlType, Bool)]
	
    -- * Transactions
    , inTransaction     -- :: Connection -> (Connection -> IO a) -> IO a
    
    -- * Utilities
    , forEachRow        -- :: (Statement -> s -> IO s) -- ^ an action
    , forEachRow'       -- :: (Statement -> IO ()) -> Statement -> IO ()
    , collectRows       -- :: (Statement -> IO a) -> Statement -> IO [a]

    -- * SQL Exceptions handling
    , SqlError(..)
    , catchSql
    , handleSql
    , sqlExceptions     -- :: Exception -> Maybe SqlError

    -- * Extra types
    , Point(..), Line(..), Path(..), Box(..), Circle(..), Polygon(..)
    , INetAddr(..), MacAddr(..)
    , SqlType(..)
    ) where

import Prelude hiding(catch)
import Control.Monad(when)
import Control.Exception(SomeException,finally,throwIO,throw,catch)

import DB.HSQL.Type
import DB.HSQL.Type.Numeric
import DB.HSQL.Type.Time
import DB.HSQL.Type.Geometric
import DB.HSQL.Type.NetAddress
import DB.HSQL.Type.Diverse()
import DB.HSQL.Error
import DB.HSQL.Core
import Database.HSQL.Types(SQL,TableId,ColDef,Connection(..),Statement(..)
                          ,SqlBind(fromSqlCStringLen))

------------------------------------------------------------------------------
-- Operations on the connection
------------------------------------------------------------------------------
-- | Closes the connection. Performing 'disconnect' on a connection that has
-- already been closed has no effect.
-- All other operations on a closed connection will fail.
disconnect :: Connection -> IO ()
disconnect conn = 
    closeHandle (connClosed conn) (connDisconnect conn)
	
-- | Submits a command to the database.
execute :: Connection  -- ^ the database connection
        -> SQL      -- ^ the text of SQL command
        -> IO ()
execute conn query = 
    checkHandle (connClosed conn) (connExecute conn query)

-- | Executes a query and returns a result set
query :: Connection    -- ^ the database connection
      -> SQL        -- ^ the text of SQL query
      -> IO Statement  -- ^ the associated statement. Must be closed with 
                       -- the 'closeStatement' function
query conn query = 
    checkHandle (connClosed conn) (connQuery conn query)

-- | List all tables in the database.
tables :: Connection   -- ^ Database connection
       -> IO [TableId]  -- ^ The names of all tables in the database.
tables conn = 
    checkHandle (connClosed conn) (connTables conn)

-- | List all columns in a table along with their types and @nullable@ flags
describe :: Connection    -- ^ Database connection
	 -> TableId        -- ^ Name of a database table
	 -> IO [ColDef] -- ^ The list of fields in the table
describe conn table = 
    checkHandle (connClosed conn) (connDescribe conn table)

------------------------------------------------------------------------------
-- transactions
------------------------------------------------------------------------------

-- | The 'inTransaction' function executes the specified action in transaction
--  mode.
-- If the action completes successfully then the transaction will be commited.
-- If the action completes with an exception then the transaction will be
-- rolled back and the exception will be throw again.
-- A transaction is to catch ANY exception, so 'SomeException' is adequate.
inTransaction :: Connection            -- ^ Database connection
              -> (Connection -> IO a)  -- ^ an action
              -> IO a                  -- ^ the returned value is the result
                                        -- returned from action
inTransaction conn action = do
  checkHandle (connClosed conn) (connBeginTransaction conn)
  r <- catch (action conn) 
             (\(err::SomeException) -> do 
                checkHandle (connClosed conn) 
                            (connRollbackTransaction conn)
	        throwIO err)
  checkHandle (connClosed conn) (connCommitTransaction conn)
  return r

------------------------------------------------------------------------------
-- Operations on the statements
------------------------------------------------------------------------------
-- | 'fetch' fetches the next rowset of data from the result set.
-- The values from columns can be retrieved with 'getFieldValue' function.
fetch :: Statement -> IO Bool
fetch stmt = checkHandle (stmtClosed stmt) (stmtFetch stmt)

-- | 'closeStatement' stops processing associated with a specific statement,
-- closes any open cursors associated with the statement, discards pending
-- results, and frees all resources associated with the statement.
-- Performing 'closeStatement' on a statement that has already been closed
-- has no effect. All other operations on a closed statement will fail.
closeStatement :: Statement -> IO ()
closeStatement stmt = closeHandle (stmtClosed stmt) (stmtClose stmt)

-- | Returns the type and the @nullable@ flag for field with specified name
getFieldValueType :: Statement -> String -> (SqlType, Bool)
getFieldValueType stmt name = (sqlType, nullable)
    where (sqlType,nullable,colNumber) = 
              findFieldInfo name (stmtFields stmt) 0

-- | Returns the list of fields with their types and @nullable@ flags
getFieldsTypes :: Statement -> [(String, SqlType, Bool)]
getFieldsTypes stmt = stmtFields stmt

findFieldInfo :: String -> [ColDef] -> Int -> (SqlType,Bool,Int)
findFieldInfo name [] colNumber = throw (SqlUnknownField name)
findFieldInfo name (fieldDef@(name',sqlType,nullable):fields) colNumber
    | name == name' = (sqlType,nullable,colNumber)
    | otherwise = findFieldInfo name fields $! (colNumber+1)


------------------------------------------------------------------------------
-- binding
------------------------------------------------------------------------------
-- | Retrieves the value of field with the specified name.
getFieldValue :: SqlBind a => Statement    -- ^ result table data
                           -> String       -- ^ field name
                           -> IO a         -- ^ field value
getFieldValue stmt name = do
  stmtGetCol stmt colNumber (name,sqlType,nullable) fromSqlCStringLen
  where (sqlType,nullable,colNumber) = 
            findFieldInfo name (stmtFields stmt) 0

{-# DEPRECATED getFieldValueMB "Use getFieldValue instead." #-}
getFieldValueMB :: SqlBind a => Statement -> String -> IO (Maybe a)
getFieldValueMB = getFieldValue

-- | Retrieves the value of field with the specified name.
-- If the field value is @null@ then the function will return the default value.
getFieldValue' :: SqlBind a => Statement
                            -> String     -- ^ Field name
                            -> a          -- ^ Default field value
                            -> IO a       -- ^ Field value
getFieldValue' stmt name def = do
  mb_v <- getFieldValue stmt name
  return (case mb_v of { Nothing -> def; Just a -> a })


------------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------------
-- | The 'forEachRow' function iterates through the result set in 'Statement'
-- and executes the given action for each row in the set.
-- The function closes the 'Statement' after the last row processing or if
-- the given action raises an exception.
forEachRow :: (Statement -> s -> IO s) -- ^ an action
           -> Statement                -- ^ the statement
           -> s                        -- ^ initial state
           -> IO s                     -- ^ final state
forEachRow f stmt s = loop s `finally` closeStatement stmt
  where loop s = do success <- fetch stmt
                    if success then f stmt s >>= loop else return s

-- | The 'forEachRow\'' function is analogous to 'forEachRow' but doesn't
-- provide state.
-- The function closes the 'Statement' after the last row processing or if the
-- given action raises an exception.
forEachRow' :: (Statement -> IO ()) -> Statement -> IO ()
forEachRow' f stmt = loop `finally` closeStatement stmt
  where loop = do success <- fetch stmt
                  when success (f stmt >> loop)

-- | The 'collectRows' function iterates through the result set in 'Statement'
-- and executes the given action for each row in the set. The values returned
-- from action are collected and returned as list. The function closes the
-- 'Statement' after the last row processing or if the given action raises an
-- exception.
collectRows :: (Statement -> IO a) -> Statement -> IO [a]
collectRows f stmt = loop `finally` closeStatement stmt
  where loop = do success <- fetch stmt
                  if success
                    then do x <- f stmt
                            xs <- loop
                            return (x:xs)
                    else return []


