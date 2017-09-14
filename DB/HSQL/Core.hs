{-| Management of handles and exception handling. 
-}
module DB.HSQL.Core where
import Prelude hiding(catch)
import Control.Monad(when,unless)
import Control.Exception(Exception,throw,catch,handle)
import Control.Concurrent.MVar(MVar,withMVar,modifyMVar_)
import Data.Typeable(cast)

import DB.HSQL.Error(SqlError(SqlClosedHandle))

-- | if closed, no action.
closeHandle :: MVar Bool -- ^ closing state ref
            -> IO () -- ^ DB action to do if not closed
            -> IO ()
closeHandle ref action =
	modifyMVar_ ref (\closed -> unless closed action 
                         >> return True)

-- | if closed, throws `SqlClosedHandle' exception.
checkHandle :: MVar Bool -- ^ closing state ref
            -> IO a -- ^ DB action to do if not closed
            -> IO a
checkHandle ref action =
	withMVar ref (\closed -> when closed (throw SqlClosedHandle) 
                      >> action)


------------------------------------------------------------------------------
-- routines for exception handling
------------------------------------------------------------------------------
-- | Casts, if possible, an `Exception' to an `SqlError'.
sqlExceptions :: Exception x 
              => x -- ^ the exception thinc to be cast
              -> Maybe SqlError
sqlExceptions = cast

-- | Deprecated: Use `Control.Exception.catch' instead.
{-# DEPRECATED catchSql "Use Control.Exception.catch instead." #-}
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catch

-- | Deprecated: Use `Control.Exception.handle' instead.
{-# DEPRECATED handleSql "Use Control.Exception.handle instead." #-}
handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = handle
