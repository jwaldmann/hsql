{-# LANGUAGE CPP,ForeignFunctionInterface #-}
{-| `SqlBind' instance for `ClockTime'.
-}
module DB.HSQL.Type.Time() where

import Control.Monad(mplus)
import System.IO.Unsafe(unsafePerformIO)
import System.Time(ClockTime(..),CalendarTime(..)
                  ,getClockTime,toCalendarTime,toUTCTime)
import Text.ParserCombinators.ReadP(ReadP,char,skipSpaces,readP_to_S)
import Text.Read.Lex(readDecP)
import Foreign(Ptr,allocaBytes,pokeByteOff)
import Foreign.C.Types(CTime(CTime),CInt)

import DB.HSQL.Type
    (SqlType(SqlTimeTZ,SqlTime,SqlDate,SqlDateTimeTZ,SqlDateTime
            ,SqlTimeStamp,SqlText))
import Database.HSQL.Types(SqlBind(..))

#include <time.h>

-- |
instance SqlBind ClockTime where
    fromSqlValue SqlTimeTZ s = f_read getTimeTZ s
	where getTimeTZ :: ReadP ClockTime
	      getTimeTZ = do
		(hour, minutes, seconds) <- readHMS
		(char '.' >> readDecP) `mplus` (return 0)
		tz <- parseTZ
		return (mkClockTime 1970 1 1 hour minutes seconds (tz*3600))
    fromSqlValue SqlTime s = f_read getTime s
	where getTime :: ReadP ClockTime
	      getTime = do
		(hour, minutes, seconds) <- readHMS
		return (mkClockTime 1970 1 1 hour minutes seconds currTZ)
    fromSqlValue SqlDate s = f_read getDate s
	where getDate :: ReadP ClockTime
	      getDate = do
		(year, month, day) <- readYMD
		return (mkClockTime year month day 0 0 0 currTZ)
    fromSqlValue SqlDateTimeTZ s = f_read getDateTimeTZ s
	where getDateTimeTZ :: ReadP ClockTime
	      getDateTimeTZ = do
	        (year, month, day, hour, minutes, seconds) <- readDateTime
		char '.' >> readDecP -- ) `mplus` (return 0)
                tz <- parseTZ
		return (mkClockTime year month day 
                                    hour minutes seconds 
                                    (tz*3600))
    -- The only driver which seems to report the type as SqlTimeStamp seems
    -- to be the MySQL driver. MySQL (at least 4.1) uses the same format for
    -- datetime and timestamp columns.
    -- Allow SqlText to support SQLite, which reports everything as SqlText
    fromSqlValue t s 
        | t == SqlDateTime || t == SqlTimeStamp || t == SqlText = 
            f_read getDateTime s
                where getDateTime :: ReadP ClockTime
		      getDateTime = do
			(year, month, day, hour, minutes, seconds) <- 
                            readDateTime
			return (mkClockTime year month day 
                                            hour minutes seconds 
                                            currTZ)
    fromSqlValue _ _ = Nothing

    toSqlValue ct = 
        '\'' : (shows (ctYear t) . score .
                shows (ctMonth t) . score .
                shows (ctDay t) . space .
                shows (ctHour t) . colon .
                shows (ctMin t) . colon .
                shows (ctSec t)) "'"
        where t = toUTCTime ct
              score = showChar '-'
              space = showChar ' '
              colon = showChar ':'


-- |
mkClockTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> ClockTime
mkClockTime year mon mday hour min sec tz =
    unsafePerformIO $ do
      allocaBytes (#const sizeof(struct tm)) $ \p_tm -> do
	(#poke struct tm,tm_sec  ) p_tm	(fromIntegral sec  :: CInt)
        (#poke struct tm,tm_min  ) p_tm	(fromIntegral min  :: CInt)
        (#poke struct tm,tm_hour ) p_tm	(fromIntegral hour :: CInt)
        (#poke struct tm,tm_mday ) p_tm	(fromIntegral mday :: CInt)
        (#poke struct tm,tm_mon  ) p_tm	(fromIntegral (mon-1) :: CInt)
        (#poke struct tm,tm_year ) p_tm	(fromIntegral (year-1900) :: CInt)
        (#poke struct tm,tm_isdst) p_tm	(-1 :: CInt)
	t <- mktime p_tm
        let t'=
#if __GLASGOW_HASKELL__ >= 603
                fromEnum t
#else
                t
#endif
        return (TOD (fromIntegral t' + fromIntegral (tz-currTZ)) 0)

foreign import ccall unsafe 
  mktime :: Ptr () -> IO CTime

-- |
{-# NOINLINE currTZ #-}
currTZ :: Int
currTZ = 
    ctTZ (unsafePerformIO (getClockTime >>= toCalendarTime)) -- Hack

-- |
parseTZ :: ReadP Int
parseTZ =  
    (char '+' >> readDecP) `mplus` (char '-' >> fmap negate readDecP)

-- |
f_read :: ReadP a -> String -> Maybe a
f_read f s = case readP_to_S f s of 
               [(x,_)] -> Just x

-- |
readHMS :: ReadP (Int, Int, Int)
readHMS = do
  hour  <- readDecP
  char ':'
  minutes <- readDecP
  char ':'
  seconds <- readDecP
  return (hour, minutes, seconds)

-- |
readYMD :: ReadP (Int, Int, Int)
readYMD = do
  year  <- readDecP
  char '-'
  month <- readDecP
  char '-'
  day   <- readDecP
  return (year, month, day)

-- |
readDateTime :: ReadP (Int, Int, Int, Int, Int, Int)
readDateTime = do
  (year, month, day) <- readYMD
  skipSpaces
  (hour, minutes, seconds) <- readHMS
  return (year, month, day, hour, minutes, seconds)
