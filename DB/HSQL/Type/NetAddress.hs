{-| Network addresses, equipped with `SqlBind' instances.
-}
module DB.HSQL.Type.NetAddress where

import Data.Char(intToDigit)
import Numeric(readHex,showIntAtBase,readDec)

import DB.HSQL.Type(SqlType(SqlINetAddr,SqlCIDRAddr,SqlMacAddr))
import Database.HSQL.Types(SqlBind(..))

-- | An IP4 address with netmask in CIDR notation.
data INetAddr 
    = INetAddr { ip4Octet1:: Int 
               , ip4Octet2:: Int 
               , ip4Octet3:: Int 
               , ip4Octet4:: Int 
               , cidrMaskBits:: Int }
    deriving (Eq,Ord,Show,Read)

instance SqlBind INetAddr where
    fromSqlValue t s
	| t == SqlINetAddr || t == SqlCIDRAddr =
            case readNum s of
              (x1,s) -> case readNum s of
                (x2,s) -> case readNum s of
                  (x3,s) -> case readNum s of
                    (x4,s) -> case readNum s of
                      (mask,_) -> Just (INetAddr x1 x2 x3 x4 mask)
       | otherwise = Nothing
      where readNum s = case readDec s of
			[(x,'.':s)] -> (x,s)
			[(x,'/':s)] -> (x,s)
			[(x,"")]    -> (x,"")
			_           -> (0,"")

    toSqlValue (INetAddr x1 x2 x3 x4 mask) = 
        '\'' : (shows x1 . dot .
		shows x2. dot .
		shows x3 . dot .
		shows x4 . slash .
		shows mask) "'"
      where dot = showChar '.'
	    slash = showChar '/'


-- | A MAC network address.
data MacAddr 
    = MacAddr { macOctet1:: Int 
              , macOctet2:: Int 
              , macOctet3:: Int 
              , macOctet4:: Int  
              , macOctet5:: Int  
              , macOctet6:: Int  }
    deriving (Eq,Ord,Show,Read)

instance SqlBind MacAddr where
    fromSqlValue SqlMacAddr s =
        case readHex s of
            [(x1,':':s)] -> case readHex s of
                [(x2,':':s)] -> case readHex s of
                    [(x3,':':s)] -> case readHex s of
                        [(x4,':':s)] -> case readHex s of
                            [(x5,':':s)] -> case readHex s of
                                [(x6,_)] -> Just (MacAddr x1 x2 x3 x4 x5 x6)
    fromSqlValue _ _ = Nothing

    toSqlValue (MacAddr x1 x2 x3 x4 x5 x6) = 
        '\'' : (showHex x1 . colon .
		showHex x2 . colon .
		showHex x3 . colon .
		showHex x4 . colon .
		showHex x5 .colon .
		showHex x6) "'"
      where colon = showChar ':'
	    showHex = showIntAtBase 16 intToDigit
