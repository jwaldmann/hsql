module DB.HSQL.Type where

{-| Variety of common data types used in databases.
-}
data SqlType
    -- numeric:         
    = SqlInteger               -- ODBC, MySQL, PostgreSQL, MSI
    | SqlBigInt                -- ODBC, MySQL, PostgreSQL, MSI
    | SqlSmallInt              -- ODBC, MySQL, PostgreSQL
    | SqlTinyInt               -- ODBC, MySQL, PostgreSQL
    | SqlMedInt                --     , MySQL,  
    | SqlDecimal
      { typeSize:: Int 
      , typeDecimals:: Int } 
      -- ODBC, MySQL, PostgreSQL
    | SqlNumeric
      { typeSize:: Int 
      , typeDecimals:: Int } 
      -- ODBC, MySQL, PostgreSQL
    | SqlReal                  -- ODBC, MySQL, PostgreSQL
    | SqlDouble                -- ODBC, MySQL, PostgreSQL
    | SqlFloat                 -- ODBC

    -- monetary:
    | SqlMoney                 --     ,      , PostgreSQL

    -- character:
    | SqlChar          { typeSize:: Int }     -- ODBC, MySQL, PostgreSQL
    | SqlVarChar       { typeSize:: Int }     -- ODBC, MySQL, PostgreSQL, MSI
    | SqlLongVarChar   { typeSize:: Int }     -- ODBC
    | SqlText                  --     ,      , PostgreSQL, MSI
    | SqlWChar         { typeSize:: Int }     -- ODBC
    | SqlWVarChar      { typeSize:: Int }     -- ODBC
    | SqlWLongVarChar  { typeSize:: Int }     -- ODBC

    -- date / time:
    | SqlDate                  -- ODBC, MySQL, PostgreSQL
    | SqlTime                  -- ODBC, MySQL, PostgreSQL
    | SqlTimeTZ                --     ,      , PostgreSQL
    | SqlAbsTime               --     ,      , PostgreSQL
    | SqlRelTime               --     ,      , PostgreSQL
    | SqlTimeInterval          --     ,      , PostgreSQL
    | SqlAbsTimeInterval       --     ,      , PostgreSQL
    | SqlTimeStamp             -- ODBC, MySQL
    | SqlDateTime              --     , MySQL
    | SqlDateTimeTZ            --     , MySQL, PostgreSQL
    | SqlYear                  --     , MySQL

    -- booleans:
    | SqlBit                   -- ODBC,      , PostgreSQL

    -- enums:
    | SqlENUM                  --     , MySQL

    -- geometric types:
    | SqlPoint                 --     ,      , PostgreSQL
    | SqlLSeg                  --     ,      , PostgreSQL
    | SqlPath                  --     ,      , PostgreSQL
    | SqlBox                   --     ,      , PostgreSQL
    | SqlPolygon               --     ,      , PostgreSQL
    | SqlLine                  --     ,      , PostgreSQL  
    | SqlCircle                --     ,      , PostgreSQL

    -- network addresses:
    | SqlINetAddr              --     ,      , PostgreSQL
    | SqlCIDRAddr              --     ,      , PostgreSQL
    | SqlMacAddr               --     ,      , PostgreSQL

    -- bit strings:
    | SqlBinary        { typeSize:: Int }     -- ODBC,      , PostgreSQL
    | SqlVarBinary     { typeSize:: Int }     -- ODBC,      , PostgreSQL
    | SqlLongVarBinary { typeSize:: Int }     -- ODBC

    -- collections:
    | SqlSET                   --     , MySQL
    
    -- lobs:
    | SqlBLOB                  --     , MySQL,           , MSI

    -- unknown:
    | SqlUnknown { typeCode:: Int } 
      -- ^ HSQL returns `SqlUnknown' for all
      -- columns for which it cannot determine
      -- the right type. The `backendTypeCode' here is the
      -- internal type code returned from the
      -- backend library
    deriving (Eq,Ord,Show,Read)
