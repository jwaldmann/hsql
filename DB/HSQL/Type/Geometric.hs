{-| Geometric 2D types, equipped with `SqlBind' instances.
-}
module DB.HSQL.Type.Geometric where

import DB.HSQL.Type
    (SqlType(SqlPoint,SqlLSeg,SqlPath,SqlBox,SqlPolygon,SqlCircle))
import Database.HSQL.Types(SqlBind(..))

-- | A 2D point.
data Point 
    = Point { pointX:: Double, pointY:: Double }
    deriving (Eq,Ord,Show,Read)

instance SqlBind Point where
    fromSqlValue SqlPoint s = case read s of
		                (x,y) -> Just (Point x y)
    fromSqlValue _ _ = Nothing

    toSqlValue (Point x y) = 
        '\'' : shows (x,y) "'"


-- | A 2D straight line.
data Line 
    = Line { lineBegin:: Point, lineEnd:: Point } 
    deriving (Eq, Show,Read)

instance SqlBind Line where
    fromSqlValue SqlLSeg s = 
        case read s of
	  [(x1,y1),(x2,y2)] -> Just (Line (Point x1 y1) (Point x2 y2))
    fromSqlValue _ _ = Nothing

    toSqlValue (Line (Point x1 y1) (Point x2 y2)) = 
        '\'' : shows [(x1,y1),(x2,y2)] "'"

-- | A 2D path, either open, or closed (looping). 
data Path 
    = OpenPath { pathPoints:: [Point] }
      -- ^ An open path
    | ClosedPath { pathPoints:: [Point] }
      -- ^ A looping path
    deriving (Eq, Show,Read)

instance SqlBind Path where
    fromSqlValue SqlPath ('(':s) = 
        case read ("["++init s++"]") of   -- closed path
	  ps -> Just (ClosedPath (map  (\(x,y) -> Point x y) ps))
    fromSqlValue SqlPath s = 
        case read s of   -- closed path        -- open path
	  ps -> Just (OpenPath (map  (\(x,y) -> Point x y) ps))
    fromSqlValue SqlLSeg s = 
        case read s of
	  [(x1,y1),(x2,y2)] -> Just (OpenPath [(Point x1 y1), (Point x2 y2)])
    fromSqlValue SqlPoint s = 
        case read s of
	  (x,y) -> Just (ClosedPath [Point x y])
    fromSqlValue _ _ = Nothing

    toSqlValue (OpenPath ps) = '\'' : shows ps "'"
    toSqlValue (ClosedPath ps) = "'(" ++ init (tail (show ps)) ++ "')"

-- | A 2D rectangle.
data Box 
    = Box { boxX1:: Double 
          , boxY1:: Double 
          , boxX2:: Double 
          , boxY2:: Double }
    deriving (Eq, Show,Read)

instance SqlBind Box where
    fromSqlValue SqlBox s = case read ("("++s++")") of
		              ((x1,y1),(x2,y2)) -> Just (Box x1 y1 x2 y2)
    fromSqlValue _ _ = Nothing

    toSqlValue (Box x1 y1 x2 y2) = 
        '\'' : shows ((x1,y1),(x2,y2)) "'"


-- | A 2D polygon (without holes).
data Polygon 
    = Polygon { polygonPoints:: [Point] } 
    deriving (Eq, Show,Read)

instance SqlBind Polygon where
    fromSqlValue SqlPolygon s = 
        case read ("["++init (tail s)++"]") of
	  ps -> Just (Polygon (map  (\(x,y) -> Point x y) ps))
    fromSqlValue _ _ = Nothing

    toSqlValue (Polygon ps) = 
        "'(" ++ init (tail (show ps)) ++ "')"


-- | A 2D circle
data Circle 
    = Circle { circleCenter:: Point 
             , circleRadius:: Double } 
    deriving (Eq, Show,Read)

instance SqlBind Circle where
    fromSqlValue SqlCircle s = case read ("("++init (tail s)++")") of
		                 ((x,y),r) -> Just (Circle (Point x y) r)
    fromSqlValue _ _ = Nothing

    toSqlValue (Circle (Point x y) r) = 
        "'<" ++ show (x,y) ++ "," ++ show r ++ "'>"

