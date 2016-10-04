module GrahamScan where
import Data.List (sortBy, sort)

data Point = Point Double Double deriving (Show, Eq)
data Vector = Vec Double Double deriving (Show, Eq)

rad :: Vector -> Double
rad (Vec x y) = atan2 y x

mag :: Vector -> Double
mag (Vec x y) = (x * x) + (y * y)

-- Vector are ordered by the radians then by length
instance Ord Vector where
    x `compare` y
        | (rad x) == (rad y)    = (mag x) `compare` (mag y)
        | otherwise             = (rad x) `compare` (rad y)

-- Points are ordered by x, to find the left most point
instance Ord Point where
    (Point x1 y1) `compare` (Point x2 y2) = x2 `compare` x1

toVec :: Point -> Point -> Vector
toVec (Point bx by) (Point ex ey) = Vec (ex-bx) (ey-by)

cross :: Vector -> Vector -> Double 
(Vec ux uy) `cross` (Vec vx vy) = (ux * vy) - (uy * vx)

sortPtsByClock :: [Point] -> [Point]
sortPtsByClock pts =
    -- Let the left most point as a pivot so it must be on the convex point list
    let pivot:rest = sort pts
    -- Now the points are clockwise ordered
    in [pivot] ++ (sortBy (\x y -> compare (toVec pivot x) (toVec pivot y)) rest)

findConvex :: [Point] -> [Point] -> [Point]
findConvex convexPts pendPts
    | pendPts == []         = convexPts
    | convexPts == []       = findConvex [nextPt] restPts
    | length convexPts == 1 = findConvex ([nextPt] ++ convexPts) restPts
    -- Convex, add point to stack
    | dir >= 0              = findConvex ([nextPt] ++ convexPts) restPts
    -- Concave, remove point from stack
    | dir < 0               = findConvex ([last2Pt] ++ stackPts) pendPts
    where 
        nextPt:restPts          = pendPts
        lastPt:last2Pt:stackPts = convexPts
        dir = cross (toVec last2Pt lastPt) (toVec lastPt nextPt)

grahamScan :: [Point] -> [Point]
grahamScan pts 
    | length pts < 3 = pts
    | otherwise = findConvex [] ( sortPtsByClock pts )


