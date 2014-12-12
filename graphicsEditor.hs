-- A simple makeshift graphics editor prototype that will shift a simple shape.
-- Types will include Pt (Point), Shape, Rect (Rectangle), Tri (Triangle), and Circle.

-- A Point contains an X and Y coordinate pair (X,Y)
data Pt = Pt Float Float deriving (Show)

-- For our purposes, a Shape will consist of a fixed number of points.
data Shape = Rect Pt Pt Pt Pt | Circle Pt Float | Tri Pt Pt Pt deriving (Show)

-- Shift is an operation which uses the current location of a shape,
-- stored as a Point and moves it a distance dx (change in x direction), 
-- and dy (change in y direction).
shift :: Shape -> Float -> Float -> Shape
shift (Rect (Pt x1 y1) (Pt x2 y2) (Pt x3 y3) (Pt x4 y4)) dx dy =
	Rect (Pt (x1+dx) (y1+dy)) (Pt (x2+dx) (y2+dy)) (Pt (x3+dx) (y3+dy)) (Pt (x4+dx) (y4+dy))
shift (Circle (Pt x y) r) dx dy = 
	Circle (Pt(x+dx) (y+dy)) r
shift (Tri (Pt x1 y1) (Pt x2 y2) (Pt x3 y3)) dx dy = 
	Tri (Pt(x1+dx) (y1+dy)) (Pt(x2+dx) (y2+dy)) (Pt(x3+dx) (y3+dy))
