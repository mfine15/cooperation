import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

initial _ = [(0.0,0.0) :: Point]

event (EventMotion (x,y)) world = world --(x,y)
event (EventKey (MouseButton LeftButton) Up mods (x,y)) world = (x,y):world
event _                   world = world

step time world = world

draw pts = Pictures $ map f pts
  where f (x,y) = translate x y (circle 10)

main = play (InWindow "Hi" (600,600) (200,200)) white 1 (initial 0) draw event step
