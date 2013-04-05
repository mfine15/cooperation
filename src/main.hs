import Prisoners
import Agent
import Graphics
import Genetics
import Helpers

import qualified Graphics.Gloss as G
main = do
  output "Simulate" sim


  --G.display (G.InWindow "My Window" (400, 400) (0,0)) G.white (render 400 int)





    where agents = generate 225

          sim = simulate 150 1 agents