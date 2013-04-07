import Prisoners
import Agent
import Helpers
import Genetics
import Data.Maybe
import Debug.Trace
import Types
import System.Random
import qualified Data.Map as Map

-- assumes the one being tested is first
similarity :: [(Bool,Bool)] -> [(Bool,Bool)] -> Float
similarity h1 h2 = (similar + equal)/(fromIntegral $ length combined)
    where combined = zip h1 h2
          equal    =  0.2 * (fromIntegral $ length $ filter (\(a,b) -> b == a) combined)
          -- causing them to do the same thing matters too
          similar  = 0.8 * (fromIntegral $ length $ filter (\((x,_),(a,_)) -> x==a) combined)

-- creates a map based on the opposite agents, so you can compare the different responses
makeMap :: Agent -> [Interaction] -> Map.Map Agent [(Bool,Bool)]
makeMap agent int = Map.fromList $ map toTuple int
    where toTuple int = if a1 int == agent
                        then (a2 int ,reverseTuples $  hist int)
                        else (a1 int ,hist int)

sumAgent' :: Agent -> Agent -> [Interaction] -> [Interaction] -> Float
sumAgent' agent baseAgent history basis = (trace (show similarities)) (sum similarities) / (fromIntegral $ length similarities)
    where similarities = map (\int ->
                             similarity (hist int)
                             (fromJust $ Map.lookup (otherAgent int agent) baseMap)) agentHistory
          baseMap = makeMap baseAgent history
          baseHistory = filter (\int -> (a1 int == baseAgent) || (a2 int == baseAgent)) basis
          -- agentHistory is a list of the main agents interaction, ordered so their iterations are always first
          agentHistory = map (showHistory agent) $ filter (\int -> (a1 int == agent) || (a2 int == agent)) history
          -- reverse if they are second, so agent's iterations are always first
          showHistory a (Interaction a1 a2 history) = if a==a1
                                                      then (Interaction a1 a2 history)
                                                      else (Interaction a1 a2 (reverseTuples history))
          otherAgent (Interaction a1 a2 _) a = if a == a1
                                               then a2
                                               else a1


main = do
        a <- newStdGen
        print (similarity (hist $ head $ ints aint) (hist $ (ints $ gint a)!!6))
        print atit
        print $ newAgent
        print $ sumAgent' (newAgent) atit (ints $ gint a) (ints aint)
    where genetics a = newAgent : generate a 50
          agents  = atit :  generate' 50
          aint    = playRound agents 10
          gint a    = playRound (genetics a) 10
          gtit a    = head (genetics a)
          atit     = Agent{
                            function = defector,
                            name = "defector",
                            position = (4,5),
                            dna=[]
                          }
          newAgent = Agent {
                            function = compose [(staticGenes!!2,1.0)],
                            name = "mean-nil",
                            position = (4,4),
                            dna=[(staticGenes!!2,1.0)]
                            }