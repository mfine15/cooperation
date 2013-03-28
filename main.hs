import Prisoners

main = do
  output "Length" (fromIntegral $ length int)
  output "Baseline" base
  output "Agents" agents
  output "Interaction" int
  output "Sums" (showSums int)
  output "winners" winners
  output "NeAgent" (makeAgent (head winners) winners)
  output "New Agents" (reproduce base int)

  where agents = generate 25
    int = playRound agents 1
    base = baseline int
    winners = [a | a <- agents, (sumAgent int a) >= (round base)]