-- Haskell Playground 1.0
let x = mkStdGen 4 :: StdGen

randomR (1, 5) x

let s = state (randomR (1, 5)) :: State StdGen Int

runState s x

:t state

runState rollDice x

:t replicateM

replicate' 5 's'

runState (rollManyDice 235) x


sequence (map print [1..10])

runState get' "sadljkhasfsd"

runState put' "123123"

runState (modify3 (++ "sajt")) "123"

runState ((rollManyDice 10) >> (modify3 (\s -> snd $ next s)) >> (rollManyDice 20)) x
