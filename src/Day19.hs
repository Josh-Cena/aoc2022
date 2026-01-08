module Day19 (solve1, solve2) where

import Data.Text (Text)
import Data.Text qualified as T
import Utils

data Blueprint = Blueprint
  { oreReq :: Int, -- x ore
    clayReq :: Int, -- x ore
    obsidianReq :: (Int, Int), -- x ore, y clay
    geodeReq :: (Int, Int) -- x ore, y obsidian
  }
  deriving (Show)

data State = State
  { ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geode :: Int,
    oreBots :: Int,
    clayBots :: Int,
    obsidianBots :: Int,
    geodeBots :: Int,
    time :: Int
  }
  deriving (Show)

data BotType = Ore | Clay | Obsidian | Geode

initState :: State
initState =
  State
    { ore = 0,
      clay = 0,
      obsidian = 0,
      geode = 0,
      oreBots = 1,
      clayBots = 0,
      obsidianBots = 0,
      geodeBots = 0,
      time = 0
    }

solve1 :: [Text] -> IO ()
solve1 input = do
  let blueprints = map parseBlueprint input
  let rewards = map (bestOutput 24) blueprints
  print $ sum [idx * reward | (idx, reward) <- zip [1 ..] rewards]

solve2 :: [Text] -> IO ()
solve2 input = do
  let blueprints = map parseBlueprint (take 3 input)
  let rewards = map (bestOutput 32) blueprints
  print $ product rewards

parseBlueprint :: Text -> Blueprint
parseBlueprint line =
  Blueprint
    { oreReq = getNum 6,
      clayReq = getNum 12,
      obsidianReq = (getNum 18, getNum 21),
      geodeReq = (getNum 27, getNum 30)
    }
  where
    words = T.words line
    getNum pos = readT (words !! pos)

-- Run the state forward for dur minutes, not building any bots.
runFor :: Int -> State -> State
runFor dur st =
  st
    { ore = ore st + oreBots st * dur,
      clay = clay st + clayBots st * dur,
      obsidian = obsidian st + obsidianBots st * dur,
      geode = geode st + geodeBots st * dur,
      time = time st + dur
    }

ceilDiv :: Int -> Int -> Int
ceilDiv a b = (a + b - 1) `div` b

timeToMeetReq :: Int -> Int -> Int -> Int
timeToMeetReq req cur rate
  | short <= 0 = 0
  | otherwise = ceilDiv short rate
  where
    short = req - cur

timeToBuildBot :: BotType -> Blueprint -> State -> Int
timeToBuildBot Ore bp st = timeToMeetReq (oreReq bp) (ore st) (oreBots st) + 1
timeToBuildBot Clay bp st = timeToMeetReq (clayReq bp) (ore st) (oreBots st) + 1
timeToBuildBot Obsidian bp st =
  let (oreSpent, claySpent) = obsidianReq bp
      durOre = timeToMeetReq oreSpent (ore st) (oreBots st) + 1
      durClay = timeToMeetReq claySpent (clay st) (clayBots st) + 1
   in max durOre durClay
timeToBuildBot Geode bp st =
  let (oreSpent, obsidianSpent) = geodeReq bp
      durOre = timeToMeetReq oreSpent (ore st) (oreBots st) + 1
      durObsidian = timeToMeetReq obsidianSpent (obsidian st) (obsidianBots st) + 1
   in max durOre durObsidian

buildBot :: BotType -> Blueprint -> State -> State
buildBot ty bp st = case ty of
  Ore -> st' {oreBots = oreBots st' + 1, ore = ore st' - oreReq bp}
  Clay -> st' {clayBots = clayBots st' + 1, ore = ore st' - clayReq bp}
  Obsidian ->
    let (oreSpent, claySpent) = obsidianReq bp
     in st'
          { obsidianBots = obsidianBots st' + 1,
            ore = ore st' - oreSpent,
            clay = clay st' - claySpent
          }
  Geode ->
    let (oreSpent, obsidianSpent) = geodeReq bp
     in st'
          { geodeBots = geodeBots st' + 1,
            ore = ore st' - oreSpent,
            obsidian = obsidian st' - obsidianSpent
          }
  where
    dur = timeToBuildBot ty bp st
    st' = runFor dur st

maxOreReq :: Blueprint -> Int
maxOreReq bp = maximum [oreReq bp, clayReq bp, fst (obsidianReq bp), fst (geodeReq bp)]

canBuildBot :: BotType -> Int -> Blueprint -> State -> Bool
-- 1. We can't build a bot if we have no bots producing its required resources.
-- 2. We don't need to build more bots than the maximum required resources per
-- minute, because we can only spend that many resources per minute.
-- 3. We can't build a bot if we don't have enough time to build it.
canBuildBot ty t bp st = hasBot && needsMore && hasTime
  where
    hasBot = case ty of
      Ore -> True
      Clay -> True
      Obsidian -> clayBots st > 0
      Geode -> obsidianBots st > 0
    needsMore = case ty of
      Ore -> oreBots st < maxOreReq bp
      Clay -> clayBots st < snd (obsidianReq bp)
      Obsidian -> obsidianBots st < snd (geodeReq bp)
      Geode -> True
    hasTime = timeToBuildBot ty bp st + time st <= t

bestOutput :: Int -> Blueprint -> Int
bestOutput t bp = go 0 [initState]
  where
    go :: Int -> [State] -> Int
    go best [] = best
    go best (st : stk)
      | time st > t = go best stk
      | time st == t = go (max best (geode st)) stk
      | otherwise =
          let curT = time st
              bots = [Ore, Clay, Obsidian, Geode]
              moves = [buildBot ty bp | ty <- bots, canBuildBot ty t bp st]
              moves' = runFor (t - curT) : moves
              stk' = foldr (\f acc -> f st : acc) stk moves'
           in go best stk'
