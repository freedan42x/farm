module Quests where

import           Brick.Widgets.List
import           Lens.Micro
import qualified Data.Vector                   as V

import           Types
import           Util
import           Info
import           Perk


canComplete :: Game -> Quest -> Bool
canComplete game quest = case quest ^. qtype of
  SubmitQuest comps ->
    and $ map (\(comp, req) -> componentCount game comp >= req) comps

unlockCulture :: Culture -> Game -> Game
unlockCulture cult game =
  game
    &  unlockedCultures
    %~ listInsert (V.length $ listElements $ game ^. unlockedCultures) cult

unlockSoil :: Int -> Game -> Game
unlockSoil i game = game & soils %~ (& ix i %~ locked .~ False)

rewardToFunction :: QuestReward -> Game -> Game
rewardToFunction = \case
  UnlockCulture cult -> unlockCulture cult . unlockPerk (initCulturePerk cult)
  UnlockSoil    num  -> unlockSoil num

completeQuest :: Game -> Quest -> Game
completeQuest game quest = case quest ^. qtype of
  SubmitQuest comps ->
    game
      &  rewardToFunction (quest ^. reward)
      &  modifyComponents (map (_2 %~ flip (-)) comps)
      &  quests
      %~ listUpdate
           (listElements (game ^. quests) <> V.fromList (quest ^. nextQuests))
      &  quests
      %~ listDeleteWith (quest ==)

unlockCultureQuest :: Culture -> [Quest] -> [(RecipeComponent, Int)] -> Quest
unlockCultureQuest cult qs comps = Quest
  { _qtype      = SubmitQuest comps
  , _rewardDesc = "Открывает " <> name cult
  , _reward     = UnlockCulture cult
  , _nextQuests = qs
  }

unlockSoilQuest :: Int -> [Quest] -> [(RecipeComponent, Int)] -> Quest
unlockSoilQuest num qs comps = Quest
  { _qtype      = SubmitQuest comps
  , _rewardDesc = "Открывает Грядка #" <> show num
  , _reward     = UnlockSoil $ num - 1
  , _nextQuests = qs
  }

cultChainQuest :: Quest
cultChainQuest = potato where
  potato    = unlockCultureQuest Potato [wheat] $ rcults [Weed ~= 7]
  wheat     = unlockCultureQuest Wheat [beetroot] $ rcults [Potato ~= 6]
  beetroot  = unlockCultureQuest Beetroot [rice] $ rcults [Wheat ~= 5]
  rice      = unlockCultureQuest Rice [buckwheat] $ ritems [FireEssence ~= 2]
  buckwheat = unlockCultureQuest Buckwheat [peas] $ ritems [Coal ~= 1]
  peas      = unlockCultureQuest Peas [broccoli] $ ritems [Cobblestone ~= 1]
  broccoli  = unlockCultureQuest Broccoli [] $ ritems [Dust ~= 1]

soilChainQuest :: Quest
soilChainQuest = soil2 where
  soil2 = unlockSoilQuest 2 [soil3] $ rmoney [G ~= 150]
  soil3 = unlockSoilQuest 3 [] $ rmoney [G ~= 500]
