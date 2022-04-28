module Quests where

import           Brick.Widgets.Core
import           Brick.Widgets.List
import           Lens.Micro
import qualified Data.Vector                   as V
import           Data.List

import           Types
import           Util
import           Info


toQuest :: QuestId -> Quest
toQuest i = case find (\quest -> quest ^. qid == i) allQuests of
  Just quest -> quest
  _          -> error $ "There is no quest with id " <> show i

addQuests :: [QuestId] -> Game -> Game
addQuests qIds game =
  game
    &  quests
    %~ (  listReplace
           (listElements (game ^. quests) <> V.fromList (map toQuest qIds))
       $  listSelected
       $  game
       ^. quests
       )

submitQuest
  :: QuestId
  -> [QuestId]
  -> String
  -> (Game -> Game)
  -> [(RecipeComponent, Int)]
  -> Quest
submitQuest i qIds rewDesc rew comps = Quest
  { _qid         = i
  , _desc        = \sel game -> hBox
                     [ str "Сдать "
                     , hBox
                     $ map
                         (\(j, (comp, k)) ->
                           componentProgress sel game comp k
                             <+> (if j /= length comps then str ", " else emptyWidget)
                         )
                     $ zip [1 ..] comps
                     ]
  , _rewardDesc  = rewDesc
  , _reward      = \game ->
                     game
                       & rew
                       & modifyComponents (map (_2 %~ flip (-)) comps)
                       & addQuests qIds
  , _canComplete = \game ->
    and $ map (\(comp, req) -> componentCount game comp >= req) comps
  }

unlockCulture :: Culture -> Game -> Game
unlockCulture cult game =
  game
    &  unlockedCultures
    %~ listInsert (V.length $ listElements $ game ^. unlockedCultures) cult

unlockSoil :: Int -> Game -> Game
unlockSoil i game = game & soils %~ (& ix i %~ locked .~ False)

-- if I don't have that perk, insert it. otherwise update the level
unlockPerk :: Culture -> Int -> Game -> Game
unlockPerk cult level game =
  let els = game ^. perks ^. listElementsL
  in  case
          V.findIndex
            (\case
              CulturePerk cult' _ _ -> cult == cult'
              _                     -> False
            )
            els
        of
          Just i -> game & perks %~ listUpdate
            (els V.// [(i, CulturePerk cult level _)])
          _ -> _ -- insert

allQuests :: [Quest]
allQuests =
  [ submitQuest (QCulture 0)
                [QCulture 1, QCultureMilestone 0]
                "Открывает Картофель"
                (unlockCulture Potato)
      $ rcults [Weed ~= 7]
    , submitQuest (QCulture 1)
                  [QCulture 2]
                  "Открывает Пшеница"
                  (unlockCulture Wheat)
      $ rcults [Potato ~= 6]
    , submitQuest (QCulture 2)
                  [QCulture 3]
                  "Открывает Свекла"
                  (unlockCulture Beetroot)
      $ rcults [Wheat ~= 5]
    , submitQuest (QCulture 3) [QCulture 4] "Открывает Рис" (unlockCulture Rice)
      $ ritems [FireEssence ~= 2]
    , submitQuest (QCulture 4)
                  [QCulture 5]
                  "Открывает Гречка"
                  (unlockCulture Buckwheat)
      $ ritems [Coal ~= 1]
    , submitQuest (QCulture 5) [QCulture 6] "Открывает Горох" (unlockCulture Peas)
      $ ritems [Cobblestone ~= 1]
    , submitQuest (QCulture 6) [] "Открывает Брокколи" (unlockCulture Broccoli)
      $ ritems [Dust ~= 1]
    ]
    <> [ submitQuest (QSoil 0) [QSoil 1] "Открывает Грядка #2" (unlockSoil 1)
         $ rmoney [G ~= 150]
       , submitQuest (QSoil 1) []        "Открывает Грядка #3" (unlockSoil 2)
         $ rmoney [G ~= 500]
       ]
    <> [ submitQuest (QCultureMilestone 0)
                     []
                     "Перк Сорняк I"
                     (unlockPerk Weed 1)
           $ rcults [Weed ~= 20]
       ]
