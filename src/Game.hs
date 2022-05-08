module Game where

import           Brick.Widgets.List
import           Lens.Micro
import qualified Data.Vector                   as V

import           Types
import           Util
import           Info
import           Quests
import           Perk


tickStep :: Game -> Game
tickStep game =
  let stepSoils = soils %~ map
        (progress %~ \case
          SDone cult -> SDone cult
          SGrowing cult passed | passed >= timeTaken cult -> SDone cult
                               | otherwise -> SGrowing cult (passed + 0.1)
          SNone -> SNone
        )

      alertDone   = game ^. alertTime & (0 >=)
      stepAlert   = if alertDone then alertMsg .~ "" else alertTime -~ 0.1

      stepRecipes = recipes %~ fmap
        (\entry -> entry & aprogress %~ \case
          CDone -> CDone
          CCrafting passed | passed >= timeTaken (entry ^. craftedItem) -> CDone
                           | otherwise -> CCrafting (passed + 0.1)
          CNone -> CNone
        )

      donePerks =
          game
            ^.  perks
            ^.  listElementsL
            &   V.filter (\perk -> perk ^. ppassed >= timeTaken perk)
            <&> (\perk -> (RCulture $ perk ^. ptype ^. pculture, (+ 1)))

      stepDonePerks = modifyComponents $ V.toList donePerks

      stepPerks     = perks %~ fmap
        (\perk -> if perk ^. ppassed >= timeTaken perk
          then perk & ppassed .~ 0
          else perk & ppassed +~ 0.1
        )

      stepUnlockPerks = perks %~ fmap
        (\perk -> if perk ^. phave >= submitCount (perk ^. ptype)
          then perk & ptype %~ (plevel +~ 1) & ppassed .~ 0
          else perk
        )
  in  game
        & stepSoils
        & stepAlert
        & stepRecipes
        & stepDonePerks
        & stepUnlockPerks
        & stepPerks

checkSoil :: Game -> Game
checkSoil game =
  let soil = (game ^. soils) !! (game ^. soilIx)
  in  if soil ^. locked
        then alert game "Закрыто!!"
        else case soil ^. progress of
          SDone cult -> gatherCulture game cult (game ^. soilIx)
          SGrowing{} -> game
          SNone{}    -> game & curMenu .~ Plant

plantCulture :: Game -> Game
plantCulture game = case game ^. unlockedCultures & listSelectedElement of
  Just (_, cult) -> if game ^. gold >= buyPrice cult
    then
      game
      &  soils
      %~ (ix (game ^. soilIx) %~ (progress .~ SGrowing cult 0))
      &  gold
      %~ (subtract $ buyPrice cult)
      &  curMenu
      .~ Farm
    else alert game "Недостаточно G"
  _ -> error "How could it be that there were nothing to plant?"

gatherCulture :: Game -> Culture -> Int -> Game
gatherCulture game cult i =
  game & modifyCulture cult (+ 1) & soils %~ (ix i %~ (progress .~ SNone))

sellCulture :: Game -> Game
sellCulture game = case game ^. cultures & listSelectedElement of
  Just (i, (cult, count)) ->
    (if count == 1
        then game & cultures %~ listRemove i
        else game & cultures %~ listModify (_2 %~ pred)
      )
      &  gold
      +~ (sellPrice cult)
  _ -> game

sellComponent :: Game -> Game
sellComponent game = if game ^. curInvSection == ICultures
  then
    maybe
      game
      (\(_, (cult, _)) ->
        modifyCulture cult (subtract 1) game & gold +~ sellPrice cult
      )
    $  game
    ^. cultures
    &  listSelectedElement
  else
    maybe
      game
      (\(_, (item, _)) ->
        modifyItem item (subtract 1) game & gold +~ sellPrice item
      )
    $  game
    ^. items
    &  listSelectedElement

doQuest :: Game -> Game
doQuest game = case game ^. quests & listSelectedElement of
  Just (_, quest) -> if canComplete game quest
    then completeQuest game quest
    else alert game "Не выполнены все условия"
  _ -> game

doPerks :: Game -> Game
doPerks game = case game ^. perks & listSelectedElement of
  Just (_, perk) -> case perk ^. ptype of
    CulturePerk cult _ -> if cultureCount game cult > 0
      then game & modifyCulture cult (subtract 1) & perks %~ listModify
        (phave +~ 1)
      else alert game $ "Не хватает " <> name cult
  _ -> game
