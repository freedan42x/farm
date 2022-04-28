module Game where

import           Brick.Widgets.List
import           Lens.Micro

import           Types
import           Util
import           Info


tickStep :: Game -> Game
tickStep game =
  let alertDone = game ^. alertTime & (0 >=)
  in  game
        &  soils
        %~ map
             (progress %~ \case
               SDone cult -> SDone cult
               SGrowing cult passed | passed >= timeTaken cult -> SDone cult
                                    | otherwise -> SGrowing cult (passed + 0.1)
               SNone -> SNone
             )
        &  (if alertDone then alertMsg .~ "" else alertTime %~ (subtract 0.1))
        &  recipes
        %~ fmap
             (\entry -> entry & aprogress %~ \case
               CDone -> CDone
               CCrafting passed
                 | passed >= timeTaken (entry ^. craftedItem) -> CDone
                 | otherwise -> CCrafting (passed + 0.1)
               CNone -> CNone
             )

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

completeQuest :: Game -> Game
completeQuest game = case game ^. quests & listSelectedElement of
  Just (i, quest) -> if (quest ^. canComplete) game
    then (quest ^. reward) game & quests %~ listRemove i
    else alert game "Не выполнены все условия"
  _ -> game

canCraft :: Game -> Item -> Bool
canCraft game item =
  and $ map (\(comp, req) -> componentCount game comp >= req) $ snd $ recipe
    item

craftItem :: Game -> Item -> Game
craftItem game item =
  game
    &  modifyComponents (map (_2 %~ flip (-)) $ snd $ recipe item)
    &  recipes
    %~ listModify (aprogress .~ CCrafting 0)

doAlchemy :: Game -> Game
doAlchemy game = case game ^. recipes & listSelectedElement of
  Just (_, entry) -> case entry ^. aprogress of
    CDone ->
      game
        &  modifyItem (entry ^. craftedItem)
                      (+ (fst $ recipe $ entry ^. craftedItem))
        &  recipes
        %~ listModify (aprogress .~ CNone)
    CCrafting _ -> game
    CNone       -> if entry ^. craftedItem & canCraft game
      then craftItem game (entry ^. craftedItem)
      else alert game "Не хватает ингредиентов"
  _ -> game
