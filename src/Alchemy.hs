module Alchemy where

import           Brick.Widgets.List
import           Lens.Micro

import           Types
import           Util
import           Recipes


initAlchemyEntry :: Item -> AlchemyEntry
initAlchemyEntry item =
  AlchemyEntry { _aprogress = CNone, _craftedItem = item }

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

alchemyMoveToComponent :: Int -> Game -> Game
alchemyMoveToComponent i game =
  maybe
      game
      (\(_, entry) ->
        maybe
            game
            (\(comp, _) -> case comp of
              RItem k ->
                game & recipes %~ listMoveToElement (initAlchemyEntry k)
              _ -> game
            )
          $  (entry ^. craftedItem & recipe & snd)
          ^? ix i
      )
    $  game
    ^. recipes
    &  listSelectedElement
