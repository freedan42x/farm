module Main where

import           Brick
import           Brick.BChan
import           Brick.Widgets.List
import           Brick.Widgets.ProgressBar
import           Graphics.Vty
import           Control.Monad
import           Control.Concurrent
import           Lens.Micro

import           Types
import           Util
import           Init
import           Draw
import           Game


handleEvent :: Game -> BrickEvent Name GameEvent -> EventM Name (Next Game)
handleEvent game (VtyEvent e) = case e of
  EvKey (KChar '2') []       -> continue $ game & curInvSection .~ IItems

  EvKey KLeft       [MShift] -> continue $ game & curMenu %~ \case
    Inventory -> Farm
    Quests    -> Farm
    Alchemy   -> Inventory
    other     -> other
  EvKey KRight [MShift] -> continue $ game & curMenu %~ \case
    Farm      -> Inventory
    Inventory -> Alchemy
    Quests    -> Alchemy
    other     -> other
  EvKey KDown       [MShift] -> continue $ game & curMenu .~ Quests
  EvKey KUp         [MShift] -> continue $ game & curMenu .~ Inventory

  EvKey (KChar 'z') []       -> continue $ case game ^. curMenu of
    Farm      -> checkSoil game
    Plant     -> plantCulture game
    Inventory -> sellComponent game
    Quests    -> completeQuest game
    Alchemy   -> doAlchemy game

  EvKey (KChar 'x') [] -> continue $ case game ^. curMenu of
    Plant -> game & curMenu .~ Farm
    _     -> game

  EvKey (KChar 'q') [] -> halt game

  ev                   -> case game ^. curMenu of
    Farm -> continue $ case ev of
      EvKey KLeft  [] -> farmMoveLeft game
      EvKey KRight [] -> farmMoveRight game
      EvKey KUp    [] -> farmMoveUp game
      EvKey KDown  [] -> farmMoveDown game
      _               -> game
    Plant -> do
      newUnlockedCultures <- handleListEvent ev $ game ^. unlockedCultures
      continue $ game & unlockedCultures .~ newUnlockedCultures
    Inventory -> case ev of
      EvKey KLeft  [] -> continue $ game & curInvSection .~ ICultures
      EvKey KRight [] -> continue $ game & curInvSection .~ IItems
      ev'             -> case game ^. curInvSection of
        ICultures -> do
          newCultures <- handleListEvent ev' $ game ^. cultures
          continue $ game & cultures .~ newCultures
        IItems -> do
          newItems <- handleListEvent ev' $ game ^. items
          continue $ game & items .~ newItems
    Quests -> do
      newQuests <- handleListEvent ev $ game ^. quests
      continue $ game & quests .~ newQuests
    Alchemy -> do
      newRecipes <- handleListEvent ev $ game ^. recipes
      continue $ game & recipes .~ newRecipes

handleEvent game (AppEvent Tick) = continue $ tickStep game
handleEvent game _               = continue game

app :: App Game GameEvent Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      =
    let niceColor = rgbColor 57 53 131
    in  const $ attrMap -- 97 53 131 is also nice
          defAttr
          [ ("selected"                     , bg niceColor)
          , ("selected-menu"                , fg magenta)
          , ("selected-inv"                 , white `on` black)
          , ("ready"                        , white `on` rgbColor 17 136 59)
          , ("completable-quest"            , white `on` rgbColor 97 53 131)
          , ("alert"                        , fg red)
          , ("component-enough"             , fg brightGreen)
          , ("selected-component-enough"    , brightGreen `on` niceColor)
          , ("component-not-enough"         , fg brightRed)
          , ("selected-component-not-enough", brightRed `on` niceColor)
          , ("star"                         , fg magenta)
          , (progressCompleteAttr           , bg $ rgbColor 17 136 59)
          , (progressIncompleteAttr         , bg black)
          ]
  }

main :: IO ()
main = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000

  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initGame
