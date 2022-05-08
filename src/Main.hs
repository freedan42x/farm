module Main where

import           Brick
import           Brick.BChan
import           Brick.Widgets.List
import           Brick.Widgets.ProgressBar
import           Graphics.Vty
import           Control.Monad
import           Control.Concurrent
import           Lens.Micro
import qualified Data.Vector                   as V
import           Data.Char

import           Types
import           Util
import           Draw
import           Game
import           Quests
import           Alchemy
import           Perk


handleEvent :: Game -> BrickEvent Name GameEvent -> EventM Name (Next Game)
handleEvent game (VtyEvent e) = case e of
  EvKey KLeft [MShift] -> continue $ game & curMenu %~ \case
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
    Quests    -> doQuest game
    Alchemy   -> doAlchemy game
    Perks     -> doPerks game

  EvKey (KChar 'x') [] -> continue $ case game ^. curMenu of
    Plant -> game & curMenu .~ Farm
    Perks -> game & curMenu .~ (game ^. prevMenu)
    _     -> game

  EvKey (KChar 'c') [] -> continue $ case game ^. curMenu of
    Perks -> game
    _     -> game & prevMenu .~ (game ^. curMenu) & curMenu .~ Perks

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
    Alchemy -> case ev of
      EvKey (KChar c) [] | c `elem` ['1', '2', '3'] ->
        continue $ game & alchemyMoveToComponent (digitToInt c - 1)
      ev' -> do
        newRecipes <- handleListEvent ev' $ game ^. recipes
        continue $ game & recipes .~ newRecipes
    Perks -> do
      newPerks <- handleListEvent ev $ game ^. perks
      continue $ game & perks .~ newPerks

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

initSoil :: Soil
initSoil = Soil { _progress = SNone, _locked = True }

initGame :: Game
initGame = Game
  { _gold             = 5000
  , _rubies           = 0
  , _soils            = (initSoil & locked .~ False) : replicate 8 initSoil
  , _soilIx           = 0
  , _curMenu          = Farm
  , _prevMenu         = Farm
  , _curInvSection    = ICultures
  , _alertMsg         = ""
  , _alertTime        = 0
  , _cultures         = list NCultures (V.fromList []) 1
  , _unlockedCultures = list NPlant (V.fromList [Weed]) 1
  , _quests = list NQuests (V.fromList [cultChainQuest, soilChainQuest]) 2
  , _recipes          = list
                          NAlchemy
                          (V.fromList $ map initAlchemyEntry $ enumFrom GrowthEssence)
                          4
  , _items            = list NItems (V.fromList []) 1
  , _perks            = list NPerks (V.fromList [initCulturePerk Weed]) 1
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
