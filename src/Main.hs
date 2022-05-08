module Main where

import           Brick
import           Brick.Forms
import           Brick.BChan
import           Brick.Widgets.List
import           Brick.Widgets.Dialog
import           Brick.Widgets.ProgressBar
import           Graphics.Vty
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Lens.Micro
import qualified Data.Text                     as T
import           Data.Char
import           System.Exit
import           System.Directory

import           LoadMenu
import           Types
import           Util
import           Draw
import           Game
import           Alchemy


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

  EvKey KDown [MShift] -> continue $ game & curMenu .~ Quests
  EvKey KUp [MShift] -> continue $ game & curMenu .~ Inventory

  EvKey (KChar k) [] | k `elem` ['z', 'я'] -> case game ^. curMenu of
    Farm      -> continue $ checkSoil game
    Plant     -> continue $ plantCulture game
    Inventory -> continue $ sellComponent game
    Quests    -> continue $ doQuest game
    Alchemy   -> continue $ doAlchemy game
    Perks     -> continue $ doPerks game
    Quit      -> case game ^. quitDialog & dialogSelection of
      Just True -> do
        liftIO $ saveGame game
        halt game
      _ -> continue $ game & curMenu .~ (game ^. prevMenuQuit)

  EvKey (KChar k) [] | k `elem` ['x', 'ч'] -> continue $ case game ^. curMenu of
    Plant -> game & curMenu .~ Farm
    Perks -> game & curMenu .~ (game ^. prevMenu)
    Quit  -> game & curMenu .~ (game ^. prevMenuQuit)
    _     -> game

  -- These are two different "c"`s btw v
  EvKey (KChar k) [] | k `elem` ['c', 'с'] -> continue $ case game ^. curMenu of
    Perks -> game
    _     -> game & prevMenu .~ (game ^. curMenu) & curMenu .~ Perks

  EvKey (KChar k) [] | k `elem` ['q', 'й'] ->
    continue $ game & prevMenuQuit .~ (game ^. curMenu) & curMenu .~ Quit

  ev -> case game ^. curMenu of
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
    Quit -> do
      newQuitDialog <- handleDialogEvent ev $ game ^. quitDialog
      continue $ game & quitDialog .~ newQuitDialog

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

handleLoadMenuEvent
  :: LoadMenuState
  -> BrickEvent LoadMenuName LmsEvent
  -> EventM LoadMenuName (Next LoadMenuState)
handleLoadMenuEvent lms e'@(VtyEvent e) = case lms ^. lmsCurMenu of
  LoadMenu -> case e of
    EvKey (KChar k) [] | k `elem` ['q', 'й'] -> halt $ lms & quit .~ True

    EvKey (KChar k) [] | k `elem` ['z', 'я'] ->
      case lms ^. saveFiles & listSelectedElement of
        Just (_, svName) -> do
          game <- liftIO $ loadGame svName
          halt $ lms & selectedSave .~ game
        _ -> continue lms

    EvKey (KChar k) [] | k `elem` ['m', 'ь'] ->
      continue $ lms & lmsCurMenu .~ CreateSave

    ev -> do
      newSaveFiles <- handleListEvent ev $ lms ^. saveFiles
      continue $ lms & saveFiles .~ newSaveFiles

  CreateSave -> case e of
    EvKey KEnter [] -> do
      let svName = (lms ^. inputSaveName & formState) ^. saveNameTxt & T.unpack
      liftIO $ saveGame $ initGame & saveName .~ svName
      continue
        $  lms
        &  lmsCurMenu
        .~ LoadMenu
        &  saveFiles
        %~ listSnoc svName
        &  inputSaveName
        %~ updateFormState (SaveName "")
    _ -> do
      newInputSaveName <- handleFormEvent e' $ lms ^. inputSaveName
      continue $ lms & inputSaveName .~ newInputSaveName

handleLoadMenuEvent lms _ = continue lms

loadMenuApp :: App LoadMenuState LmsEvent LoadMenuName
loadMenuApp = App
  { appDraw         = drawLoadMenuUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleLoadMenuEvent
  , appStartEvent   = pure
  , appAttrMap      = let niceColor = rgbColor 57 53 131
                      in  const $ attrMap defAttr [("selected", bg niceColor)]
  }

main :: IO ()
main = do
  createDirectoryIfMissing False saveDirectory

  initial <- initLoadMenu
  lms     <- defaultMain loadMenuApp initial

  when (lms ^. quit) exitSuccess

  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000

  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app $ lms ^. selectedSave
