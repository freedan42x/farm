module LoadMenu where

import qualified Data.Vector                   as V
import           Brick.Forms
import           Brick.Widgets.List
import           Brick.Widgets.Dialog
import           Lens.Micro
import           System.Directory
import           Data.Binary

import           Types
import           Quests
import           Alchemy
import           Perk


saveDirectory :: String
saveDirectory = "saves/"

initSoil :: Soil
initSoil = Soil { _progress = SNone, _locked = True }

initGame :: Game
initGame = Game
  { _saveName         = ""
  , _gold             = 0
  , _rubies           = 0
  , _soils            = (initSoil & locked .~ False) : replicate 8 initSoil
  , _soilIx           = 0
  , _curMenu          = Farm
  , _prevMenu         = Farm
  , _prevMenuQuit     = Farm
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
  , _quitDialog = dialog Nothing (Just (0, [("Нет", False), ("Да", True)])) 29
  }

allSaves :: IO (List LoadMenuName FilePath)
allSaves = do
  files <- listDirectory saveDirectory
  pure $ list NLoadMenu (V.fromList files) 1

saveStateToGame :: SaveState -> Game
saveStateToGame st = initGame
  { _gold             = st ^. save_gold
  , _rubies           = st ^. save_rubies
  , _soils            = st ^. save_soils
  , _cultures         = list NCultures (V.fromList $ st ^. save_cultures) 1
  , _unlockedCultures = list NPlant (V.fromList $ st ^. save_unlockedCultures) 1
  , _quests           = list NQuests (V.fromList $ st ^. save_quests) 2
  , _items            = list NItems (V.fromList $ st ^. save_items) 1
  , _perks            = list NPerks (V.fromList $ st ^. save_perks) 1
  }

gameToSaveState :: Game -> SaveState
gameToSaveState game = SaveState
  { _save_gold             = game ^. gold
  , _save_rubies           = game ^. rubies
  , _save_soils            = game ^. soils
  , _save_cultures         = game ^. cultures ^. listElementsL . to V.toList
  , _save_unlockedCultures = game
                             ^. unlockedCultures
                             ^. listElementsL
                             .  to V.toList
  , _save_quests           = game ^. quests ^. listElementsL . to V.toList
  , _save_items            = game ^. items ^. listElementsL . to V.toList
  , _save_perks            = game ^. perks ^. listElementsL . to V.toList
  }

loadGame :: String -> IO Game
loadGame svName = do
  state <- decodeFile $ saveDirectory <> svName :: IO SaveState
  pure $ saveStateToGame state & saveName .~ svName

saveGame :: Game -> IO ()
saveGame game =
  encodeFile (saveDirectory <> game ^. saveName) $ gameToSaveState game

initLoadMenu :: IO LoadMenuState
initLoadMenu = do
  saves <- allSaves
  pure $ LoadMenuState
    { _saveFiles     = saves
    , _inputSaveName = newForm [editTextField saveNameTxt NSaveName (Just 1)]
                         $ SaveName ""
    , _lmsCurMenu    = LoadMenu
    , _selectedSave  = initGame
    , _quit          = False
    }
