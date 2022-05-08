{-# LANGUAGE RankNTypes #-}
module Types where

import           Brick.Forms
import           Brick.Widgets.List
import           Brick.Widgets.Dialog
import           Lens.Micro
import           Lens.Micro.TH
import           GHC.Generics
import           Data.Binary
import qualified Data.Text                     as T


data Culture = Weed | Potato | Wheat | Beetroot | Rice | Buckwheat | Peas | Broccoli deriving (Eq, Ord, Generic)

data Item
  = GrowthEssence
  | WaterEssence
  | FireEssence
  | Pebble
  | Coal
  | StrengthEssence
  | Cobblestone
  | Stone
  | Sand
  | Dust
  | Redstone
  | Glowstone
  | CopperNugget
  | CopperIngot
  | TinNugget
  | TinIngot
  | BronzeIngot
  deriving (Eq, Ord, Enum, Generic)

data Currency = G | R deriving Generic

data RecipeComponent = RCulture Culture | RItem Item | RCurrency Currency deriving Generic

type Recipe = (Int, [(RecipeComponent, Int)])

data SoilProgress = SNone | SGrowing Culture Float | SDone Culture deriving Generic

data Soil = Soil { _progress :: SoilProgress
                 , _locked :: Bool
                 } deriving Generic

data QuestType = SubmitQuest { _components :: [(RecipeComponent, Int)]
                             } deriving Generic

data QuestReward = UnlockCulture Culture | UnlockSoil Int deriving Generic

data Quest = Quest { _qtype :: QuestType
                   , _rewardDesc :: String
                   , _reward :: QuestReward
                   , _nextQuests :: [Quest]
                   } deriving Generic

data CraftProgress = CNone | CCrafting Float | CDone

data AlchemyEntry = AlchemyEntry { _aprogress :: CraftProgress
                                 , _craftedItem :: Item
                                 }

data PerkType = CulturePerk { _pculture :: Culture
                            , _plevel :: Int
                            } deriving Generic

data Perk = Perk { _ptype :: PerkType
                 , _phave :: Int
                 , _ppassed :: Float
                 } deriving Generic

data GameEvent = Tick

data Name = NPlant | NCultures | NQuests | NAlchemy | NItems | NPerks deriving (Show, Eq, Ord)

data Menu = Farm | Plant | Inventory | Quests | Alchemy | Perks | Quit deriving Eq

data InventorySection = ICultures | IItems deriving Eq

data Game = Game { _saveName :: String
                 , _gold :: Int
                 , _rubies :: Int
                 , _soils :: [Soil]
                 , _soilIx :: Int
                 , _curMenu :: Menu
                 , _prevMenu :: Menu
                 , _prevMenuQuit :: Menu
                 , _curInvSection :: InventorySection
                 , _alertMsg :: String
                 , _alertTime :: Float
                 , _cultures :: List Name (Culture, Int)
                 , _unlockedCultures :: List Name Culture
                 , _quests :: List Name Quest
                 , _recipes :: List Name AlchemyEntry
                 , _items :: List Name (Item, Int)
                 , _perks :: List Name Perk
                 , _quitDialog :: Dialog Bool
                 }

data LoadMenuName = NLoadMenu | NSaveName deriving (Show, Eq, Ord)

data LmsMenu = LoadMenu | CreateSave deriving Eq

data SaveName = SaveName { _saveNameTxt :: T.Text }

data LmsEvent

data LoadMenuState = LoadMenuState { _saveFiles :: List LoadMenuName FilePath
                                   , _inputSaveName :: Form SaveName LmsEvent LoadMenuName
                                   , _lmsCurMenu :: LmsMenu
                                   , _selectedSave :: Game
                                   , _quit :: Bool
                                   }

data SaveState = SaveState { _save_gold :: Int
                           , _save_rubies :: Int
                           , _save_soils :: [Soil]
                           , _save_cultures :: [(Culture, Int)]
                           , _save_unlockedCultures :: [Culture]
                           , _save_quests :: [Quest]
                           , _save_items :: [(Item, Int)]
                           , _save_perks :: [Perk]
                           } deriving Generic

makeLenses ''Soil
makeLenses ''QuestType
makeLenses ''Quest
makeLenses ''AlchemyEntry
makeLenses ''PerkType
makeLenses ''Perk
makeLenses ''Game
makeLenses ''SaveName
makeLenses ''LoadMenuState
makeLenses ''SaveState

instance Eq Quest where
  q1 == q2 = q1 ^. rewardDesc == q2 ^. rewardDesc

instance Eq AlchemyEntry where
  e1 == e2 = e1 ^. craftedItem == e2 ^. craftedItem

instance Binary Culture
instance Binary SoilProgress
instance Binary Soil
instance Binary Item
instance Binary Currency
instance Binary RecipeComponent
instance Binary QuestType
instance Binary QuestReward
instance Binary Quest
instance Binary PerkType
instance Binary Perk
instance Binary SaveState
