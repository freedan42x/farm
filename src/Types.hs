module Types where

import           Brick.Types
import           Brick.Widgets.List
import           Lens.Micro.TH


data Culture = Weed | Potato | Wheat | Beetroot | Rice | Buckwheat | Peas | Broccoli deriving (Eq, Ord)

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
  deriving (Eq, Ord, Enum)

data Currency = G | R

data RecipeComponent = RCulture Culture | RItem Item | RCurrency Currency

type Recipe = (Int, [(RecipeComponent, Int)])

data SoilProgress = SNone | SGrowing Culture Float | SDone Culture

data Soil = Soil { _progress :: SoilProgress
                 , _locked :: Bool
                 }

data QuestId = QCulture Int | QSoil Int | QCultureMilestone Int deriving (Eq, Show)

data Quest = Quest { _qid :: QuestId
                   , _desc :: Bool -> Game -> Widget Name
                   , _rewardDesc :: String
                   , _reward :: Game -> Game
                   , _canComplete :: Game -> Bool
                   }

data CraftProgress = CNone | CCrafting Float | CDone

data AlchemyEntry = AlchemyEntry { _aprogress :: CraftProgress
                                 , _craftedItem :: Item
                                 }

data Perk = CulturePerk Culture Int Float

data GameEvent = Tick

data Name = NPlant | NCultures | NQuests | NAlchemy | NItems | NPerks deriving (Show, Eq, Ord)

data Menu = Farm | Plant | Inventory | Quests | Alchemy deriving Eq

data InventorySection = ICultures | IItems deriving Eq

data Game = Game { _gold :: Int
                 , _rubies :: Int
                 , _soils :: [Soil]
                 , _soilIx :: Int
                 , _curMenu :: Menu
                 , _curInvSection :: InventorySection
                 , _alertMsg :: String
                 , _alertTime :: Float
                 , _cultures :: List Name (Culture, Int)
                 , _unlockedCultures :: List Name Culture
                 , _quests :: List Name Quest
                 , _recipes :: List Name AlchemyEntry
                 , _items :: List Name (Item, Int)
                 , _perks :: List Name Perk
                 }

makeLenses ''Soil
makeLenses ''Quest
makeLenses ''AlchemyEntry
makeLenses ''Game
