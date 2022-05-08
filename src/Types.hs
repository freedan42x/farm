module Types where

import           Brick.Widgets.List
import           Lens.Micro
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

data QuestId = QCulture Int | QSoil Int deriving (Eq, Show)

data QuestType = SubmitQuest { _components :: [(RecipeComponent, Int)]
                             }

data Quest = Quest { _qtype :: QuestType
                   , _rewardDesc :: String
                   , _afterCompletion :: Game -> Game
                   , _nextQuests :: [Quest]
                   }

data CraftProgress = CNone | CCrafting Float | CDone

data AlchemyEntry = AlchemyEntry { _aprogress :: CraftProgress
                                 , _craftedItem :: Item
                                 }

data PerkType = CulturePerk { _pculture :: Culture
                            , _plevel :: Int
                            }

data Perk = Perk { _ptype :: PerkType
                 , _phave :: Int
                 , _ppassed :: Float
                 }

data GameEvent = Tick

data Name = NPlant | NCultures | NQuests | NAlchemy | NItems | NPerks deriving (Show, Eq, Ord)

data Menu = Farm | Plant | Inventory | Quests | Alchemy | Perks deriving Eq

data InventorySection = ICultures | IItems deriving Eq

data Game = Game { _gold :: Int
                 , _rubies :: Int
                 , _soils :: [Soil]
                 , _soilIx :: Int
                 , _curMenu :: Menu
                 , _prevMenu :: Menu
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
makeLenses ''QuestType
makeLenses ''Quest
makeLenses ''AlchemyEntry
makeLenses ''PerkType
makeLenses ''Perk
makeLenses ''Game

instance Eq Quest where
  q1 == q2 = q1 ^. rewardDesc == q2 ^. rewardDesc

instance Eq AlchemyEntry where
  e1 == e2 = e1 ^. craftedItem == e2 ^. craftedItem
