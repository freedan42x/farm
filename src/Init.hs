module Init where

import           Brick.Widgets.List
import           Lens.Micro
import qualified Data.Vector                   as V

import           Types
import           Quests


initSoil :: Soil
initSoil = Soil { _progress = SNone, _locked = True }

aentry :: Item -> AlchemyEntry
aentry item = AlchemyEntry { _aprogress = CNone, _craftedItem = item }

initGame :: Game
initGame = Game
  { _gold             = 0
  , _rubies           = 0
  , _soils            = (initSoil & locked .~ False) : replicate 8 initSoil
  , _soilIx           = 0
  , _curMenu          = Farm
  , _curInvSection    = ICultures
  , _alertMsg         = ""
  , _alertTime        = 0
  , _cultures         = list NCultures (V.fromList []) 1
  , _unlockedCultures = list NPlant (V.fromList [Weed]) 1
  , _quests = list NQuests (V.fromList $ map toQuest [QCulture 0, QSoil 0]) 2
  , _recipes          = list NAlchemy
                             (V.fromList $ map aentry $ enumFrom GrowthEssence)
                             4
  , _items            = list NItems (V.fromList []) 1
  , _perks            = list NPerks (V.fromList []) 1
  }
