module Recipes where

import           Types
import           Util


single :: a -> (Int, a)
single = (1, )

many :: Int -> a -> (Int, a)
many = (,)

recipe :: Item -> Recipe
recipe = \case
  GrowthEssence -> single $ rcults [Weed ~= 2]
  WaterEssence  -> single $ ritems [GrowthEssence ~= 1] <> rcults [Potato ~= 3]
  FireEssence   -> single $ rcults [Wheat ~= 3, Beetroot ~= 2]
  Pebble ->
    many 4 $ ritems [GrowthEssence ~= 1, WaterEssence ~= 1, FireEssence ~= 1]
  Coal            -> single $ ritems [FireEssence ~= 1] <> rcults [Rice ~= 2]
  StrengthEssence -> single $ rcults [Buckwheat ~= 3]
  Cobblestone     -> many 2 $ ritems [Pebble ~= 9, StrengthEssence ~= 1]
  Stone           -> single $ ritems [Cobblestone ~= 1, Coal ~= 1]
  Sand            -> many 2 $ ritems [Stone ~= 1, WaterEssence ~= 2]
  Dust            -> many 2 $ ritems [Sand ~= 1] <> rcults [Peas ~= 3]
  Redstone        -> many 4 $ ritems [Dust ~= 1] <> rcults [Beetroot ~= 2]
  Glowstone       -> many 4 $ ritems [Dust ~= 1] <> rcults [Broccoli ~= 2]
  CopperNugget    -> single $ ritems [Pebble ~= 2, Redstone ~= 1]
  CopperIngot     -> single $ ritems [CopperNugget ~= 4, Coal ~= 1]
  TinNugget       -> single $ ritems [Pebble ~= 2, Glowstone ~= 1]
  TinIngot        -> single $ ritems [TinNugget ~= 4, Coal ~= 1]
  BronzeIngot     -> many 4 $ ritems [CopperIngot ~= 3, TinIngot ~= 1]
