module Info where

import           Lens.Micro

import           Types
import           Recipes


class Timewise e where
  timeTaken :: e -> Float

class Namewise e where
  name :: e -> String

class Info e where
  buyPrice :: e -> Int
  sellPrice :: e -> Int

profit :: Culture -> Float
profit = \case
  Weed      -> 1
  Potato    -> 1.5
  Wheat     -> 1.8
  Beetroot  -> 2
  Rice      -> 2.1
  Buckwheat -> 2.2
  Peas      -> 2.3
  Broccoli  -> 2.5

instance Timewise Culture where
  timeTaken = \case
    Weed      -> 5
    Potato    -> 4
    Wheat     -> 5
    Beetroot  -> 5.5
    Rice      -> 6.5
    Buckwheat -> 10
    Peas      -> 8
    Broccoli  -> 7

instance Namewise Culture where
  name = \case
    Weed      -> "Сорняк"
    Potato    -> "Картофель"
    Wheat     -> "Пшеница"
    Beetroot  -> "Свекла"
    Rice      -> "Рис"
    Buckwheat -> "Гречка"
    Peas      -> "Горох"
    Broccoli  -> "Брокколи"

instance Info Culture where
  buyPrice = \case
    Weed      -> 0
    Potato    -> 10
    Wheat     -> 14
    Beetroot  -> 19
    Rice      -> 25
    Buckwheat -> 32
    Peas      -> 40
    Broccoli  -> 53

  sellPrice cult = ceiling (profit cult * timeTaken cult) + buyPrice cult

instance Timewise Item where
  timeTaken = \case
    GrowthEssence   -> 4
    WaterEssence    -> 3.5
    FireEssence     -> 3.5
    Pebble          -> 3
    Coal            -> 2.5
    StrengthEssence -> 8
    Cobblestone     -> 8
    Stone           -> 7
    Sand            -> 10
    Dust            -> 12
    Redstone        -> 15
    Glowstone       -> 15
    CopperNugget    -> 1.5
    CopperIngot     -> 5
    TinNugget       -> 3
    TinIngot        -> 12
    BronzeIngot     -> 27

instance Namewise Item where
  name = \case
    GrowthEssence   -> "Эссенция роста"
    WaterEssence    -> "Эссенция воды"
    FireEssence     -> "Эссенция огня"
    Pebble          -> "Камушек"
    Coal            -> "Уголь"
    StrengthEssence -> "Эссенция силы"
    Cobblestone     -> "Булыжник"
    Stone           -> "Камень"
    Sand            -> "Песок"
    Dust            -> "Пыль"
    Redstone        -> "Красная пыль"
    Glowstone       -> "Светящаяся пыль"
    CopperNugget    -> "Самородок меди"
    CopperIngot     -> "Слиток меди"
    TinNugget       -> "Самородок олова"
    TinIngot        -> "Слиток олова"
    BronzeIngot     -> "Слиток бронзы"

instance Info Item where
  buyPrice = const 0

  sellPrice item =
    let (rCount, r) = recipe item
    in  ceiling (timeTaken item) + sum
          (map (\(comp, count) -> count * sellPrice comp `div` rCount) r)

instance Namewise Currency where
  name = \case
    G -> "G"
    R -> "R"

instance Info Currency where
  buyPrice  = const 0

  sellPrice = \case
    G -> 1
    R -> error "Items.hs: Not yet decided with ruby sell price."

instance Timewise RecipeComponent where
  timeTaken = \case
    RCulture  k -> timeTaken k
    RItem     k -> timeTaken k
    RCurrency _ -> 0

instance Namewise RecipeComponent where
  name = \case
    RCulture  k -> name k
    RItem     k -> name k
    RCurrency k -> name k

instance Info RecipeComponent where
  buyPrice = \case
    RCulture  k -> buyPrice k
    RItem     k -> buyPrice k
    RCurrency k -> buyPrice k

  sellPrice = \case
    RCulture  k -> sellPrice k
    RItem     k -> sellPrice k
    RCurrency k -> sellPrice k

instance Timewise PerkType where
  timeTaken (CulturePerk cult level) =
    timeTaken cult / (fromIntegral level * 0.25)

instance Timewise Perk where
  timeTaken perk = timeTaken $ perk ^. ptype

instance Namewise PerkType where
  name (CulturePerk cult level) = name cult <> " Lv. " <> toRoman level
   where
    toRoman n =
      ["0", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"] !! n


instance Namewise Perk where
  name perk = name $ perk ^. ptype
