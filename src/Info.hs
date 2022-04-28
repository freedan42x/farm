module Info where

import           Lens.Micro

import           Types


class Timewise e where
  timeTaken :: e -> Float

class Info e where
  name :: e -> String
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

-- Weed
-- profit: 1 G/s
-- sellPrice: 5 G
-- timeTaken: 5s
-- perkCultCount: 20
-- perkTimeSpent: perkCultCount * timeTaken = 100s
-- perkMoneySpent: perkCultCount * sellPrice = 100 G
-- paybackCycles = 2 * profit = 2
-- paybackTime = paybackCycles * perkTimeSpent
-- there exist interval t such that done k times (with k = paybackTime/t) gives perkMoneySpent
-- (sellPrice * paybackTime) / t = perkMoneySpent
-- t = (sellPrice * paybackTime) / perkMoneySpent
-- t = (sellPrice * paybackCycles * perkTimeSpent) / (perkCultCount * sellPrice)
-- t = (2 * profit * perkCultCount * timeTaken) / (perkCultCount)
-- t = 2 * profit * timeTaken = 10
-- 20

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

instance Info Culture where
  name = \case
    Weed      -> "Сорняк"
    Potato    -> "Картофель"
    Wheat     -> "Пшеница"
    Beetroot  -> "Свекла"
    Rice      -> "Рис"
    Buckwheat -> "Гречка"
    Peas      -> "Горох"
    Broccoli  -> "Брокколи"

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

instance Info Item where
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

  buyPrice = const 0

  sellPrice item =
    let (rCount, r) = recipe item
    in  ceiling (timeTaken item) + sum
          (map (\(comp, count) -> count * sellPrice comp `div` rCount) r)

instance Info Currency where
  name = \case
    G -> "G"
    R -> "R"

  buyPrice  = const 0

  sellPrice = \case
    G -> 1
    R -> error "Items.hs: Not yet decided with ruby sell price."

instance Timewise RecipeComponent where
  timeTaken = \case
    RCulture  k -> timeTaken k
    RItem     k -> timeTaken k
    RCurrency k -> 0

instance Info RecipeComponent where
  name = \case
    RCulture  k -> name k
    RItem     k -> name k
    RCurrency k -> name k

  buyPrice = \case
    RCulture  k -> buyPrice k
    RItem     k -> buyPrice k
    RCurrency k -> buyPrice k

  sellPrice = \case
    RCulture  k -> sellPrice k
    RItem     k -> sellPrice k
    RCurrency k -> sellPrice k

rcults :: [(Culture, Int)] -> [(RecipeComponent, Int)]
rcults = map $ _1 %~ RCulture

ritems :: [(Item, Int)] -> [(RecipeComponent, Int)]
ritems = map $ _1 %~ RItem

rmoney :: [(Currency, Int)] -> [(RecipeComponent, Int)]
rmoney = map $ _1 %~ RCurrency

single :: a -> (Int, a)
single = (1, )

many :: Int -> a -> (Int, a)
many = (,)

(~=) :: a -> Int -> (a, Int)
(~=) = (,)

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

instance Timewise Perk where
  timeTaken (CulturePerk cult 1 _) = case cult of
    Weed -> _
    _    -> error "timeTaken: no information"
  timeTaken (CulturePerk cult 2 _) = case cult of
    Weed -> _
    _    -> error "timeTaken: no information"
  timeTaken _ = error $ "timeTaken: no information"
