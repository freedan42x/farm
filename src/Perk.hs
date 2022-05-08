module Perk where

import           Brick.Widgets.List
import           Lens.Micro
import qualified Data.Vector                   as V

import           Types
import           Info


roundPrec :: Float -> Int -> Float
roundPrec x p = let k = 10 ^ p in (fromIntegral $ round $ x * k) / k

perkDesc :: PerkType -> String
perkDesc perk = if perk ^. plevel == 0
  then
    "Пока ничего не производит. Нужно "
    <> name (perk ^. pculture)
    <> "("
    <> show (submitCount perk)
    <> ")"
  else
    let s = roundPrec (timeTaken perk) 1
    in  "Производит "
        <> name (perk ^. pculture)
        <> " каждые "
        <> (if fromIntegral (round s) == s then show $ round s else show s)
        <> "с"

submitCount :: PerkType -> Int
submitCount = submitCountCur . (plevel +~ 1) where
  submitCountCur perk = case perk ^. plevel of
    0 -> 0
    _ -> submitCountCur (perk & plevel -~ 1) + ceiling
      (25 * fromIntegral (perk ^. plevel) * profit (perk ^. pculture))

unlockPerk :: Perk -> Game -> Game
unlockPerk perk game =
  game & perks %~ listInsert (V.length $ game ^. perks ^. listElementsL) perk

initCulturePerk :: Culture -> Perk
initCulturePerk cult = Perk
  { _ptype   = CulturePerk { _pculture = cult, _plevel = 0 }
  , _phave   = 0
  , _ppassed = 0
  }
