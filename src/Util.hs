{-# LANGUAGE RankNTypes #-}
module Util where

import           Graphics.Vty.Image
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.List
import           Lens.Micro
import           Data.Maybe
import qualified Data.Vector                   as V

import           Types


rcults :: [(Culture, Int)] -> [(RecipeComponent, Int)]
rcults = map $ _1 %~ RCulture

ritems :: [(Item, Int)] -> [(RecipeComponent, Int)]
ritems = map $ _1 %~ RItem

rmoney :: [(Currency, Int)] -> [(RecipeComponent, Int)]
rmoney = map $ _1 %~ RCurrency

(~=) :: a -> Int -> (a, Int)
(~=) = (,)

farmMoveLeft, farmMoveRight, farmMoveUp, farmMoveDown :: Game -> Game
farmMoveLeft = soilIx %~ (\x -> if x `notElem` [0, 3, 6] then x - 1 else x)
farmMoveRight = soilIx %~ (\x -> if x `notElem` [2, 5, 8] then x + 1 else x)
farmMoveUp = soilIx %~ (\x -> if x >= 3 then x - 3 else x)
farmMoveDown = soilIx %~ (\x -> if x < 6 then x + 3 else x)

listUpdate :: V.Vector e -> List n e -> List n e
listUpdate v l = listReplace v (listSelected l) l

listSnoc :: e -> List n e -> List n e
listSnoc x l = listInsert (V.length $ l ^. listElementsL) x l

listLookup :: Eq k => List n (k, e) -> k -> Maybe e
listLookup l k = fmap snd $ V.find ((k ==) . fst) $ l ^. listElementsL

cultureCount :: Game -> Culture -> Int
cultureCount game = fromMaybe 0 . listLookup (game ^. cultures)

itemCount :: Game -> Item -> Int
itemCount game = fromMaybe 0 . listLookup (game ^. items)

componentCount :: Game -> RecipeComponent -> Int
componentCount game = \case
  RCulture  k -> cultureCount game k
  RItem     k -> itemCount game k
  RCurrency G -> game ^. gold
  RCurrency R -> game ^. rubies

listDeleteWith :: (e -> Bool) -> List n e -> List n e
listDeleteWith p l = case V.findIndex p (l ^. listElementsL) of
  Just i -> listRemove i l
  _      -> l

listUpdateAt
  :: (Ord k, Eq e) => k -> e -> (e -> e) -> List n (k, e) -> List n (k, e)
listUpdateAt k zero f l =
  let els = l ^. listElementsL
  in  case V.findIndex ((k ==) . fst) els of
        Just i -> case els V.! i & _2 %~ f of
          (_, x) | x == zero -> listRemove i l
          res                -> listUpdate (els V.// [(i, res)]) l
        _ -> listInsert
          (fromMaybe (V.length els) $ V.findIndex ((k <) . fst) els)
          (k, f zero)
          l

modifyCulture :: Culture -> (Int -> Int) -> Game -> Game
modifyCulture cult f game = game & cultures %~ listUpdateAt cult 0 f

modifyItem :: Item -> (Int -> Int) -> Game -> Game
modifyItem item f game = game & items %~ listUpdateAt item 0 f

modifyComponent :: RecipeComponent -> (Int -> Int) -> Game -> Game
modifyComponent comp f game = case comp of
  RCulture  k -> modifyCulture k f game
  RItem     k -> modifyItem k f game
  RCurrency G -> game & gold %~ f
  RCurrency R -> game & rubies %~ f

modifyComponents :: [(RecipeComponent, Int -> Int)] -> Game -> Game
modifyComponents fs game = foldl (&) game $ map (uncurry modifyComponent) fs

alert :: Game -> String -> Game
alert game msg = game & alertMsg .~ msg & alertTime .~ 1.5


-- https://github.com/jtdaugherty/brick/issues/74
-- Thank you, @sgraf812 !

calculateLim :: Padding -> Lens' (Context n) Int -> RenderM n (Int, Int)
calculateLim padding widthOrHeight = do
  c <- getContext
  let lim = case padding of
        Max   -> c ^. widthOrHeight
        Pad i -> c ^. widthOrHeight - i
  return (c ^. widthOrHeight, lim)

marginLeft :: Padding -> Widget n -> Widget n
marginLeft padding p =
  let (f, sz) = case padding of
        Max   -> (id, Greedy)
        Pad i -> (const i, hSize p)
  in  Widget sz (vSize p) $ do
        (w, lim) <- calculateLim padding availWidthL
        result   <- render $ hLimit lim p
        let offsetX = f (w - result ^. imageL . to imageWidth)
            offset  = Location (offsetX, 0)
        return $ addResultOffset offset $ result & imageL %~ translateX offsetX

marginRight :: Padding -> Widget n -> Widget n
marginRight padding p =
  let (f, sz) = case padding of
        Max   -> (id, Greedy)
        Pad i -> (const i, hSize p)
  in  Widget sz (vSize p) $ do
        (w, lim) <- calculateLim padding availWidthL
        result   <- render $ hLimit lim p
        let childWidth = result ^. imageL . to imageWidth
            width      = childWidth + f (w - childWidth)
        return $ result & imageL %~ resizeWidth width

marginTop :: Padding -> Widget n -> Widget n
marginTop padding p =
  let (f, sz) = case padding of
        Max   -> (id, Greedy)
        Pad i -> (const i, vSize p)
  in  Widget (hSize p) sz $ do
        (h, lim) <- calculateLim padding availHeightL
        result   <- render $ vLimit lim p
        let offsetY = f (h - result ^. imageL . to imageHeight)
            offset  = Location (0, offsetY)
        return $ addResultOffset offset $ result & imageL %~ translateY offsetY

marginBottom :: Padding -> Widget a -> Widget a
marginBottom padding p =
  let (f, sz) = case padding of
        Max   -> (id, Greedy)
        Pad i -> (const i, vSize p)
  in  Widget (hSize p) sz $ do
        (h, lim) <- calculateLim padding availHeightL
        result   <- render $ vLimit lim p
        let childHeight = result ^. imageL . to imageHeight
            height      = childHeight + f (h - childHeight)
        return $ result & imageL %~ resizeHeight height

hCenterFarm :: Int -> Int -> Widget n -> Widget n
hCenterFarm allWidgetsOccupiedWidth farmWidth p =
  Widget (hSize p) (vSize p) $ do
    c <- getContext
    let w = c ^. availWidthL
        k = (w - allWidgetsOccupiedWidth) `div` 2
    result <- render $ hLimit w p
    let w' = k + (farmWidth - imageWidth (result ^. imageL)) `div` 2
    pure $ addResultOffset (Location (w', 0)) $ result & imageL %~ translateX w'
