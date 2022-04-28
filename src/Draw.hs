module Draw where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Brick.Widgets.ProgressBar
import           Lens.Micro
import qualified Data.Vector                   as V

import           Types
import           Info
import           Util


selectedMenu :: Bool -> Widget Name -> Widget Name
selectedMenu True  = withAttr "selected-menu"
selectedMenu False = id

selected :: Bool -> Widget Name -> Widget Name
selected True = withAttr "selected"
selected _    = id

selectedInv :: Bool -> Widget Name -> Widget Name
selectedInv True = withAttr "selected-inv"
selectedInv _    = id

selectedBorderLabel :: Bool -> Widget Name -> Widget Name -> Widget Name
selectedBorderLabel True label = overrideAttr borderAttr ("selected-menu")
  . borderWithLabel (withAttr "selected-menu" label)
selectedBorderLabel False label = borderWithLabel label

drawSoil :: Game -> Int -> Widget Name
drawSoil game i =
  let soil = (game ^. soils) !! i
  in
    setAvailableSize (17, 10) $ vBox
      [ selected (i == game ^. soilIx && game ^. curMenu == Farm)
      $  hCenter
      $  str
      $  "Грядка #"
      <> show (i + 1)
      , padTop (Pad 1) $ if soil ^. locked
        then hCenter $ str "Закрыто"
        else case soil ^. progress of
          SGrowing cult passed -> padLeftRight 1 $ vBox
            [ hCenter $ str $ name cult
            , padBottom (Pad 1)
            $ padTop Max
            $ progressBar Nothing
            $ passed
            / timeTaken cult
            ]
          SDone cult -> padLeftRight 1 $ vBox
            [ hCenter $ str $ name cult
            , padBottom (Pad 1) $ padTop Max $ withAttr "ready" $ hCenter $ str
              "Готово"
            ]
          _ -> str ""
      ]

drawFarm :: Game -> Widget Name
drawFarm game =
  setAvailableSize (55, 31)
    $ joinBorders
    $ selectedBorderLabel (game ^. curMenu == Farm) (str "Ферма")
    $ vBox
        [ hBox
          [drawSoil game 0, vBorder, drawSoil game 1, vBorder, drawSoil game 2]
        , hBorder
        , hBox
          [drawSoil game 3, vBorder, drawSoil game 4, vBorder, drawSoil game 5]
        , hBorder
        , hBox
          [drawSoil game 6, vBorder, drawSoil game 7, vBorder, drawSoil game 8]
        ]

drawPlant :: Game -> Widget Name
drawPlant game =
  overrideAttr listSelectedFocusedAttr "selected"
    $ setAvailableSize (29, 11)
    $ selectedBorderLabel (game ^. curMenu == Plant) (str "Выбор растения")
    $ renderList draw (game ^. curMenu == Plant) (game ^. unlockedCultures)
 where
  draw _ cult = hBox
    [ padLeft (Pad 1) $ str $ name cult
    , padRight (Pad 1) $ padLeft Max $ str $ show (buyPrice cult) <> " G"
    ]

drawStats :: Game -> Widget Name
drawStats game = setAvailableSize (31, 3) $ border $ padBottom Max $ hBox
  [ padLeft (Pad 1) $ str $ show $ game ^. gold
  , padRight (Pad 1) $ padLeft Max $ str "G"
  ]

drawInventory :: Game -> Widget Name
drawInventory game =
  joinBorders
    $ overrideAttr listSelectedFocusedAttr "selected"
    $ setAvailableSize (31, 13)
    $ selectedBorderLabel (game ^. curMenu == Inventory) (str "Инвентарь")
    $ let sectCults = game ^. curInvSection == ICultures
      in
        vBox
          [ vLimit 1 $ hBox
            [ selectedInv sectCults $ padLeft (Pad 3) $ str "Культуры   "
            , vBorder
            , selectedInv (not sectCults) $ padRight (Pad 3) $ padLeft Max $ str
              "Предметы"
            ]
          , hBorder
          , renderList
            draw
            (game ^. curMenu == Inventory)
            (if sectCults
              then fmap (_1 %~ RCulture) $ game ^. cultures
              else fmap (_1 %~ RItem) $ game ^. items
            )
          ]
 where
  draw _ (k, count) = hBox
    [ padLeft (Pad 1) $ str $ name k <> "(" <> show count <> ")"
    , padRight (Pad 1) $ padLeft Max $ str $ show (sellPrice k) <> " G"
    ]

drawQuests :: Game -> Widget Name
drawQuests game =
  joinBorders
    $ setAvailableSize (31, 13)
    $ selectedBorderLabel (game ^. curMenu == Quests) (str "Задания")
    $ renderListWithIndex draw (game ^. curMenu == Quests) (game ^. quests)
 where
  draw i sel quest = vBox
    [ selected (sel && game ^. curMenu == Quests)
    $ padLeft (Pad 1)
    $ padRight Max
    $ (quest ^. desc) (sel && game ^. curMenu == Quests) game
    , withAttr "star" $ str $ " *  " <> quest ^. rewardDesc
    , if i + 1 == (game ^. quests ^. listElementsL & V.length)
      then emptyWidget
      else hBorder
    ]

drawAlchemy :: Game -> Widget Name
drawAlchemy game =
  joinBorders
    $ setAvailableSize (31, 31)
    $ selectedBorderLabel (game ^. curMenu == Alchemy) (str "Алхимия")
    $ renderListWithIndex draw (game ^. curMenu == Alchemy) (game ^. recipes)
 where
  draw i sel entry =
    let
      item   = entry ^. craftedItem
      (c, r) = recipe item
    in
      vBox
        [ (if sel && game ^. curMenu == Alchemy then withAttr "selected" else id
          )
          $ hBox
              [ padLeft (Pad 1) $ padRight Max $ case entry ^. aprogress of
                CCrafting passed ->
                  hLimit (textWidth $ name item)
                    $ progressBar (Just $ name item)
                    $ passed
                    / timeTaken item
                CDone ->
                  withAttr "ready"
                    $ hLimit (textWidth $ name item)
                    $ hCenter
                    $ str
                    $ name item
                _ -> str $ name item
              , padRight (Pad 1) $ str $ "(" <> show c <> ")"
              ]
        , vBox $ r <&> \(comp, k) ->
          hBox [str $ " -  ", componentProgress False game comp k]
        , if i + 1 == (game ^. recipes ^. listElementsL & V.length)
          then emptyWidget
          else hBorder
        ]

drawPerks :: Game -> Widget Name
drawPerks game = _

drawAlert :: Game -> Widget Name
drawAlert game =
  marginLeft (Pad 1) $ marginRight (Pad 1) $ hCenter $ str $ game ^. alertMsg

drawUI :: Game -> [Widget Name]
drawUI game =
  [ marginTop (Pad 1) $ withAttr "alert" $ drawAlert game
  , if game ^. curMenu == Plant
    then hCenterFarm (55 + 62 + 4) 55 $ vCenterT $ drawPlant game
    else emptyWidget
  , border $ center $ hBox
    [ drawFarm game
    , padLeft (Pad 2) $ vBox
      [ drawStats game
      , padTop (Pad 1) $ drawInventory game
      , padTop (Pad 1) $ drawQuests game
      ]
    , padLeft (Pad 2) $ drawAlchemy game
    ]
  ]
