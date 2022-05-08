module Draw where

import           Brick
import           Brick.Forms
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Brick.Widgets.Dialog
import           Brick.Widgets.ProgressBar
import           Lens.Micro
import qualified Data.Vector                   as V

import           Types
import           Info
import           Util
import           Perk
import           Recipes


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

widgetIf :: Bool -> Widget n -> Widget n
widgetIf True  = id
widgetIf False = const emptyWidget

componentProgress :: Bool -> Game -> RecipeComponent -> Int -> Widget Name
componentProgress sel game comp need =
  let
    have = componentCount game comp
    prog = if have >= need
      then
        withAttr
          ( attrName
          $ (if sel then ("selected-" <>) else id)
          $ "component-enough"
          )
        $ str
        $ show need
      else
        withAttr
          ( attrName
          $ (if sel then ("selected-" <>) else id)
          $ "component-not-enough"
          )
        $  str
        $  show have
        <> "/"
        <> show need
  in
    case comp of
      RCurrency k -> hBox [prog, str $ " " <> name k]
      _           -> hBox [str $ name comp, str "(", prog, str ")"]

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

drawQuestDesc :: Game -> Quest -> Bool -> Widget Name
drawQuestDesc game quest sel = case quest ^. qtype of
  SubmitQuest comps -> hBox
    [ str "Сдать "
    , hBox
    $ map
        (\(i, (comp, k)) -> componentProgress sel game comp k
          <+> widgetIf (i /= length comps) (str ", ")
        )
    $ zip [1 ..] comps
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
    $ drawQuestDesc game quest (sel && game ^. curMenu == Quests)
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

cat :: String
cat = unlines
  [ "                   _ |\\_"
  , "                   \\` ..\\"
  , "              __,.-\" =__Y="
  , "            .\"        )"
  , "      _    /   ,    \\/\\_"
  , "     ((____|    )_-\\ \\_-`"
  , "     `-----'`-----` `--`"
  ]

drawPerks :: Game -> Widget Name
drawPerks game =
  joinBorders
    $ setAvailableSize (75, 20)
    $ selectedBorderLabel (game ^. curMenu == Perks) (str "Перки")
    $ hBox
        [ renderList draw (game ^. curMenu == Perks) (game ^. perks)
        , vBorder
        , padLeft (Pad 1) $ vBox
          [ strWrap $ game ^. perks & listSelectedElement & maybe
            ""
            (\(_, perk) -> perkDesc $ perk ^. ptype)
          , center $ str cat
          ]
        ]
 where
  draw sel perk =
    (if sel && game ^. curMenu == Perks then withAttr "selected" else id) $ hBox
      [ padLeft (Pad 1)
      $  padRight Max
      $  hLimit (textWidth $ name $ perk ^. ptype)
      $  progressBar (Just $ name $ perk ^. ptype)
      $  perk
      ^. ppassed
      /  timeTaken (perk ^. ptype)
      , padLeft Max
      $  padRight (Pad 1)
      $  str
      $  "("
      <> show (perk ^. phave)
      <> "/"
      <> show (submitCount $ perk ^. ptype)
      <> ")"
      ]

drawAlert :: Game -> Widget Name
drawAlert game =
  marginTop (Pad 4) $ withAttr "alert" $ hCenterLayer $ str $ game ^. alertMsg

drawQuit :: Game -> Widget Name
drawQuit game =
  overrideAttr buttonSelectedAttr "selected"
    $ renderDialog (game ^. quitDialog)
    $ vBox
        [ hCenter $ str "Действительно выйти?"
        , hCenter $ str "Прогресс будет сохранен"
        , str " "
        ]

drawControls :: Widget Name
drawControls = marginBottom (Pad 2) $ marginTop Max $ marginLeft (Pad 3) $ vBox
  [ str "Z/X           Выбрать/Отменить"
  , str "[←↑→↓]        Навигация"
  , str "Shift+[←↑→↓]  Смена фокуса"
  , str "[123]         Компонент алхимии"
  , str "C             Перки"
  , str "Q             Выход"
  ]

drawUI :: Game -> [Widget Name]
drawUI game =
  [ widgetIf (game ^. curMenu == Quit) $ drawQuit game
  , drawControls
  , drawAlert game
  , widgetIf (game ^. curMenu == Perks) $ centerLayer $ drawPerks game
  , widgetIf (game ^. curMenu == Plant)
    $ hCenterFarm (55 + 62 + 4) 55
    $ vCenterLayer
    $ drawPlant game
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

drawChooseSave :: LoadMenuState -> Widget LoadMenuName
drawChooseSave lms =
  overrideAttr listSelectedFocusedAttr "selected"
    $ setAvailableSize (29, 11)
    $ borderWithLabel (str "Выберите сохранение")
    $ vBox
        [ renderList draw True (lms ^. saveFiles)
        , widgetIf (lms ^. lmsCurMenu == CreateSave)
        $  renderForm
        $  lms
        ^. inputSaveName
        ]
  where draw _ fp = hCenter $ str fp

drawLmsControls :: Widget LoadMenuName
drawLmsControls =
  marginBottom (Pad 2) $ marginTop Max $ marginLeft (Pad 3) $ vBox
    [ str "Z/Enter       Выбрать"
    , str "[↑↓]          Навигация"
    , str "M             Создать новое сохранение"
    , str "Q             Выход"
    ]

drawLoadMenuUI :: LoadMenuState -> [Widget LoadMenuName]
drawLoadMenuUI lms = [drawLmsControls, border $ center $ drawChooseSave lms]
