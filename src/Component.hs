{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Data.IntSet qualified as IS
import Language.Javascript.JSaddle (JSM, liftJSM, FromJSVal(..), ToJSVal(..))
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.CSS as CSS

import Model
import World

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

assetsUrl :: MisoString
assetsUrl = "assets/"

-------------------------------------------------------------------------------
-- actions
-------------------------------------------------------------------------------

data Action
  = ActionSayHelloWorld
  | ActionKey IS.IntSet

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel ActionSayHelloWorld =
  io_ $ do
    consoleLog "Hello World"
    alert "Hello World"

updateModel (ActionKey keys)
  | IS.member 37 keys = do
      modelPos %= over _1 (subtract 1)
      io_ $ consoleLog "left"
  | IS.member 38 keys = do
      modelPos %= over _2 (subtract 1)
      io_ $ consoleLog "up"
  | IS.member 39 keys = do
      modelPos %= over _1 (+1)
      io_ $ consoleLog "right"
  | IS.member 40 keys = do
      modelPos %= over _2 (+1)
      io_ $ consoleLog "down"
  | otherwise = pure ()

-------------------------------------------------------------------------------
-- resources
-------------------------------------------------------------------------------

data Resources = Resources
  { _resEmpty :: Image
  , _resWall :: Image
  }

instance ToJSVal Resources where
  toJSVal Resources{..} = toJSVal [_resEmpty, _resWall]

instance FromJSVal Resources where
  fromJSVal v = do
    [empty', wall'] <- fromJSValUnchecked v
    pure $ Just $ Resources empty' wall'

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

assetSizeD :: Double
assetSizeD = fromIntegral assetSize

viewModel :: Model -> View Model Action
viewModel m = 
  div_ []
    [ p_ [] [ button_ [ onClick ActionSayHelloWorld ] [ "Alert Hello World!" ] ]
    , Canvas.canvas
        [ width_ "300"
        , height_ "300"
        ]
        initCanvas
        (drawCanvas m)
    ]

initCanvas :: DOMRef -> Canvas Resources
initCanvas _ = liftJSM $ 
  Resources <$> newImage (assetsUrl <> "empty.png")
            <*> newImage (assetsUrl <> "wall.png")

drawCanvas :: Model -> Resources -> Canvas ()
drawCanvas Model{..} Resources{..} = do
  globalCompositeOperation DestinationOver
  clearRect (0, 0, 300, 300)
  let (i, j) = _modelPos
  drawImage (_resWall, fromIntegral (assetSize*i), fromIntegral (assetSize*j))
  fillStyle (Canvas.color CSS.red)
  fillRect (0, 0, 300, 300)

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { subs = [ keyboardSub ActionKey ]
    , logLevel = DebugAll
    }

