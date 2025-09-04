{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Data.IntSet qualified as IS
import Language.Javascript.JSaddle (liftJSM, FromJSVal(..), ToJSVal(..))
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.CSS as CSS

import Model
import Game

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
      modelGame %= playMove MoveUp
      modelNbMoves += 1
  | IS.member 38 keys = do
      modelGame %= playMove MoveLeft
      modelNbMoves += 1
  | IS.member 39 keys = do
      modelGame %= playMove MoveDown
      modelNbMoves += 1
  | IS.member 40 keys = do
      modelGame %= playMove MoveRight
      modelNbMoves += 1
  | otherwise = pure ()

-------------------------------------------------------------------------------
-- resources
-------------------------------------------------------------------------------

data Resources = Resources
  { _resBox1 :: Image
  , _resBox2 :: Image
  , _resEmpty :: Image
  , _resPlayer :: Image
  , _resTarget :: Image
  , _resWall :: Image
  }

instance ToJSVal Resources where
  toJSVal Resources{..} = toJSVal [_resEmpty, _resWall]

instance FromJSVal Resources where
  fromJSVal v = do
    [b1, b2, e, p, t, w] <- fromJSValUnchecked v
    pure $ Just $ Resources b1 b2 e p t w

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

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
  Resources <$> newImage (assetsUrl <> "box1.png")
            <*> newImage (assetsUrl <> "box2.png")
            <*> newImage (assetsUrl <> "empty.png")
            <*> newImage (assetsUrl <> "player.png")
            <*> newImage (assetsUrl <> "target.png")
            <*> newImage (assetsUrl <> "wall.png")

drawCanvas :: Model -> Resources -> Canvas ()
drawCanvas Model{..} Resources{..} = do
  globalCompositeOperation DestinationOver
  clearRect (0, 0, 300, 300)

  forGame _modelGame $ \ij c -> do
    let (x, y) = ij2xy ij
    case c of
      CellT -> drawImage (_resTarget, x, y)
      CellW -> drawImage (_resWall, x, y)
      _     -> drawImage (_resEmpty, x, y)

  let (xp, yp) = _modelGame & _gameCurrentPos & ij2xy
  drawImage (_resPlayer, xp, yp)

ij2xy :: (Int, Int) -> (Double, Double)
ij2xy (i, j) = (fromIntegral (assetSize*j), fromIntegral (assetSize*i))

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  let initialMode = mkModel 1
  in (component initialMode updateModel viewModel)
      { subs = [ keyboardSub ActionKey ]
      , logLevel = DebugAll
      }

