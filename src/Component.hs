{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Control.Monad (forM_)
import Data.IntSet qualified as IS
import Language.Javascript.JSaddle (liftJSM, FromJSVal(..), ToJSVal(..))
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
-- import Miso.CSS qualified as CSS

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
  | ActionSetLevel Int
  | ActionKey IS.IntSet

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel ActionSayHelloWorld =
  io_ $ do
    consoleLog "Hello World"
    alert "Hello World"

updateModel (ActionSetLevel n) = do
  put $ mkModel n
  io_ $ consoleLog $ "level " <> ms (show n)

updateModel (ActionKey keys)
  | IS.member 37 keys = doPlayMove $ playMove MoveLeft
  | IS.member 38 keys = doPlayMove $ playMove MoveUp
  | IS.member 39 keys = doPlayMove $ playMove MoveRight
  | IS.member 40 keys = doPlayMove $ playMove MoveDown
  | otherwise = pure ()
  where
    doPlayMove f = do
      mg <- f <$> use modelGame
      forM_ mg $ \g -> do
        modelGame .= g
        modelNbMoves += 1

{-
  g <- use modelGame
  if computeTerminated g
    then issue $ ActionSetLevel (getLevel g + 1)
    else doKey
-}

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
  toJSVal Resources{..} = 
    toJSVal
      [ _resBox1
      , _resBox2
      , _resEmpty
      , _resPlayer
      , _resTarget
      , _resWall
      ]

instance FromJSVal Resources where
  fromJSVal v = do
    [b1, b2, e, p, t, w] <- fromJSValUnchecked v
    pure $ Just $ Resources b1 b2 e p t w

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m@Model{..} = 
  div_ []
    [ p_ [] [ button_ [ onClick ActionSayHelloWorld ] [ "Alert Hello World!" ] ]
    , p_ [] [ text ("nb moves: " <> ms (show _modelNbMoves) <> status) ]
    , Canvas.canvas
        [ width_ $ ms $ show w
        , height_ $ ms $ show h
        ]
        initCanvas
        (drawCanvas m w h)
    ]
  where
    (w, h) = ij2xy $ getNiNj _modelGame
    status = if computeRunning _modelGame then "..." else ", done !!!"

initCanvas :: DOMRef -> Canvas Resources
initCanvas _ = liftJSM $ 
  Resources <$> newImage (assetsUrl <> "box1.png")
            <*> newImage (assetsUrl <> "box2.png")
            <*> newImage (assetsUrl <> "empty.png")
            <*> newImage (assetsUrl <> "player.png")
            <*> newImage (assetsUrl <> "target.png")
            <*> newImage (assetsUrl <> "wall.png")

drawCanvas :: Model -> Double -> Double -> Resources -> Canvas ()
drawCanvas Model{..} w h Resources{..} = do
  -- clear canvas
  clearRect (0, 0, w, h)

  -- draw world map
  forGame _modelGame $ \ij c -> do
    let (x, y) = ij2xy ij
    case c of
      CellT -> drawImage (_resTarget, x, y)
      CellW -> drawImage (_resWall, x, y)
      _     -> drawImage (_resEmpty, x, y)

  -- draw boxes
  let (bs1, bs2) = computeBoxes12 _modelGame 
  forM_ bs1 $ \ij -> 
    let (x, y) = ij2xy ij
    in drawImage (_resBox1, x, y)
  forM_ bs2 $ \ij -> 
    let (x, y) = ij2xy ij
    in drawImage (_resBox2, x, y)

  -- draw player
  let (xp, yp) = _modelGame & _gamePlayer & ij2xy
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
      -- , initialAction = Just (ActionSetLevel 1)
      , logLevel = DebugAll
      }

