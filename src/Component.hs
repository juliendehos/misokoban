{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IntSet qualified as IS
import Data.Maybe (isNothing)
import Language.Javascript.JSaddle (liftJSM, FromJSVal(..), ToJSVal(..))
import Miso
import Miso.Canvas as Canvas
import Miso.CSS qualified as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.String (fromMisoStringEither)

import Model
import Game
import GeneratedWorlds (allWorlds)

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

assetsUrl :: MisoString
assetsUrl = "assets/"

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

xy2ij :: (Double, Double) -> (Int, Int)
xy2ij (x, y) = (floor y `div` assetSize, floor x `div` assetSize)

ij2xy :: (Int, Int) -> (Double, Double)
ij2xy (i, j) = (fromIntegral (assetSize*j), fromIntegral (assetSize*i))

-------------------------------------------------------------------------------
-- actions
-------------------------------------------------------------------------------

data Action
  = ActionSetLevel Int
  | ActionKey IS.IntSet
  | ActionAskLevel MisoString
  | ActionAskTime
  | ActionSetTime Double
  | ActionPointer PointerEvent
  | ActionUndo

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel (ActionSetLevel l) =
  put $ mkModel l

updateModel (ActionKey keys)
  | IS.member 37 keys = doPlayMove $ playMove MoveLeft
  | IS.member 38 keys = doPlayMove $ playMove MoveUp
  | IS.member 39 keys = doPlayMove $ playMove MoveRight
  | IS.member 40 keys = doPlayMove $ playMove MoveDown
  | otherwise = pure ()

updateModel (ActionAskLevel lStr) = do
  case fromMisoStringEither lStr of
    Left err -> io_ $ consoleLog $ ms err
    Right l -> issue $ ActionSetLevel l

updateModel ActionAskTime = do
  io $ do
    liftIO $ threadDelay 1_000_000
    ActionSetTime <$> now

updateModel (ActionSetTime t) = do
  modelTime .= t
  issue ActionAskTime

updateModel (ActionPointer event) = do
  when (button event == 0) $ do
    (ip, jp) <- getPlayer <$> use modelGame
    let
      (ie, je) = xy2ij $ offset event 
      di = ie - ip
      dj = je - jp
    case (abs di > abs dj, di > 0, dj > 0) of
      (True, True, _)   -> doPlayMove $ playMove MoveDown
      (True, False, _)  -> doPlayMove $ playMove MoveUp
      (False, _, True)  -> doPlayMove $ playMove MoveRight
      (False, _, False) -> doPlayMove $ playMove MoveLeft

updateModel ActionUndo = do
  previous <- use modelPrevious
  forM_ previous $ \g -> do
    modelPrevious .= Nothing
    modelGame .= g

doPlayMove :: (Game -> Maybe Game) -> Transition Model Action
doPlayMove f = do
  g0 <- use modelGame
  let mg1 = f g0
  forM_ mg1 $ \g1 -> do
    modelPrevious .= if isRunning g1 then Just g0 else Nothing
    modelGame .= g1
    modelNbMoves += 1

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
  div_ [ id_ "main div" ]
    [ p_ [] 
        [ "This is "
        , a_ [ href_ "https://en.wikipedia.org/wiki/Sokoban" ] [ "Sokoban" ]
        , ", implemented in "
        , a_ [ href_ "https://github.com/dmjio/miso" ] [ "Miso" ]
        , ", and inspired by "
        , a_ [ href_ "https://www.mathsisfun.com/games/sokoban.html" ] [ "Math is fun" ]
        , "."
        ]
    , p_ [] [ "Use arrow keys or click inside the map to move the player." ]
    , p_ [] 
        [ select_ [ onChange ActionAskLevel ] (map fmtOption [1 .. length allWorlds])
        , button_ (undoOpts ++ [ onClick ActionUndo ]) [ "undo" ] 
        , button_ [ onClick (ActionSetLevel _modelLevel) ] [ "reset" ] 
        , button_ [ onClick (ActionSetLevel (1 + _modelLevel)) ] [ "next level" ] 
        ]
    , p_ [] [ text ("nb moves: " <> ms (show _modelNbMoves) <> status) ]
    , Canvas.canvas
        [ width_ $ ms $ show w
        , height_ $ ms $ show h
        , CSS.style_ [ CSS.border "1px solid black" ]
        , onPointerUp ActionPointer
        ]
        initCanvas
        (drawCanvas m w h)
    ]

  where
    undoOpts = [ disabled_ | isNothing _modelPrevious ]

    (w, h) = ij2xy $ getNiNj _modelGame

    status = if isRunning _modelGame then "" else ", done !!!"

    fmtOption l = 
      let lStr = ms $ show l
      in option_
          [ selected_ (_modelLevel == l), value_ lStr ]
          [ text ("level " <> lStr) ]

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
  let (bs1, bs2) = getBoxes12 _modelGame 
  forM_ bs1 $ \ij -> 
    let (x, y) = ij2xy ij
    in drawImage (_resBox1, x, y)
  forM_ bs2 $ \ij -> 
    let (x, y) = ij2xy ij
    in drawImage (_resBox2, x, y)

  -- draw player
  let (xp, yp) = _modelGame & getPlayer & ij2xy
  drawImage (_resPlayer, xp, yp)

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component initialModel updateModel viewModel)
    { subs = [ keyboardSub ActionKey ]
    , events = defaultEvents <> pointerEvents
    , initialAction = Just ActionAskTime
    -- , logLevel = DebugAll
    }

