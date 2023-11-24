{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (bracket)

import Sound.ProteaAudio

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( str
  )
import qualified Brick.AttrMap as A
import qualified Brick.Types as T

import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (use, (.=))

data MyState = MyState {
    _currentSound :: Sound,
    _playing :: Bool
}
makeLenses ''MyState

drawUI :: MyState -> [Widget ()]
drawUI _ = [ str "test" ]

appEvent :: BrickEvent () e -> T.EventM () MyState ()
appEvent (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey V.KEnter [] -> M.halt
        _ -> do
            soundState <- use currentSound
            playingState <- use playing

            _ <- liftIO $ soundUpdate soundState (not playingState) 1 1 0 1
            playing .= not playingState
appEvent _ = return ()

initialState :: Sound -> MyState
initialState sound = MyState { _currentSound = sound, _playing = False }

theApp :: M.App MyState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const $ A.attrMap V.defAttr []
          }

withSample :: (Sample -> IO ()) -> IO ()
withSample = bracket aquire release where
    aquire = do
        result <- initAudio 64 48000 1024
        unless result $ fail "Failed to initialize the audio system!!"
        sampleFromFile "audio/test.mp3" 1.0

    release sample = do
        result <- sampleDestroy sample
        unless result $ fail "Failed to destroy sample."
        finishAudio

main :: IO ()
main = withSample $ \sample -> do
    sound <- soundPlay sample 1 1 0 1

    _ <- M.defaultMain theApp $ initialState sound
    return ()
