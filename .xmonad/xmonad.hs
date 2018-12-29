import qualified System.IO as IO
import qualified XMonad as XMonad
import qualified XMonad.Actions.SpawnOn as SpawnOn
import qualified XMonad.Config.Desktop as Desktop
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Util.Run as Run

main = do
  xmproc <- Run.spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  XMonad.xmonad $ Desktop.desktopConfig
    { XMonad.logHook = mconcat
      [ XMonad.logHook Desktop.desktopConfig
      , DynamicLog.dynamicLogWithPP DynamicLog.xmobarPP
        { DynamicLog.ppOutput  = IO.hPutStrLn xmproc
        , DynamicLog.ppOrder   = \(ws:l:t:_) -> [ws,t]
        , DynamicLog.ppSep     = "   "
        , DynamicLog.ppCurrent = DynamicLog.wrap "[" "]"
        , DynamicLog.ppTitle   = id
        }
      ]
    , XMonad.startupHook = mconcat
      [ XMonad.startupHook Desktop.desktopConfig
      , SpawnOn.spawnOn "9" "emacs"
      , SpawnOn.spawnOn "8" "firefox"
      , SpawnOn.spawnOn "7" "vlc"
      ]
    , XMonad.manageHook = mconcat
      [ XMonad.manageHook Desktop.desktopConfig
      , SpawnOn.manageSpawn
      ]
    , XMonad.layoutHook =
        NoBorders.smartBorders
        $ XMonad.layoutHook Desktop.desktopConfig
    , XMonad.borderWidth = 1
    , XMonad.normalBorderColor  = "#4f4f4f"
    , XMonad.focusedBorderColor = "#dcdccc"
    , XMonad.modMask = XMonad.mod4Mask
    }
