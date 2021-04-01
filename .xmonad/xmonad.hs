import qualified System.IO as IO
import qualified XMonad as XMonad
import qualified XMonad.Actions.SpawnOn as SpawnOn
import qualified XMonad.Config.Desktop as Desktop
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.InsertPosition as InsertPosition
import qualified XMonad.Hooks.SetWMName as SetWMName
import qualified XMonad.Layout as Layout
import qualified XMonad.Layout.Grid as Grid
import qualified XMonad.Layout.IfMax as IfMax
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Util.Run as Run

main = do
  xmproc <- Run.spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  XMonad.xmonad $ Desktop.desktopConfig
    { XMonad.logHook = mconcat
      [ DynamicLog.dynamicLogWithPP DynamicLog.xmobarPP
        { DynamicLog.ppOutput  = IO.hPutStrLn xmproc
        , DynamicLog.ppOrder   = \(ws:l:t:_) -> [ws,t]
        , DynamicLog.ppSep     = "   "
        , DynamicLog.ppCurrent = DynamicLog.wrap "[" "]"
        , DynamicLog.ppTitle   = id }
      , XMonad.logHook Desktop.desktopConfig ]
    , XMonad.startupHook = mconcat
      [ SetWMName.setWMName "LG3D"
      , SpawnOn.spawnOn "9" "emacs"
      , SpawnOn.spawnOn "8" "firefox"
      , XMonad.startupHook Desktop.desktopConfig ]
    , XMonad.manageHook = mconcat
      [ InsertPosition.insertPosition InsertPosition.End InsertPosition.Newer
      , SpawnOn.manageSpawn
      , XMonad.manageHook Desktop.desktopConfig ]
    , XMonad.layoutHook =
        Desktop.desktopLayoutModifiers
        $ NoBorders.smartBorders
        $ ((Layout.|||)
           (IfMax.IfMax 3
             (Layout.Tall 1 (1/36) (1/2))
             Grid.Grid)
            Layout.Full)
    , XMonad.borderWidth = 1
    , XMonad.normalBorderColor  = "#4f4f4f"
    , XMonad.focusedBorderColor = "#dcdccc"
    , XMonad.modMask = XMonad.mod4Mask }
