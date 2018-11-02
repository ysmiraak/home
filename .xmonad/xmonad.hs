import qualified System.IO as IO
import qualified XMonad as XMonad
import qualified XMonad.Actions.SpawnOn as SpawnOn
import qualified XMonad.Config.Desktop as Desktop
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Util.Run as Run

main = do
  xmproc <- Run.spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  XMonad.xmonad $ Desktop.desktopConfig
    { XMonad.modMask = XMonad.mod4Mask -- use the windows key as mod
    , XMonad.borderWidth = 0 -- no border
    , XMonad.layoutHook =
        Tabbed.simpleTabbed     -- add a fullscreen tabbed layout
        XMonad.||| XMonad.layoutHook Desktop.desktopConfig
    , XMonad.manageHook =
        SpawnOn.manageSpawn     -- react on spawned process
        XMonad.<+> XMonad.manageHook Desktop.desktopConfig
    , XMonad.startupHook = do
        XMonad.startupHook Desktop.desktopConfig
        SpawnOn.spawnOn "9" "emacs"
        SpawnOn.spawnOn "8" "firefox"
        SpawnOn.spawnOn "7" "vlc"
    , XMonad.logHook =          -- stdin for xmobar
        DynamicLog.dynamicLogWithPP DynamicLog.xmobarPP
        { DynamicLog.ppOutput = IO.hPutStrLn xmproc
        , DynamicLog.ppOrder = \(ws:l:t:_) -> [ws,t] -- only show workspaces and title
        , DynamicLog.ppSep = "   "                   -- separated by spaces
        , DynamicLog.ppCurrent = DynamicLog.wrap "[" "]" -- wrap current workspace
        , DynamicLog.ppTitle = id -- vanilla title
        }
    } `EZConfig.additionalKeysP`
    [ ("M-f", XMonad.sendMessage ManageDocks.ToggleStruts)
    ]
