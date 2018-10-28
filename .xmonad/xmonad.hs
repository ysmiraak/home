import qualified System.IO as IO
import qualified XMonad as XMonad
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
    , XMonad.logHook =          -- stdin for xmobar
        DynamicLog.dynamicLogWithPP DynamicLog.xmobarPP
        { DynamicLog.ppOutput = IO.hPutStrLn xmproc
        , DynamicLog.ppOrder = \(ws:l:t:_) -> [ws,t] -- only show workspaces and title
        , DynamicLog.ppSep = "  "                    -- separated by two spaces
        , DynamicLog.ppCurrent = DynamicLog.wrap "[" "]" -- wrap current workspace
        , DynamicLog.ppTitle = id -- vanilla title
        }
    } `EZConfig.additionalKeysP`
    [ ("M-S-t", XMonad.sendMessage ManageDocks.ToggleStruts)
    ]

-- todo auto workspaces
-- 9 emacs firefox
-- 8 vlc
-- 1 xterm
