import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ def
    { modMask = mod4Mask

    , borderWidth = 0

    , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook

    , layoutHook = avoidStruts  $  layoutHook def
    , manageHook = manageDocks <+> manageHook def
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppOrder = \(ws:l:t:_) -> [ws]
                }
    }
