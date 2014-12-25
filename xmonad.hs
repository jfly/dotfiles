import XMonad hiding ( (|||) ) -- don't use the normal ||| operator
import XMonad.Layout.LayoutCombinators -- use the one from LayoutCombinators instead

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

import qualified XMonad.StackSet as W
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-NoBorders.html
import XMonad.Layout.NoBorders
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-LayoutCombinators.html

--- JFLY
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat, doCenterFloat)

-- Rebind Mod to the Windows key
myModMask = mod4Mask

myTerminal = "roxterm"
tall = Tall 1 (3/100) (1/2)
myLayout = avoidStruts $ smartBorders $ tall ||| Mirror tall ||| Full

-- https://github.com/hcchu/dotfiles/blob/master/.xmonad/xmonad.hs
showVolume = spawn "sleep 0.1; show-volume.sh"

myKeys =
    [
        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-ManageDocks.html
        ((myModMask, xK_b), sendMessage ToggleStruts),

        -- launch a terminal (changed from return to semicolon)
        ((myModMask .|. shiftMask, xK_semicolon), spawn myTerminal),

        -- Swap the focused window and the master window
        -- The default uses return, but semicolon is easier, and
        -- doesn't conflict with browers =)
        ((myModMask, xK_semicolon), windows W.swapMaster),

        -- jump directly to the Full layout
        ((myModMask, xK_m), sendMessage $ JumpToLayout "Full"),
        ((myModMask, xK_t), sendMessage $ JumpToLayout "Tall"),

        -- force window back to tiling mode
        ((myModMask .|. shiftMask, xK_t), withFocused $ windows . W.sink),

        ((0, xF86XK_AudioMute), showVolume),
        ((0, xF86XK_AudioRaiseVolume), showVolume),
        ((0, xF86XK_AudioLowerVolume), showVolume),
        -- TODO - closing lid still doesn't turn off screen on my t410 =(
        ((0, xF86XK_Display), spawn "xset dpms force off")
    ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
        -- Get real fullscreen to cover xmobar (like youtube videos)
        -- https://bbs.archlinux.org/viewtopic.php?id=138910
        -- http://www.vicfryzel.com/2011/02/26/xmonad-fullscreen-flash-video
        manageHook = (isFullscreen --> doFullFloat) <+> manageHook defaultConfig <+> manageDocks,
        --manageHook = (doF W.focusDown <+> doFullFloat) <+> manageHook defaultConfig <+> manageDocks,
        --manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = myLayout,
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "green" "" . shorten 100
        },

        modMask = myModMask,
        XMonad.terminal = myTerminal
    } `additionalKeys` myKeys
