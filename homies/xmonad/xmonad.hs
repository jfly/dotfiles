import Data.List
import XMonad hiding ( (|||) ) -- don't use the normal ||| operator
import XMonad.Config.Desktop
import XMonad.Layout.LayoutCombinators -- use the one from LayoutCombinators instead
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ThreeColumns
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalMouseBindings)
import Graphics.X11.ExtraTypes.XF86
import System.IO

import qualified XMonad.StackSet as W
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-NoBorders.html
import XMonad.Layout.NoBorders
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-LayoutCombinators.html

-- Rebind Mod to the Windows key
myModMask = mod4Mask

myTerminal = "alacritty"
tall = Tall 1 (3/100) (1/2)
threeCol = ThreeCol 1 (3/100) (0.36) -- just enough space for 100 columns wide in vim
myLayout = avoidStruts $ smartBorders $ toggleLayouts Full tall ||| toggleLayouts Full threeCol ||| toggleLayouts Full (Mirror tall)

myBorderWidth = 2

windowPlacement = composeAll [
        -- use `xprop` to get window information

        className =? "Chromium" <&&> fmap (isInfixOf "Google Play Music") title --> doShift "9",
        appName =? "meet.google.com__zhw-huyd-oam" <&&> className =? "Chromium" --> doShift "video",

        -- Fix for GIMP windows
        className =? "Gimp" --> doFloat,

        role =? "send to play" --> doShift "play",
        role =? "send to wrk" --> doShift "wrk",
        role =? "send to test" --> doShift "test",

        -- Experimenting with Alacritty
        appName =? "send to play" --> doShift "play",
        appName =? "send to wrk" --> doShift "wrk",
        appName =? "send to test" --> doShift "test",

        -- Music stuff
        className =? "Mcg" --> doShift "music",

        appName =? "picker" --> doFloat
    ] where role = stringProperty "WM_WINDOW_ROLE"

-- https://github.com/hcchu/dotfiles/blob/master/.xmonad/xmonad.hs
muteAndShowVolume = "set_volume.py toggle-mute; show-volume.sh"
changeVolume s = "set_volume.py " ++ s ++ "; show-volume.sh"

-- https://obsproject.com/forum/threads/hotkey-to-mute-mic-input.22852/
toggleMicMute = "pactl set-source-mute $(pacmd list-sources|awk '/\\* index:/{ print $3 }') toggle; show-mic-mute.sh"
changeBrightness s = "sudo change-brightness.py " ++ s ++ "; show-brightness.sh"

fullscreenChrome :: X ()
fullscreenChrome = do
    sendMessage ToggleStruts
    spawn "sleep 0.1 && xdotool key --clearmodifiers F11"
    return ()

myWorkspaces = ["`", "web", "play", "wrk", "test", "video", "todo", "7", "8", "9", "0", "-", "=", "<=", "music"]
myWorkspaceKeys = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace, xK_m]


altMask = mod1Mask
myKeys =
    [
        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-ManageDocks.html
        ((myModMask, xK_b), sendMessage ToggleStruts),
        ((myModMask, xK_F11), fullscreenChrome),

        -- Launch a terminal (changed from return to semicolon)
        ((myModMask .|. shiftMask, xK_semicolon), spawn $ "cd $(xcwd); exec " ++ myTerminal),

        -- Swap the focused window and the master window
        -- The default uses return, but semicolon is easier, and
        -- doesn't conflict with browers =)
        ((myModMask, xK_semicolon), windows W.swapMaster),

        -- Toggle between.
        ((myModMask, xK_space), sendMessage ToggleLayout),  -- Note: this doesn't work great with space as meta
        ((myModMask, xK_g), sendMessage ToggleLayout), -- Added as an alternative when using space as meta

        -- Go to next layout.
        ((myModMask, xK_t), sendMessage NextLayout),

        -- Reset to default layout
        -- WIP: https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs#L193
        -- ((myModMask .|. shiftMask, xK_t), setLayout $ myLayout),

        -- We stole this shortcut to emulate DWM's monocle shortcut
        -- Move focus to the master window
        ((myModMask, xK_n), windows W.focusMaster),

        -- Force window back to tiling mode
        ((myModMask .|. shiftMask, xK_t), withFocused $ windows . W.sink),

        -- Toggle last workspace
        ((myModMask, xK_Tab), toggleWS),

        -- Run demenu2 with custom font
        ((myModMask, xK_p), spawn "dmenu_run -fn 'Monospace:size=11:bold:antialias=true'"),

        ((0, xF86XK_AudioMute), spawn muteAndShowVolume),
        ((0, xF86XK_AudioRaiseVolume), spawn $ changeVolume "5+"),
        ((0, xF86XK_AudioLowerVolume), spawn $ changeVolume "5-"),
        ((0, xF86XK_AudioMicMute), spawn toggleMicMute),
        ((0, xF86XK_AudioPlay), spawn "mpc toggle"),
        ((0, xF86XK_AudioPrev), spawn "mpc prev"),
        ((0, xF86XK_AudioNext), spawn "mpc next"),

        ((0, xF86XK_MonBrightnessDown), spawn $ changeBrightness "5%-"),
        ((0, xF86XK_MonBrightnessUp), spawn $ changeBrightness "5%+"),
        ((0, xF86XK_HomePage), spawn $ changeBrightness "5%-"),
        ((0, xF86XK_Search), spawn $ changeBrightness "5%+"),
        ((shiftMask, xK_F5), spawn "colorscheme dark"),
        ((shiftMask, xK_F6), spawn "colorscheme light"),

        -- Prompt the user for an area of the screen
        -- note the sleep 0.2 as a workaround for the ancient:
        --  https://code.google.com/p/xmonad/issues/detail?id=476
        ((0, xK_Print), spawn "sleep 0.2; jscrot --select"),
        ((controlMask, xK_Print), spawn "jscrot --video"),
        ((shiftMask, xK_Print), spawn "jscrot"),

        ((controlMask .|. altMask, xK_Left), spawn "xrandr -o right && setbg"),
        ((controlMask .|. altMask, xK_Right), spawn "xrandr -o left && setbg"),
        ((controlMask .|. altMask, xK_Down), spawn "xrandr -o normal && setbg"),
        ((controlMask .|. altMask, xK_Up), spawn "xrandr -o inverted && setbg"),

        -- Create our own play/pause and prev/next buttons.
        ((myModMask, xK_s), spawn "xdotool key --clearmodifiers XF86AudioPlay"),
        ((myModMask, xK_d), spawn "xdotool key --clearmodifiers XF86AudioNext"),
        ((myModMask .|. shiftMask, xK_d), spawn "xdotool key --clearmodifiers XF86AudioPrev"),

        -- Dunst shortcuts
        ((controlMask, xK_space), spawn "dunstctl close"),
        ((controlMask, xK_grave), spawn "dunstctl history-pop"),

        ((myModMask, xK_a), spawn "autoperipherals"),
        ((myModMask .|. shiftMask, xK_a), spawn "mobile.sh")
    ] ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces myWorkspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- Swap workspaces
    [
        ((myModMask .|. controlMask, k), windows $ swapWorkspaces "web" "play")
        -- ((myModMask .|. controlMask, k), sequence_ [windows $ swapWithCurrent i, windows $ W.greedyView "web"])
        | (i, k) <- zip myWorkspaces myWorkspaceKeys
    ]

main = do
    xmonad $ ewmh desktopConfig {
        manageHook = manageDocks <+> manageSpawn <+> windowPlacement <+> manageHook desktopConfig,
        layoutHook = myLayout,
        modMask = myModMask,
        XMonad.terminal = myTerminal,
        XMonad.borderWidth = myBorderWidth,
        workspaces = myWorkspaces
        -- startupHook = do
            -- spawnOn "web" "chromium"
            -- spawnOn "play" "roxterm -e \"bash -c '(cd gitting; bash)'\""
            -- spawnOn "wca" "chromium --profile-directory='Profile 1'"
    } `additionalKeys` myKeys
      `additionalMouseBindings` [
        -- ((0, 9), \_ -> spawn "jscrot --select"),
        -- ((0, 8), \_ -> spawn "center-mouse.sh")
        ]
