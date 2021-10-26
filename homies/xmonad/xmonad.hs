import Data.Map
import Data.List
import System.Exit
import Graphics.X11.ExtraTypes.XF86

import XMonad hiding ( (|||) ) -- don't use the normal ||| operator
import XMonad.Layout.LayoutCombinators -- use the one from LayoutCombinators instead
import XMonad.Config.Desktop
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ThreeColumns
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W

-- Rebind Mod to the Windows key
myModMask = mod4Mask

myTerminal = "alacritty"
tall = Tall 1 (3/100) (1/2)
threeCol = ThreeCol 1 (3/100) (0.36) -- just enough space for 100 columns wide in vim
myLayout = avoidStruts $ smartBorders $ toggleLayouts Full tall ||| toggleLayouts Full threeCol ||| toggleLayouts Full (Mirror tall)

myBorderWidth = 2

musicWs = "🎵"
videoWs = "📹"
myWorkspaces = ["`", "wrk", "be", "fe", "test", videoWs, "6", "7", "8", "9", "0", "-", "=", "<=", musicWs]
myWorkspaceKeys = [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace, xK_m]

workspaceSenders = [ appName =? ("send to " ++ wsName) --> doShift wsName | wsName <- myWorkspaces ]

windowPlacement = composeAll ([
        -- use `xprop` to get window information:
        -- https://wiki.haskell.org/Xmonad/Frequently_asked_questions#A_handy_script_to_print_out_window_information

        className =? "Chromium" <&&> fmap (isInfixOf "Google Play Music") title --> doShift musicWs,
        appName =? "meet.google.com__zhw-huyd-oam" <&&> className =? "Chromium" --> doShift videoWs,

        -- Fix for GIMP windows
        className =? "Gimp" --> doFloat,

        -- Music stuff
        className =? "Mcg" --> doShift musicWs,

        appName =? "picker" --> doFloat
    ] ++ workspaceSenders) where role = stringProperty "WM_WINDOW_ROLE"

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


altMask = mod1Mask
myKeys conf@(XConfig {XMonad.modMask = modMask}) = Data.Map.fromList $
    [ ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    -- move focus up or down the window stack
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    , ((modMask, xK_n), windows W.focusMaster) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window
    -- Swap the focused window and the master window The default uses
    -- return, but semicolon is easier to reach =)
    , ((modMask, xK_semicolon), windows W.swapMaster)

    -- resizing the split
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the main area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the main area

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-ManageDocks.html
    , ((modMask, xK_b), sendMessage ToggleStruts)
    , ((modMask, xK_F11), fullscreenChrome)

    -- Launch a terminal
    , ((modMask .|. shiftMask, xK_semicolon), spawn $ "cd $(xcwd); exec " ++ myTerminal)

    -- Toggle layout.
    , ((modMask, xK_space), sendMessage ToggleLayout) -- Note: this doesn't work great with space as meta
    , ((modMask, xK_g), sendMessage ToggleLayout) -- Added as an alternative when using space as meta

    -- Go to next layout.
    , ((modMask, xK_t), sendMessage NextLayout)

    -- Reset to default layout
    , ((modMask .|. shiftMask, xK_g), setLayout $ XMonad.layoutHook conf)

    -- Force window back to tiling mode
    , ((modMask .|. shiftMask, xK_t), withFocused $ windows . W.sink)

    -- Toggle last workspace
    , ((modMask, xK_Tab), toggleWS)

    -- Run demenu2 with custom font
    , ((modMask, xK_p), spawn "dmenu_run -fn 'Monospace:size=11:bold:antialias=true'")

    , ((0, xF86XK_AudioMute), spawn muteAndShowVolume)
    , ((0, xF86XK_AudioRaiseVolume), spawn $ changeVolume "5+")
    , ((0, xF86XK_AudioLowerVolume), spawn $ changeVolume "5-")
    , ((0, xF86XK_AudioMicMute), spawn toggleMicMute)
    , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
    , ((0, xF86XK_AudioPrev), spawn "mpc prev")
    , ((0, xF86XK_AudioNext), spawn "mpc next")

    , ((0, xF86XK_MonBrightnessDown), spawn $ changeBrightness "5%-")
    , ((0, xF86XK_MonBrightnessUp), spawn $ changeBrightness "5%+")
    , ((0, xF86XK_HomePage), spawn $ changeBrightness "5%-")
    , ((0, xF86XK_Search), spawn $ changeBrightness "5%+")
    , ((shiftMask, xK_F5), spawn "colorscheme dark")
    , ((shiftMask, xK_F6), spawn "colorscheme light")
    -- Create our own play/pause and prev/next buttons.
    , ((modMask, xK_s), spawn "xdotool key --clearmodifiers XF86AudioPlay")
    , ((modMask, xK_d), spawn "xdotool key --clearmodifiers XF86AudioNext")
    , ((modMask .|. shiftMask, xK_d), spawn "xdotool key --clearmodifiers XF86AudioPrev")

    -- Prompt the user for an area of the screen
    -- note the sleep 0.2 as a workaround for the ancient:
    --  https://code.google.com/p/xmonad/issues/detail?id=476
    , ((0, xK_Print), spawn "sleep 0.2; jscrot --select")
    , ((controlMask, xK_Print), spawn "jscrot --video")
    , ((shiftMask, xK_Print), spawn "jscrot")

    , ((controlMask .|. altMask, xK_Left), spawn "xrandr -o right && setbg")
    , ((controlMask .|. altMask, xK_Right), spawn "xrandr -o left && setbg")
    , ((controlMask .|. altMask, xK_Down), spawn "xrandr -o normal && setbg")
    , ((controlMask .|. altMask, xK_Up), spawn "xrandr -o inverted && setbg")

    -- Dunst shortcuts
    , ((controlMask, xK_space), spawn "dunstctl close")
    , ((controlMask, xK_grave), spawn "dunstctl history-pop")

    , ((modMask, xK_a), spawn "autoperipherals")
    ] ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces myWorkspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

main = do
    xmonad $ ewmh desktopConfig {
        manageHook = manageDocks <+> manageSpawn <+> windowPlacement <+> manageHook desktopConfig,
        layoutHook = myLayout,
        modMask = myModMask,
        XMonad.terminal = myTerminal,
        XMonad.borderWidth = myBorderWidth,
        XMonad.keys = myKeys,
        workspaces = myWorkspaces
    }
