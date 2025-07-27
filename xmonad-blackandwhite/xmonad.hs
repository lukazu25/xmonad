import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import System.IO
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.ManageHelpers (doRectFloat)

-- Black and White colors
black, white, black2, white2 :: String
black = "#000000"
white = "#FFFFFF"
black2 = "#1c1c1c"
white2 = "#e0e0e0"

-- Custom black and white tabbed theme
myTabTheme :: Theme
myTabTheme = def
  { activeColor         = white2
  , inactiveColor       = black
  , urgentColor         = white
  , activeBorderColor   = white2
  , inactiveBorderColor = black
  , urgentBorderColor   = white
  , activeTextColor     = black
  , inactiveTextColor   = white
  , urgentTextColor     = black
  , decoHeight          = 24
  , fontName            = "xft:JetBrainsMono Nerd Font-10"
  }

-- Layouts
myLayouts = avoidStruts $
  tiledLayouts ||| full ||| grid ||| tabbedLayout
  where
    -- Apply spacing only to these layouts
    tiledLayouts = spacingRaw False (Border 24 4 4 4) False (Border 4 4 4 4) True $
                     tiled ||| mirrorTiled

    tiled = ResizableTall 1 (3/100) (1/2) [] -- This is ResizableTile
    mirrorTiled = Mirror tiled
    full = Full
    grid = spacingRaw False (Border 24 4 4 4) False (Border 4 4 4 4) True Grid
    tabbedLayout = noBorders $ tabbed shrinkText myTabTheme

-- ManageHook with floating rules
myManageHook :: ManageHook
myManageHook = className =? "pavucontrol" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ docks . ewmhFullscreen . ewmh $ def
    { manageHook = manageDocks <+> myManageHook <+> manageHook def
    , layoutHook = myLayouts
    , handleEventHook = handleEventHook def
    , logHook = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor white "" . shorten 50  -- Title in white
        , ppCurrent = xmobarColor white "" . wrap "[" "]"  -- Current workspace in white
        , ppHidden = xmobarColor white ""  -- Hidden windows in white
        , ppHiddenNoWindows = xmobarColor black ""  -- Hidden windows with no content in black
        , ppUrgent = xmobarColor white "" . wrap "!" "!"  -- Urgent windows in white with "!" wrap
        }
    , modMask = mod4Mask
    , terminal = "alacritty"
    , borderWidth = 4
    , normalBorderColor = black
    , focusedBorderColor = white2
    , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , startupHook = do
        spawn "xrandr -s 1920x1080"
        spawn "nitrogen --restore"
        spawn "pkill -u $USER dunst; dunst &"
        spawn "setxkbmap -layout us,ru,ge -option 'grp:alt_shift_toggle'"
    } `additionalKeys`
    [ -- Launch terminal
      ((mod4Mask .|. shiftMask, xK_Return), spawn "alacritty")

      -- Launch dmenu
    , ((mod4Mask, xK_p), spawn "dmenu_run -fn 'JetBrainsMono Nerd Font-12' -nb '#000000' -nf '#ffffff' -sb '#ffffff' -sf '#000000'")

      -- Kill window
    , ((mod4Mask .|. shiftMask, xK_c), kill)

      -- Sink window (put it into the tile layout)
    , ((mod4Mask, xK_t), withFocused $ windows . W.sink)

      -- Audio controls (volume and mute)
    , ((0, xF86XK_AudioLowerVolume), spawn "pamixer --decrease 5 && notify-send 'Volume' 'Decreased by 5%'")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pamixer --increase 5 && notify-send 'Volume' 'Increased by 5%'")
    , ((0, xF86XK_AudioMute), spawn "pamixer --toggle-mute && notify-send 'Volume' 'Muted/Unmuted'")

      -- Brightness controls
    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +10% && notify-send 'Brightness' 'Increased by 10%'")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%- && notify-send 'Brightness' 'Decreased by 10%'")

      -- Resize window bindings for ResizableTile layout in all directions
    , ((mod4Mask, xK_h), sendMessage Shrink)       -- Shrink window horizontally
    , ((mod4Mask, xK_l), sendMessage Expand)       -- Expand window horizontally
    , ((mod4Mask, xK_j), sendMessage MirrorShrink) -- Shrink window vertically
    , ((mod4Mask, xK_k), sendMessage MirrorExpand) -- Expand window vertically
    ]
