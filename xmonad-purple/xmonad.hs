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

-- Colors
colorFocused = "#71337a"
colorNormal  = "#333333"
pureWhite    = "#ffffff"
magenta      = "#8f3f71"
blue         = "#cc6493"
white        = "#f8f8f2"
yellow       = "#f1fa8c"
red          = "#ff5555"
lowWhite     = "#bbbbbb"
grey         = "#696969"

-- Custom tabbed theme
myTabTheme :: Theme
myTabTheme = def
  { activeColor         = colorFocused
  , inactiveColor       = colorNormal
  , urgentColor         = red
  , activeBorderColor   = colorFocused
  , inactiveBorderColor = colorNormal
  , urgentBorderColor   = red
  , activeTextColor     = pureWhite
  , inactiveTextColor   = lowWhite
  , urgentTextColor     = yellow
  , decoHeight          = 24
  , fontName            = "xft:JetBrainsMono Nerd Font-12"
  }

-- Layouts
myLayouts = avoidStruts $
  tiledLayouts ||| full ||| grid ||| tabbedLayout
  where
    tiledLayouts = spacingRaw False (Border 24 4 4 4) False (Border 4 4 4 4) True $
                     tiled ||| mirrorTiled
    tiled = ResizableTall 1 (3/100) (1/2) []
    mirrorTiled = Mirror tiled
    full = Full
    grid = spacingRaw False (Border 24 4 4 4) False (Border 4 4 4 4) True Grid
    tabbedLayout = noBorders $ tabbed shrinkText myTabTheme

-- Floating rules
myManageHook :: ManageHook
myManageHook = className =? "pavucontrol" --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)

-- Main
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ docks . ewmhFullscreen . ewmh $
    additionalKeys
      (def
        { manageHook = manageDocks <+> myManageHook <+> manageHook def
        , layoutHook = myLayouts
        , handleEventHook = handleEventHook def
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor white "" . shorten 50
            , ppCurrent = xmobarColor pureWhite "" . wrap " " "" . xmobarBorder "VBoth" colorFocused 4
            , ppHidden = xmobarColor lowWhite "" . wrap " " ""
            , ppHiddenNoWindows = xmobarColor grey "" . wrap " " ""
            , ppUrgent = xmobarColor yellow "" . wrap "! " " !"  
            , ppSep = xmobarColor grey "" "   •   "
            }
        , modMask = mod4Mask
        , terminal = "alacritty"
        , borderWidth = 4
        , normalBorderColor = colorNormal
        , focusedBorderColor = colorFocused
        , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
        , startupHook = do
            spawn "xrandr -s 1920x1080"
            spawn "nitrogen --restore"
            spawn "pkill -u $USER dunst; dunst &"
            spawn "setxkbmap -layout us,ru,ge -option 'grp:alt_shift_toggle'"
        })
      [ -- Launch terminal
        ((mod4Mask .|. shiftMask, xK_Return), spawn "alacritty")
        -- Launch dmenu
      , ((mod4Mask, xK_p),
         spawn "dmenu_run -fn 'JetBrainsMono Nerd Font-12' -nb '#333333' -nf '#f8f8f2' -sb '#71337a' -sf '#ffffff'")
        -- Kill focused window
      , ((mod4Mask .|. shiftMask, xK_c), kill)
        -- Sink window into tiling
      , ((mod4Mask, xK_t), withFocused $ windows . W.sink)
        -- Volume
      , ((0, xF86XK_AudioLowerVolume),
         spawn "pamixer --decrease 5 && notify-send 'Volume' 'Decreased by 5%'")
      , ((0, xF86XK_AudioRaiseVolume),
         spawn "pamixer --increase 5 && notify-send 'Volume' 'Increased by 5%'")
      , ((0, xF86XK_AudioMute),
         spawn "pamixer --toggle-mute && notify-send 'Volume' 'Muted/Unmuted'")
        -- Brightness
      , ((0, xF86XK_MonBrightnessUp),
         spawn "brightnessctl set +10% && notify-send 'Brightness' 'Increased by 10%'")
      , ((0, xF86XK_MonBrightnessDown),
         spawn "brightnessctl set 10%- && notify-send 'Brightness' 'Decreased by 10%'")
        -- Resizing with ResizableTall
      , ((mod4Mask, xK_h), sendMessage Shrink)
      , ((mod4Mask, xK_l), sendMessage Expand)
      , ((mod4Mask, xK_j), sendMessage MirrorShrink)
      , ((mod4Mask, xK_k), sendMessage MirrorExpand)
      ]
