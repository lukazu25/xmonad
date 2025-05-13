import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import System.IO
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86

-- Gruvbox colors
gruvboxBg0, gruvboxFg0, gruvboxYellow, gruvboxRed :: String
gruvboxBg0 = "#282828"
gruvboxFg0 = "#ebdbb2"
gruvboxYellow = "#d65d0e"
gruvboxRed = "#cc241d"

-- Custom Gruvbox tabbed theme
myTabTheme :: Theme
myTabTheme = def
  { activeColor         = gruvboxYellow
  , inactiveColor       = gruvboxBg0
  , urgentColor         = gruvboxRed
  , activeBorderColor   = gruvboxYellow
  , inactiveBorderColor = gruvboxBg0
  , urgentBorderColor   = gruvboxRed
  , activeTextColor     = gruvboxBg0
  , inactiveTextColor   = gruvboxFg0
  , urgentTextColor     = gruvboxFg0
  , decoHeight          = 24
  , fontName            = "xft:Fira Code-10"
  }

-- Layouts
myLayouts = avoidStruts $
  tiledLayouts ||| full ||| grid ||| tabbedLayout
  where
    -- Apply spacing only to these layouts
    tiledLayouts = spacingRaw False (Border 24 4 4 4) False (Border 4 4 4 4) True $
-- add smartBorders tiled ||| smartBorders mirrorTiled if you dont want border on single window
                     tiled ||| mirrorTiled

    tiled = Tall 1 (3/100) (1/2)
    mirrorTiled = Mirror tiled
    full = Full
    grid = Grid
    tabbedLayout = noBorders $ tabbed shrinkText myTabTheme

main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"
  xmonad $ docks . ewmhFullscreen . ewmh $ def
    { manageHook = manageDocks <+> manageHook def
    , layoutHook = myLayouts
    , handleEventHook = handleEventHook def
    , logHook = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor gruvboxFg0 "" . shorten 50
        , ppCurrent = xmobarColor gruvboxYellow "" . wrap "[" "]"
        , ppHidden = xmobarColor gruvboxFg0 ""
        , ppHiddenNoWindows = xmobarColor gruvboxBg0 ""
        , ppUrgent = xmobarColor gruvboxRed ""
        }
    , modMask = mod4Mask
    , terminal = "alacritty"
    , borderWidth = 2
    , normalBorderColor = gruvboxBg0
    , focusedBorderColor = gruvboxYellow
    , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    , startupHook = do
        spawn "xrandr --output Virtual-1 --mode 1920x1080"
        spawn "nitrogen --restore"
        spawn "pkill -u $USER dunst; dunst &"
        spawn "setxkbmap -layout us,ru,ge -option 'grp:alt_shift_toggle'"
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_Return), spawn "alacritty")
    , ((mod4Mask, xK_p), spawn "dmenu_run -fn 'Fira Code-12' -nb '#282828' -nf '#ebdbb2' -sb '#d79921' -sf '#282828'")
    , ((mod4Mask .|. shiftMask, xK_c), kill)
    , ((mod4Mask, xK_t), withFocused $ windows . W.sink)
    , ((mod4Mask, xK_comma), sendMessage (IncMasterN 1))
    , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1)))
    , ((0, xF86XK_AudioLowerVolume), spawn "pamixer --decrease 5 && notify-send 'Volume' 'Decreased by 5%'")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pamixer --increase 5 && notify-send 'Volume' 'Increased by 5%'")
    , ((0, xF86XK_AudioMute), spawn "pamixer --toggle-mute && notify-send 'Volume' 'Muted/Unmuted'")
    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +10% && notify-send 'Brightness' 'Increased by 10%'")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%- && notify-send 'Brightness' 'Decreased by 10%'")
    ]
