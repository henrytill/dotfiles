import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

manageWorkspaces = composeAll . concat $
    [ [ className =? c --> doFloat       | c <- myFloats ]
    , [ className =? c --> doCenterFloat | c <- myCenterFloats ]
    , [ title     =? t --> doFloat       | t <- myTitleFloats ]
    , [ className =? c --> doShift "9"   | c <- browsers ]
    ]
  where
    myFloats       = []
    myCenterFloats = [ "Arandr"
                     , "Xmessage"
                     ]
    myTitleFloats  = []
    browsers       = [ "Firefox" ]

myKeys =
    [ ("<XF86AudioMute>"       , spawn "amixer -q sset Master toggle")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3- unmute")
    , ("<XF86ScreenSaver>"     , spawn "i3lock")
    , ("<Print>"               , spawn "scrot ~/tmp/%Y%m%d%k%M%S-screenshot.png")
    ]

myLayout = laeiouts
  where
    withTitles l = avoidStruts $ noFrillsDeco shrinkText myTheme $ spacing 1 l
    laeiouts     = tiled ||| mirrored ||| fulled
    tiled        = named "Tall"
                   (withTitles (Tall nmaster delta ratio))
    mirrored     = named "Mirrored"
                   (withTitles (Mirror (Tall nmaster delta ratio)))
    fulled       = named "Full" $ noBorders Full
    nmaster      = 1
    ratio        = 1/2
    delta        = 3/100

myLogHook h = dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn h
    , ppSort   = getSortByXineramaRule
    }

myManageHook = composeAll
    [ manageHook defaultConfig
    , manageDocks
    , manageWorkspaces
    ]

myTheme = defaultTheme
    { activeColor         = blue
    , inactiveColor       = grey
    , activeBorderColor   = blue
    , inactiveBorderColor = grey
    , activeTextColor     = "white"
    , inactiveTextColor   = lightGrey
    , decoHeight          = 14
    }
  where
    -- i3 colors
    blue      = "#285577"
    grey      = "#222222"
    -- medGrey   = "#5f676a"
    lightGrey = "#888888"

main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmobarrc"
  xmonad $ defaultConfig
    { modMask            = mod4Mask
    , focusFollowsMouse  = False
    , layoutHook         = myLayout
    , logHook            = myLogHook xmproc
    , manageHook         = myManageHook
    , borderWidth        = 1
    , normalBorderColor  = inactiveBorderColor myTheme
    , focusedBorderColor = activeBorderColor myTheme
    }
    `additionalKeysP` myKeys
