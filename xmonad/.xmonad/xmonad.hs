import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import System.Exit

manageWorkspaces = composeAll . concat $
    [ [ className =? c --> doFloat       | c <- myFloats ]
    , [ className =? c --> doCenterFloat | c <- myCenterFloats ]
    , [ title     =? t --> doFloat       | t <- myTitleFloats ]
    ]
  where
    myFloats       = []
    myCenterFloats = [ "Arandr"
                     , "Xmessage"
                     ]
    myTitleFloats  = []

myKeys =
    [ ("M-p"                   , spawn "dmenu_run -fn 'M+ 1mn:pixelsize=14'")
    , ("M-S-b"                 , spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    , ("<XF86AudioMute>"       , spawn "amixer -q sset Master toggle")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3- unmute")
    , ("<XF86ScreenSaver>"     , spawn "i3lock")
    , ("<Print>"               , spawn "scrot ~/tmp/%Y%m%d%k%M%S-screenshot.png")
    , ("C-M-<Backspace>"       , io (exitWith ExitSuccess))
    ]

myLayout = laeiouts
  where
    nmaster      = 1
    ratio        = 1/2
    delta        = 3/100
    spaced l     = avoidStruts $ spacing 10 $ layoutHintsWithPlacement (0.5, 0.5) l
    laeiouts     = tiled ||| mirrored ||| fulled
    tiled        = named "Tall"     $ spaced (Tall nmaster delta ratio)
    mirrored     = named "Mirrored" $ spaced (Mirror (Tall nmaster delta ratio))
    fulled       = named "Full"     $ noBorders Full

myLogHook h = dynamicLogWithPP $ defaultPP
    { ppOutput = hPutStrLn h
    , ppSort   = getSortByXineramaRule
    }

myManageHook = composeAll
    [ manageHook defaultConfig
    , manageDocks
    , manageWorkspaces
    ]

main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmobarrc"
  xmonad $ defaultConfig
    { modMask            = mod4Mask
    , terminal           = "urxvt"
    , focusFollowsMouse  = False
    , layoutHook         = myLayout
    , logHook            = myLogHook xmproc
    , manageHook         = myManageHook
    , borderWidth        = 2
    , normalBorderColor  = darkGrey
    , focusedBorderColor = grey
    }
    `additionalKeysP` myKeys
  where
    grey      = "#222222"
    darkGrey  = "#111111"
