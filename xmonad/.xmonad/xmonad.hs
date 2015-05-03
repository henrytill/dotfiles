import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import System.Exit
import System.Posix.Unistd

myConfig logproc = defaultConfig
    { modMask            = mod4Mask
    , terminal           = "urxvt"
    , focusFollowsMouse  = False
    , layoutHook         = myLayout
    , logHook            = myLogHook logproc
    , manageHook         = myManageHook
    , borderWidth        = 2
    , normalBorderColor  = "grey90"
    , focusedBorderColor = "grey75"
    }
    `additionalKeysP` myKeys

myKeys :: [(String, X ())]
myKeys =
    [ ("M-S-b"                 , spawn myRestart)
    , ("M-S-e"                 , spawn "emacsclient -c")
    , ("M-b"                   , windowPromptBring myXPConfig)
    , ("M-g"                   , windowPromptGoto  myXPConfig)
    , ("M-p"                   , shellPrompt       myXPConfig { position = Top })
    , ("<XF86AudioMute>"       , spawn "amixer -q sset Master toggle")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3- unmute")
    , ("<XF86ScreenSaver>"     , spawn "i3lock")
    , ("<Print>"               , spawn "scrot ~/tmp/%Y%m%d%k%M%S-screenshot.png")
    , ("C-M-<Backspace>"       , io exitSuccess)
    ]
  where
    myRestart = unwords
        [ "if type xmonad;"
        , "then xmonad --recompile && xmonad --restart;"
        , "else xmessage xmonad not in \\$PATH: \"$PATH\";"
        , "fi"
        ]
    myXPConfig = defaultXPConfig
        { bgColor           = "#2a2a2a"
        , bgHLight          = "darkseagreen2"
        , fgColor           = "#dcdcdc"
        , fgHLight          = "#2a2a2a"
        , font              = "xft:M+ 1mn:pixelsize=14"
        , height            = 24
        , promptBorderWidth = 0
        , promptKeymap      = emacsLikeXPKeymap
        }

myLayout = tiled ||| mirrored ||| fulled
  where
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100
    tiled    = named "Tall"
             . avoidStruts
             . spacing 10
             . layoutHintsWithPlacement (0.5, 0.5)
             $ Tall nmaster delta ratio
    mirrored = named "Mirrored"
             . avoidStruts
             . spacing 10
             . layoutHintsWithPlacement (0.5, 0.5)
             . Mirror
             $ Tall nmaster delta ratio
    fulled   = named "Full"
             . noBorders
             $ Full

myLogHook logproc = dynamicLogWithPP defaultPP
    { ppOutput = hPutStrLn logproc
    , ppSort   = getSortByXineramaRule
    }

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageHook defaultConfig
    , manageDocks
    , className =? "Arandr"   --> doCenterFloat
    , className =? "Xmessage" --> doCenterFloat
    ]

main :: IO ()
main = do
    host   <- fmap nodeName getSystemID
    xmproc <- spawnPipe (myStatusBar host)
    xmonad (myConfig xmproc)
  where
    myStatusBar h = "xmobar $HOME/etc/xmobar/xmobarrc-" ++ h
