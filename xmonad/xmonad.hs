-- IMPORTS --
-- main
import XMonad
import System.IO
import Control.Monad
import Data.Monoid
import System.Exit
import XMonad.Operations
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Actions
import XMonad.Actions.MouseResize
import XMonad.Actions.CycleWS as C
import XMonad.Actions.Promote

-- Hooks
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

-- Layouts + mods
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.MultiToggle
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

------------------------------------------------------------------------
-- self explanatory
myTerminal      = "kitty"
myFileManager   = "dolphin"
myBrowser       = "firefox-developer-edition"
myTedit         = "neovide"
myLauncher      = "dmenu_run"
myModMask       = mod4Mask

-- does window focus follows mouse
myFocusFollowsMouse = False
myClickJustFocuses = False

-- b o r d e r s
myBorderWidth = 3
focusBordCol  = "#89dced"
normBordCol   = "#898b4fa"

------------------------------------------------------------------------
-- Key bindings if it wasnt clear by the myKeys
-- changing mod key to alt also breaks some configs below
myKeys =
  powerKeys
    ++ launchKeys
    ++ scratchPadKeys
    ++ miscKeys
    ++ layoutKeys
    ++ wmKeys
  where
    powerKeys =
      [  ("M-M1-q",     io (exitWith ExitSuccess))
        ,("M-M1-r",     spawn "xmonad --recompile")
        ,("M-S-r",      spawn "xmonad --restart")
        ,("C-M-M1-l",   spawn "~/.xmonad/lock.sh")
        ,("C-M-M1-q",   spawn "systemctl poweroff")
        ,("C-M-M1-r",   spawn "systemctl reboot")
      ]

    launchKeys =
      [  ("M-e",        spawn (myFileManager))
        ,("M-s",        spawn "dmenu_run -l 15")
        ,("M-c",        spawn (myTedit))
        ,("M-S-c",      spawn (myBrowser))
        ,("M-M1-c",     spawn "emacsclient -c -a 'emacs'")
      ]

    scratchPadKeys =
      [ ("M-<Return>", openScratchPad "kitty")
       ,("M-m",        openScratchPad "mixer")
       ,("M-b",        openScratchPad "btop")
       ,("M-x",        openScratchPad "spotifytui")
      ]

    miscKeys =
      [  ("<Print>",    spawn "maim -s | xclip -selection clipboard -t image/png")
        ,("M-n",        spawn "xcolor -f HEX! --scale 4 -s clipboard")
        ,("M1-<Space>", spawn "python ~/.xmonad/change_layout.py")
        ,("C-S-q",      kill)
      ]

    layoutKeys =
      [  ("M-<Tab>",   sendMessage NextLayout)
        ,("M-f",       sendMessage (MT.Toggle NBFULL))
        ,("M-C-f",     sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
        ,("M-<Backspace>", promote)
        ,("M-j",       windows W.focusDown)
        ,("M-h",       windows W.focusDown)
        ,("M-l",       windows W.focusUp)
        ,("M-k",       windows W.focusUp)
        ,("M-t",       withFocused $ toggleFloat $ rectCentered 0.7)
        ,("M-S-t",     withFocused $ toggleFloat $ vertRectCentered 0.9)
        ,("M-[",       sendMessage (IncMasterN 1)),
         ("M-]",       sendMessage (IncMasterN (-1)))
      ]

    wmKeys =
      [  ("M-S-j",     windows W.swapDown)
        ,("M-S-k",     windows W.swapUp)
        ,("M-,",       nextScreen)
        ,("M-M1-h",    sendMessage Shrink)
        ,("M-M1-l",    sendMessage Expand)
      ]

myRemovedKeys :: [String]
myRemovedKeys = ["M-.", "M-S-q"]

myKeysConfig :: XConfig a -> XConfig a
myKeysConfig config = config `additionalKeysP` myKeys `removeKeysP` myRemovedKeys
------------------------------------------------------------------------

-- startup hook (autostart type thing)
myStartupHook :: X ()
myStartupHook = do
        traverse spawnOnce
          [ "xrandr --output DisplayPort-0 --auto --output DisplayPort-1 --right-of DisplayPort-0"
          , "xsetroot -cursor_name left_ptr"
          , "/usr/bin/emacs --daemon"
          , "picom"
          , "lxsession"
          , "wal -R"
          ]
	setWMName "LG3D"

-- workspace names + window count

myWorkspaces = ["I","II","III","IV","V","VI","VII", "VIII", "IX"]

ignoredWorkspaces = ["NSP"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- ScratchPads - basically floating temporary instances
myScratchpads :: [NamedScratchpad]
myScratchpads = [kitty, mixer, btop, spotifytui]
  where
    kitty = NS "kitty" spawn find manage
      where
        spawn  = myTerminal ++ " -T term"
        find   = title =? "term"
        manage = customFloating $ rectCentered 0.45
    mixer = NS "mixer" spawn find manage
      where
        spawn  = myTerminal ++ " -T PulseMixer -e pulsemixer"
        find   = title =? "PulseMixer"
        manage = customFloating $ rectCentered 0.5
    btop = NS "btop" spawn find manage
      where
        spawn  = myTerminal ++ " -T btop -e btop"
        find   = title =? "btop"
        manage = customFloating $ rectCentered 0.55
    spotifytui = NS "spotifytui" spawn find manage
      where
        spawn  = myTerminal ++ " -T spotify-tui -e spt"
        find   = title =? "spotify-tui"
        manage = customFloating $ rectCentered 0.55

openScratchPad :: String -> X ()
openScratchPad = namedScratchpadAction myScratchpads

rectCentered :: Rational -> W.RationalRect
rectCentered percentage = W.RationalRect offset offset percentage percentage
  where
    offset = (1 - percentage) / 2


vertRectCentered :: Rational -> W.RationalRect
vertRectCentered height = W.RationalRect offsetX offsetY width height
  where
    width = height / 2
    offsetX = (1 - width) / 2
    offsetY = (1 - height) / 2

-- Layouts

defaultTall = Tall 1 0.05 0.55

tall = renamed [Replace "Default"] $ limitWindows 6 $ defaultSpacing defaultTall

myLayout = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) tall

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

defaultSpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
defaultSpacing = mySpacing 4

toggleFloat :: W.RationalRect -> Window -> X ()
toggleFloat r w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w r s
    )
------------------------------------------------------------------------
-- window rules:
-- find class name/other with xprop WM_CLASS
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "mpv"             --> doShift (myWorkspaces !! 0)
    , className =? "dotnet"          --> doShift (myWorkspaces !! 0)
    , className =? "Navigator"       --> doShift (myWorkspaces !! 1)
    , className =? "firefoxdeveloperedition" --> doShift (myWorkspaces !! 1)
    , className =? "discord"         --> doShift (myWorkspaces !! 2)
    , className =? "steam"           --> doShift (myWorkspaces !! 2)
    , className =? "Gimp"            --> doShift (myWorkspaces !! 3)
    , className =? "lutris"          --> doShift (myWorkspaces !! 3)
    , className =? "riotclientux.exe"--> doShift (myWorkspaces !! 3)
    , className =? "leagueclientux.exe" --> doShift (myWorkspaces !! 3)
    , className =? "deluge"          --> doCenterFloat
    , className =? "Lxappearance"    --> doCenterFloat
    , className =? "qt5ct"           --> doCenterFloat
    , className =? "Kvantum Manager" --> doCenterFloat
    , className =? "dolphin"         --> doCenterFloat
    , className =? "Font-manager"    --> doCenterFloat
    , className =? "Xmessage"        --> doCenterFloat
    , className =? "MPlayer"         --> doFloat
    , className =? "Lutris"          --> doFloat
    , resource  =? "desktop_window"  --> doIgnore
    , title     =? "Wine System Tray" --> doHide
    , insertPosition End Older
    , manageDocks
    , namedScratchpadManageHook myScratchpads
    ]
    where
      doHide = ask >>= doF . W.delete :: ManageHook

------------------------------------------------------------------------
-- event handling

myEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift (myWorkspaces !! 4))
------------------------------------------------------------------------
-- status bars and logging

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.9

-- main function
main :: IO ()
main = xmonad . withSB (statusBar <> statusBar2) . docks . myKeysConfig $ defaultSettings

statusBar :: StatusBarConfig
statusBar = statusBarProp "xmobar ~/.config/xmobar/xmobarrc" $ pure pp

statusBar2 :: StatusBarConfig
statusBar2 = statusBarProp "xmobar -x 1 ~/.config/xmobar/xmobarrc" $ pure pp

pp =
      filterOutWsPP
        ignoredWorkspaces
        xmobarPP
          { ppCurrent = xmobarColor "#f9e4a3" "" . wrap ("<box type=Bottom width=3 mb=2 color=#f9e4a3>") "</box>"
          , ppVisible = xmobarColor "#f9e4a3" ""
          , ppHidden = xmobarColor "#cdd6f4" "" . wrap ("<box type=Bottom width=3 mt=2 color=#cdd6f4>") "</box>"
          , ppHiddenNoWindows = xmobarColor "#cdd6f4" ""
          , ppTitle = xmobarColor "#cdd6f4" "" . shorten 40
          , ppSep = "<fc=#12162a> <fn=1></fn> </fc>"
          , ppUrgent = xmobarColor "#d20f39" "" .wrap "!" "!"
          , ppExtras = [windowCount]
          , ppOrder = \(ws:l:t:ex) -> [ws]++ex++[t]
          }

defaultSettings =
  def
    {
        terminal           = myTerminal,
        modMask            = myModMask,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        workspaces         = myWorkspaces,
        normalBorderColor  = normBordCol,
        focusedBorderColor = focusBordCol,
        layoutHook         = myLayout,
        manageHook         = myManageHook ,
        handleEventHook    = myEventHook ,
        startupHook        = myStartupHook,
        logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
    }
------------------------------------------------------------------------
