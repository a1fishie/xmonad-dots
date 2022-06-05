-- IMPORTS --

-- main
import XMonad
import System.IO (hPutStrLn)
import Data.Monoid
import System.Exit
import XMonad.Operations
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Actions
import XMonad.Actions.MouseResize
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote

-- Hooks
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.ManageHelpers

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad as NS

-- Layouts + mods
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Simplest
import XMonad.Layout.NoBorders
import XMonad.Layout.SubLayouts
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.MultiToggle
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- self explanatory
myTerminal      = "kitty"
myFileManager   = "dolphin"
myBrowser       = "firefox-developer-edition"
myTedit         = "neovide"
myLauncher      = "dmenu_run"

-- focus follows mouse
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2

myModMask       = mod4Mask -- windows key, change to mod1Mask for alt (i will hunt you)
-- changing to alt also breaks some configs below

-- b o r d e r s
focusBordCol  = "#89dced"
normBordCol = "#898b4fa"

-- startup hook (autostart type thing)
myStartupHook :: X ()
myStartupHook = do
          spawnOnce "xrandr --output DisplayPort-0 --auto --output DisplayPort-1 --right-of DisplayPort-0"
          spawnOnce "xsetroot -cursor_name left_ptr"
          spawnOnce "picom"
          spawnOnce "lxsession"
          spawnOnce "wal -R"
          spawnOnce "spotify"

-- workspace names + window count + clickyyy
xmobarEscape :: String -> String
xmobarEscape = concatMap doubltLts
 where
        doubleLts '<' = "<<"
        doubltLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ ["I","II","III","IV","V","VI","VII","VIII","IX"]

  where
        clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- ScratchPads - basically floating temporary instances
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                ]
  where
    spawnTerm  = myTerminal ++ " -T scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w

-- spacing between window
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True



myTabTheme = def { fontName            = "xft:Meslo LG S"
                 , activeColor         = "#89dceb"
                 , inactiveColor       = "#89b4fa"
                 , activeBorderColor   = "#89dceb"
                 , inactiveBorderColor = "#89b4fa"
                 , activeTextColor     = "#ffffff"
                 , inactiveTextColor   = "#000000"
                 }
-- layouts:

-- add more in format layout = renamed [Replace "layout"]
-- customize options (found in docs) and add to myLayoutHook

threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing 8
           $ ThreeCol 1 (3/100) (1/2)
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
spirals  = renamed [Replace "spirals"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing 8
           $ spiral (6/7)
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $  mkToggle (NBFULL ?? NOBORDERS ?? EOT) myLayout
             where myLayout = spirals
                         |||  threeCol
                         |||  tall
------------------------------------------------------------------------
-- window rules:
--
-- find class name/other with xprop WM_CLASS
--
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "mpv"            --> doShift (myWorkspaces !! 0)
    , className =? "Navigator"      --> doShift (myWorkspaces !! 1)
    , className =? "firefoxdeveloperedition" --> doShift (myWorkspaces !! 1)
    , className =? "discord"        --> doShift (myWorkspaces !! 2)
    , className =? "Gimp"           --> doShift (myWorkspaces !! 3)
    , className =? "spotify"        --> doShift (myWorkspaces !! 4)
    , className =? "Lxappearance"   --> doCenterFloat
    , className =? "qt5ct"          --> doCenterFloat
    , className =? "dolphin"        --> doCenterFloat
    , className =? "Font-manager"   --> doCenterFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Lutris"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- event handling

myEventHook = mempty

------------------------------------------------------------------------
-- status bars and logging

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount= 0.9

------------------------------------------------------------------------
-- Key bindings if it wasnt clear by the myKeys
myKeys :: [(String, X ())]
myKeys = [
  -- XMonad stuff exit, recomile, restart
  ("M-M1-q",     io (exitWith ExitSuccess))
 ,("M-M1-r",     spawn "xmonad --recompile")
 ,("M-S-r",      spawn "xmonad --restart")
 ,("C-M-M1-l",   spawn "~/.xmonad/lock.sh")
 ,("C-M-M1-q",   spawn "systemctl poweroff")

  -- applications - terminal in a scratchpad, dmenu, text editor, browser, fileman
 ,("M-<Return>", namedScratchpadAction myScratchPads "terminal")
 ,("M-e",        spawn (myFileManager))
 ,("M-s",        spawn "dmenu_run -l 15")
 ,("M-c",        spawn (myTedit))
 ,("M-S-c",      spawn (myBrowser))

  -- g a p s x o x o
 ,("C-M1-j",     decWindowSpacing 4)
 ,("C-M1-k",     incWindowSpacing 4)
 ,("C-M1-h",     decScreenSpacing 4)
 ,("C-M1-l",     incScreenSpacing 4)

  -- misc, namely screenshot + color picker + python script
 ,("<Print>",    spawn "maim -s | xclip -selection clipboard -t image/png")
 ,("M-n",        spawn "xcolor -f HEX! --scale 4 -s clipboard")
 ,("M1-<Space>", spawn "python ~/.xmonad/change_layout.py")

  -- kill windows
 ,("C-S-q",      kill)
 ,("M-b",        refresh)

  -- focus windows n stuff
 ,("M-<Tab>",   sendMessage NextLayout)
 ,("M-f",       sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
 ,("M-<Backspace>", promote)
 ,("M-j",       windows W.focusDown)
 ,("M-k",       windows W.focusUp)
 ,("M-m",       windows W.focusMaster)

 ,("M-S-j",     windows W.swapDown)
 ,("M-S-k",     windows W.swapUp)

 ,("M-M1-h",    sendMessage Shrink)
 ,("M-M1-l",    sendMessage Expand)

 ,("M-t",       withFocused $ windows . W.sink)

 ,("M-.",       nextScreen)
 ,("M-,",       prevScreen)

 ,("M-S-<Up>",   sendMessage (IncMasterN 1))
 ,("M-S-<Down>", sendMessage (IncMasterN (-1)))

 ]

------------------------------------------------------------------------
-- main function
main :: IO ()
main = do
    xmproc0 <- spawnPipe "xmobar -x 1 ~/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 2 ~/.config/xmobar/xmobarrc1"
    xmonad $ docks def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = normBordCol,
        focusedBorderColor = focusBordCol,
        layoutHook         = myLayoutHook,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
        , ppCurrent = xmobarColor "#89b4fa" "" . wrap ("<box type=Bottom width=2 mb=2 color=#89b4fa>") "</box>"
        , ppVisible = xmobarColor "#89b4fa" ""
        , ppHidden = xmobarColor "#cdd6f4" "" . wrap ("<box type=Bottom width=2 mt=2 color=#cdd6f4>") "</box>"
        , ppHiddenNoWindows = xmobarColor "#cdd6f4" ""
        , ppTitle = xmobarColor "#cdd6f4" "" . shorten 25
        , ppSep = "<fc=#12162a> <fn=1></fn> </fc>"
        , ppUrgent = xmobarColor "#d20f39" "" .wrap "!" "!"
        , ppExtras = [windowCount]
        , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }
           } `additionalKeysP` myKeys
