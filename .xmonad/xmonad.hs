{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import System.IO
import Control.Monad (liftM,liftM2)
import Data.List(delete)
import qualified XMonad.StackSet as W
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad hiding ( (|||) )

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.ManageHook
import qualified XMonad.Hooks.InsertPosition as IP

import XMonad.Actions.CycleWS
import XMonad.Actions.SinkAll
import XMonad.Actions.FocusNth
import qualified XMonad.Actions.RotSlaves as RotSlaves
import qualified XMonad.Actions.CopyWindow as CopyWindow
import qualified XMonad.Actions.SwapWorkspaces as SwapWorkspaces

import XMonad.Layout.GridVariants
import XMonad.Layout.OneBig
import XMonad.Layout.LayoutCombinators ((|||))
import qualified XMonad.Layout.ToggleLayouts as ToggleLayouts
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Layout.Spacing as Spacing
import qualified XMonad.Layout.Renamed as Renamed

import XMonad.Config.Xfce
import XMonad.Util.EZConfig

-- My own stuff.
import XMonad.Layout.Recursive
import XMonad.Operations.JPsExtras


myLayoutModifiers = avoidStruts .
                    maximize .
                    withGaps
    where withGaps = Spacing.spacing 3
          cleanDescription = Renamed.renamed [Renamed.CutWordsLeft 1]
          maximize = ToggleLayouts.toggleLayouts $ NoBorders.noBorders Full

myLayout = tall ||| wide ||| recursive ||| grid ||| big
    where
     named name = Renamed.renamed [Renamed.Replace name]
     delta      = 1/12
     wide       = named "Wide" $ Mirror $ Tall 2 delta (2/3)
     tall       = named "Tall" $ Tall 1 delta (1/2)
     recursive  = named "Recursive" $ Recursive False (1/2) (delta/2)
     grid       = named "Grid" $ Mirror $ Grid (1/1.6)
     big        = named "Big" $ OneBig (2/3) (2/3)

myWorkspaces    = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

type KeyFunction = (XConfig Layout -> Map.Map (ButtonMask, KeySym) (X ()))
allKeys::[KeyFunction]-> KeyFunction
allKeys fs = (foldl Map.union Map.empty) . (zipWith ($) fs) . repeat

toggleFocusedWindowStickiness:: X ()
toggleFocusedWindowStickiness = do
  workSpaces <- CopyWindow.wsContainingCopies
  case workSpaces of
    [] -> windows CopyWindow.copyToAll
    _  -> CopyWindow.killAllOtherCopies


myNormalKeys::KeyFunction
myNormalKeys conf = mkKeymap conf $ myNormalKeyMap conf
myNormalKeyMap conf =
    [
      ("M-y", kill),
      ("M-<Space>", sendMessage NextLayout),
      ("M-,", sendMessage (IncMasterN 1)),
      ("M-.", sendMessage (IncMasterN (-1))),
      ("M-a", sendMessage $ ToggleLayouts.ToggleLayout),
      ("M-w", toggleFocusedWindowStickiness),
      ("M-S-m", windows W.swapMaster),
      ("M-o", swapPrevScreen >> prevScreen),
      ("M-S-<Return>", windows moveToSecondTop),
      ("M-+", spawn "roxterm"),
      ("M-S-x", spawn "xrandr-config"),
      ("M-p", withFocused $ windows . W.sink),
      ("M-S-p", sinkAll),
      ("M-<Right>" , nextWS),
      ("M-<Left>"  , prevWS),
      ("M-i", nextScreen),
      ("M-u", prevScreen),
      ("M-n", focusNth 1),
      ("M-S-n", windows moveToSecondTop),
      ("M-m", windows W.focusMaster  ),
      ("M-S-<Right>", shiftToNext >> nextWS),
      ("M-S-<Left>",  shiftToPrev >> prevWS),
      ("M-S-i", shiftNextScreen >> nextScreen),
      ("M-S-u", shiftPrevScreen >> prevScreen),
      ("M-S-k", windows W.swapDown),
      ("M-S-j", windows W.swapUp),
      ("M-<Tab>", RotSlaves.rotAllDown >> windows W.focusMaster),
      ("M-S-<Tab>", RotSlaves.rotAllUp >> windows W.focusMaster),
      ("M-k", windows W.focusDown),
      ("M-j", windows W.focusUp),
      ("M-h", sendMessage Shrink),
      ("M-l", sendMessage Expand),
      ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
    ]

myWorkspaceKeys::KeyFunction
myWorkspaceKeys (XConfig {modMask = modm}) = Map.fromList $
        [((m .|. modm, k), windows $ f i)
            | (i, k) <- zip (myWorkspaces) ([xK_1 .. xK_9]++[xK_0])
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (SwapWorkspaces.swapWithCurrent, controlMask)]]

myMouseBindings:: XConfig Layout -> Map.Map (KeyMask, Button) (Window -> X ())
myMouseBindings conf =
  let myModMask = modMask conf
   in Map.fromList
      [
        ((myModMask, button1),                 const $ windows W.swapMaster),
        ((myModMask, button3),                 killWindow),
        ((myModMask, button4),                 const $ windows W.swapUp),
        ((myModMask, button5),                 const $ windows W.swapDown),
        ((myModMask .|. shiftMask, button4),   const $ sendMessage Expand),
        ((myModMask .|. shiftMask, button5),   const $ sendMessage Shrink)
      ]

-- myServerHook::(XConfig Layout) -> Event -> X All
myServerHook = serverModeEventHook' cmds
    where cmds = do conf <- asks config
                    return $ myNormalKeyMap conf


myManageHook :: ManageHook
myManageHook = baseHook <+>
               transience' <+>
               placement <+>
               (composeAll matchHooks)
   where baseHook = manageHook xfceConfig
         placement = IP.insertPosition IP.Above IP.Newer
         matchHooks = [
                        className  =? "Synapse"  --> doCenterFloat,
                        className  =? "Do"  --> doIgnore,
                        className  =? "Notification-daemon" --> doSideFloat SW,
                        className  =? "Xfce4-notifyd" --> doIgnore,
                        className  =? "Geary" --> doShift (last myWorkspaces),
			                  title =? "Whisker Menu" --> doFloat
                      ]

activeWindowBorderColor="#5D97D3"
inactiveWindowBorderColor="#000000"

myConfig =
        ewmh $
           xfceConfig {
                        normalBorderColor =  inactiveWindowBorderColor,
                        focusedBorderColor = activeWindowBorderColor,
                        terminal = "Terminal",
                        borderWidth = 1,
                        modMask = mod4Mask,
                        focusFollowsMouse = True,
                        handleEventHook = mconcat [myServerHook],
                        manageHook = myManageHook,
                        keys = allKeys [myNormalKeys,myWorkspaceKeys,(keys xfceConfig)],
                        layoutHook  = myLayoutModifiers (myLayout),
                        workspaces = myWorkspaces,
                        mouseBindings = myMouseBindings
                      }

withDBusLogging xmonadMain config = do
  dbus <- D.connectSession
  getWellKnownName dbus
  let modifiedConfig = config { logHook = extendedLogHook }
      extendedLogHook = (logHook config) <+> dynamicLogWithPP (prettyPrinter dbus)
  xmonadMain $ modifiedConfig

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = escaped
    , ppCurrent  = omit
    , ppVisible  = omit
    , ppHidden   = omit
    , ppUrgent   = omit
    , ppLayout   = tagged "u"
    , ppSep      = " ‣ "
    }

omit = const ""

tagged:: String -> String -> String
tagged tag text = "<" ++ tag ++ ">" ++ text ++ "</" ++ tag ++ ">"

escaped :: String -> String
escaped = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

main::IO()
main = do
    xmonad `withDBusLogging` myConfig
