{- xmonad.hs
 - Author: Daniel
 - Version: 0.1.0
 -}
 
-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig(additionalKeys)
import IO
import Data.Ratio
-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Loggers 
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.Accordion
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.LayoutModifier
import XMonad.Util.EZConfig   
import XMonad.Layout.Gaps
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
------------------------------------------------------------------------------- 
-- Main --
main = do
              pipe1 <- spawnPipe "dzen2 -bg black -fg red -ta l -w 523 -h 20" 
              conkyBarPipe <- spawnPipe myConkyBar
	      conkyBarPipe2 <-spawnPipe myConkyBar2
              mpdpipe <- spawnPipe "~/dzen_mpd"
	      xmonad $ withUrgencyHook dzenUrgencyHook defaultConfig {
              workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , keys = keys' 
              , layoutHook = layoutHook'
              , manageHook = manageHook' 
              , logHook = dynamicLogWithPP $ defaultPP 
          {
          ppCurrent           = wrap (dzfg "dark orange" box) "" . dzenColor "#AA9DCF" "#333" . pad
          , ppOutput          = hPutStrLn pipe1 
          , ppVisible         = dzfg "#AA9DCF" 
          , ppHidden          = wrap (dzfg "white" emptybox) "" . dzfg "#AA9DCF"
          , ppHiddenNoWindows = const ""
          , ppLayout          = dzfg "#6B6382" 
          , ppSep             = " "  
          , ppWsSep           = dzfg "orange"  " " 
          , ppUrgent          = dzenColor "#212121"  "*"
          , ppTitle           = const ""
          }
}
          where  dzfg c      = dzenColor c ""
                 emptybox = "^p(;+7)^ro(5x5)^p(+2;-7)"
                 box = "^p(;+7)^r(5x5)^p(+2;-7)"
 
         

              

------------------------------------------------------------------------------
-- conky-
myConkyBar :: String
myConkyBar = "sleep 1 && conky -c ~/.conkyrc1 | dzen2 -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859' -bg black  -fg white -x 0 -y 880 -w 350 -h 20 -ta l -e '' "

myConkyBar2 :: String
myConkyBar2 = "sleep 1 && conky -c ~/.conkyrc2 | dzen2 -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859' -bg black -fg white -x 480 -y 880 -w 850 -h 20 -ta l -e ''"

myXPConfig = defaultXPConfig
    {
  font = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
  ,fgColor = "dark orange"
  , bgColor = "#000000"
  , bgHLight = "#000000"
  , fgHLight = "#FF0000"
  , position = Top
    }


-------------------------------------------------------------------------------
-- Hooks -- 
manageHook' :: ManageHook 
manageHook' = composeAll [ (doF W.swapDown), (isFullscreen --> doFullFloat), manageHook defaultConfig, manageDocks , className =? "OpenOffice.org 3.1" --> doShift "CODE II", className =? "Gimp"  --> doFloat, className =? "Gran Paradiso"  --> doShift "INTERWEBS", className =? "Shiretoko"  --> doShift "INTERWEBS", className =? "VLC (XVideo output)"    --> doCenterFloat           ]

layoutHook' = customLayout 
-------------------------------------------------------------------------------
------------- Looks --

-- bar 
 
-- borders
borderWidth' :: Dimension
borderWidth' = 2
 
normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#000000"
focusedBorderColor' = "#3579A8"
 
-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["IRC","INTERWEBS","Cal","CODE I","CODE II","Torrent","MUSIC"]
 
-- layouts
customLayout =  smartBorders . avoidStruts $ funkyhack  lays
  where
    tiled = ResizableTall 1 (2/100) (1/2) []
    funkyhack = onWorkspaces ["IRC","Cal"] (noBorders $ simplestFloat |||  Mirror tiled ||| Full)
    lays = spiral (1 % 1) ||| smartBorders Circle ||| tiled ||| Mirror tiled ||| Mag.magnifier Grid ||| smartBorders (Mirror tiled) ||| (reflectHoriz tiled)
  
  
 
------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvt"
 
-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
modMask' = mod4Mask
 
-- keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
-- launching and killing programs
        --terminal
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask .|. shiftMask, xK_p     ), shellPrompt myXPConfig)
    , ((modMask .|. shiftMask, xK_a	), spawn "urxvt -e alsamixer")
    , ((modMask .|. shiftMask, xK_m	), spawn "urxvt  -e ncmpcpp")
    , ((modMask .|. shiftMask, xK_f	), spawn "firefox")
  , ((modMask .|. shiftMask, xK_t	), spawn "thunar")
    , ((controlMask .|. mod1Mask, xK_l), spawn "gnome-screensaver-command --lock")
    , ((modMask .|. shiftMask, xK_g	), spawn "gedit")
    , ((modMask .|. shiftMask, xK_s	), spawn "urxvt -e /home/daniel/bin/ssinfo.pl")
    , ((0, 		       xK_Print	), spawn "scrot")
    , ((controlMask .|. modMask, xK_g	), spawn "/home/daniel/bin/goodsong")
    , ((modMask                , xK_p   ), spawn  "exec `dmenu_path | yeganesh -- -nb black -nf orange -b -fn terminus`")
	--Volume controls
     , ((modMask 		, xK_Prior), spawn "amixer sset Master 1+")
     , ((modMask		, xK_Next), spawn "amixer sset Master 1-")
     , ((modMask		, xK_End), spawn "amixer sset Master toggle")
	--kill focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,                xK_Right), nextWS) 
    , ((modMask,                xK_Left ), prevWS)
    , ((modMask,                xK_Down ), moveTo Next NonEmptyWS)
    , ((modMask,                xK_Up   ), moveTo Prev NonEmptyWS)
    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask .|. controlMask, xK_f   ), focusUrgent)
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- refresh
    , ((modMask,               xK_n     ), refresh)
 
    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)
 
    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)
 
    -- mpd controls
    , ((modMask              , xK_F9 ) , spawn "mpc toggle; /home/daniel/bin/mpcstat")
    , ((modMask              ,  xK_F12  ), spawn "mpc next; /home/daniel/bin/mpcstat")
    , ((modMask              ,  xK_F11 ), spawn "mpc prev; /home/daniel/bin/mpcstat")

    , ((modMask              , xK_F10), spawn "mpc stop; /home/daniel/bin/mpcstat")
    , ((controlMask .|. modMask , xK_space) , spawn "mpc status; /home/daniel/bin/mpcstat")
    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


