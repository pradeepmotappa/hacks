
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Monoid

import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Actions.NoBorders
import XMonad.Actions.WithAll           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-WithAll.html
import XMonad.Actions.SinkAll
import qualified XMonad.Actions.Submap as SM


import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ScreenCorners


import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.LayoutHints
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.Mosaic
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.BorderResize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.MultiToggle        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-MultiToggle.html
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Minimize           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Minimize.html
import XMonad.Layout.Maximize
import XMonad.Actions.WindowMenu
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-GridSelect.html
import XMonad.Actions.WithAll           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-WithAll.html
import XMonad.Actions.SinkAll

import XMonad.Layout.Renamed              -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Renamed.html

-- Window Layout Mode
import XMonad.Layout
import XMonad.Layout.ResizableTile      -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-ResizableTile.html
import XMonad.Layout.Circle             -- https://hackage.haskell.org/package/xmonad-contrib-0.12/docs/XMonad-Layout-Circle.html
import XMonad.Layout.Grid               -- https://hackage.haskell.org/package/xmonad-contrib-0.12/docs/XMonad-Layout-Circle.html
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-SimpleFloat.html#t:SimpleDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps               -- https://hackage.haskell.org/package/xmonad-contrib-0.12/docs/XMonad-Layout-Gaps.html
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts      -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-ToggleLayouts.html
import XMonad.Layout.MultiToggle        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-MultiToggle.html
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps               -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Gaps.html
import XMonad.Layout.Minimize           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Minimize.html
import XMonad.Layout.Maximize           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Maximize.html
import XMonad.Layout.Monitor            -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Monitor.html
import XMonad.Layout.PositionStoreFloat -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-PositionStoreFloat.html
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.BorderResize



import XMonad.Prompt
import XMonad.Prompt.Layout
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Util.Run
import XMonad.Util.Font
import XMonad.Util.Themes
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Image

import Graphics.X11.ExtraTypes.XF86

import System.Environment
import System.Cmd
import System.IO
import System.Exit
import Control.Concurrent

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- terminals
    [ ((modMask,                 xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask,   xK_Return), spawn "urxvt")

    -- launcher
    , ((modMask .|. shiftMask,   xK_p), spawn "gmrun")

    -- file manager
    --, ((modMask,                 xK_Up    ), runOrRaise "nautilus ~" (className =? "Nautilus"))
    --, ((modMask .|. shiftMask,   xK_Up    ), spawn "nautilus ~")
    , ((modMask,                 xK_Up    ), spawn "nautilus ~")


    -- shell/window prompts
    , ((modMask,                 xK_F2 ), runOrRaisePrompt mySP)

    -- Volume keys
    , ((0, 0x1008ff11), spawn "amixer -q set Master 5%-")
    , ((0, 0x1008ff13), spawn "amixer -q set Master 5%+")
    , ((0, 0x1008ff12), spawn "amixer -q set Master toggle")

    -- browser
    , ((modMask,               xK_f     ), runOrRaise
        "firefox" (className =? "Firefox"))
     -- browser
    , ((modMask,               xK_c     ), runOrRaise
        "google-chrome-stable"(className =? "Chrome"))

    -- print screen
    , ((0,                     xK_Print ), unsafeSpawn "scrot -e 'mv $f ~/Pictures'")


    -- cycle through workspaces
    , ((modMask,               xK_Right ), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask,               xK_Left  ), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))

    -- move windows through workspaces
    , ((modMask .|. shiftMask, xK_Right ), shiftTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. shiftMask, xK_Left  ), shiftTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. controlMask, xK_Right), shiftTo Next EmptyWS)
    , ((modMask .|. controlMask, xK_Left), shiftTo Prev EmptyWS)

    -- Rotate through layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)


    -- Move focus to the next/previous window
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((mod1Mask,              xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((mod1Mask .|. shiftMask, xK_Tab  ), windows W.focusUp)

    -- Swap the focused window with next/prev window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Shrink/Expand the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- Swap the focused window and the master window
    , ((modMask,            xK_semicolon), windows W.swapMaster)

    -- Increment/Deincrement the number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
    -- Reset layout of current workspace
    , ((modMask .|. shiftMask, xK_n     ), setLayout $ XMonad.layoutHook conf)


    -- Mosaic
    , ((modMask , xK_a                  ), sendMessage Taller)
    , ((modMask , xK_z                  ), sendMessage Wider)
    , ((modMask .|. controlMask, xK_n   ), sendMessage Reset)

    --Grid Select
    , ((modMask , xK_g			), goToSelected  $ gsconfig1 )
    , ((modMask .|. shiftMask,  xK_a ), gridselectWorkspace wsconfig  (\ws -> W.view ws))                 -- select workspace
    , ((modMask .|. shiftMask, xK_s), goToSelected  $ wiconfig  myGoToSelectedColorizer)                -- above same, for custom colorizer
    --, ((modMask x .|. shiftMask, xK_b), bringSelected wiconfig2)                                        -- bring window list and summon a window you select
    , ((modMask .|. shiftMask, xK_b), bringSelected $ wiconfig2 myBringSelectedColorizer)               -- bring window list and summon a window you select
    , ((modMask .|. shiftMask, xK_g), gotoMenu)                                                         -- avobe goToSelected same, like dmenu appearence
    , ((modMask .|. shiftMask, xK_i), bringMenu)                                                        -- bring window, like dmenu appaerance


      -- Jump to layout
    , (((modMask .|. controlMask .|. shiftMask), xK_j), submap . M.fromList $
      [
          -- ((0, xK_t), sendMessage $ JumpToLayout "tiled")
          --,((0, xK_u), sendMessage $ JumpToLayout "full")
          --,((0, xK_f), sendMessage $ JumpToLayout "float")
      ])
        -- Resize viewed windows to the correct size
      , ((modMask,               xK_n     ), refresh)

      -- Makes it possible to minimize windows, temporarily removing them from the
      -- layout until they are restored.
      -- minimize the focused window then focus down

      ,((modMask, xK_minus        ), withFocused minimizeWindow <+> windows W.focusDown)

    -- restore the next minimized window. then focus up.
    , ((modMask, xK_equal        ), sendMessage RestoreNextMinimizedWin <+> windows W.focusUp)

    -- Temporarily yanks the focused window out of the layout to mostly fill
    -- the screen : maxmize toggle key.
    , ((modMask, xK_backslash    ), withFocused (sendMessage . maximizeRestore))

    -- a simple binding that pushes all floating windows on the current workspace back into tiling
    , ((modMask .|. controlMask .|. shiftMask, xK_t), sinkAll)



    -- toggle focused window fullscreen
    , ((modMask,               xK_m     ), sendMessage (XMonad.Layout.ToggleLayouts.Toggle "Full"))

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    --, ((modMask .|. shiftMask, xK_s     ), sendMessage Arrange)

    -- toggle the status bar gap
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- close focused window
    , ((modMask .|. shiftMask,  xK_c     ), kill)

    -- Restart xmonad
    , ((modMask              , xK_q     ),
        broadcastMessage ReleaseResources >> restart "xmonad" True)

 -- Logout of  xmonad
    , ((modMask .|. shiftMask , xK_q     ),io (exitWith ExitSuccess))
    ]

    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]

    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]



--Custom Taffy Bar colors

--myTaffyBarPP = taffybarDefaultPP {
--    ppCurrent = taffybarColor "#f8f8f8" "DodgerBlue4"   . wrap " " " "
--  , ppVisible = taffybarColor "#f8f8f8" "LightSkyBlue4" . wrap " " " "
--  , ppUrgent  = taffybarColor "#f8f8f8" "red4"          . wrap " " " "
-- , ppLayout  = taffybarColor "DarkOrange" "" . wrap " [" "] "
--, ppTitle   = taffybarColor "#61ce3c" "" . shorten 50
--}




-- | A synthwave monochrome colorizer based on window class
myGoToSelectedColorizer  :: Window -> Bool -> X (String, String)
myGoToSelectedColorizer  = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x78,0x3e,0x57) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

myBringSelectedColorizer :: Window -> Bool -> X (String, String)
myBringSelectedColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg



-- gridSelect select Workspace layout
wsconfig            = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 300
    , gs_cellpadding  = 16
    , gs_originFractX = 0.5
    , gs_originFractY = 0.0
    , gs_font         = myGSFont
    }

-- gridSelect move Workspace layout

-- gridSelect select window layout
wiconfig colorizer  = (buildDefaultGSConfig myGoToSelectedColorizer )
--wiconfig = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 16
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myGSFont
  }

-- gridSelect bring window layout
wiconfig2 colorizer = (buildDefaultGSConfig myBringSelectedColorizer)
--wiconfig2 = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 16
    , gs_originFractX = 0.5     --1.0
    , gs_originFractY = 0.3     --0.5
    , gs_font         = myGSFont
    }

-- gridSelect popup menu layout




-- shell prompt theme
mySP = defaultXPConfig
    { bgColor           = "black"
    , fgColor           = "white"
    , bgHLight          = "gray"
    , fgHLight          = "black"
    , borderColor       = "orange"
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 20
    --, autoComplete      = Just 1000
    , historySize       = 1000 }




    -------------------------------------------------------------------------------
    -- Variables
    -------------------------------------------------------------------------------
    -- definee base16 color: systhwave theme
    --
color0   = "#33303b"
color1   = "#87404f"
color2   = "#4c9982"
color3   = "#71949a"
color4   = "#615772"
color5   = "#783e57"
color6   = "#554757"
color7   = "#c0a79a"
color8   = "#2f2c37"
color9   = "#87404f"
color10  = "#4c9982"
color11  = "#71949a"
color12  = "#615772"
color13  = "#783e57"
color14  = "#554757"
color15  = "#c0a79a"
colorbg0 = "#312e39"
colorbg1 = "#c0a79a"
colorfg0 = "#ffffff"
colorfg1 = "#ce260b"

type Hex = String
type ColorCode = (Hex,Hex)
type ColorMap = M.Map Colors ColorCode

data Colors = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | BG |FG
    deriving (Ord,Show,Eq)

colors :: ColorMap
colors = M.fromList
    [ (Black   , (color0,   color8  ))
    , (Red     , (color1,   color9  ))
    , (Green   , (color2,   color10 ))
    , (Yellow  , (color3,   color11 ))
    , (Blue    , (color4,   color12 ))
    , (Magenta , (color5,   color13 ))
    , (Cyan    , (color6,   color14 ))
    , (White   , (color7,   color15 ))
    , (BG      , (colorbg0, colorbg1))
    , (FG      , (colorfg0, colorfg1))
    ]

myColor :: Colors -> Int -> Hex
myColor color n =
    case M.lookup color colors of
        Nothing -> "#000000"
        Just (c1,c2) -> if n == 0
                        then c1
                        else c2
---
myButtonThemeFont::String
myButtonThemeFont   = "xft:Noto Sans:bold:pixelsize=10,xft:Hack:bold:pixelsize=10"
myGSFont::String
myGSFont = "xft:Noto Sans:bold:pixelsize=12"

myButtonTheme :: Theme
myButtonTheme = defaultThemeWithImageButtons
    { activeColor         = myColor Magenta 0
    , inactiveColor       = myColor Black   1
    , urgentColor         = myColor BG      1
    , activeBorderColor   = myColor Magenta 0
    , inactiveBorderColor = myColor Black   1
    , activeTextColor     = myColor White   1
    , inactiveTextColor   = myColor Blue    0
    , urgentTextColor     = myColor Red     0
    , fontName            = myButtonThemeFont
    , decoHeight          = 18
    , windowTitleIcons    = [ (menuButton , CenterLeft   3)
                            , (closeButton, CenterRight  3)
                            , (maxiButton , CenterRight 18)
                            , (miniButton , CenterRight 33)
                            ]
    }
    where
        convertToBool' :: [Int] -> [Bool]
        convertToBool' = map (\x -> x == 1)
        convertToBool :: [[Int]] -> [[Bool]]
        convertToBool  = map convertToBool'
        menuButton' :: [[Int]]
        menuButton' =  [[0,0,0,0,0,0,0,0,0,0],
                        [0,1,0,0,0,0,0,0,1,0],
                        [0,0,1,1,0,0,1,1,0,0],
                        [0,0,1,0,0,0,0,1,0,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,0,1,0,0,0,0,1,0,0],
                        [0,0,1,1,0,0,1,1,0,0],
                        [0,1,0,0,0,0,0,0,1,0],
                        [0,0,0,0,0,0,0,0,0,0]]
        menuButton :: [[Bool]]
        menuButton  = convertToBool menuButton'
        miniButton' :: [[Int]]
        miniButton' =  [[0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,1,1,1,1,1,1,1,1,0],
                        [0,1,1,1,1,1,1,1,1,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0]]
        miniButton :: [[Bool]]
        miniButton  = convertToBool miniButton'
        maxiButton' :: [[Int]]
        maxiButton' =  [[0,0,0,0,0,0,0,0,0,0],
                        [0,0,0,0,1,1,0,0,0,0],
                        [0,0,0,0,1,1,0,0,0,0],
                        [0,0,0,0,1,1,0,0,0,0],
                        [0,1,1,1,1,1,1,1,1,0],
                        [0,1,1,1,1,1,1,1,1,0],
                        [0,0,0,0,1,1,0,0,0,0],
                        [0,0,0,0,1,1,0,0,0,0],
                        [0,0,0,0,1,1,0,0,0,0],
                        [0,0,0,0,0,0,0,0,0,0]]
        maxiButton :: [[Bool]]
        maxiButton  = convertToBool maxiButton'
        closeButton' :: [[Int]]
        closeButton' = [[0,0,0,0,0,0,0,0,0,0],
                        [0,1,1,0,0,0,0,1,1,0],
                        [0,1,1,1,0,0,1,1,1,0],
                        [0,0,1,1,0,0,1,1,0,0],
                        [0,0,0,1,1,1,1,0,0,0],
                        [0,0,0,1,1,1,1,0,0,0],
                        [0,0,1,1,0,0,1,1,0,0],
                        [0,1,1,1,0,0,1,1,1,0],
                        [0,1,1,0,0,0,0,1,1,0],
                        [0,0,0,0,0,0,0,0,0,0]]
        closeButton :: [[Bool]]
        closeButton = convertToBool closeButton'



floatLayout = noBorders ( imageButtonDeco shrinkText myButtonTheme simplestFloat )
  --avoidStruts $ gaps [(U,8), (D,8), (L,8), (R,8)] $ spacing 16 emptyBSP |||
                --noBorders ( imageButtonDeco shrinkText myButtonTheme $ simpleFloat' shrinkText myTheme { decoHeight =0 } ) |||
                --noBorders ( imageButtonDeco shrinkText myButtonTheme simplestFloat )  -- |||
                --noBorders ( imageButtonDeco shrinkText myButtonTheme positionStoreFloat )



someLayout = avoidStruts
  (noBorders (tiled) ||| noBorders (Grid) ||| Circle   ||| layoutHints (tabbed shrinkText myTab) ||| mosaic  2[2,3]) ||| noBorders(imageButtonDeco shrinkText myButtonTheme simplestFloat)
  where
        tiled   = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta   = 3/100
        ratio   = 1/2


myLayout  = lessBorders OnlyFloat                       $   -- OnlyFloat, Never, EmptyScreen, OtherIndicated, Screen
      smartBorders                                $
      borderResize                                $
      minimize $ maximize                         $
      mkToggle (NOBORDERS ?? FULL ?? EOT)         .
      mkToggle (single NBFULL)                    .
      mkToggle (single MIRROR)                   $
      toggleLayouts (avoidStruts $ Full)          $
      someLayout           -- $
--      floatLayout


-------------------------------------------------------------------------------
-- Fade Windows
-------------------------------------------------------------------------------
-- FadeHook
--
-- need compositer: xcompmgr or compton
-- xcompmgr -cCfF
-- xcompmgr -c -t-5 -l-5 -r4Z.2 -o.55"
-- myFadeHook = composeAll [isUnfocused --> transparency 0.2
--                         ,                opaque
--                         ]


-- Tags/Workspaces
-- clickable workspaces via dzen/xdotool
myWorkspaces            :: [String]
myWorkspaces            = clickable . (map dzenEscape) $ ["1:Terms","2:Web","3:IDE","4:DB","5:xls","6","7:Chat",  "8:β","9:Notes"]

  where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]



-- special windows
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Pidgin"         --> doFloat
    , title     =? "glxgears"       --> doFloat
    , title 	=? "inferno"	    --> doFloat
    , title     =? "Contact List"   --> doFloat
    , className =? "Empathy"        --> doFloat
    , className =? "Gnome-panel"    --> doIgnore
    , className =? "XVkbd"          --> doIgnore
    , className =? "Cellwriter"     --> doIgnore
    , className =? "Gtkdialog"      --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Gnome-terminal" --> doF (W.shift (myWorkspaces !! 0))
    , className =? "UXTerm" --> doF (W.shift (myWorkspaces !! 0))
    , className =? "Firefox" --> doF (W.shift (myWorkspaces !! 1))
    , className =? "Google-chrome" --> doF (W.shift (myWorkspaces !! 1))
    , className =? "Zim" --> doF (W.shift (myWorkspaces !! 8))
    , className =? "jetbrains-pycharm-ce" --> doF (W.shift (myWorkspaces !! 2))
    , className =? "jetbrains-idea-ce" --> doF (W.shift (myWorkspaces !! 2))
    , className =? "Atom" --> doF (W.shift (myWorkspaces !! 2))
    , className =? "Atom" --> doF (W.shift (myWorkspaces !! 2))
    , className =? "jetbrains-datagrip" --> doF (W.shift (myWorkspaces !! 3))
    , className =? "MongoDB Compass" --> doF (W.shift (myWorkspaces !! 3))
    , className =? "robo3t" --> doF (W.shift (myWorkspaces !! 3))
    , className =? "Rhythmbox" --> doF (W.shift (myWorkspaces !! 7))
    , className =? "Banshee" --> doF (W.shift (myWorkspaces !! 7))
    , className =? "Quodlibet" --> doF (W.shift (myWorkspaces !! 7))
    , className =? "Transmission-gtk" --> doF (W.shift (myWorkspaces !! 7))
    , className =? "Vlc" --> doF (W.shift (myWorkspaces !! 7))
    , className =? "Smplayer" --> doF (W.shift (myWorkspaces !! 7))
    , className =? "Slack" --> doF (W.shift (myWorkspaces !! 6))
    , className =? "Skype" --> doF (W.shift (myWorkspaces !! 6))
    , className =? "libreoffice" --> doF (W.shift (myWorkspaces !! 5))
    , isFullscreen 		    --> doFullFloat
    --                                      x y w h
    , scratchpadManageHook $ W.RationalRect 0 0 1 0.42
    , manageDocks ] <+> manageHook defaultConfig

    where
      myClassTerminalShifts = [ "Xterm","UXTerm","Gnome-terminal" ]
-- Grid Select Section
--gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 60 ,gs_cellwidth = 250, gs_font = "xft:Droid Sans:pixelsize=20",gs_cellpadding = 5 }
gsconfig1  = defaultGSConfig  { gs_cellheight = 60 ,gs_cellwidth = 250, gs_font = "xft:Cantarell Bold:pixelsize=18",gs_cellpadding = 5 }


-- | A green monochrome colorizer based on window classimport XMonad.Layout.IM
myColorizer = colorRangeFromClassName
                      (0x57,0xFF,0x1F) -- lowest inactive bg
                      (0x24,0x57,0xFF) -- highest inactive bg
                      (0x52,0x37,0xA4) -- active bg
                      black            -- inactive fg
                      white            -- active fg
   where black = minBound
         white = maxBound




appFontXft :: String
appFontXft = "xft:Cantarell Bold:pixelsize=9"
--appFontXft = concat [ "xft:"
                     --,"Sans:"
					 --,"pixelsize=11:"
					 --,"weight=regular:"
					 --,"width=semicondensed:"
					 --,"dpi=96:hinting=true:"
					 --,"hintstyle=hintslight:"
					 --,"antialias=true:"
					 --,"rgba=rgb:"
					 --,"lcdfilter=lcdlight"]
-- Color of current window title in xmobar.--#FFB6B0
xmobarTitleColor = "red"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "red"



-- decoration theme
myDeco = defaultTheme
    { activeColor         = "orange"
    , inactiveColor       = "#222222"
    , urgentColor         = "yellow"
--    , activeBorderColor   = "orange"
    , activeBorderColor   = "blue"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "yellow"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#222222"
    , urgentTextColor     = "yellow"
    , decoHeight          = 10 }


myEventHook e = do
		screenCornerEventHook e


myStartupHook = do
		spawn "/usr/lib/unity-settings-daemon/unity-settings-daemon"
--		spawn "/usr/libexec/notification-daemon"
--		spawn "thermald --no-daemon --dbus-enable"
		spawn "/usr/libexec/gnome-fallback-mount-helper"
--		spawn "/usr/local/bin/tomate"
		spawn "/usr/bin/gnome-sound-applet"
--		spawn "whenjobs --daemon-start"
		spawn "/usr/bin/nm-applet"
		spawn "/usr/bin/synapse"
		spawn "taffybar"
		spawn "/usr/bin/start-pulseaudio-x11"
		spawn "/usr/bin/gsettings-data-convert"
		spawn "/usr/bin/xdg-user-dirs-gtk-update"
		spawn "/usr/bin/trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 200 --widthtype pixel  --transparent true --height 22"
		spawn "/usr/bin/compton"
		spawn "/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh"
--		spawn "/usr/bin/tasque"
--		spawn "/bin/bash /root/scriptz/synclient.sh"
		spawn "/usr/bin/zim"
--		spawn "/usr/local/bin/artha"


-- tab theme
myTab = defaultTheme
    { activeColor         = "black"
    , inactiveColor       = "black"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "black"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#222222"
    , urgentTextColor     = "yellow" }

--myLogHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace >> updatePointer Nearest
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -w '600' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '600' -w '650' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '22' -fg '#777777' -bg '#222222' -fn 'Cantarell Bold:size=9'"
--myStartMenu = "/home/roh/.xmonad/start /home/roh/.xmonad/start_apps"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = "     "
    , ppLayout  = dzenColor "#afd700" "#303030" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#ffffff" ""
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . dzenEscape
    }


main = do
	status <- spawnPipe myDzenStatus    -- xmonad status on the left
        conky  <- spawnPipe myDzenConky     -- conky stats on the right
--	dzenStartMenu	<- spawnPipe myStartMenu
	env <- getEnvironment
	case lookup "DESKTOP_AUTOSTART_ID" env of
        	Just id -> do
        	    forkIO $ (>> return ()) $ rawSystem "dbus-send" ["--session","--print-reply=string","--dest=org.gnome.SessionManager","/org/gnome/SessionManager","org.gnome.SessionManager.RegisterClient","string:xmonad","string:"++id]
        	    return ()
        	Nothing -> return ()
	xmonad $ withUrgencyHook NoUrgencyHook $ desktopConfig {
			     terminal           = "gnome-terminal"
                           , borderWidth        = 1
                           , normalBorderColor  = "black"
                           , focusedBorderColor = "orange"
                           , focusFollowsMouse  = True
                           , modMask            = mod4Mask
                           , keys               = myKeys
			   , mouseBindings      = myMouseBindings
			   , workspaces = myWorkspaces
			   , startupHook	= myStartupHook <+> ewmhDesktopsStartup >> setWMName "LG3D"
                           , layoutHook         =  smartBorders $avoidStruts $ myLayout
                           , manageHook         =  manageDocks <+> myManageHook
							 <+> manageHook defaultConfig
                           , handleEventHook    = myEventHook <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> handleEventHook desktopConfig
			   , logHook 		= myLogHook status
			   }
