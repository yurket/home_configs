import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks as XHD

import qualified XMonad.Hooks.EwmhDesktops as XHE
import qualified XMonad.Hooks.SetWMName as XHS

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Actions.FlexibleResize as Flex

myTerminal      = "xterm"

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["[1]","[2]emacs","[3]","[4]web","[5]im","[6]","[7]","[8]mail","[9]emerge"]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#4fdc09"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((controlMask ,          xK_grave ), spawn $ XMonad.terminal conf)
    , ((mod4Mask    ,          xK_grave ), spawn "xterm -e alsamixer" )

    -- my proggies
    , ((modMask .|. controlMask, xK_b     ), spawn "firefox-bin")
    , ((modMask .|. controlMask, xK_e     ), spawn "emacs")
    , ((modMask .|. controlMask, xK_p     ), spawn "pidgin")
    , ((modMask .|. controlMask, xK_s     ), spawn "skypeforlinux")
    , ((modMask .|. controlMask, xK_m     ), spawn "xterm -e mc")

    -- xmms2d
    , ((mod4Mask               , xK_Home     ), spawn "xmms2 prev")
    , ((mod4Mask               , xK_End      ), spawn "xmms2 next")
    , ((mod4Mask               , xK_Insert   ), spawn "xmms2 play")
    , ((mod4Mask               , xK_Delete   ), spawn "xmms2 pause")
    , ((mod4Mask               , xK_Prior    ), spawn "xmms2 seek +30")
    , ((mod4Mask               , xK_Next     ), spawn "xmms2 seek -30")
    , ((mod4Mask               , xK_Up       ), spawn "xmms2 seek +20")
    , ((mod4Mask               , xK_Down     ), spawn "xmms2 seek -30")
    , ((mod4Mask               , xK_Right    ), spawn "xmms2 seek +10")
    , ((mod4Mask               , xK_Left     ), spawn "xmms2 seek -10")
    -- xmms2d fun stuff
    , ((mod4Mask               , xK_equal       ), spawn "xmms2 server config vocoder.speed 100")
    , ((mod4Mask               , xK_KP_Add      ), spawn "xmms2 server config vocoder.speed $((`xmms2 server config vocoder.speed | cut -d' ' -f3` + 10))")
    , ((mod4Mask               , xK_KP_Subtract ), spawn "xmms2 server config vocoder.speed $((`xmms2 server config vocoder.speed | cut -d' ' -f3` - 10))")

    -- launch gmrun
    , ((mod4Mask .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((mod4Mask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((mod4Mask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((mod4Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((mod4Mask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((mod4Mask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((mod4Mask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((mod4Mask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((mod4Mask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((mod4Mask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((mod4Mask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((mod4Mask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((mod4Mask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((mod4Mask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((mod4Mask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((mod4Mask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((mod4Mask              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    -- TODO, update this binding with avoidStruts , ((modMask              , xK_b     ),

    -- Quit xmonad
    , ((mod4Mask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((mod4Mask             , xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++

    --
    -- control-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m , k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, mod4Mask), (W.shift, shiftMask .|. modMask)]]


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    -- Mouse scroll wheel to raise/lower windows
    , ((modMask, button5), (\w -> windows W.swapDown))
    , ((modMask, button4), (\w -> windows W.swapUp))
    ]

--- myLayout = smartBorders $ avoidStruts $ (Full ||| tiled ||| Mirror tiled)
myLayout = smartBorders $ avoidStruts $ (noBorders Full ||| tiled ||| Mirror tiled)
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 3/5
     delta   = 3/100


--- can get class name with xprop util
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Wine"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Skype"          --> doShift "5[im]"
    , className =? "Pidgin"         --> doShift "5[im]"
    , className =? "Firefox"        --> doShift "4[web]"
 ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myStartupHook = do spawn "xterm"

-- fixing Android studio
-- myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------
xmobarCmdT = "xmobar -o -B '#020823' -F '#adbadd' -t '%StdinReader% }{ %battery% ||| %cpu% || %memory% || %wlp0s19f2u4% || %enp4s6% || /\\/\\/\\/\\/\\||  <fc=#ee9a00>%date%</fc>| %UMMS% | %uname% '"


main = do dinT <- spawnPipe xmobarCmdT
          xmonad $ fullscreenFix $ addFullScreenHook $ XHE.ewmh $ XHD.docks $ defaultConfig {
      -- simple stuff
      terminal           = myTerminal,
      focusFollowsMouse  = myFocusFollowsMouse,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,

      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      -- manageHook         = myManageHook,
      logHook            = dynamicLogWithPP $ {- dzenPP -} xmobarPP { ppOutput = hPutStrLn dinT },


      startupHook        = myStartupHook
}

addFullScreenHook :: XConfig a -> XConfig a
addFullScreenHook c = c { handleEventHook = handleEventHook c <+> XHE.fullscreenEventHook }

-- https://github.com/mpv-player/mpv/issues/888
-- workarounds firefox fullscreen (on F11)
fullscreenFix :: XConfig a -> XConfig a
fullscreenFix c = c {
                      startupHook = startupHook c +++ setSupportedWithFullscreen
                    }
                  where x +++ y = mappend x y

setSupportedWithFullscreen :: X ()
setSupportedWithFullscreen = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

    -- XHS.setWMName "xmonad"
    -- fix intelij Idea (Android studio, arduino)
    XHS.setWMName "LG3D"
