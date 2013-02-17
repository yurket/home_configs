import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#4fdC09"

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/lite/.xmobarrc"
    xmonad $ defaultConfig
        { 
           	manageHook = manageDocks <+> manageHook defaultConfig,
        	layoutHook = avoidStruts  $  layoutHook defaultConfig,
        	modMask = mod4Mask,     -- Rebind Mod to the Windows key
        	normalBorderColor  = myNormalBorderColor,
        	focusedBorderColor = myFocusedBorderColor,
		workspaces	   = myWorkspaces
        } ---`additionalKeys`
---        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
---        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
---        , ((0, xK_Print), spawn "scrot")
---        ]


        