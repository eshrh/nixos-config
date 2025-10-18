{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import Control.Monad (ap, forM_, when, (<=<))
import Control.Monad.Extra (findM, orM)
import Data.Bifunctor
import Data.Char (isSpace)
import Data.Default
import Data.Foldable (asum)
import Data.Functor
import Data.List qualified as L
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Tree
import GHC.IO.Handle
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (killAllOtherCopies)
import XMonad.Actions.CycleWS
import XMonad.Actions.GroupNavigation
import XMonad.Actions.MouseGestures
import XMonad.Actions.OnScreen
import XMonad.Config.Dmwit (outputOf)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run

commandKeys :: [(String, X ())]
commandKeys =
  [ ("M-c", sendMessage NextLayout),
    ("M-g", toggleFullscreen),
    ("M-h", windows W.focusDown),
    ("M-t", windows W.focusUp),
    ("M-q", kill),
    ("M-[", sendMessage Shrink),
    ("M-]", sendMessage Expand),
    ("M-f", withFocused toggleFloat),
    ("M-S-h", windows W.swapDown),
    ("M-S-t", windows W.swapUp),
    ("M-s", windows W.swapMaster),
    -- Prompts
    ("M-/", scratchpadCloseOrPrompt),
    ("M-<Space>", shellPrompt promptConf),
    -- Audio
    ("C-M-a", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
    ("C-M-o", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
    ("C-M-x", spawn "playerctl play-pause"),
    ("C-M-w", spawn "mpc toggle"),
    ("C-M-b", spawn "playerctl previous"),
    ("C-M-m", spawn "playerctl next"),
    -- Application commands
    ("M-S-q", spawn "xmonad --recompile && xmonad --restart"),
    ("M-<Return>", spawn "alacritty"),
    ("M-S-u", spawn "firefox"),
    ("M-S-,", spawn "ames -w"),
    ("M-S-.", spawn "ames -r"),
    ("M-S-a", spawn "scrot -s")
  ]

type WindowAction = Window -> X ()

type KeyBind = (KeyMask, KeySym)

floatDimensions :: W.RationalRect
floatDimensions = W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

toggleFloat :: WindowAction
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w floatDimensions s
    )

type StackOp i l a s sd = i -> W.StackSet i l a s sd -> W.StackSet i l a s sd

type KeyMap = [(KeyBind, X ())]

-- compile time guarantee that (length wksp) = (length numberKeys)
keysToWorkspaces :: [(KeySym, String)]
keysToWorkspaces =
  zip
    ([xK_1 .. xK_9] ++ [xK_0])
    ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

numberKeys :: [KeySym]
numberKeys = map fst keysToWorkspaces

wksp :: [String]
wksp = map snd keysToWorkspaces

-- Divide the 10 workspace keys evenly across all screens
-- Requires recompilation to change `flipped` if the screens are not
-- ordered left to right.
keyGen ::
  (Ord a, Eq s, Eq i) =>
  KeyMask ->
  (StackOp i l a s sd -> x -> X ()) ->
  [(KeySym, x)] ->
  StackOp i l a s sd ->
  KeyMap
keyGen modm comb objects action =
  [ ((mod .|. modm, keysym), comb action target)
    | (keysym, target) <- objects,
      (action, mod) <- [(action, 0), (W.shift, shiftMask)]
  ]

windowKeys :: Int -> Bool -> XConfig m -> M.Map KeyBind (X ())
windowKeys nmonitors flipped (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    wkspKeys modm nmonitors flipped
      ++ keyGen modm applyScreenFunction objects W.view
  where
    applyScreenFunction f sc = screenWorkspace sc >>= flip whenJust (windows . f)
    objects = zip ((if flipped then reverse else id) [xK_d, xK_n]) [0, 1]

wkspKeys :: KeyMask -> Int -> Bool -> KeyMap
wkspKeys modm nmonitors flipped =
  concat $ zipWith (makeKeys modm) chunks [0 .. nmonitors - 1]
  where
    chunks =
      (if flipped then reverse else id) $
        chunksOf (div (length wksp) nmonitors) keysToWorkspaces
    makeKeys modm chunk screen =
      keyGen modm (\f i -> windows $ f i) chunk (viewOnScreen (S screen))

------------------------------------------------------------------------
-- Scratchpads

mkScratchpadFromTerm :: String -> NamedScratchpad
mkScratchpadFromTerm name =
  NS
    name
    ("alacritty --title '" ++ name ++ "' -e " ++ name)
    (title =? name)
    (customFloating floatDimensions)

mkScratchpadFromProgram :: String -> String -> NamedScratchpad
mkScratchpadFromProgram name binary =
  NS name binary (titleContainsString name) (customFloating floatDimensions)

scratchpads :: [NamedScratchpad]
scratchpads =
  map mkScratchpadFromTerm ["htop", "inori", "pulsemixer"]
    ++ [mkScratchpadFromProgram "qBittorrent" "qbittorrent"]

titleContainsString :: String -> Query Bool
titleContainsString = (title <&>) . L.isInfixOf

scratchpadNames :: [String]
scratchpadNames = map (\(NS n _ _ _) -> n) scratchpads

toggleScratchpad :: String -> X ()
toggleScratchpad = namedScratchpadAction scratchpads

data Scratch = Scratch

instance XPrompt Scratch where
  showXPrompt Scratch = "scratchpad: "

scratchPrompt :: XPConfig -> X ()
scratchPrompt c =
  mkXPrompt
    Scratch
    c
    (mkComplFunFromList' c scratchpadNames)
    toggleScratchpad

whichNameIsInfix :: Query (Maybe String)
whichNameIsInfix = findM titleContainsString scratchpadNames

scratchpadCloseOrPrompt :: X ()
scratchpadCloseOrPrompt = do
  win <- gets (W.index . windowset)
  floating <- gets (W.floating . windowset)
  let floats = filter (`M.member` floating) win
  namesInfix <- mapM (runQuery whichNameIsInfix) floats
  maybe (scratchPrompt promptConf) toggleScratchpad (asum namesInfix)

------------------------------------------------------------------------
-- Shell prompt config

-- not quite satisfied with built ins.
xpkeymap :: (Char -> Bool) -> M.Map KeyBind (XP ())
xpkeymap p =
  M.fromList $
    map
      (first $ (,) controlMask)
      [ (xK_z, killBefore),
        (xK_k, killAfter),
        (xK_a, startOfLine),
        (xK_e, endOfLine),
        (xK_y, pasteString),
        (xK_f, moveCursor Next),
        (xK_b, moveCursor Prev),
        (xK_p, moveHistory W.focusUp'),
        (xK_n, moveHistory W.focusDown'),
        (xK_w, killWord' p Prev),
        (xK_g, quit)
      ]
      ++ map
        (first $ (,) 0)
        [ (xK_Return, setSuccess True >> setDone True),
          (xK_KP_Enter, setSuccess True >> setDone True),
          (xK_Escape, quit),
          (xK_BackSpace, deleteString Prev)
        ]

promptConf :: XPConfig
promptConf =
  def
    { height = 50,
      bgColor = "#1d2021",
      fgColor = "#fbf1c7",
      fgHLight = "#1d2021",
      bgHLight = "#fbf1c7",
      borderColor = "#fbf1c7",
      promptBorderWidth = 2,
      promptKeymap = xpkeymap isSpace,
      alwaysHighlight = True,
      historySize = 256,
      maxComplRows = Just 3,
      autoComplete = Just 0
    }


------------------------------------------------------------------------
-- Custom copy-to-all functions

-- The copyToAll' function from Actions.CopyWindow inserts the target window
-- as the focus element of each stack. I don't think like it.

copy' :: (Eq s, Eq i, Eq a) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copy' n s
  | Just w <- W.peek s = copyWindow' w n s
  | otherwise = s

copyToAll' :: (Eq s, Eq i, Eq a) => W.StackSet i l a s sd -> W.StackSet i l a s sd
copyToAll' s = foldr (copy' . W.tag) s (W.workspaces s)

-- Inserts into stack without changing focus.
copyWindow' :: (Eq a, Eq i, Eq s) => a -> i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyWindow' w n = copy'
  where
    copy' s =
      if n `W.tagMember` s
        then W.view (W.currentTag s) $ insertUp' w $ W.view n s
        else s
    insertUp' a =
      W.modify
        (Just $ W.Stack a [] [])
        ( \(W.Stack t l r) ->
            if a `elem` t : l ++ r
              then Just $ W.Stack t l r
              else Just $ W.Stack t (L.delete a l) (a : r)
        )

------------------------------------------------------------------------
-- Mouse gestures

toggleFullscreen :: X ()
toggleFullscreen =
  sendMessage (ModifyWindowBorderEnabled not)
    >> sendMessage ToggleStruts

gestures :: M.Map [Direction2D] WindowAction
gestures =
  M.fromList
    [ ([], focus),
      ([R], \w -> focus w >> shiftNextScreen),
      ([L], \w -> focus w >> shiftPrevScreen),
      ([D], toggleFloat),
      ([U], const toggleFullscreen),
      ([U, L], const shiftToPrev),
      ([U, R], const shiftToNext),
      ([U, D], const $ windows copyToAll'),
      ([D, U], const killAllOtherCopies)
    ]


mouseControls :: XConfig m -> M.Map (ButtonMask, Button) WindowAction
mouseControls (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w
            >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      ( (modm, button2),
        \w ->
          focus w
            >> mouseResizeWindow w
            >> windows W.shiftMaster
      ),
      ((modm, 3), mouseGesture gestures)
    ]

------------------------------------------------------------------------

layout =
  renamed [CutWordsLeft 1] $ spacing 10 $ tiled ||| noBorders Full
  where
    tiled = Tall 1 (3 / 100) (1 / 2)

-- Float PIP in the corner of all wksps
firefoxPIPHook :: ManageHook
firefoxPIPHook =
  (title =? "Picture-in-Picture")
    <&&> (appName =? "Toolkit")
    <&&> (className =? "firefox")
    --> doRectFloat (W.RationalRect (5 / 6) (5 / 6) (1 / 6) (1 / 6))
    <+> doF copyToAll'

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace pat rep s =
  if take plen s == pat
    then rep ++ replace pat rep (drop plen s)
    else head s : replace pat rep (tail s)
  where
    plen = length pat

replaceList :: [(String, String)]
replaceList =
  [ ("Firefox Developer Edition", "firefox"),
    ("Mozilla Firefox", "firefox"),
    ("GNU Emacs", "emacs"),
    (" at ", " @ ")
  ]

replaceAll :: String -> String
replaceAll s = foldl (flip (uncurry replace)) s replaceList

fg :: String -> (String -> String)
fg color = xmobarColor color ""

layoutDispatch :: String -> String
layoutDispatch layout = case layout of
  "Tall" -> "[||]"
  "Full" -> "[  ]"
  _ -> layout

ppFunc :: [Handle] -> X ()
ppFunc xmhandles =
  do
    (dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP)
      xmobarPP
        { ppOutput = \x -> mapM_ (`hPutStrLn` x) xmhandles,
          ppCurrent = wrap "[" "]",
          ppVisible = fg "#928374" . wrap "(" ")",
          ppHidden = fg "#928374" . wrap "{" "}",
          ppHiddenNoWindows = fg "#665c54" . wrap "(" ")",
          ppTitle = fg "#fabd2f". shorten 50 . replaceAll,
          ppSep = " / ",
          ppUrgent = wrap "!" "!",
          ppLayout = layoutDispatch,
          ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
        }

trims :: String -> String
trims = L.dropWhileEnd isSpace . dropWhile isSpace

main :: IO ()
main = do
  output <-
    outputOf
      "xrandr --listactivemonitors 2>/dev/null | awk '{print $1 $4}'"

  when (null $ lines output) $ putStrLn "No displays" >> exitSuccess

  -- parse into [(index, name)]
  let monitors =
        map
          (Data.Bifunctor.second tail . span (/= ':'))
          ((tail . lines) output)

  -- spawn an xmobar for every screen
  xmhandles <- mapM (\(i, _) -> spawnPipe ("xmobar -x " ++ i ++ " -F \"#fbf1c7\" -B \"#1d2021\"")) monitors

  let flippedkeys = False
  xmonad $
    docks $
      ewmh
        def
          { focusFollowsMouse = True,
            clickJustFocuses = False,
            borderWidth = 4,
            modMask = mod1Mask,
            workspaces = wksp,
            normalBorderColor = "#32302f",
            focusedBorderColor = "#458588",
            keys = windowKeys (length monitors) flippedkeys,
            mouseBindings = mouseControls,
            layoutHook = avoidStruts layout,
            logHook = ppFunc xmhandles,
            manageHook =
              firefoxPIPHook
                <+> manageDocks
                <+> namedScratchpadManageHook scratchpads,
            handleEventHook = ewmhDesktopsEventHook,
            startupHook = return ()
          }
        `additionalKeysP` commandKeys
