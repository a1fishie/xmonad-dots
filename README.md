# xmonad-dots

todo: screenshots, rewriting half the star wars saga into the config (nearly done)

### include:

- 5 hour, 3 days a week therapy sessions
- three days worth of tears
- bloat
- random colors that came out of my vomit
- a semi-functioning xmobar (doesnt work)
- c l i c k y workspaces
- a beautiful wallpaper
- wait i forgot to upload the wallpaper
- pop out terminal!! (scratchpads ftw, NSP = fake desktop)

# install guide

please ensure two semi-functioning limbs of your choosing are availble at present.

1. Run `git clone https://github.com/a1fishie/xmonad-dots` into a directory you wish to perish.
2. `cd xmonad-dots`
3. simply copy the files from `.config` into your `.config` folder.
4. forget everything, going on a tangent in order to ~~fix~~ implement unneccesary things.
5. copy the `.xmonad` folder into your home directory
6. remember to `sudo make install` the dmenu folder
7. quit your current session by brutally snapping your cpu into millions of tiny pieces.
8. configure your .xinitrc (basic ass hoe) or login manager (supremacy) to boot into xmonad
9. voila! you have now obtained the most basic dots you could image, aren't you proud of yourself!

# keybinds
i won't lie to you, i dont know half of these myself

<details><summary>domain e x p a n s i o n</summary>

default meta = 'windows' key, if you prefer alt i will hunt you. (to tell you to change some other keybinds)
you can also click the workspaces to move (except NSP for the reason above)

| Keybind                     | Function                                                           |
| :-------------------------- | ------------------------------------------------------------------ |
| Ctrl+Meta+Alt+Q             | Quit.                                                              |
| Meta+Alt+Q                  | Quit XMonad                                                        |
| Meta+Alt+R                  | Recompile XMonad                                                   |
| Meta+Shift+R                | Restart XMonad                                                     |
| Meta+Enter                  | Open terminal in a scratchpad: toggleable window  (kitty)          |
| Meta+S                      | Open Launcher (dmenu - yet to config properly)                     |
| Meta+C                      | Open text editor of choice (default neovide)                       |
| Meta+Shift+C                | Open your browser of choice (change variable near top)             |
| Meta+E                      | Open file manager of choice (dolphin by default, questionable ik)  |
| Ctrl+Alt+J/K/H/L            | Dec/Inrease WINDOW spacing  (J/K) Dec/Increase SCREEN spacing (H/L)|
| PrintScrn                   | maim Screenshot (saved to clipboard)                               |
| Meta+N                      | c o l o r p i c k e r!!! (hex)                                     |
| Alt+Space                   | script to change layout (configurable in the xmonad folder)        |
| Ctrl+Shift+Q                | Kills currently focused window                                     |
| Meta+B                      | Refreshes current desktop??                                        |
| Meta+Tab                    | Changes Layout (default threeCol and tall, add more in xmonad.hs)  |
| Meta+F                      | Toggle fullscreen layout (useful for fullscreen videos and games)  |
| Meta+J/K                    | Change window focus (vim-esque)                                    |
| Meta+M                      | Focus master window                                                |
| Meta+Backspace              | Promotes window to become master                                   |
| Meta+Shift+J/K              | Move window left/right                                             |
| Meta+Alt+H/L                | Shrink/grow focused window                                         |
| Meta+T                      | Push window into tiling ("sink")                                   |
| Mod+[1-9]                   | Change workspace                                                   |
| Mod+Shift+[1-9]             | Move focused window to workspace                                   |
| Mod+Shift+Up/Down           | Increase master window number (becomes apparent in tall layout)    |
| Mod+MLeftClick              | Move window, places into floating                                  |
| Mod+MRightClick             | Resize window in floating                                          |

</details>

# Dependencies
<details><summary>wonder what could go here</summary>
<pre> xmonad xmobar lxsession xorg-xrandr xorg-xprop python-pywal picom maim xcolor nerd-fonts-fira-code awesome-terminal-fonts ttf-meslo </pre>
</details>


# Khajit has fixes if you have coin, alternatively: troubleshooting

### why is there blank icons what
Simple, you or I forgot a font, which one I do not know. - will update soon enough

### why caps no work????
1. Open `.xmonad/change_layout.py`
2. Change variable to `False`
3. Recompile + Restart Xmonad (crashing Xorg also works)

<details><summary>It says it can't compile cuz somethingsomething spotify</summary>

You are really living in the 16th century if you are without spotify.
anyway,
1. Open xmonad.hs using your favorite text editor (neovim!!)
2. Go to line 259 (259gg in neovim/vim)
3. Remove/append `spawnOnce "spotify"` with either a dd (remove line) or by replacing `spotify` with your media player
4. recompile

If xmobar also cries, change `"spotify"` in `xmobarrc`, line 25 and recompile
</details>

<details><summary> why app look bad wah wah wah</summary>

Basically, theres like 25 different theming platforms that you *can* use.
I prefer LXAppearance and Qt5ct, with Kvantum of course.

### Kvantum? You mean Quantum??? I thought that was physics???
You'd be right except wrong quantum.

1. Install `Kvantum` (community repo on Arch)
2. Download QT5 theme from either KDE store or the AUR.
3. Have an absolute breakdown because you can't find a good one
4. Realize you dont need to do anything if you got it from the AUR
5. Unzip the theme into a useless directory (i prefer /boot/efi)
6. Launch Kvantum and under 'Select Kvantum Folder', navigate to where you extracted the theme
7. Skip steps 5 onwards if you installed from the AUR
8. Under `Change/Delete Theme` select your theme and press `Use This Theme`
9. Congratulations. You just wasted 45-75 minutes looking at bad qt5 themes.
</details>
<details><summary> Kvantum is done. Now what???? </summary>
time for `LXAppearance` and `QT5CT`.

1. Download said programs (if you use the KDE suite you might wanna get `qt5ct-kde` cuz better integration)
2. Set themes in LXAppearance, Icon Theme, and Mouse Cursor Theme.
3. Forget to hit apply and close LXAppearance
4. Realize you wasted another 45 minutes choosing icon themes.
5. Open Qt5ct
6. Realize it's throwing errors at you
7. Google frantically for another 25 minutes, searching for the answer on StackOverflow
8. Export `QT_QPA_PLATFORMTHEME=qt5ct` in your environment variable
9. Restart Qt5ct
10. Select `kvantum` under `Style:`, feel free to mess around with the other settings too
11. Hit apply.
12. Changes should take effect immediately.


# Walls
Basically i stole some walls off the internet and stuck to one and some-what (terribly) made color palette, somewhat from pywall.

Inspired by [@CordlessCoder](https://github.com/CordlessCoder)
got me into wms in the first place (also stole a python script off him <3 thank you.)
