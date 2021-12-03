# Changelog

All notable changes to this project will be documented in this file.

## [4.0.0] - 2021-12-03

### Bug Fixes

- Enable black format on-save by default (22d2cda)
- Disable `capitalize-word` (21ab4b3)
- Increase saved items in recentf (a805aad)
- Use two space is better than four (9b53eb0)
- Need brighter color for selectrum buffer list (7814f66)
- Change all indent mode to 2 (788cc3d)
- Set cURL as default org-cliplink command (0440d9c)
- I change my `firefox-dev` executable to `firefox` (885e492)
- Org-contrib now in separate repo (46131e4)
- Prompt a git push to `main` branch too (32ca75e)
- Emacs28 is now able to work with ligatures (2a592b9)
- Small selectrum minibuffer in small screen (cbcf437)
- Remove ws-writing (76cc610)
- Remove ctrlf-mode (2535f06)
- Disable overwrite-mode (ebed6c7)
- Remove ability to automatically `chmod +x` on scripts file (dd6c795)
- I am using `fish` as shell now (37934a1)
- Disable consult live preview (50c5784)
- Remove marginalia (009c25f)
- Remove dependency to dired+ (0fc1f02)
- Keycast doesn't work with doom-modeline (0cae3ef)
- Remove unused hydras (96fb264)
- Change move-text keybinding (f9cf79a)
- Remove unused beginning-end buffer custom key (67a42e4)
- Revert swapping `C-p` to `C-h` (c566a2e)
- `magit-diff-refine-hunk` accept symbol not a list (edf807e)
- Remove unused code (b544d54)
- I settle with external package to manage path (a4e96ec)
- Wrong module name `aza-screencast` (5625872)
- Use font-awesome as fallback font for unicode icons (b165d1f)
- Move anzu to hydra menu (23cbd8d)
- Remove unused packages (d51ff6a)
- Remove unnecessary parenthesis (0ce7a8e)
- Remap hard keychord (99e9da6)
- Remove unnecessary config in vterm (9434f46)
- Always display magit status in full frame (b04e257)
- New version of mu4e works out of the box with its default recipe (f72dea7)
- Ace-window label too small (053de65)
- Use default lsp settings (b5fc359)
- Xah-dired-sort (17cd379)
- Replace `st` in `term-here` with `wezterm` (c35306a)
- Use shorter keybinding in dired (7c180fe)
- `pyenv` still needed even I am on lsp. (a87aa02)
- They are merged into `git-modes` (9eeaccb)
- Reconfigure hydra (f449ed0)
- Add programming mode to rust mode (14f1e18)
- Disable ui doc (cd456a7)
- More accessible keys (09d9592)
- Disable pomodoros and emojify (761cb8f)
- Remove emojify. Emacs 29 supports emoji out-of-the-box. (aa321df)
- Show the previous directory name in modeline (fd2bf12)
- `doom-modeline` font not inherited from `default-frame-alist font` anymore (b087996)
- Use `elgot` and plain `rust-mode` (efdd5e8)
- Don't activate `venv` in non python env (8b4aba7)
- Revert winner-mode to its default keybindings (5b54552)
- Don't highlight long long lines in whitespace-mode (f36b6f5)
- Disable mu4e (b2fdd53)
- Disable latex (e695629)

### Features

- Ability to use `.dir-locals.el` to toggle black formatter (e30d45e)
- Support dockerfile mode (e3f9928)
- More timezone (eff3974)
- More consult command (eec44e9)
- Enable highlight-indentation in python mode (8d9c3ba)
- A real dired rainbow (572d71a)
- Omit more uninteresting files (3bd8ae1)
- Open other window with consult (90477f8)
- Defer native compilation (be431b0)
- Avoid accidental push to master branch (f47def3)
- Hide username when saving buffer (0903723)
- Use directory name for tmux panes (a7f6c9f)
- Revert zoom feature in hydra (30133a0)
- Password generator (907dba7)
- Support lua (7574bad)
- Add dublin to world clock (cd28304)
- Hide python egg (b57be1e)
- Support winner mode in hydra (9c21225)
- Activate venv automatically using dir-locals (511834d)
- Omit `target` dir (bf757fb)
- Omit `htmlcov` in dired (109b606)
- Support csv file (28d1af7)
- Release scripts (3e044ee)

## [3.0] - 2021-02-01

### Bug Fixes

- Disable lsp vetur formatter (6147aa8)
- Deprecated function of lyspell-correct-helm (c0b72d5)
- Abbrev location (6a68e76)
- Smtp config (d852d6a)
- Aza-secrets functions not loaded (29cc11c)
- Use-package 2 warning (0992fc2)
- Use-package v2 (acc18ab)
- Remove `newline-mark` from writing-mode (ee2e13a)
- Flashing screen in helm projectile-ag (d25a8a5)
- Use-package v2 (2468b70)
- Drop ercn. always fail in straight build (672b3cb)
- Disable minions (0cd1388)
- Hide unused lighter (2bbd212)
- Drop apheleia (2e87cda)
- Don't dim company-box buffer (20bab15)
- Use lsp for rust (cd07854)
- Turn on auto-fill when ws-writing off (2c5c4e2)
- Prompt comment in CANCELLED item (def368d)
- Disable lsp-format. it always breaks (8ec0d02)
- Disable annoying UI pops up in lsp-mode (996625e)
- Use line number instead of newline icon (0ff07e5)
- Disable percent position in modeline (88a1c56)
- Move request.el litter (fdc43df)
- Make filename in modeline shorter (a679e9f)
- Switch to light theme. (1b9d82b)
- Use `exec-path-from-shell` (f567a05)
- Turn-on/off doesn't work for autofill. (4a11396)
- Disable helm-projectile. (6225969)
- Disable format-on-save rust-mode (0ca583b)
- Remove realgud (625727d)
- Move aza-fix and aza-enhc to the top of module list (6d7a9b2)
- Adjust dired-rainbow to white theme (15ae45f)
- Hide org clock from modeline (67fe704)
- Drop elfeed (bdb9c9a)
- Drop google-translate (5e28166)
- Drop define-word (85072bc)
- Drop atomic-chrome (69515bb)
- Drop explain-pause mode (8749bb9)
- Drop org-brain (b717bc9)
- Drop pdf-tools (cf3e74b)
- Migrate from helm to selectrum (8062a56)
- Emacs28 has built-in current flyspell language displayed (b60129d)
- Lsp can't save buffer (76a7a25)
- Helm-lsp is not used anymore (217803f)
- Emacs28 doesn't save abbrevs count before quit (11be40f)
- ‘notifications’ is not a known ERC module (dd20244)
- Drop erc-view-log (a3b5ffb)
- Format on save now works well (2608791)
- Make flyspell-correct use default mode (2f92943)
- Emacs28 show flyspell language by default (16287f2)
- Hide more non-interesting temp buffer (9592984)
- Use line number to distinguish breaks (3b1a3fc)
- Newer version of consult load `consult-selectrum` implicitly (315dc20)
- Workrave is better than activity-watch (225b5d2)
- Replace `helm-history` with `consult-history` (35ec49c)
- Too much warning in gccemacs (de57c2f)
- Use upstream source for dimmer.el (6108c6e)
- `Wrong number of arguments` in mu 1.4.x (17e250c)
- Load auto-capitalize early (4047846)
- Set diary location early (ac8c717)

### Features

- Make pomodoro more accessible with hydra (c82fe2e)
- Omit uninteresting ansible files (14ff301)
- Use current light value as default in `light-set-value` (04ef84d)
- To-snake-case function (12d132a)
- Connect-remote (e6d9bde)
- Mail and rss menu in hydra (b710b80)
- Hide "*straight-process" "*elfeed-log" "*trace of SMTP session" from helm buffer (5516a82)
- Respect projectile file as priority (5c5d02b)
- Unfill region (e2ff438)
- Environment that prefer soft-break 🎉 (33de188)
- Support keychain (7f58ceb)
- Use datetime for magit-log-margin (4b13251)
- Hide nextcloud meta files (0fe608e)
- Add ws-writing-mode hook in markdown and org (248fd63)
- Add icons in org todo state (8b64197)
- Support toml mode (1626910)
- Emoji in org-capture (f65f8ca)
- Respect front-matter header (88dbd02)
- Support appt and world times (271a8d3)
- Support svelte mode (bbc25cb)
- Use rust-analyzer as default (81477c9)
- Change theme to doom-theme (9151d0c)
- Change font to fira-code (3ffc212)
- Toggle minor-mode info in modeline (3478275)
- Support org-pomodoro (9865d98)
- Support fish files (460957e)
- Make mue4 header larger and follow ISO datetime (3c5c334)
- Recognice .markdown extension (14832b8)
- Support more timezone (42ad621)
- Support activity watch (df6d7c6)
- Hide some minor modes (947fc4d)
- Make TRAMP faster (007571e)
- Enable format buffer before save (2bfe7ac)
- Filter some buffer from consult (65f6f40)
- Use ctrlf instead of isearch (229fe3b)
- Ignore straight from recentf (491228a)
- Use pyls instead of elpy (d408b9d)
- Check more location for org-mode reminder (2ff567b)

## [1.0.0] - 2018-11-12

### Bug Fixes

- Profide elpy-shell-clear-shell only when elpy loaded (367ae6b)
