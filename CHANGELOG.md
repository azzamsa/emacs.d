# Changelog

All notable changes to this project will be documented in this file.

## [4.1.0] - 2021-12-03

### <!-- 0 -->Features

- Now other can use my config easily (ab22061)

## [4.0.0] - 2021-12-03

### <!-- 0 -->Features

- Release scripts (3e044ee)
- Support csv file (28d1af7)
- Omit `htmlcov` in dired (109b606)
- Omit `target` dir (bf757fb)
- Activate venv automatically using dir-locals (511834d)
- Support winner mode in hydra (9c21225)
- Hide python egg (b57be1e)
- Add dublin to world clock (cd28304)
- Support lua (7574bad)
- Password generator (907dba7)
- Revert zoom feature in hydra (30133a0)
- Use directory name for tmux panes (a7f6c9f)
- Hide username when saving buffer (0903723)
- Avoid accidental push to master branch (f47def3)
- Defer native compilation (be431b0)
- Open other window with consult (90477f8)
- Omit more uninteresting files (3bd8ae1)
- A real dired rainbow (572d71a)
- Enable highlight-indentation in python mode (8d9c3ba)
- More consult command (eec44e9)
- More timezone (eff3974)
- Support dockerfile mode (e3f9928)
- Ability to use `.dir-locals.el` to toggle black formatter (e30d45e)

### <!-- 1 -->Bug fixes

- Disable latex (e695629)
- Disable mu4e (b2fdd53)
- Don't highlight long long lines in whitespace-mode (f36b6f5)
- Revert winner-mode to its default keybindings (5b54552)
- Don't activate `venv` in non python env (8b4aba7)
- Use `elgot` and plain `rust-mode` (efdd5e8)
- `doom-modeline` font not inherited from `default-frame-alist font` anymore (b087996)
- Show the previous directory name in modeline (fd2bf12)
- Remove emojify. Emacs 29 supports emoji out-of-the-box. (aa321df)
- Disable pomodoros and emojify (761cb8f)
- More accessible keys (09d9592)
- Disable ui doc (cd456a7)
- Add programming mode to rust mode (14f1e18)
- Reconfigure hydra (f449ed0)
- They are merged into `git-modes` (9eeaccb)
- `pyenv` still needed even I am on lsp. (a87aa02)
- Use shorter keybinding in dired (7c180fe)
- Replace `st` in `term-here` with `wezterm` (c35306a)
- Xah-dired-sort (17cd379)
- Use default lsp settings (b5fc359)
- Ace-window label too small (053de65)
- New version of mu4e works out of the box with its default recipe (f72dea7)
- Always display magit status in full frame (b04e257)
- Remove unnecessary config in vterm (9434f46)
- Remap hard keychord (99e9da6)
- Remove unnecessary parenthesis (0ce7a8e)
- Remove unused packages (d51ff6a)
- Move anzu to hydra menu (23cbd8d)
- Use font-awesome as fallback font for unicode icons (b165d1f)
- Wrong module name `aza-screencast` (5625872)
- I settle with external package to manage path (a4e96ec)
- Remove unused code (b544d54)
- `magit-diff-refine-hunk` accept symbol not a list (edf807e)
- Revert swapping `C-p` to `C-h` (c566a2e)
- Remove unused beginning-end buffer custom key (67a42e4)
- Change move-text keybinding (f9cf79a)
- Remove unused hydras (96fb264)
- Keycast doesn't work with doom-modeline (0cae3ef)
- Remove dependency to dired+ (0fc1f02)
- Remove marginalia (009c25f)
- Disable consult live preview (50c5784)
- I am using `fish` as shell now (37934a1)
- Remove ability to automatically `chmod +x` on scripts file (dd6c795)
- Disable overwrite-mode (ebed6c7)
- Remove ctrlf-mode (2535f06)
- Remove ws-writing (76cc610)
- Small selectrum minibuffer in small screen (cbcf437)
- Emacs28 is now able to work with ligatures (2a592b9)
- Prompt a git push to `main` branch too (32ca75e)
- Org-contrib now in separate repo (46131e4)
- I change my `firefox-dev` executable to `firefox` (885e492)
- Set cURL as default org-cliplink command (0440d9c)
- Change all indent mode to 2 (788cc3d)
- Need brighter color for selectrum buffer list (7814f66)
- Use two space is better than four (9b53eb0)
- Increase saved items in recentf (a805aad)
- Disable `capitalize-word` (21ab4b3)
- Enable black format on-save by default (22d2cda)

## [3.0] - 2021-02-01

### <!-- 0 -->Features

- Check more location for org-mode reminder (2ff567b)
- Use pyls instead of elpy (d408b9d)
- Ignore straight from recentf (491228a)
- Use ctrlf instead of isearch (229fe3b)
- Filter some buffer from consult (65f6f40)
- Enable format buffer before save (2bfe7ac)
- Make TRAMP faster (007571e)
- Hide some minor modes (947fc4d)
- Support activity watch (df6d7c6)
- Support more timezone (42ad621)
- Recognice .markdown extension (14832b8)
- Make mue4 header larger and follow ISO datetime (3c5c334)
- Support fish files (460957e)
- Support org-pomodoro (9865d98)
- Toggle minor-mode info in modeline (3478275)
- Change font to fira-code (3ffc212)
- Change theme to doom-theme (9151d0c)
- Use rust-analyzer as default (81477c9)
- Support svelte mode (bbc25cb)
- Support appt and world times (271a8d3)
- Respect front-matter header (88dbd02)
- Emoji in org-capture (f65f8ca)
- Support toml mode (1626910)
- Add icons in org todo state (8b64197)
- Add ws-writing-mode hook in markdown and org (248fd63)
- Hide nextcloud meta files (0fe608e)
- Use datetime for magit-log-margin (4b13251)
- Support keychain (7f58ceb)
- Environment that prefer soft-break 🎉 (33de188)
- Unfill region (e2ff438)
- Respect projectile file as priority (5c5d02b)
- Hide "*straight-process" "*elfeed-log" "*trace of SMTP session" from helm buffer (5516a82)
- Mail and rss menu in hydra (b710b80)
- Connect-remote (e6d9bde)
- To-snake-case function (12d132a)
- Use current light value as default in `light-set-value` (04ef84d)
- Omit uninteresting ansible files (14ff301)
- Make pomodoro more accessible with hydra (c82fe2e)

### <!-- 1 -->Bug fixes

- Set diary location early (ac8c717)
- Load auto-capitalize early (4047846)
- `Wrong number of arguments` in mu 1.4.x (17e250c)
- Use upstream source for dimmer.el (6108c6e)
- Too much warning in gccemacs (de57c2f)
- Replace `helm-history` with `consult-history` (35ec49c)
- Workrave is better than activity-watch (225b5d2)
- Newer version of consult load `consult-selectrum` implicitly (315dc20)
- Use line number to distinguish breaks (3b1a3fc)
- Hide more non-interesting temp buffer (9592984)
- Emacs28 show flyspell language by default (16287f2)
- Make flyspell-correct use default mode (2f92943)
- Format on save now works well (2608791)
- Drop erc-view-log (a3b5ffb)
- ‘notifications’ is not a known ERC module (dd20244)
- Emacs28 doesn't save abbrevs count before quit (11be40f)
- Helm-lsp is not used anymore (217803f)
- Lsp can't save buffer (76a7a25)
- Emacs28 has built-in current flyspell language displayed (b60129d)
- Migrate from helm to selectrum (8062a56)
- Drop pdf-tools (cf3e74b)
- Drop org-brain (b717bc9)
- Drop explain-pause mode (8749bb9)
- Drop atomic-chrome (69515bb)
- Drop define-word (85072bc)
- Drop google-translate (5e28166)
- Drop elfeed (bdb9c9a)
- Hide org clock from modeline (67fe704)
- Adjust dired-rainbow to white theme (15ae45f)
- Move aza-fix and aza-enhc to the top of module list (6d7a9b2)
- Remove realgud (625727d)
- Disable format-on-save rust-mode (0ca583b)
- Disable helm-projectile. (6225969)
- Turn-on/off doesn't work for autofill. (4a11396)
- Use `exec-path-from-shell` (f567a05)
- Switch to light theme. (1b9d82b)
- Make filename in modeline shorter (a679e9f)
- Move request.el litter (fdc43df)
- Disable percent position in modeline (88a1c56)
- Use line number instead of newline icon (0ff07e5)
- Disable annoying UI pops up in lsp-mode (996625e)
- Disable lsp-format. it always breaks (8ec0d02)
- Prompt comment in CANCELLED item (def368d)
- Turn on auto-fill when ws-writing off (2c5c4e2)
- Use lsp for rust (cd07854)
- Don't dim company-box buffer (20bab15)
- Drop apheleia (2e87cda)
- Hide unused lighter (2bbd212)
- Disable minions (0cd1388)
- Drop ercn. always fail in straight build (672b3cb)
- Use-package v2 (2468b70)
- Flashing screen in helm projectile-ag (d25a8a5)
- Remove `newline-mark` from writing-mode (ee2e13a)
- Use-package v2 (acc18ab)
- Use-package 2 warning (0992fc2)
- Aza-secrets functions not loaded (29cc11c)
- Smtp config (d852d6a)
- Abbrev location (6a68e76)
- Deprecated function of lyspell-correct-helm (c0b72d5)
- Disable lsp vetur formatter (6147aa8)

## [1.0.0] - 2018-11-12

### <!-- 1 -->Bug fixes

- Profide elpy-shell-clear-shell only when elpy loaded (367ae6b)

