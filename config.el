;; -*- lexical-binding: t; -*-

;;
;; Personal info
;;

(setq user-full-name "azzamsa"
      ;; prevent search engines from indexing the user's email address
      user-mail-address (rot13 "abercyl@nmmnzfn.pbz"))

;;
;; UI
;;
(setq +font-family "JetBrainsMono Nerd Font"
      +font-size 19
      +emoji-font "Twemoji")

(setq camp-fonts `(:font-family ,+font-family
                                :font-size ,+font-size
                                :variable-pitch-font-family ,+font-family
                                :variable-pitch-font-size ,+font-size
                                :unicode-font-family ,+emoji-font))

;;
;; Camp
;;

(setq camp-scratch-file "~/.local/share/meta/scratch.md")
(+ensure-directory camp-scratch-file)

(setq camp-abbrev-file "~/.local/share/meta/abbrevs.el")
(setq abbrev-file-name (+ensure-directory camp-abbrev-file))
