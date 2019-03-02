(defun aza-rust-path ()
  (setenv "PATH" (concat "~/.cargo/bin:"
                         (getenv "PATH")))
  (setenv "PATH" (concat "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src:"
                         (getenv "PATH")))

  (add-to-list 'exec-path "~/.cargo/bin")
  (add-to-list 'exec-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))


(defun aza-latex-path ()
  (setenv "PATH" (concat "/usr/local/texlive/2018/bin/x86_64-linux:"
                         (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/texlive/2018/bin/x86_64-linux"))

(defun aza-go-path ()
  (setenv "PATH" (concat "/usr/local/go/bin:"
                         (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/go/bin"))

(provide 'aza-path)
