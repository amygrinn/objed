;; emacs -Q -batch -L . -f batch-byte-compile file.el

;;  bail out on compilation warnings and errors
(setq byte-compile-error-on-warn t)
(setq byte-compile--use-old-handlers nil)

;; compile *.el files
(dolist (file (append
               (directory-files-recursively (expand-file-name "deps" default-directory)
                                            "^[.].*\\.el$")
               (directory-files default-directory nil "^[^.].*\\.el$")))
  (unless (byte-compile-file file)
    (kill-emacs 1)))
