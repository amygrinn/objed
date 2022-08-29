(setq package-user-dir
      (expand-file-name (format ".elpa/%s/elpa" emacs-version)))
(package-initialize)
(add-to-list 'load-path default-directory)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'avy)
  (package-install 'avy))

