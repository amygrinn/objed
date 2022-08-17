(let* ((name "objed")
       (out "dist/")
       (pkg (with-temp-buffer
              (insert-file-contents (concat name ".el"))
              (package-buffer-info)))
       (version (package-desc-version pkg))
       (descriptor-file (concat name "-pkg.el"))
       (package-name (apply #'format "%s-%d.%d.%d"
                            name version))
       (files (append
               (list descriptor-file)
               (directory-files default-directory nil "^[^.].*\\.el$")
               (directory-files-recursively
                (expand-file-name "deps" default-directory)
                "^[^.].*\\.el$"))))
  (or (file-exists-p out) (make-directory out))
  (with-temp-file descriptor-file
    (insert ";; -*- no-byte-compile: t; -*-\n")
    (pp `(define-package ,name ,(package-version-join version)
           ,(package-desc-summary pkg)
           ,(mapcar
             (lambda (r)
               (list (car r) (package-version-join (cadr r))))
             (package-desc-reqs pkg)))
        (current-buffer)))
  (shell-command
   (concat (executable-find "tar")
           ;; Flatten files
           " --transform 's;^\\(.*/\\)*;" package-name "/;' "
           " -cvf " out package-name ".tar "
           (string-join files " ")))
  (delete-file descriptor-file))

