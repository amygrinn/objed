(defun objed-key-notify ()
  "Send notification for last key entered."
  (let ((keys (key-description (this-command-keys-vector))))
    (unless (string-empty-p keys)
      (notifications-notify
       :title keys))))

(add-hook 'post-command-hook #'objed-key-notify nil t)

(remove-hook 'post-command-hook #'objed-key-notify t)
