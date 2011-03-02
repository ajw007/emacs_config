(defun bats-deploy-crontab ()
  "Deploy the current buffer containing a BATS crontab"
  (interactive)
  (save-buffer)
  (let* ((user-host (bats-dest-from-crontab-name (buffer-file-name)))
         (user (car user-host))
         (host (cadr user-host)))
    (bats-deploy-crontab-to-host host user)))


(defun bats-deploy-crontab-to-host (host user)
  (let* ((write-dest (format "/ssh:%s:crontab.new" host))
         (diff-buffer (create-file-buffer "crontab-diff")))
    (copy-file (buffer-file-name) write-dest t)
    (shell-command (format "ssh -q %s 'sudo -u %s crontab -l | diff - crontab.new'" host user) diff-buffer)
    (switch-to-buffer-other-window diff-buffer)
    (when (yes-or-no-p "Ok to deploy? ")
      (shell-command (format "ssh -q %s 'sudo -u %s crontab crontab.new'" host user))
      (kill-buffer diff-buffer))
    (delete-file write-dest)))


(defun bats-dest-from-crontab-name (filename)
  "Extract the username and host from a crontab filename"
  (unless (string-match "\\.crontab$" filename) (throw 'invalid-filename t))
  (let ((basename (file-name-nondirectory filename)))
    (split-string (car (split-string basename "\\.")) "_")))

(provide 'bats-crontabs)
