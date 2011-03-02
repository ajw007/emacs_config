;; Flymake handler for python code
;;
;; Author: Menno Smits
;;
;; Inspired by flymake-ruby.el by Steve Purcell
;; http://github.com/purcell/emacs.d/blob/master/site-lisp/flymake-ruby/flymake-ruby.el
;;
;; Usage:
;; (require 'flymake-pyflakes)

(defvar flymake-pyflakes-allowed-file-name-masks '(("\\.py\\'" flymake-pyflakes-init)))

(defun flymake-pyflakes-create-tempfile (filename prefix)
  (make-temp-file (or prefix "flymake-pyflakes")))

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-pyflakes-create-tempfile)))
    (list "pyflakes" (list temp-file))))


(defun flymake-pyflakes-load ()
  (let ((file-name (buffer-file-name (current-buffer))))
    ; Don't start flymake when a symlink is being followed as the
    ; buffer gets killed immediately leading to a "Buffer has a
    ; running process" warning.
    (when (string= file-name (file-truename file-name))
      (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-pyflakes-allowed-file-name-masks)
      (flymake-mode t))))

(add-hook 'python-mode-hook 'flymake-pyflakes-load)

(provide 'flymake-pyflakes)



