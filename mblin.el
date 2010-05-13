(visit-tags-table "~/cpp/TAGS")

(defun bats-file-cache ()
  (interactive)
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/cpp")
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/python")
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/sql")
  (file-cache-delete-svn))

(setq jabber-account-list '(("mburrows@lxchat" (:connection-type . ssl))
                            ("maburrow@googlemail.com"
                              (:network-server . "talk.google.com")
                              (:port . 443)
                              (:connection-type . ssl))
                            ))

;; Load GNUS
(load-file "~/elisp/dotgnus.el")
(bats-mail)

;; Setup SLIME mode
(add-to-list 'load-path "~/elisp/external/slime")
(require 'slime-autoloads)


(eval-after-load "slime"
  '(progn
    (setq inferior-lisp-program "/usr/local/bin/sbcl"
          lisp-indent-function 'common-lisp-indent-function
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol
          slime-autodoc-use-multiline-p t)
    
    (slime-setup '(slime-fancy slime-fuzzy))
    
    (define-key slime-mode-map (kbd "RET") 'newline-and-indent)
    (define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
    (define-key slime-mode-map (kbd "C-j") 'newline)
    (define-key slime-mode-map [(control right)] 'forward-word)
    (define-key slime-mode-map [(control left)] 'backward-word)
    (define-key slime-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
    (define-key slime-mode-map (kbd "C-M-[") 'paredit-backward-slurp-sexp)
    (define-key slime-mode-map (kbd "M-[") 'paredit-forward-barf-sexp)
    (define-key slime-mode-map (kbd "C-M-]") 'paredit-backward-barf-sexp)))

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
