;; Setup SLIME mode
(add-to-list 'load-path "~/elisp/external/slime")
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
    (setq slime-lisp-implementations
     '((sbcl ("sbcl" "--core" "/home/mburrows/elisp/external/slime/sbcl.core-for-slime"))))

    (setq inferior-lisp-program "/usr/bin/sbcl"
          lisp-indent-function 'common-lisp-indent-function
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol
          common-lisp-hyperspec-root "/usr/local/share/doc/HyperSpec/"
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

;; ERC
(require 'erc)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs" "#stumpwm")))
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") 
      (erc-track-switch-buffer 1) 
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.freenode.net" :port 6667 :nick "dic3m4n" :full-name "Matt Burrows"))))

(global-set-key (kbd "<f9> i") 'djcb-erc-start-or-switch)

;; (load "~/.ercpass")
;; (require 'erc-services)
;; (erc-services-mode 1)
;; (setq erc-prompt-for-nickserv-password nil)
;; (setq erc-nickserv-passwords
;;       `((freenode (("dic3m4n_" . ,freenode-diceman-pass)))))
