;;; Site specific config for my work laptop

;; Slime configuration
(load-file "~/elisp/mb:slime.el")

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
