;;
;; ERC (IRC client)
;;
(require 'erc)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs" "#haskell" "#xmonad" "#c++")))
 (erc-track-mode t)
 (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
 ;; don't show any of this
 (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

 (defun djcb-erc-start-or-switch ()
   "Connect to ERC, or switch to last active buffer"
   (interactive)
   (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
       (erc-track-switch-buffer 1) ;; yes: switch to last active
     (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
       (erc :server "irc.freenode.net" :port 6667 :nick "dic3m4n"))))

(global-set-key (kbd "<f9>")            'djcb-erc-start-or-switch)

;; (load "~/.ercpass")
;; (require 'erc-services)
;; (erc-services-mode 1)
;; (setq erc-prompt-for-nickserv-password nil)
;; (setq erc-nickserv-passwords
;;       `((freenode (("dic3m4n_" . ,freenode-diceman-pass)))))
