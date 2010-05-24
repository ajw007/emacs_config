;; Setup SLIME mode
(add-to-list 'load-path "~/elisp/external/slime")
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy slime-fuzzy))

(add-hook 'slime-mode-hook
          (lambda ()
            (progn
              (setq lisp-indent-function 'common-lisp-indent-function
                    slime-complete-symbol-function 'slime-fuzzy-complete-symbol
                    common-lisp-hyperspec-root "/usr/local/share/doc/HyperSpec/"
                    slime-autodoc-use-multiline-p t)
              (define-key slime-mode-map (kbd "M-TAB") 'slime-fuzzy-complete-symbol))))

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'paredit-mode-hook 
          (lambda ()
            (progn
              (define-key paredit-mode-map [(control right)] 'forward-word)
              (define-key paredit-mode-map [(control left)] 'backward-word)
              (define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
              (define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-slurp-sexp)
              (define-key paredit-mode-map (kbd "M-[") 'paredit-forward-barf-sexp)
              (define-key paredit-mode-map (kbd "C-M-]") 'paredit-backward-barf-sexp)
              (define-key paredit-mode-map (kbd "<return>") 'newline-and-indent)
              (define-key paredit-mode-map (kbd "C-j") 'newline))))


