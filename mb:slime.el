;; Setup SLIME mode
(add-to-list 'load-path "~/elisp/external/slime")
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
    ;(setq slime-lisp-implementations
    ; '((sbcl ("sbcl" "--core" "/home/mburrows/elisp/external/slime/sbcl.core-for-slime"))))

    (setq inferior-lisp-program "/usr/bin/sbcl"
          lisp-indent-function 'common-lisp-indent-function
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol
          common-lisp-hyperspec-root "/usr/local/share/doc/HyperSpec/"
          slime-autodoc-use-multiline-p t)
    
    (slime-setup '(slime-fancy slime-fuzzy))

    (define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
    (define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
    (define-key slime-mode-map (kbd "C-b") 'backward-sexp)
    (define-key slime-mode-map (kbd "C-M-b") 'backward-char)
    (define-key slime-mode-map (kbd "C-f") 'forward-sexp)
    (define-key slime-mode-map (kbd "C-M-f") 'forward-char)

    (define-key slime-mode-map (kbd "M-TAB") 'slime-fuzzy-complete-symbol)
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


