;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My emacs customisations                                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add local elisp dirs to the load path
;;
(let* ((my-lisp-dir "~/elisp/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start emacs as a server, push files to it using 'emacsclient'
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sort out annoyances
;;
(global-font-lock-mode 1)
(setq visible-bell t)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(show-paren-mode 1)
(transient-mark-mode t)
(delete-selection-mode t)
(setq case-fold-search t)
(setq truncate-lines 1)
(display-time-mode t)
(setq-default indent-tabs-mode nil)
(setq save-place t)
(setq fill-column 120)
(column-number-mode)
(setq gdb-create-source-file-list nil)
(setq global-auto-revert-mode t)
(global-hl-line-mode t)

;; Sort out compilation window behavior
(setq compilation-scroll-output 'first-error)
(setq compilation-window-height 10)

;; Make scrolling less jumpy
(setq
  scroll-margin 0                  
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

;; Make text mode the default
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

;; We don't like line or list expansion
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially 
                                         try-complete-file-name 
                                         try-expand-all-abbrevs 
                                         try-expand-dabbrev 
                                         try-expand-dabbrev-all-buffers 
                                         try-expand-dabbrev-from-kill 
                                         try-complete-lisp-symbol-partially 
                                         try-complete-lisp-symbol))

;; Make Emacs ask me if I want to exit. I have a tendency to hit C-x C-c by
;; accident sometimes. When I'm on a machine without this and I'm using Emacs, I
;; end up cursing myself.
(global-set-key "\C-x\C-c"
                (lambda () (interactive)
                  (if (string-equal "y" (read-string "Exit Emacs (y/n)? "))
                      (save-buffers-kill-emacs))))

;; Copy-paste should work with other X clients
(setq x-select-enable-clipboard t        
      interprogram-paste-function      
      'x-cut-buffer-or-selection-value)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some handy packages
;;

;; Turn on midnight mode to clean buffers every evening
(require 'midnight)

;; Support for Subversion version control
(require 'vc-svn)
(require 'psvn)

;; Sort out the handling of identically named buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Perty colours
(require 'color-theme)
(load-file "~/elisp/external/color-theme/themes/color-theme-library.el")
(load-file "~/elisp/themes/color-theme-wombat.el")
(color-theme-wombat)

;; Put backup files in one directory rather than spreading them all
;; over the file system
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs-backup"))))

;; Turn on IDO mode with filecache
(require 'filecache)

(defun file-cache-populate ()
  (interactive)
  (message "Loading file cache...")
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find "/home/awatkins/workspace")
  (file-cache-add-directory-using-find "/home/awatkins/scratch")
  (file-cache-add-directory-using-find "/home/awatkins/.emacs.d"))

(eval-after-load "filecache" '(file-cache-populate))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must

;; Load TRAMP for SSH access
(require 'tramp)
(setq tramp-default-method "ssh")

;; Makes #! scripts executable after saving
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Yank history
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;; GIT integration
(require 'magit)

;; Used to HTML-ize a buffer
(require 'htmlize)

;; Session management
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; Find File At Point
(require 'ffap)

;; Turn on ansi color term mode for the shell
(ansi-color-for-comint-mode-on)

;; NXML mode
(require 'nxml-mode)
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))

;; JS2 mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Load snippet package
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/external/yasnippet/snippets")
(yas/load-directory "~/elisp/snippets") ; my custom snippets
(setq yas/wrap-around-region t)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/x-prompt))

;; Make buffer list perty and grouped
(defalias 'list-buffers 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"      
               ("Org" (mode . org-mode))  
               ("Directories" (mode . dired-mode))
               ("C++" 
                (or
                 (mode . c-mode)
                 (mode . c++-mode)
                 (name . "\.inc$"))) 
               ("Python" (mode . python-mode))
               ("SQL" 
                (or
                 (mode . sql-mode)
                 (name . "\.sqli$")))
               ("Lisp"
                (or 
                 (mode . lisp-mode)
                 (mode . emacs-lisp-mode)))
               ("Subversion" (name . "\*svn"))
               ("Magit" (name . "\*magit"))
               ("Chat" (name . "\*.*jabber.*\*"))
               ("Help"
                (or
                 (name . "\*Help\*")
                 (name . "\*Apropos\*")
                 (name . "\*info\*")))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;; Enable breadcrumbs, bound to F3
(require 'breadcrumb)

;; Provide a menu of tags when there's multiple matches
(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; Setup bookmarks
(setq 
  bookmark-default-file "~/elisp/bookmarks" 
  bookmark-save-flag 1)                     

;; Select current word then semantic unit
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(global-set-key "\M-8" 'extend-selection)

;; Don't use the scrollbar, we want to see code not chrome
(load-file "~/elisp/external/sml-modeline.el")
(sml-mode t)
(scroll-bar-mode -1)

;; Session management
(require 'winring)
(winring-initialize)

(require 'list-register)
(global-set-key (kbd "C-x r v") 'list-register)

;; Org mode setup
;;(load-file "~/elisp/my-org-mode.el")
(require 'org-utils)
(require 'org-config)

;; Turn on symbol highlighting
(require 'highlight-symbol)

;; Automatically pair braces and quotes
(require 'autopair)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language modes
;;

;; Python mode with IPython shell support
(require 'python-mode)
(setq ipython-command "/usr/bin/ipython")
(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n") ; fix tab completion
(setq py-python-command-args '("-colors" "Linux"))
(require 'ipython)
(setq py-pychecker-command "/home/mburrows/scripts/pychecker.sh")
(setq py-pychecker-command-args (quote ("")))
(setq python-check-command "/home/mburrows/scripts/pychecker.sh")
(add-hook 'python-mode-hook 
          #'(lambda () 
              (progn 
                (autopair-mode)
                (setq autopair-handle-action-fns
                      (list #'autopair-default-handle-action
                            #'autopair-python-triple-quote-action)))))

;; CC Mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(setq c-basic-offset 4)
(setq c-default-style '((c++-mode . "k&r") (java-mode . "java") (awk-mode . "awk") (other . "gnu")))
(add-hook 'c-mode-hook #'(lambda () (autopair-mode)))

;; html-helper mode
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . html-helper-mode) auto-mode-alist))

;; Haskell mode
;(load-library "~/elisp/external/haskellmode-emacs/haskell-site-file")
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flymake-cursor)
(require 'flymake-pyflakes)
(require 'bats-crontabs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions
;;
(defun dot-emacs ()
  "Visit .emacs"
  (interactive)
  (find-file "~/elisp/my-customisations.el"))

(defun load-emacs ()
  "Load .emacs"
  (interactive)
  (load-file "~/.emacs"))

(defun indent-buffer ()
  "Indents the whole buffer."
  (interactive)
  (let ((beg (point-min))
        (end (point-max)))
    (indent-region beg end)))

;; Extend recenter line command
;; e.g. C-l = recenter middle, C-l C-l = top, C-l C-l C-l = bottom
(defvar my-recenter-points nil)
(defun my-recenter ()
  (interactive)
  (if (or (not (eq last-command 'my-recenter))
          (null my-recenter-points))
      (setq my-recenter-points '(1 nil -2)))
  (apply 'recenter (list (pop my-recenter-points))))

;; Swaps two buffers
(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))


;; Move current line up or down with M-up or M-down
(defun move-line (n)
   "Move the current line up or down by N lines."
   (interactive "p")
   (let ((col (current-column))
         start
         end)
     (beginning-of-line)
     (setq start (point))
     (end-of-line)
     (forward-char)
     (setq end (point))
     (let ((line-text (delete-and-extract-region start end)))
       (forward-line n)
       (insert line-text)
       ;; restore point to original column in moved line
       (forward-line -1)
       (forward-char col))))
 
(defun move-line-up (n)
   "Move the current line up by N lines."
   (interactive "p")
   (move-line (if (null n) -1 (- n))))
 
(defun move-line-down (n)
   "Move the current line down by N lines."
   (interactive "p")
   (move-line (if (null n) 1 n)))

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

;; Many times you'll do a kill-line command with the only intention of getting the contents of the line into the
;; killring. Here's an idea stolen from Slickedit, if you press copy or cut when no region is active you'll copy or cut
;; the current line:
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called rinteractively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Most times you'll want to open a fresh line and move to it without breaking the current one even if the point is not
;; at the end of the line. Let's copy how vi opens new lines and use it with C-o and use [return] for doing hard
;; linebreaks:
(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun vi-open-prev-line (arg)
  "Move to the prev line (like vi) and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

;; Another cool vi feature, pressing % when on a left or right parenthese will jump to the matching parenthese:
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\{") (forward-list 1) (backward-char 1))
        ((looking-at "\}") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; A somewhat insanely powerful trick, evaluate a region via a shell command and replace the region with the resulting
;; output. Normally you would access this command via C-u M-| but since we're trying to optimize things a bit:
(defun custom-shell-command-on-region nil
  "Replace region with ``shell-command-on-region''.
By default, this will make mark active if it is not and then
prompt you for a shell command to run and replaces region with
the results.  This is handy for doing things like getting
external program locations in scripts and running grep and
whatnot on a region."
  (interactive)
  (save-excursion
    (if (equal mark-active nil)
        (push-mark nil nil -1))
    (setq string
          (read-from-minibuffer "Shell command on region: " nil nil nil
                                'shell-command-history))
    (shell-command-on-region (region-beginning) (region-end) string -1)
    ; Get rid of final newline cause I normally did by hand anyway.
    (delete-char -1)))

(defun remove-line-breaks ()
  "Remove line endings in a paragraph."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

;; Many times you'll want to search for the word or expression at the point. Here is a feature stolen from vi:
(defun isearch-forward-current-word-keep-offset ()
  "Mimic vi search foward at point feature."
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" (thing-at-point 'symbol) "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))    ; offset from start of symbol/word
    (setq offset (- (length curword) offset)) ; offset from end
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-forward re-curword nil t)
        (backward-char offset)
      ;; else
      (progn (goto-char (point-min))
             (if (re-search-forward re-curword nil t)
                 (progn (message "Searching from top. %s" (what-line))
                        (backward-char offset))
               ;; else
               (message "Searching from top: Not found"))))
    (setq case-fold-search old-case-fold-search)))

;; Duplicate line
(defun duplicate-line-down()
  "Duplicate line."
  (interactive)
  (let (
        (beg (line-beginning-position))
        (end (line-end-position)))
    (copy-region-as-kill beg end)
    (beginning-of-line)
    (forward-line 1)
    (yank)
    (newline)
    (forward-line -1)))

(defun duplicate-line-up()
  "Duplicate line."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (copy-region-as-kill beg end)
    (forward-line -1)
    (end-of-line)
    (newline)
    (yank)))

(defun eol-insert-semicolon()
  "Move to EOL and insert a semicolon."
  (interactive)
  (end-of-line)
  (insert ";"))

(defun eol-insert-semicolon-and-newline()
  "Move to EOL and insert a semicolon and a newline."
  (interactive)
  (end-of-line)
  (insert ";")
  (newline-and-indent))

;; ignore svn directories and pyc files in my file cache
(add-to-list 'file-cache-filter-regexps "/[.]svn")
(add-to-list 'file-cache-filter-regexps "/pp")
(add-to-list 'file-cache-filter-regexps "[.]pyc$")

(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
	 (lambda ()
	   (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))

(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (call-interactively 'compile)
    (delete-other-windows)
    (split-window-vertically 15)
    (switch-to-buffer "*compilation*")
    (setq truncate-lines nil)
    (other-window 1)
    )
  )

(defun my-recompile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (call-interactively 'recompile)
    (delete-other-windows)
    (split-window-vertically 15)
    (switch-to-buffer "*compilation*")
    (setq truncate-lines nil)
    (other-window 1)
    )
  )

;; Helper for compilation. Close the compilation window if there was no error at
;; all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings                                                         
;;
(global-set-key "\M-,"          	'pop-tag-mark)
(global-set-key "\C-z"          	'undo)
(global-set-key "\C-l"          	'my-recenter)
(global-set-key "\C-o"          	'vi-open-next-line)
(global-set-key "%"             	'match-paren)
(global-set-key [(meta ?!)]     	'custom-shell-command-on-region)
(global-set-key "\M-s"          	'isearch-forward-current-word-keep-offset)

(global-set-key "\C-ca"         	'align)
(global-set-key "\C-c\C-b"         	'extend-selection)
(global-set-key "\C-cc"         	'my-compile)
(global-set-key "\C-cd"         	'dot-emacs)
(global-set-key "\C-ce"         	'eval-region)
(global-set-key "\C-cf"         	'file-cache-ido-find-file)
(global-set-key "\C-ch"         	'list-matching-lines)
(global-set-key "\C-ci"         	'magit-status)
(global-set-key "\C-c\C-l"		'session-jump-to-last-change)
(global-set-key "\C-x\C-l"		'session-jump-to-last-change)
(global-set-key "\C-cm"         	'manual-entry)
(global-set-key "\C-co"         	'ff-find-other-file)
(global-set-key [(control tab)]         'ff-find-other-file)
(global-set-key "\C-cr"         	'load-emacs)
(global-set-key "\C-c\C-r"         	'revert-buffer)
(global-set-key "\C-cs"         	'svn-status)
(global-set-key "\C-cw"         	'swap-windows)

(global-set-key [(control s)]   	'isearch-forward-regexp)
(global-set-key [(control r)]   	'isearch-backward-regexp)

(global-set-key [(control ? )]          'hippie-expand)
(global-set-key [(control ?')]          'set-mark-command)
(global-set-key [(control right)]       'forward-word)
(global-set-key [(control left)]        'backward-word)

(global-set-key [(meta up)]     	'move-line-up)
(global-set-key [(meta down)]   	'move-line-down)
(global-set-key [(control meta down)]   'duplicate-line-down)
(global-set-key [(control meta up)]     'duplicate-line-up)
(global-set-key [(control meta return)] 'eol-insert-semicolon)
(global-set-key [(shift meta return)]   'eol-insert-semicolon-and-newline)
(global-set-key [(meta return)]         'vi-open-next-line)
(global-set-key [(meta left)]   	'winring-prev-configuration)
(global-set-key [(meta right)]  	'winring-next-configuration)

(global-set-key (kbd "<f1>")            'my-compile)
(global-set-key (kbd "<f2>")            'my-recompile)
(global-set-key [(control f3)]          'bc-set)
(global-set-key [(f3)]                  'bc-previous)
(global-set-key "\C-c\C-j"              'bc-local-previous)
(global-set-key [(shift f3)]            'bc-next)
(global-set-key [(meta f3)]             'bc-list)
(global-set-key [(control f4)]          'highlight-symbol-at-point)
(global-set-key [f4]                    'highlight-symbol-next)
(global-set-key [(shift f4)]            'highlight-symbol-prev)
;;(global-set-key (kbd "<f5>")            'slime-selector)
(global-set-key (kbd "<f6>")            'gud-next)
(global-set-key (kbd "<f7>")            'gud-step)
(global-set-key (kbd "<f8>")            'gud-finish)
; F9-F12 are mostly taken by org-mode
(global-set-key (kbd "<f9> t")          'visit-ansi-term)

(global-set-key (kbd "<M-prior>") 	'previous-error) 
(global-set-key (kbd "<M-next>")  	'next-error)

(global-set-key (kbd "M-o")  		'other-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard translations
;;
;;(keyboard-translate ?\@ ?\")
;;(keyboard-translate ?\" ?\@)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anything config
;;
(require 'anything-config)
(setq fit-frame-inhibit-fitting-flag t)
(setq anything-sources
       (list anything-c-source-fixme
             anything-c-source-buffers
             anything-c-source-buffer-not-found
             anything-c-source-file-name-history
             anything-c-source-files-in-current-dir
             anything-c-source-imenu
             anything-c-source-file-cache
             anything-c-source-bookmarks
             anything-c-source-info-pages
             anything-c-source-man-pages
             anything-c-source-emacs-commands             
             anything-c-source-calculation-result
             anything-c-source-occur))

(global-set-key (kbd "C-;") 'anything)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'rb 'revert-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load host/site specific config if it exists, make sure this gets eval'd last
;;
(let ((site-lib "~/elisp/site.el"))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

