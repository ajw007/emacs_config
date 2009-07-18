;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My emacs customisations                                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst darwinp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;; Add local elisp dir to the load path
(add-to-list 'load-path "~/elisp")

;; Start emacs as a server, push files to it using 'emacsclient --no-wait'
(server-start)

;; Sort out annoyances
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
(blink-cursor-mode 0)
(setq scroll-bar-mode 'right)
(display-time-mode t)
(setq-default indent-tabs-mode nil)
(setq save-place t)
(setq fill-column 100)
(column-number-mode)

;; Sort out compilation window behavior
(setq compilation-scroll-output 'first-error)
(setq compilation-window-height 10)

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

;; We like line numbers, so lets have them everywhere
(unless darwinp (global-linum-mode t))

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
(color-theme-initialize)
(load-file "~/elisp/themes/color-theme-wombat.el")
(color-theme-wombat)

;; Put backup files in one directory rather than spreading them all
;; over the file system
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs-backup"))))

;; Turn on IDO mode with filecache
(require 'filecache)
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must

;; Load TRAMP for SSH access
(require 'tramp)
(setq tramp-default-method "ssh")

;; Makes #! scripts executable after saving
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; GIT integration
(require 'magit)

;; Eliminate multiple buffers when browsing a directory (very
;; annoying)
(require 'dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

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

;; Load snippet package
(add-to-list 'load-path "~/elisp/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/yasnippet/snippets")

;; Make buffer list perty
(defalias 'list-buffers 'ibuffer)

(require 'stumpwm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language modes                                                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Python mode
(add-to-list 'load-path "~/elisp/python-mode-1.0")
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq ipython-command "/usr/local/bin/ipython")
(setq py-python-command-args '("-colors" "Linux"))
(require 'ipython)

;; CC Mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(setq c-basic-offset 4)
(setq c-default-style '((c++-mode . "k&r") (java-mode . "java") (awk-mode . "awk") (other . "gnu")))
(setq c-indent-comment-alist '((anchored-comment column . 0) (end-block space . 1) (cpp-end-block space . 2) (other align space . 1)))
(setq c-offsets-alist '((case-label . 0) (arglist-close . 0) (innamespace . 0)))

;; html-helper mode
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml$" . html-helper-mode) auto-mode-alist))

;; Haskell mode
(load "~/elisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Winring configuration
(require 'winring)
(setq winring-show-names t)
(setq winring-prompt-on-create 'nil)

(winring-initialize)
(winring-new-configuration)
(winring-prev-configuration)

;; ERC (IRC client)
(require 'erc)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs")))
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
;;
;; Org mode setup
;;
(require 'org-install)
(require 'org-checklist)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(setq org-log-done (quote time))
(setq org-log-into-drawer nil)
(setq org-cycle-separator-lines 0)
(setq org-reverse-note-order nil)
(setq org-agenda-files (quote ("~/Dropbox/org/refile.org"
                               "~/Dropbox/org/work.org"
                               "~/Dropbox/org/home.org")))

;; ToDo keywords and triggers
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                                (sequence "WAITING(w)" "SOMEDAY(S)" "PROJECT(P)"))))

(setq org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                               ("STARTED" :foreground "light yellow" :weight bold)
                               ("DONE" :foreground "forest green" :weight bold)
                               ("WAITING" :foreground "orange" :weight bold)
                               ("SOMEDAY" :foreground "magenta" :weight bold)
                               ("PROJECT" :foreground "pink" :weight bold)))

(setq org-use-fast-todo-selection t)

(setq org-todo-state-tags-triggers
      (quote (("WAITING" ("WAITING" . t) ("NEXT"))
              ("SOMEDAY" ("WAITING" . t))
              (done ("NEXT") ("WAITING"))
              ("TODO" ("WAITING"))
              ("STARTED" ("WAITING"))
              ("PROJECT" ("PROJECT" . t)))))

(setq org-stuck-projects (quote ("+PROJECT" nil ("NEXT") "")))
(setq org-enforce-todo-dependencies t)
(setq org-insert-heading-respect-content t)

;; Tags
(setq org-tag-alist (quote ((:startgroup)
                            ("@work" . ?w)
                            ("@home" . ?h)
                            ("@grantham" . ?g)
                            ("@london" . ?l)
                            (:endgroup)
                            ("NEXT" . ?n)
                            ("PROJECT" . ?p)
                            ("WAITING" . ?f))))

(setq org-fast-tag-selection-single-key 'expert)

;; Remember templates
(require 'remember)
(org-remember-insinuate)
(setq org-default-notes-file "~/Dropbox/org/refile.org")

;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)
(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))       

(setq org-remember-default-headline "Tasks")
(setq org-remember-store-without-prompt t)
(setq org-remember-clock-out-on-exit nil)

(setq org-remember-templates (quote (("todo" ?t "* TODO %?\n   %u\n   %a" nil bottom nil)
                                     ("note" ?n "* %?     :NOTE:\n   %u\n   %a" nil bottom nil))))

;; Refile settings
(setq org-completion-use-ido t)
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)
                            
;; Custom agenda views
(setq org-agenda-custom-commands 
      (quote (("p" "Projects" tags "/PROJECT" ((org-use-tag-inheritance nil)))
              ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Work Tasks" tags-todo "@work|@london" nil)
              ("h" "Home Tasks" tags-todo "@home|@grantham" nil)
              ("f" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("N" "Notes" tags "NOTE" nil))))

(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-include-diary nil)
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
(setq org-agenda-repeating-timestamp-show-all t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-start-on-weekday nil)
(setq org-deadline-warning-days 30)
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-agenda-sorting-strategy
      (quote ((agenda time-up priority-down effort-up category-up)
              (todo priority-down)
              (tags priority-down))))

;; Disable display of the time grid
(setq org-agenda-time-grid
      (quote (nil "----------------"
                  (800 1000 1200 1400 1600 1800 2000))))

;; Clock control
(setq org-clock-persistence-insinuate)
(setq org-clock-history-length 35)
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state "STARTED")
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist t)
(setq org-time-stamp-rounding-minutes (quote (1 5)))

;; Search results
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings nil)

;; Special key bindings for org mode headlines
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-table-export-default-format "orgtbl-to-csv")

;; SLIME mode
(add-to-list 'load-path "~/src/slime/")
(if darwinp 
    (setq inferior-lisp-program "/opt/local/bin/sbcl")
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))
(require 'slime-autoloads)
(slime-setup '(slime-repl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions                                                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (forward-char col)))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; Always end searches at the beginning of the matching expression.
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

;; Many times you'll do a kill-line command with the only intention of
;; getting the contents of the line into the killring. Here's an idea
;; stolen from Slickedit, if you press copy or cut when no region is
;; active you'll copy or cut the current line:

(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
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

;; Most times you'll want to open a fresh line and move to it without
;; breaking the current one even if the point is not at the end of the
;; line. Let's copy how vi opens new lines and use it with C-o and use
;; [return] for doing hard linebreaks:

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

;; Another cool vi feature, pressing % when on a left or right
;; parenthese will jump to the matching parenthese:

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; A somewhat insanely powerful trick, evaluate a region via a shell
;; command and replace the region with the resulting output. Normally
;; you would access this command via C-u M-| but since we're trying to
;; optimize things a bit:

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

(defun delete-enclosed-text ()
  "Delete text between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^(<[“") (setq p1 (point))
      (skip-chars-forward "^)>]”") (setq p2 (point))
      (delete-region p1 p2))))

(defun remove-line-breaks ()
  "Remove line endings in a paragraph."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

;; Many times you'll want to search for the word or expression at the
;; point. Here is a feature stolen from vi:

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
  (let (
        (beg (line-beginning-position))
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

;; Filecache configuration

(defun file-cache-delete-svn ()
  (file-cache-delete-file-regexp ".*\\.svn.*"))

(defun bats-file-cache ()
  (interactive)
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/cpp")
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/python")
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/sql")
  (file-cache-delete-svn))

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
    (split-window-vertically 12)
    (switch-to-buffer "*compilation*")
    (other-window 1)
    )
  )

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-,"          	'pop-tag-mark)
(global-set-key "\C-z"          	'advertised-undo)
(global-set-key "\C-l"          	'my-recenter)
(global-set-key "\C-o"          	'vi-open-next-line)
(global-set-key "%"             	'match-paren)
(global-set-key [(meta ?!)]     	'custom-shell-command-on-region)
(global-set-key "\M-s"          	'isearch-forward-current-word-keep-offset)

(global-set-key "\C-ca"         	'org-agenda)
(global-set-key "\C-c\C-a"         	'align)
(global-set-key "\C-cb"         	'org-iswitchb)
(global-set-key "\C-c\C-a"         	'align-regexp)
(global-set-key "\C-cc"         	'my-compile)
(global-set-key "\C-cd"         	'dot-emacs)
(global-set-key "\C-ce"         	'eval-region)
(global-set-key "\C-cf"         	'file-cache-ido-find-file)
(global-set-key "\C-ch"         	'list-matching-lines)
(global-set-key "\C-ci"         	'magit-status)
(global-set-key "\C-x\C-l"		'session-jump-to-last-change)
(global-set-key "\C-cm"         	'manual-entry)
(global-set-key "\C-co"         	'ff-find-other-file)
(global-set-key [(control tab)]         'ff-find-other-file)
(global-set-key "\C-cr"         	'load-emacs)
(global-set-key "\C-c\C-r"         	'revert-buffer)
(global-set-key (kbd "<f11>")           'org-clock-goto)
(global-set-key (kbd "C-<f11>")         'org-clock-in)
(global-set-key (kbd "C-M-r")           'org-remember)
(global-set-key (kbd "<f7>")            'org-narrow-to-subtree)
(global-set-key (kbd "<S-f7>")          'widen)
(global-set-key [(f12)]         	'org-agenda)
(global-set-key "\C-cl"         	'org-store-link)
(global-set-key "\C-cs"         	'svn-status)
(global-set-key "\C-ct"         	'org-todo)
(global-set-key "\C-cw"         	'swap-windows)

(global-set-key [(control s)]   	'isearch-forward-regexp)
(global-set-key [(control r)]   	'isearch-backward-regexp)

(global-set-key [(control ? )]          'hippie-expand)
(global-set-key [(control ?')]          'set-mark-command)
(global-set-key [(control return)]      'set-mark-command)
(global-set-key [(control right)]       'forward-word)
(global-set-key [(control left)]        'backward-word)

(global-set-key [(meta up)]     	'move-line-up)
(global-set-key [(meta down)]   	'move-line-down)
(global-set-key [(control meta down)]   'duplicate-line-down)
(global-set-key [(control meta up)]     'duplicate-line-up)
(global-set-key [(control meta return)] 'eol-insert-semicolon)
(global-set-key [(meta left)]   	'winring-prev-configuration)
(global-set-key [(meta right)]  	'winring-next-configuration)

(require 'breadcrumb)
(global-set-key [(f1)]          	'bc-list)
(global-set-key [(control f2)]          'bc-set)
(global-set-key [(f2)]                  'bc-previous)
(global-set-key [(shift f2)]            'bc-next)
(global-set-key [(f3)]          	'bc-local-previous)
(global-set-key [(shift f3)]            'bc-local-next)

(global-set-key (kbd "<f5>")            'visit-ansi-term)
(global-set-key [(f6)] 			'djcb-erc-start-or-switch)
(global-set-key (kbd "<M-prior>") 	'previous-error) 
(global-set-key (kbd "<M-next>")  	'next-error)

;;; WINDOW SPLITING
(global-set-key (kbd "M-5") 		'query-replace)
(global-set-key (kbd "M-3") 		'split-window-horizontally)
(global-set-key (kbd "M-2") 		'split-window-vertically)
(global-set-key (kbd "M-1") 		'delete-other-windows)
(global-set-key (kbd "M-0") 		'delete-window)
(global-set-key (kbd "M-o") 		'other-window)

;; Anything config

(require 'anything-config)
(setq fit-frame-inhibit-fitting-flag t)
(setq anything-sources
       (list anything-c-source-buffers
             anything-c-source-buffer-not-found
             anything-c-source-file-name-history
             anything-c-source-files-in-current-dir
             anything-c-source-file-cache
             anything-c-source-bookmarks
             anything-c-source-occur
             anything-c-source-info-pages
             anything-c-source-man-pages
             anything-c-source-calculation-result
             anything-c-source-google-suggest
             anything-c-source-locate
             anything-c-source-emacs-commands))

(global-set-key (kbd "C-;") 'anything)

;; Auto-complete mode
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-yasnippet)
(require 'auto-complete-python)
(require 'auto-complete-etags)
(require 'ac-dabbrev)
(setq ac-auto-start nil)
(global-set-key "\M-/" 'ac-start)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-dwim t)

(custom-set-variables
 '(ac-sources
   '(ac-source-yasnippet
     ac-source-words-in-buffer
     ac-source-dabbrev)))

