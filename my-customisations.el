(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(c-basic-offset 4)
 '(c-default-style (quote ((c++-mode . "k&r") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(delete-selection-mode nil nil (delsel))
 '(display-time-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line)))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(size-indication-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(truncate-lines t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My customisations                                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst darwinp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;; Add local elisp dir to the load path
(add-to-list 'load-path "~/elisp")

;; Start emacs as a server, push files to it using 'emacsclient --no-wait'
(server-start)

;; Display the time
(display-time)

;; Turn on midnight mode to clean buffers every evening
(require 'midnight)

;; Support for Subversion version control
(require 'vc-svn)
(require 'psvn)

;; Perty colours
(require 'color-theme)
(color-theme-initialize)
(load-file "~/elisp/themes/color-theme-wombat.el")
(color-theme-wombat)

;; Put backup files in one directory rather than spreading them all
;; over the file system
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs-backup"))))

;; Tabs are evil, use spaces always
(setq-default indent-tabs-mode nil)

;; Classic emacs selection
(transient-mark-mode 1)

;; CEDET
(load-file "~/elisp/cedet/common/cedet.el")
(global-ede-mode t)
(semantic-load-enable-excessive-code-helpers)
(require 'semantic-ia)
(require 'semantic-gcc)

(semantic-add-system-include "/opt/boost/include/boost-1_33_1" 'c++-mode)
(semantic-add-system-include "/opt/ecn/users/mburrows/source/ecn/source/python" 'python-mode)

(defun my-cedet-hook ()
  ;;(local-set-key "." 		'semantic-complete-self-insert)
  ;;(local-set-key ">" 		'semantic-complete-self-insert)
  (local-set-key [(shift control down)] 'senator-next-tag)
  (local-set-key [(shift control up)]   'senator-previous-tag)
  (local-set-key (kbd "C-c C-j")     	'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-b")     	'semantic-mrub-switch-tags)
  (local-set-key (kbd "C-c C-r")     	'semantic-symref)
  (local-set-key (kbd "C-c C-/")     	'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c C-.")	'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c C-p") 	'semantic-analyze-proto-impl-toggle))

(add-hook 'c-mode-common-hook 'my-cedet-hook)

(ede-cpp-root-project "BATS" 
                      :name "BATS"
                      :file "~/ecn-git/source/cpp/Makefile"
                      :include-path '("/"
                                      "/libcryptopp"
                                      "/libecnarcabook"
                                      "/libecnarcamcast"
                                      "/libecncams"
                                      "/libecncamsbook"
                                      "/libecndbut"
                                      "/libecndrop"
                                      "/libecnfast"
                                      "/libecnfastref"
                                      "/libecnfixbook"
                                      "/libecnisebook"
                                      "/libecnkrb5"
                                      "/libecnlinesvr"
                                      "/libecnme"
                                      "/libecnmktdata"
                                      "/libecnnasdaq"
                                      "/libecnnsxbook"
                                      "/libecnpg"
                                      "/libecnpouch"
                                      "/libecnpqxx"
                                      "/libecnqix"
                                      "/libecnqixbook"
                                      "/libecnqs"
                                      "/libecnqtsvr"
                                      "/libecnreplay"
                                      "/libecnroute"
                                      "/libecnrtc"
                                      "/libecnrules"
                                      "/libecnshmlog"
                                      "/libecnsip"
                                      "/libecnsipbook"
                                      "/libecnsiphdr"
                                      "/libecnsiprdr"
                                      "/libecnsmarts"
                                      "/libecnsniffer"
                                      "/libecnsoup"
                                      "/libecnsvr"
                                      "/libecntbc"
                                      "/libecntbm"
                                      "/libecntraderpt"
                                      "/libecnut"
                                      "/libecnweb"
                                      "/libecnwire"
                                      "/libexpat"
                                      "/libmtfanalysis"
                                      "/libmtfmbbogen"
                                      "/libmtfqs"
                                      "/libmtfsymbology"
                                      "/libmtftraderpt"
                                      "/libtbarcabook"
                                      "/libtbfix"
                                      "/libtbfixflow"
                                      "/libtbhttpsvr"
                                      "/libtbipc"
                                      "/libtbsvr"
                                      "/libtbtkr"
                                      "/libtbut"
                                      "/libzlib"
                                      "/mtf_analysis"
                                      "/mtf_competition"
                                      "/mtf_mbbogen"
                                      "/mtf_qs"
                                      "/mtf_qs_stats"
                                      "/mtf_trf"))

(ede-cpp-root-project "Python" 
                      :name "Python"
                      :file "~/ecn-git/source/python/setup_mtf.py"
                      :include-path '("/"))

;; Turn on IDO mode with filecache
(require 'filecache)
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must

;; Load TRAMP for SSH access
(require 'tramp)
(setq tramp-default-method "ssh")

;; Get rid of annoying yes/no prompts (replace them with y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; Makes #! scripts executable after saving
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Eliminate multiple buffers when browsing a directory (very
;; annoying)
(require 'dired-single)

(require 'magit)

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

;; Turn off the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

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

;; Load jabber client
(add-to-list 'load-path "~/elisp/emacs-jabber-0.7.1")
(require 'jabber)

;; Make buffer list perty
(defalias 'list-buffers 'ibuffer)

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

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

(add-hook 'python-mode-hook 'my-cedet-hook)

;; CC Mode
(defun my-c-mode-common-hook ()
  (setq c-basic-offset 4)
  ;; make sure that return does an auto indent
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  ;; make sure access labels dont have an indent
  (c-set-offset 'access-label '-)
  ;; dont indent after a namespace 
  (c-set-offset 'innamespace '-)
  )

(c-set-offset 'innamespace '-)
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

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

(global-set-key "\C-ca"         	'align)
(global-set-key "\C-cc"         	'compile)
(global-set-key "\C-cd"         	'dot-emacs)
(global-set-key "\C-ce"         	'eval-region)
(global-set-key "\C-cf"         	'file-cache-ido-find-file)
(global-set-key "\C-ch"         	'list-matching-lines)
(global-set-key "\C-cm"         	'manual-entry)
(global-set-key "\C-co"         	'ff-find-other-file)
(global-set-key [(control tab)]         'ff-find-other-file)
(global-set-key "\C-cr"         	'load-emacs)
(global-set-key "\C-cs"         	'shell)
(global-set-key "\C-cw"         	'swap-windows)

(global-set-key [(control s)]   	'isearch-forward-regexp)
(global-set-key [(control r)]   	'isearch-backward-regexp)

;; (setq skeleton-pair t)
;; (global-set-key (kbd "[")       	'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "(")       	'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{")       	'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"")      	'skeleton-pair-insert-maybe)

(global-set-key [(control ? )]          'hippie-expand)
(global-set-key [(control return)]      'set-mark-command)
(global-set-key [(control right)]       'forward-word)
(global-set-key [(control left)]        'backward-word)

(global-set-key [(meta up)]     	'move-line-up)
(global-set-key [(meta down)]   	'move-line-down)
(global-set-key [(control meta down)]   'duplicate-line-down)
(global-set-key [(control meta up)]     'duplicate-line-up)
(global-set-key [(shift control down)]  'end-of-defun)
(global-set-key [(shift control up)]    'beginning-of-defun)
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

;;; WINDOW SPLITING
(global-set-key (kbd "M-5") 		'query-replace)
(global-set-key (kbd "M-3") 		'split-window-horizontally)
(global-set-key (kbd "M-2") 		'split-window-vertically)
(global-set-key (kbd "M-1") 		'delete-other-windows)
(global-set-key (kbd "M-0") 		'delete-window)
(global-set-key (kbd "M-o") 		'other-window)

(require 'anything-config)
;;(setq fit-frame-inhibit-fitting-flag t)

;; My anything sources

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
             ;;anything-c-source-google-suggest
             anything-c-source-locate
             anything-c-source-emacs-commands))

(global-set-key (kbd "C-;") 		'anything)
(global-set-key "\C-xb" 		'anything)
