;;; 
;;; Org Mode
;;;
;;; Most of this customisation comes from the following great article:
;;;    http://doc.norang.ca/org-mode.html
;;;
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(load-file "~/elisp/org-mode/lisp/org-install.el") ; use latest dev version
;;(require 'org-install)
(require 'org-checklist)

(defun bh/weekday-p ()
  (let ((wday (nth 6 (decode-time))))
    (and (< wday 6) (> wday 0))))

(defun bh/working-p ()
  (let ((hour (nth 2 (decode-time))))
    (and (bh/weekday-p) (and (>= hour 8) (<= hour 18)))))

(defun bh/network-p ()
  (= 0 (call-process "/bin/ping" nil nil nil
                     "-c1" "-q" "-t1" "www.google.com")))

(defun bh/org-auto-exclude-function (tag)
  (and (cond
       ((string= tag "@home")
        (bh/working-p))
       ((string= tag "@work")
        (not (bh/working-p))))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;; Key bindings
(global-set-key "\C-cl" 	 'org-store-link)
(global-set-key "\C-cb" 	 'org-iswitchb)
(global-set-key "\C-ct"          'org-todo)
(global-set-key (kbd "<f9> b")   'bbdb)
(global-set-key (kbd "<f9> c")   'calendar)
(global-set-key (kbd "<f9> g")   'gnus)
(global-set-key (kbd "<f9> m")   'bh/clock-in-read-mail-and-news-task)
(global-set-key (kbd "<f9> o")   'bh/clock-in-organization-task)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-interrupted-task)
(global-set-key (kbd "<f9> n")   'org-narrow-to-subtree)
(global-set-key (kbd "<f9> w")   'widen)
(global-set-key (kbd "<f9> I")   'org-clock-in)
(global-set-key (kbd "<f9> O")   'org-clock-out)
(global-set-key (kbd "<f10>")    'org-cycle-agenda-files)
(global-set-key [(f11)]	         'org-clock-goto)
(global-set-key (kbd "C-<f11>")  'org-clock-in)
(global-set-key (kbd "M-<f11>")  'org-resolve-clocks)
(global-set-key [(f12)]          'org-agenda)

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode to spell check everywhere
            (flyspell-mode 1)))

(setq org-agenda-files '("~/Dropbox/org/refile.org"
                         "~/Dropbox/org/work.org"
                         "~/Dropbox/org/home.org"))

;; ToDo keywords
(setq org-use-fast-todo-selection t)

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!)")
                          (sequence "WAITING(f@/!)" "SOMEDAY(S!)" "PROJECT(p@)")))

(setq org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                               ("STARTED" :foreground "light yellow" :weight bold)
                               ("DONE" :foreground "forest green" :weight bold)
                               ("WAITING" :foreground "orange" :weight bold)
                               ("SOMEDAY" :foreground "magenta" :weight bold)
                               ("PROJECT" :foreground "pink" :weight bold)))

;; ToDo triggers
(setq org-todo-state-tags-triggers '(("WAITING" ("WAITING" . t) ("NEXT"))
                                     ("SOMEDAY" ("WAITING" . t))
                                     (done ("NEXT") ("WAITING"))
                                     ("TODO" ("WAITING"))
                                     ("STARTED" ("WAITING"))
                                     ("PROJECT" ("PROJECT" . t))))

;; Remember mode
(require 'remember)
(org-remember-insinuate)
(setq org-default-notes-file "~/Dropbox/org/refile.org")
(global-set-key (kbd "C-M-r") 'org-remember)

; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

; Keep clocks running
(setq org-remember-clock-out-on-exit nil)

; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

; 2 remember templates for TODO tasks and Notes
(setq org-remember-templates '(("todo" ?t "* TODO %?
  %u
  %a" nil bottom nil)
                               ("note" ?n "* %?                                        :NOTE:
  %u
  %a" nil bottom nil)
                               ("someday" ?s "* SOMEDAY %?                             :WAITING:
  %u
  %a" nil bottom nil)))

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path 'file)

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Custom agenda views
(setq org-agenda-custom-commands 
      '(("p" "Projects" tags "/PROJECT" ((org-use-tag-inheritance nil)))
        ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
        ("n" "Next Actions" tags "NEXT" ((org-agenda-todo-ignore-with-date nil)))
        ("w" "Work Tasks" tags-todo "@work-WAITING|@london" nil)
        ("h" "Home Tasks" tags-todo "@home-WAITING|@grantham|@online" nil)
        ("o" "Online Tasks" tags-todo "@online" nil)
        ("f" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
        ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
        ("A" "Tasks to be Archived" todo "DONE" nil)
        ("N" "Notes" tags "NOTE" nil)))

; Resume clocking tasks when emacs is restarted
(setq org-clock-persistence-insinuate)
; Yes it's long... but more is better ;)
(setq org-clock-history-length 35)
; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK" "CLOCK")))
;; Save clock data in the CLOCK drawer and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer "CLOCK")
; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Disable auto clock resolution, use M-F11 instead
(setq org-clock-auto-clock-resolution nil)

; Round to 5 minutes
(setq org-time-stamp-rounding-minutes '(1 5))
; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
; Agenda log mode items to display (clock time only by default)
(setq org-agenda-log-mode-items '(clock))
; Agenda clock report parameters (no links, 2 levels deep)
(setq org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 2))

;; Tags with fast selection keys
(setq org-tag-alist '((:startgroup)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      ("@online" . ?o)
                      ("@grantham" . ?g)
                      ("@london" . ?l)
                      (:endgroup)
                      ("NEXT" . ?n)
                      ("WAITING" . ?f)
                      ("PROJECT" . ?p)))

; Allow setting single tags with a single key
(setq org-fast-tag-selection-single-key t)

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

; Define a stuck project as one that has no next action
(setq org-stuck-projects '("/PROJECT" nil ("NEXT") ""))

; Erase all reminders and rebuilt reminders for today from the agenda
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(my-org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'my-org-agenda-to-appt)

; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

; Keep tasks with dates off the global todo lists
;(setq org-agenda-todo-ignore-with-date t)

; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

; We don't use the diary
(setq org-agenda-include-diary nil)

; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files '(agenda-archives))

; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down effort-up category-up)
        (todo priority-down)
        (tags priority-down)))

; Start the weekly agenda on a Monday
(setq org-agenda-start-on-weekday 1)

; Disable display of the time grid
(setq org-agenda-time-grid '(nil "----------------"
                                 (800 1000 1200 1400 1600 1800 2000)))

; Enable task and checklist blocking
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t)

; Hide leading stars
(setq org-hide-leading-stars t)

;; Never leave empty lines in collapsed view
(setq org-cycle-separator-lines 0)

; Insert new headings after the current subtree
(setq org-insert-heading-respect-content t)

; Notes up top
(setq org-reverse-note-order nil)

; Search result formatting
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings nil)

; Special key handling
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

; Deadline warning
(setq org-deadline-warning-days 10)

; Export settings
(setq org-table-export-default-format "orgtbl-to-csv")

;; Logging
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Make moving between links in an org-mode buffer easier
(add-hook 'org-load-hook
            (lambda ()
              (define-key 'org-mode-map "\C-n" 'org-next-link)
              (define-key 'org-mode-map "\C-p" 'org-previous-link)))

;; Setup some link abbrevs for easier typing
(setq org-link-abbrev-alist
      '(("bug" . "http://bugzilla/show_bug.cgi?id=")
        ("goo"   . "http://www.google.com/search?q=")))
