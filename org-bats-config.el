;-----------------------------------------------------------
; BATS specific org-mode settings
;-----------------------------------------------------------
(require 'org-config)

; Agenda files
(setq org-agenda-files (list "~/Notes/TODO.org"))

(setq org-link-abbrev-alist '(("bug" . "http://bugzilla/show_bug.cgi?id=")))

(defun org-meeting-timeline ()
  (interactive)
  (org-timeline)
  (universal-argument)
  (universal-argument)
  (org-agenda-log-mode)
  (end-of-buffer))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c M-m") 'org-meeting-timeline)))

(provide 'org-bats-config)
