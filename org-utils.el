(require 'org-install)

(defun org-table-get-lines ()
  (let* ((beg (org-table-begin))
	 (end (org-table-end))
	 (txt (buffer-substring-no-properties beg end))
         (lines (org-split-string txt "[ \t]*\n[ \t]*"))
         (lines (org-table-clean-before-export lines)))
    (mapcar
     (lambda (x)
       (if (string-match org-table-hline-regexp x)
           'hline
         (org-split-string (org-trim x) "\\s-*|\\s-*")))
     lines)))

(defun org-table-inplace-to-tsv ()
  (interactive)
  (org-table-do-inplace-convert 'orgtbl-to-tsv))

(defun org-table-inplace-to-csv ()
  (interactive)
  (org-table-do-inplace-convert 'orgtbl-to-csv))

(defun org-table-do-inplace-convert (transform)
  (unless (org-at-table-p)
    (error "No table at point"))
  (org-table-align) ;; make sure we have everything we need
  (let* ((beg (org-table-begin))
         (end (org-table-end))
         (table (org-table-get-lines))
         (converted (funcall transform table nil)))
    (delete-region beg end)
    (insert converted)
    (insert "\n")))

(defun org-today-in-internal-format ()
  (let ((now (decode-time)))
    (encode-time 0 0 0 (nth 3 now) (nth 4 now) (nth 5 now))))

(provide 'org-utils)    
