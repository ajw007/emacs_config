(visit-tags-table "~/cpp/TAGS")

(defun bats-file-cache ()
  (interactive)
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/cpp")
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/python")
  (file-cache-add-directory-using-find "/opt/ecn/users/mburrows/source/ecn/source/sql")
  (file-cache-delete-svn))

(setq jabber-account-list '(("mburrows@batsutil")
                            ("maburrow@googlemail.com"
                              (:network-server . "talk.google.com")
                              (:port . 443)
                              (:connection-type . ssl))
                            ))

(jabber-connect-all)
