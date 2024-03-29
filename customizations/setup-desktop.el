;; Use the desktop library to save the state of Emacs from one session to another
;; M+x desktop-save
;; M+x desktop-read

;; Automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30)

;; (desktop-save-mode 1)

;; ;; remove desktop after it's been read
;; (add-hook 'desktop-after-read-hook
;;           '(lambda ()
;;              ;; desktop-remove clears desktop-dirname
;;              (setq desktop-dirname-tmp desktop-dirname)
;;              (desktop-remove)
;;              (setq desktop-dirname desktop-dirname-tmp)))

;; (defun saved-session ()
;;   (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; ;; ;; use session-restore to restore the desktop manually
;; (defun session-restore ()
;;   "Restore a saved emacs session."
;;   (interactive)
;;   (if (saved-session)
;;       (desktop-read)
;;     (message "No desktop found.")))

;; ;; ;; use session-save to save the desktop manually
;; (defun session-save ()
;;   "Save an emacs session."
;;   ;; (interactive)
;;   (if (saved-session)
;;       (if (y-or-n-p "Overwrite existing desktop? ")
;;           (desktop-save-in-desktop-dir)
;;         (message "Session not saved."))
;;     (desktop-save-in-desktop-dir)))

;; ;; ;; ask user whether to restore desktop at start-up
;; (add-hook 'after-init-hook
;;           '(lambda ()
;;              (if (saved-session)
;;                  (if (y-or-n-p "Restore desktop? ")
;;                      (session-restore)))))
