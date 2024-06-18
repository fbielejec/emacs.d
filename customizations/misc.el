;;;;
;; misc functions
;;;;

(defun touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))

(defun yank-github-link ()
  "Quickly share a github link of what you are seeing in a buffer. Yanks
a link you can paste in the browser."
  (interactive)
  (let* ((remote (or (magit-get-push-remote) "origin"))
         (url (magit-get "remote" remote "url"))
         (project (if (string-prefix-p "git" url)
                      (substring  url 15 -4)   ;; git link
                    (substring  url 19 -4))) ;; https link
         (link (format "https://github.com/%s/blob/%s/%s#L%d"
                       project
                       (magit-get-current-branch)
                       (magit-current-file)
                       (count-lines 1 (point)))))
    (kill-new link)))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; turn off company for magit (it remaps Tab)

;; (setq company-global-modes '(not magit-mode))
;; (setq company-global-modes '(not org-mode))

(add-hook 'org-mode-hook (lambda() (company-mode 0)))
(add-hook 'magit-mode-hook (lambda() (company-mode 0)))
