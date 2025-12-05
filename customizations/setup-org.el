(use-package org
  :hook ((org-mode . filip/org-mode-hook))
  :init
  ;; Startup behavior
  (setq org-startup-truncated nil
        org-confirm-babel-evaluate nil
        org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "IN-REVIEW" "|" "DONE"))
        org-agenda-files '("~/CloudStation/scrapbook.org"
                           "~/CloudStation/Cryptography_101/zero_knowledge_proofs/"
                           "~/CloudStation/Computing_101/leet/"
                           "~/CloudStation/Cryptography_101/zkSNARKs-notes/"
                           )
        ;; org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
        org-plantuml-jar-path (expand-file-name "/home/filip/Programs/plantuml-nodot.1.2023.7.jar"))

  ;; Custom agenda command
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY (e.g. ?A, ?B, ?C)."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (when (= pri-value pri-current)
        subtree-end)))

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "")
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (air-org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline))))))))))

  :config
  ;; bigger latex previews (somehow doesn't work with :init)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  ;; Keybindings
  (define-key org-mode-map (kbd "M-.") #'org-open-at-point)
  (define-key org-mode-map (kbd "M-,") #'org-mark-ring-goto)

  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (plantuml . t)
     (emacs-lisp . t)
     (fortran . t)
     (gnuplot . t)
     (latex . t)
     (python . t)
     (jupyter . t)
     (maxima . t)
     (octave . t)
     (org . t)
     (R . t)
     (shell . t)
     (dot . t))))

;; org-tree-slide custom keybindings
(use-package org-tree-slide
  :ensure t
  :after org
  :config
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

(defun filip/org-mode-hook ()
  ;; Show the simple agenda on Emacs start
  (add-hook 'emacs-startup-hook
            (lambda ()
              (org-agenda nil "c")))
  ;; Enable TOC generation on save if org-make-toc is available
  (when (require 'org-make-toc nil 'noerror)
    (add-hook 'before-save-hook #'org-make-toc-mode nil 'local)))

;; Optional tools
(use-package anki-editor
  :ensure t)

(use-package org-roam
  :ensure t)

;; Automatically inserts tables of contents
(use-package org-make-toc
  :ensure t)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  ;; M-x completion-at-point to complete the names of nodes inside of double-square brackets [[]]
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))
