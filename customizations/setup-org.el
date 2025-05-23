;;;;
;; orgmode
;;;;

;; Start-up with line wrapping (toggle-truncate-lines)
(setq org-startup-truncated nil)

;; override org mode keybindings
(eval-after-load "org"
  '(progn
     ;;(define-key org-mode-map (kbd "<M-S-left>") nil)
     ;;(define-key org-mode-map (kbd "<M-S-right>") nil)
     (define-key org-mode-map (kbd "<M-left>") nil)
     (define-key org-mode-map (kbd "<M-right>") nil)
     ))

;; orgmode gets slow
;; (setq undo-limit 50)

(require 'ob)

 ;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; cycle between these states
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "IN-REVIEW" "|" "DONE")))

;; ;; org files to build the agenda from
(setq org-agenda-files '("~/CloudStation/"
                         "~/CloudStation/scrapbook.org"))

;; custom agenda view
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
   PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

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

;; <dependency>
;;     <groupId>net.sourceforge.plantuml</groupId>
;;     <artifactId>plantuml</artifactId>
;;     <version>1.2020.3</version>
;; </dependency>
;; sudo apt-get install graphviz

(setq org-plantuml-jar-path
      (expand-file-name "/home/filip/Programs/plantuml-nodot.1.2023.7.jar"))

;; python/jupyter/maxima code blocks
;; (setq ob-ipython-command "/home/filip/.local/bin/jupyter")

;; org-mode presentations
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

(org-babel-do-load-languages 'org-babel-load-languages
  '((C . t)
    (plantuml . t)
    (emacs-lisp . t)
    (fortran . t)
    (gnuplot . t)
    ;; (ipython . t)
    (latex . t)
    (python . t)
    (maxima . t)
    (octave . t)
    (org . t)
    (R . t)
    (shell . t)
    (maxima . t)
    ;; graphviz
    (dot . t)
))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook 'filip/org-mode-hook)
(defun filip/org-mode-hook ()
  ;; needs org-make-toc package
  (add-hook 'before-save-hook 'org-make-toc-mode))

;; bigger latex previews
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
