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
;;(setq undo-limit 50)

;; configure org-mode for Clojure code blocks
(require 'ob)
(require 'ob-clojure)
;;(require 'org-babel-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)))

;; cycle between these states
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "IN-REVIEW" "|" "DONE")))

;; ;; org files to build the agenda from
(setq org-agenda-files '("~/CloudStation/"
                         "~/CloudStation/scrapbook.org"
                         "~/CloudStation/aleph/"))

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

;; integrate plantuml [diagrams] support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

;; <dependency>
;;     <groupId>net.sourceforge.plantuml</groupId>
;;     <artifactId>plantuml</artifactId>
;;     <version>1.2020.3</version>
;; </dependency>
;; sudo apt-get install graphviz

(setq org-plantuml-jar-path
      (expand-file-name "/home/filip/Programs/plantuml-nodot.1.2023.7.jar"))

;; setup Maxima Source Code Blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((maxima . t))) ; this line activates maxima

;; python/jupyter/maxima code blocks
(setq ob-ipython-command "/home/filip/.local/bin/jupyter")

;; org-mode presentations
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))


(org-babel-do-load-languages 'org-babel-load-languages
  '((C . t)
    (emacs-lisp . t)
    (fortran . t)
    (gnuplot . t)
    (ipython . t)
    (latex . t)
    (python . t)
    (maxima . t)
    (octave . t)
    (org . t)
    (R . t)
    (shell . t)
))

;; increse the scale of latex previews
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; TODO make shift + arrow keybinding work in orgmode

;; (setq org-support-shift-select t
;;       org-replace-disputed-keys t)
;; (add-hook 'org-shiftup-final-hook 'shrink-window)
;; (add-hook 'org-shiftleft-final-hook 'enlarge-window-horizontally)
;; (add-hook 'org-shiftdown-final-hook 'enlarge-window)
;; (add-hook 'org-shiftright-final-hook 'shrink-window-horizontally)

;; (setq org-replace-disputed-keys t)
;; (define-key org-mode-map (kbd "S-<down>") 'enlarge-window)
;; (define-key org-mode-map (kbd "S-<up>") 'shrink-window)
;; (define-key org-mode-map (kbd "S-<left>") 'enlarge-window-horizontally)
;; (define-key org-mode-map (kbd "S-<right>") 'shrink-window-horizontally)
