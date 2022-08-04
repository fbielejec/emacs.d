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
     (define-key org-mode-map (kbd "<M-right>") nil)))
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

;; org files to build the agenda from
(setq org-agenda-files '("~/Dropbox/"
                         "~/Dropbox/district0x/"))


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
      (expand-file-name "/home/filip/.m2/repository/net/sourceforge/plantuml/plantuml/6703/plantuml.1.2020.3.jar"))
