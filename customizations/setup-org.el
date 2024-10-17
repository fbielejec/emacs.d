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
                         "~/CloudStation/chatka/"
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

;; org-mode presentations
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

;; sage
(org-babel-do-load-languages 'org-babel-load-languages
  '((C . t)
    (emacs-lisp . t)
    (fortran . t)
    (gnuplot . t)
    (ipython . t)
    (latex . t)
    (ledger . t)
    (python . t)
    (maxima . t)
    (octave . t)
    (org . t)
    (R . t)
    (shell . t)
))

;; (setq sage-shell:sage-executable "/usr/bin/sage")

;; (sage-shell:define-alias)
;; ;; Turn on eldoc-mode
;; (add-hook 'sage-shell-mode-hook #'eldoc-mode)
;; (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

;; (setq sage-shell:use-prompt-toolkit t)
;; (setq sage-shell:completion-function 'pcomplete)

;; (require 'ob-sagemath)
;; ;; Ob-sagemath supports only evaluating with a session.
;; (setq org-babel-default-header-args:sage '((:session . t)
;;                                            (:results . "output")))

;; ;; C-c s for asynchronous evaluating (only for SageMath code blocks).
;; (with-eval-after-load "org"
;;   (define-key org-mode-map (kbd "C-c s") 'ob-sagemath-execute-async))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; ;; Do not evaluate code blocks when exporting.
;; (setq org-export-babel-evaluate nil)

;; ;; Show images when opening a file.
;; (setq org-startup-with-inline-images t)

;; ;; Show images after evaluating code blocks.
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; (eval-after-load "sage-shell-mode"
;;   '(sage-shell:define-keys sage-shell-mode-map
;;      "C-c C-i"  'helm-sage-complete
;;      "C-c C-h"  'helm-sage-describe-object-at-point
;;      "M-r"      'helm-sage-command-history
;;      "C-c o"    'helm-sage-output-history))

;; (setq sage-shell:input-history-cache-file "~/.emacs.d/.sage_shell_input_history")

;; (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view-mode)
