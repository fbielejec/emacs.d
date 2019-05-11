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

