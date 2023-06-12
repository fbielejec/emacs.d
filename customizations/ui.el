;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Fantasque Sans Mono font (install from the repos)
;; (set-frame-font (font-spec :name "Fantasque Sans Mono-10" :width 'normal :height 100))
(set-frame-font (font-spec :name "Fira Mono Medium-14" :width 'normal :height 100))

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; disable toolbar
(tool-bar-mode -1)

;; Show line numbers
;;(global-linum-mode)
(global-display-line-numbers-mode)
;; line numbers color
;; (set-face-foreground 'linum "#656555")

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (add-to-list 'load-path "~/.emacs.d/themes")
;;(load-theme 'tomorrow-night-bright t)
;; (load-theme 'zenburn t)
(load-theme 'gruvbox t)

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; tree view:  https://www.emacswiki.org/emacs/NeoTree
;; (add-to-list 'load-path "/home/filip/Dropbox/ClojureProjects/neotree")
;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)

;; for window resizing (press with shift)
(global-set-key (kbd "S-<down>") 'enlarge-window)
(global-set-key (kbd "S-<up>") 'shrink-window)
(global-set-key (kbd "S-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<right>") 'shrink-window-horizontally)

;; switch focus to minibuffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'
