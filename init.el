;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(setq package-archives '(
                         
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ;; ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")

                         ))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous
    ido-completing-read+
    
    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; figwheel-repl
    ;; inf-clojure

    ;; declare and configure packages macro
    use-package

    ;; PATH vars
    exec-path-from-shell

    ;; best theme ever
    zenburn-theme

    ;; see customizations/editing.el
    multiple-cursors

    ;; editing dockerfiles
    dockerfile-mode

    ;; JSX integration
    rjsx-mode

    ;; Java in emacs
    lsp-java

    ;; Solidity editing
    solidity-mode

    ;; syntax checking extension
    flycheck
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
;; (add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Language-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-java.el")
(load "setup-solidity.el")

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default bold shadow italic underline bold bold-italic bold])
;;  '(coffee-tab-width 2)
;;  '(custom-safe-themes
;;    (quote
;;     ("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" default)))
;;  '(fci-rule-color "#2a2a2a")
;;  '(package-selected-packages
;;    (quote
;;     (highlight dockerfile-mode dash treemacs lsp-java cider clojure-mode sesman highlight-parentheses multiple-cursors ess ag yaml-mode use-package tagedit solidity-mode smex skewer-mode rjsx-mode rainbow-delimiters projectile paredit markdown-mode magit json-mode inf-clojure ido-ubiquitous git-commit-mode flycheck exec-path-from-shell clojure-mode-extra-font-locking buffer-move aggressive-indent))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (magit cider flycheck dap-mode lsp-ui company-lsp yasnippet treemacs use-package lsp-java zenburn-theme tagedit smex rjsx-mode rainbow-delimiters projectile paredit multiple-cursors ido-completing-read+ exec-path-from-shell dockerfile-mode clojure-mode-extra-font-locking))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
