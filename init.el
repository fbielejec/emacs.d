;;;;
;; Startup
;;;;

;; Avoid garbage collection at statup
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 300000000 ; 300mb
          gc-cons-percentage 0.1)))

;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

(setq package-archives
      '(
        ("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        )
      ;; higher number is preffered
      package-archive-priorities
      '(
        ("GNU ELPA"     . 5)
        ("MELPA Stable" . 10)
        ("MELPA"        . 0)
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

    ;; refactoring support for Clojure projects
    ;; https://github.com/clojure-emacs/clj-refactor.el
    clj-refactor

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

    ;; declare and configure packages macro
    use-package

    ;; PATH vars
    exec-path-from-shell

    ;; best theme ever
    ;; zenburn-theme
    gruvbox-theme

    ;; see customizations/editing.el
    multiple-cursors

    ;; editing dockerfiles
    dockerfile-mode

    ;; editing yaml
    yaml-mode

    ;; JSX integration
    rjsx-mode

    ;; Java in emacs
    lsp-java
    idle-highlight-mode
    ;; lsp-java-treemacs

    ;; Solidity editing
    solidity-mode

    ;; syntax checking extension
    ;; flycheck

    ;; better searching w/ helm-projectile-grep
    helm-projectile

    ;; buf-move-(left/right)
    buffer-move

    ;; for rust editing
    racer
    company
    rust-mode
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;
;; Package specific config
;;;;

;; from use-package documentation
(eval-when-compile
  (require 'use-package))

;; make sure flycheck comes from Melpa (for lsp-java)
(when (>= emacs-major-version 24)
  ;; syntax checking extension
  (require 'flycheck)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/")
   t))

;;;;
;; Customization
;;;;

;; disable showing hunderds of native comp errprs nasty
(setq warning-minimum-level :error)

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

(load "printing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; For saving and restoring buffers
(load "setup-desktop.el")

;; Language-specific
(load "setup-org.el")
(load "setup-clojure.el")
;;(load "setup-js.el")
(load "setup-java.el")
;;(load "setup-solidity.el")
(load "setup-rust.el")
(load "setup-ess.el")
(load "setup-python.el")
;;(load "setup-wat.el")
(load "setup-chatgpt.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   '(chatgpt epc ctable concurrent quelpa-use-package quelpa typescript-mode graphql-mode lsp-mode clojure-mode flycheck company-lsp zenburn-theme yaml-mode which-key use-package toml-mode tagedit solidity-mode smex rustic rjsx-mode rainbow-delimiters racer magit-popup magit lsp-ui lsp-java ido-completing-read+ idle-highlight-mode helm-projectile helm-lsp gruvbox-theme ghub flycheck-rust expand-region exec-path-from-shell ein dockerfile-mode company clojure-mode-extra-font-locking clj-refactor buffer-move)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
