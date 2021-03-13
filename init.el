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

;; Language-specific
(load "setup-org.el")
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-java.el")
(load "setup-solidity.el")
(load "setup-rust.el")
(load "setup-ess.el")
(load "setup-python.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("6b2c6e5bc1e89cf7d927d17f436626eac98a04fdab89e080f4e193f6d291c93d" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" default)))
 '(package-selected-packages
   (quote
    (ron-mode helm-ag gruvbox-theme protobuf-mode helm-lsp lsp-treemacs which-key elpy ein kotlin-mode flycheck-rust toml-mode clj-refactor ess graphql-mode racer buffer-move helm-projectile yaml-mode magit cider dap-mode lsp-ui company-lsp yasnippet treemacs use-package lsp-java zenburn-theme tagedit smex rjsx-mode rainbow-delimiters projectile paredit multiple-cursors ido-completing-read+ exec-path-from-shell dockerfile-mode clojure-mode-extra-font-locking)))
 '(safe-local-variable-values
   (quote
    ((cider-path-translations
      ("/root" . "/home/filip")
      ("/usr/src/app" . "/home/filip/Dropbox/MobileProjects/clash-backend"))
     (cider-ns-refresh-before-fn . "com.stuartsierra.component.repl/stop")
     (cider-ns-refresh-after-fn . "com.stuartsierra.component.repl/start")
     (cider-known-endpoints
      ("localhost" "9656"))
     (eval define-clojure-indent
           (animation/interpolate 0)
           (animation/start 0)
           (animation/parallel 0))
     (eval define-clojure-indent
           (animation/start 0)
           (animation/parallel 0))
     (cider-shadow-cljs-default-options . "app")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
