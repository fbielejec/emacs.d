;; ------------------------------
;; TYPESCRIPT / JAVASCRIPT SETUP
;; ------------------------------

;; Major modes
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook ((typescript-mode . setup-typescript-mode)
         (typescript-tsx-mode . setup-typescript-mode))
  :config
  (setq typescript-indent-level 2))

;; LSP
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((typescript-mode . lsp)
         (typescript-tsx-mode . lsp)
         ;; (js-mode . lsp)
         ;; (js2-mode . lsp)
         ;; (web-mode . maybe-lsp-for-web-mode)
)
  :init
  (setq lsp-enable-on-type-formatting nil
        lsp-clients-typescript-server "typescript-language-server")
  :config
  (setq lsp-typescript-preferences '(:includeCompletionsForModuleExports t
                                     :includeCompletionsWithInsertText t)
        lsp-typescript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces t
        lsp-typescript-format-place-open-brace-on-new-line-for-functions nil
        lsp-typescript-format-place-open-brace-on-new-line-for-control-blocks nil))

;; LSP UI
;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-header t
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-border (face-foreground 'default)
;;         lsp-ui-sideline-enable t
;;         lsp-ui-sideline-ignore-duplicate t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-imenu-enable t
;;         lsp-ui-imenu-kind-position 'top
;;         lsp-ui-peek-enable t
;;         lsp-ui-peek-list-width 60
;;         lsp-ui-peek-peek-height 25))


;; Company (auto-completion)
(use-package company
  :ensure t
  :defer t
  :init
  ;;(push 'company-lsp company-backends)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-limit 10
        company-show-numbers t
        company-selection-wrap-around t
        )
  )

;; Flycheck
(use-package flycheck
  :ensure t
  :defer t
  :commands flycheck-mode)

(use-package yasnippet
  :ensure t
  :defer t
  :commands yas-minor-mode
  :config (yas-reload-all))

;; Paredit
(use-package paredit
  :ensure t
  :defer t
  :commands enable-paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

;; jest-test-run-at-point	C-c C-c	Runs only the test under cursor
;; jest-test-run-file	C-c C-f	Runs only the current file
;; jest-test-run	C-c C-t	Runs the whole suite
(use-package jest-test-mode
  :ensure t
  :hook (typescript-mode . jest-test-mode)
  :hook (tsx-mode . jest-test-mode)
  ;; :hook (js-mode . jest-test-mode)
  )

;; Prettier for code formatting
(use-package prettier-js
  :ensure t
  :defer t
  :custom
  ;; If you want prettier via npx:
  (setq prettier-pre-warm 't)
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
                           "--single-quote" "true"
                           "--print-width" 80)))


;; ------------------------------
;; OPTIONAL TREE-SITTER SUPPORT
;; ------------------------------
;; Enable if available
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (use-package tsx-ts-mode
    :ensure t
    :defer t
    :mode "\\.tsx\\'"
    :hook ((tsx-ts-mode . my/ts-setup)))

  (use-package typescript-ts-mode
    :ensure t
    :defer t
    :mode "\\.ts\\'"
    :hook ((typescript-ts-mode . my/ts-setup)))

  ;; (use-package js-ts-mode
  ;;   :ensure t
  ;;   :defer t
  ;;   :mode "\\.jsx?\\'"
  ;;   :hook ((js-ts-mode . my/ts-setup)))

  )

;; -----
;; REPL
;; -----

(require 'project)

(defun ts/project-root ()
  "Return the project root using project.el."
  (or (project-root (project-current))
      default-directory))

(defun ts/repl-buffer ()
  (get-buffer "*ts-repl*"))

;; M-x ts/run-repl
;; needs `npm install -g ts-node`
(defun ts/run-repl ()
  "Start or switch to a TypeScript REPL at the project root."
  (interactive)
  (if (ts/repl-buffer)
      (pop-to-buffer "*ts-repl*")
    (let* ((default-directory (ts/project-root)))
      (make-comint "ts-repl" "ts-node" nil "--transpile-only")
      (pop-to-buffer "*ts-repl*"))))

(defun ts/repl-restart ()
  "Kill the ts-repl buffer and start a fresh one."
  (interactive)
  (when (get-buffer "*ts-repl*")
    (kill-buffer "*ts-repl*"))
  (ts/run-repl))

(defun ts/repl-soft-reset ()
  "Clear most globals in the TypeScript REPL (soft reset)."
  (interactive)
  (comint-send-string
   "*ts-repl*"
   "Object.keys(global).forEach(k => { if (!k.startsWith('_')) delete global[k]; });\n"))

;; autocomplete inside REPL (company mode)
;; history cycling with M-p / M-n
(add-hook 'comint-mode-hook #'company-mode)

(setq comint-move-point-for-output t
      comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t)

;; C-c C-r
(defun ts/send-region (start end)
  (interactive "r")
  (comint-send-string "*ts-repl*" (concat (buffer-substring-no-properties start end) "\n")))

;; C-c C-l
(defun ts/send-line ()
  (interactive)
  (ts/send-region (line-beginning-position) (line-end-position)))

;; C-c C-e
(defun ts/send-expression ()
  (interactive)
  (let ((expr (thing-at-point 'sexp t)))
    (comint-send-string "*ts-repl*" (concat expr "\n"))))

;; Load entire file: C-c C-k
(defun ts/load-file ()
  (interactive)
  (when buffer-file-name
    (comint-send-string "*ts-repl*"
                        (format "import('%s')\n" (file-relative-name buffer-file-name (ts/project-root))))))

;; back and forth between code and REPL: C-c C-z
(defun ts/switch-to-repl ()
  (interactive)
  (pop-to-buffer (ts/repl-buffer)))

(local-set-key (kbd "C-c C-z") #'ts/switch-to-repl)

;; minor mode for TS buffers / REPL
(define-minor-mode ts-cider-mode
  "Minor mode for CIDER/SLIME-like TypeScript development."
  :lighter " tsREPL"
  :keymap (let ((map (make-sparse-keymap)))
            ;; CIDER-style bindings
            (define-key map (kbd "C-c C-z") #'ts/run-repl)
            (define-key map (kbd "C-c C-e") #'ts/send-expression)
            (define-key map (kbd "C-c C-r") #'ts/send-region)
            (define-key map (kbd "C-c C-l") #'ts/send-line)
            (define-key map (kbd "C-c C-b") #'ts/send-buffer)
            (define-key map (kbd "C-c C-k") #'ts/load-file)
            map))

(add-hook 'typescript-mode-hook #'ts-cider-mode)
(add-hook 'tsx-mode-hook #'ts-cider-mode)
(add-hook 'js-mode-hook #'ts-cider-mode)

;; ------------
;; MASTER HOOK
;; ------------

(defun setup-typescript-mode ()
  "Setup TypeScript development environment."
  ;; Enable completion and snippets
  (company-mode 1)
  (yas-minor-mode 1)

  ;; Enable syntax checking
  (flycheck-mode 1)

  ;; Enable structured editing
  (enable-paredit-mode)

  ;; Enable prettier formatting
  (prettier-js-mode 1)

  ;; Enable rainbow delimiters
  (rainbow-delimiters-mode 1)

  ;; Enable which-key locally
  (which-key-mode 1)

  ;; Set up formatting and whitespace cleanup on save
  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)

  ;; Set indentation
  (setq tab-width 2
        indent-tabs-mode nil)

  ;; Enable emmet for TSX files
  (when (string-match-p "\\.tsx\\'" buffer-file-name)
    (emmet-mode 1))

  ;; Keybindings for TypeScript development
  (local-set-key (kbd "C-c C-f") 'lsp-format-buffer)
  (local-set-key (kbd "C-c C-r") 'lsp-rename)
  (local-set-key (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (local-set-key (kbd "C-c C-a") 'lsp-execute-code-action)
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-,") 'lsp-find-references)
  (local-set-key (kbd "C-c C-l") 'lsp-ui-imenu)
  )
