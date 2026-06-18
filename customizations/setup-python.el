;; -*- lexical-binding: t -*-
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Python development setup — mirrors setup-rust.el.
;;
;; NB: NO global minor modes. Everything is enabled buffer-locally from the
;; `python-mode' hook, so it only ever affects .py buffers / `python-mode'.
;;
;; Reuses shared infrastructure from setup-rust.el (already configured there):
;;   - lsp-mode / lsp-ui          (LSP client + UI)
;;   - flycheck                   (inline error/warning highlights, fed by LSP)
;;   - company + tab-indent-or-complete  (TAB completion / snippet expansion via
;;                                        the global company-mode-map binding)
;;   - yasnippet                  (snippet engine)
;; This file only adds the Python-specific pieces.
;;
;; External requirement: a Python LSP server on PATH. Default here is pylsp:
;;     pipx install "python-lsp-server[all]"
;; (Alternative: pyright via `npm i -g pyright' + the lsp-pyright package.)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; python-mode (built-in python.el) + LSP, navigation, completion

(use-package python
  :ensure nil                          ; built-in; do NOT install external python-mode
  :bind (:map python-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ;; ("C-c C-c l" . flycheck-list-errors)
              ;; ("C-c C-c a" . lsp-execute-code-action)
              ;; ("C-c C-c r" . lsp-rename)
              ;; ("C-c C-c q" . lsp-workspace-restart)
              ;; ("C-c C-c Q" . lsp-workspace-shutdown)
              ;; ("C-c C-c h" . lsp-ui-doc-glance)
              )
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil)
  :hook (python-mode . rk/python-mode-hook))

(defun rk/python-mode-hook ()
  "Per-buffer Python dev setup: LSP, completion, snippets, lint, ws-on-save."
  ;; LSP: code navigation, completion source, diagnostics -> flycheck
  (lsp-deferred)
  ;; completion + snippets (buffer-local; TAB binding comes from setup-rust.el)
  (company-mode 1)
  (yas-minor-mode 1)
  ;; inline errors/warnings (buffer-local)
  (flycheck-mode 1)
  ;; structured editing, to match the Rust hook
  (enable-paredit-mode)
  ;; remove trailing whitespace on save (buffer-local)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
  ;; For full format-on-save like rustfmt (uses pylsp's yapf), uncomment:
  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  )

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Python LSP server settings (pylsp). Additive to setup-rust.el's lsp-mode block.

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  ;; flake8 wraps pycodestyle + pyflakes + mccabe — enable it and disable the
  ;; individual ones to avoid duplicate diagnostics.
  (lsp-pylsp-plugins-flake8-enabled t)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pyflakes-enabled nil)
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-pylint-enabled nil)     ; flip to t for stricter (slower) linting
  (lsp-pylsp-plugins-pydocstyle-enabled nil) ; noisy; enable for docstring lint
  ;; formatting: yapf (used by the optional lsp-format-buffer on-save hook above)
  (lsp-pylsp-plugins-yapf-enabled t)
  (lsp-pylsp-plugins-autopep8-enabled nil))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Snippet library (yasnippet engine itself is configured in setup-rust.el).
;; This only installs snippet definitions — it enables no mode.

(use-package yasnippet-snippets :ensure t)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; virtualenv awareness so the LSP resolves project imports (avoids false
;; "unresolved import" warnings). Installed for the INTERACTIVE commands only —
;; deliberately NOT enabling the global `pyvenv-mode'. Per project, run:
;;     M-x pyvenv-activate   (point at the venv dir)  then  M-x lsp-workspace-restart

(use-package pyvenv
  :ensure t
  :commands (pyvenv-activate pyvenv-workon pyvenv-deactivate))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Jupyter REPL

(use-package jupyter
  :ensure t
  :config
  ;; REPL history on M-<up>/M-<down>
  (define-key jupyter-repl-mode-map (kbd "M-<up>") 'jupyter-repl-history-previous)
  (define-key jupyter-repl-mode-map (kbd "M-<down>") 'jupyter-repl-history-next))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Optional: tree-sitter major mode (Emacs 29+). Uncomment to try.
;; (setq major-mode-remap-alist '((python-mode . python-ts-mode)))
