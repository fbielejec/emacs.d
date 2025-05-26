;; -*- lexical-binding: t -*-

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(setq python-interpreter "/usr/bin/python3")

(use-package python-mode
  :ensure t
  :hook (python-mode . my/python-setup)
  :custom
  (python-shell-interpreter "python3")
  :config
  (defun my/python-setup ()
    "Setup Python development environment."
    ;; Enable LSP
    (lsp-deferred)

    ;; Enable paredit for structured editing
    (paredit-mode 1)

    ;; Enable Tree-sitter
   ;; (when (fboundp 'tree-sitter-hl-mode)
   ;;   (tree-sitter-hl-mode))
   ;;  (when (fboundp 'tree-sitter-mode)
   ;;   (tree-sitter-mode))

    ;; Setup whitespace cleaning on save
    (add-hook 'before-save-hook 'whitespace-cleanup nil t)
    (add-hook 'before-save-hook 'aggressive-indent-mode nil t)

    ;; Enable Flycheck
    (flycheck-mode 1)

    ;; Enable Yasnippet
    (yas-minor-mode 1)

    ;; Enable Company completion
    (company-mode 1)

    ;; Set indentation (PEP-8 style)
    (setq python-indent-offset 4
          python-indent-guess-indent-offset t
          python-indent-guess-indent-offset-verbose nil))

  ;; Configure LSP for Python
  (use-package lsp-mode
    :ensure t
    :commands lsp-deferred
    :init
    (setq lsp-pylsp-plugins-autopep8-enabled t
          lsp-pylsp-plugins-pycodestyle-enabled t
          lsp-pylsp-plugins-pydocstyle-enabled t
          lsp-pylsp-plugins-pylint-enabled t
          lsp-pylsp-plugins-yapf-enabled t
          lsp-pylsp-configuration-sources ["flake8"]
          lsp-pylsp-plugins-flake8-enabled t))

  ;; Configure Tree-sitter
 ;; (use-package tree-sitter
 ;;   :ensure t
 ;;   :config
 ;;   (use-package tree-sitter-langs
 ;;     :ensure t))

  ;; Configure Paredit
  (use-package paredit
    :ensure t
    :hook ((python-mode . paredit-mode)))

  ;; Configure Flycheck
  (use-package flycheck
    :ensure t
    :hook (python-mode . flycheck-mode))

  ;; Configure Yasnippet
  (use-package yasnippet
    :ensure t
    :config
    (yas-global-mode 1)
    (use-package yasnippet-snippets
      :ensure t))

  ;; Configure Company (completion)
  (use-package company
    :ensure t
    :hook (python-mode . company-mode)
    :config
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.1
          company-tooltip-limit 10
          company-show-numbers t
          company-selection-wrap-around t
          company-transformers '(company-sort-by-backend-importance)))

  ;; Configure whitespace handling
  (use-package aggressive-indent
    :ensure t
    :hook (python-mode . aggressive-indent-mode)))

;; experimental: evaluating tree sitter major mode
;; (setq major-mode-remap-alist
;;       '(
;;         (python-mode . python-ts-mode)
;;         ))

;; Optional: Set up virtualenv integration
;; (use-package pyvenv
;;   :ensure t
;;   :hook (python-mode . pyvenv-mode)
;;   :config
;;   (setq pyvenv-post-activate-hooks '(lambda () (lsp-restart-workspace)))
;;   (setq pyvenv-post-deactivate-hooks '(lambda () (lsp-restart-workspace))))
