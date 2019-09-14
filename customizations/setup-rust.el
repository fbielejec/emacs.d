(require 'use-package)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(defun rust-save-hook()
  (when (eq major-mode 'rust-mode)
    (whitespace-cleanup)))

(use-package racer
  :requires rust-mode

  :init (setq racer-rust-src-path
              (concat (string-trim
                       (shell-command-to-string "rustc --print sysroot"))
                      "/lib/rustlib/src/rust/src"))

  :config
  (add-hook 'rust-mode-hook 'enable-paredit-mode)
  (add-hook 'before-save-hook 'rust-save-hook)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))
