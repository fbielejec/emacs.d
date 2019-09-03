(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(use-package racer
  :requires rust-mode

  :init (setq racer-rust-src-path
              (concat (string-trim
                       (shell-command-to-string "rustc --print sysroot"))
                      "/lib/rustlib/src/rust/src"))

  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(add-hook 'rust-mode-hook 'enable-paredit-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(defun rust-save-hook()
  (when (eq major-mode 'rust-mode)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'rust-save-hook)
