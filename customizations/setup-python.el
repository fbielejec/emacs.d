(require 'ein)

(setq ein:worksheet-enable-undo t)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(add-hook 'python-mode-hook 'enable-paredit-mode)

(shell-command "jupyter kernelspec list")
