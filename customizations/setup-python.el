(require 'ein)
(require 'ein-notebook)
(require 'ein-subpackages)

(setq python-interpreter "/usr/bin/python3")
(setq ein:worksheet-enable-undo t)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(add-hook 'python-mode-hook 'enable-paredit-mode)

(shell-command "jupyter kernelspec list")
