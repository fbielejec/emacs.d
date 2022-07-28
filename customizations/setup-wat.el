(add-to-list 'load-path "~/CloudStation/Emacs/wat-mode")
(require 'wat-mode)

(add-hook 'wat-mode-hook 'enable-paredit-mode)
