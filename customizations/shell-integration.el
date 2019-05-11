;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; set shell to be bash (fish confuses ag)
(setq explicit-shell-file-name "/bin/bash")

;; shell scripts indentation
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
