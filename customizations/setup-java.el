;;;;
;; Java
;;;;

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(require 'cc-mode)

;; (condition-case nil
;;     (require 'use-package)
;;   (file-error
;;    (require 'package)
;;    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;    (package-initialize)
;;    (package-refresh-contents)
;;    (package-install 'use-package)
;;    (require 'use-package)))

(require 'use-package)

(use-package projectile :ensure t)
(use-package treemacs :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui :ensure t)

(use-package lsp-java
  :ensure t
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))
(use-package lsp-java-treemacs :after (treemacs))

;; (when (eq major-mode 'java-mode)
;;   (add-hook 'before-save-hook 'whitespace-cleanup))

;; add lombok support
;; https://projectlombok.org/
(setq lsp-java-vmargs '("-noverify"
                        "-Xmx1G"
                        "-XX:+UseG1GC"
                        "-XX:+UseStringDeduplication"
                        "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar"
                        "-Xbootclasspath/a:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar"))

(add-hook 'java-mode-hook
          (lambda () (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)))

(add-hook 'java-mode-hook #'enable-paredit-mode)
