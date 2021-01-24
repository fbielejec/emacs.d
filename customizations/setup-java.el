;;;;
;; Java
;;;;

;; --- CONF 2 --- ;;

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
;; (use-package helm-lsp)
;; (use-package helm
;;   :config (helm-mode))
(use-package lsp-treemacs)

;; --- CONF 3 --- ;;

;; (use-package lsp-mode
;;   :hook java-mode)

;; ;; Seems I have to demand load this or it's not availble when
;; ;; opening a java file.
;; ;; 2018-07-18: switch to defer with timeout instead of demand
;; (use-package lsp-java
;;   :defer 3
;;   :init  
;;   (progn
;;     (require 'lsp-ui-flycheck)
;;     (require 'lsp-ui-sideline)
;;     (add-hook 'java-mode-hook #'lsp-java-enable)
;;     (add-hook 'java-mode-hook #'flycheck-mode)
;;     (add-hook 'java-mode-hook #'company-mode)
;;     (add-hook 'java-mode-hook (lambda () (lsp-ui-flycheck-enable t)))
;;     (add-hook 'java-mode-hook #'lsp-ui-sideline-mode))
  
;;   ;; :config
;;   ;; ;; this is a bummer, having to add each project individually :-(
;;   ;; (setq lsp-java--workspace-folders
;;   ;;       (list
;;   ;;        (expand-file-name "~/Projects/CopperheadRecovery/copperhead")))

;;   )

;; ;; (use-package java-snippets
;; ;;   :init
;; ;;   (add-hook 'java-mode-hook #'yas-minor-mode))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable t
;; 	lsp-ui-sideline-show-symbol t
;; 	lsp-ui-sideline-show-hover t
;; 	lsp-ui-sideline-showcode-actions t
;; 	lsp-ui-sideline-update-mode 'point))

;; (use-package company-lsp
;;   :after company
;;   :init
;;   (add-to-list 'company-backends #'company-lsp)

;;   :config
;;   (setq company-lsp-enable-snippet t
;;        company-lsp-cache-candidates t))

;; --- CONF 1 --- ;;

;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)

;; (require 'cc-mode)

;; ;; (condition-case nil
;; ;;     (require 'use-package)
;; ;;   (file-error
;; ;;    (require 'package)
;; ;;    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; ;;    (package-initialize)
;; ;;    (package-refresh-contents)
;; ;;    (package-install 'use-package)
;; ;;    (require 'use-package)))

;; (require 'use-package)

;; (use-package projectile :ensure t)
;; (use-package treemacs :ensure t)
;; (use-package yasnippet :ensure t)
;; (use-package lsp-mode :ensure t)
;; (use-package hydra :ensure t)
;; (use-package company-lsp :ensure t)
;; (use-package lsp-ui :ensure t)

;; (use-package lsp-java
;;   :ensure t
;;   :after lsp
;;   :config (add-hook 'java-mode-hook 'lsp))

;; (use-package dap-mode
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))

;; (use-package dap-java :after (lsp-java))
;; (use-package lsp-java-treemacs :after (treemacs))

;; (when (eq major-mode 'java-mode)
;;   (add-hook 'before-save-hook 'whitespace-cleanup))

;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))
;; (use-package lsp-treemacs)


;; --- CONFIG EXTRAS --- ;;

;; (setq lsp-java-java-path "/usr/lib/jvm/java-11-oracle/bin/java")
(setq lsp-java-java-path "/usr/bin/java")

;; add lombok support
(setq lombok-version "1.18.16")
;; https://projectlombok.org/
(setq lsp-java-vmargs '("-noverify"
                        "-Xmx1G"
                        "-XX:+UseG1GC"
                        "-XX:+UseStringDeduplication"
                        ;; (format "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)
                        ;; (format "-Xbootclasspath/a:/home/filip/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)
                        "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"
                        ;; "-Xbootclasspath/a:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"
                        ))

(add-hook 'java-mode-hook
          (lambda () (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)))

(add-hook 'java-mode-hook #'enable-paredit-mode)
