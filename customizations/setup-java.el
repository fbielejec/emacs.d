;;;;
;; Java
;;;;

;; --- WIP : CONF --- ;;

(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))

(defun tkj-default-code-style-hook()
  (setq c-basic-offset 2
        c-label-offset 0
        tab-width 2
        indent-tabs-mode nil
        compile-command "mvn -q -o -f ~/src/content-engine/engine/engine-core/pom.xml test -DtrimStackTrace=false"
        require-final-newline nil))
(add-hook 'java-mode-hook 'tkj-default-code-style-hook)

(use-package flycheck
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15))))

(use-package idle-highlight-mode)

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  (git-gutter+-mode)
  (gtags-mode)
  (idle-highlight-mode)
  (subword-mode)
  (yas-minor-mode)
  (set-fringe-style '(8 . 0))
  (define-key c-mode-base-map (kbd "C-M-j") 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun tkj-projectile-grep-region()
  "Search the project for the text in region"
  (interactive)
  (projectile-grep (buffer-substring (mark) (point))))

(use-package projectile :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t
  :bind
  (:map lsp-mode-map
        (("\C-\M-b" . lsp-find-implementation)
         ("M-RET" . lsp-execute-code-action)))
  :config
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-highlight-symbol-at-point nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        )

  ;; Performance tweaks, see
  ;; https://github.com/emacs-lsp/lsp-mode#performance
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  )

(use-package hydra :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-delay 5.0
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil))

(use-package lsp-java
  :ensure t
  :init
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx3G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         ;; customize this path for lombok support
         "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"         
         )

        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil

        ;; Fetch less results from the Eclipse server
        lsp-java-completion-max-results 20

        ;; Currently (2019-04-24), dap-mode works best with Oracle
        ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
        ;;
        ;; lsp-java-java-path "~/.emacs.d/oracle-jdk-12.0.1/bin/java"
        ;; lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
        ;; find the path with which java
        lsp-java-java-path "/usr/bin/java"        
        )

  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-register-debug-template
   "localhost:5005"
   (list :type "java"
         :request "attach"
         :hostName "localhost"
         :port 5005))
  (dap-register-debug-template
   "lxd"
   (list :type "java"
         :request "attach"
         :hostName "10.152.112.168"
         :port 5005))
  )

(use-package dap-java
  :ensure nil
  :after (lsp-java)

  ;; The :bind here makes use-package fail to lead the dap-java block!
  ;; :bind
  ;; (("C-c R" . dap-java-run-test-class)
  ;;  ("C-c d" . dap-java-debug-test-method)
  ;;  ("C-c r" . dap-java-run-test-method)
  ;;  )

  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue)
  )

(use-package treemacs
  :init
  (add-hook 'treemacs-mode-hook
            (lambda () (treemacs-resize-icons 15))))


;; --- CONF --- ;;

;; (use-package projectile)
;; (use-package flycheck)
;; (use-package yasnippet :config (yas-global-mode))
;; (use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
;;   :config (setq lsp-completion-enable-additional-text-edit nil))
;; (use-package hydra)
;; (use-package company)

;; ;; (use-package lsp-ui)

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable t
;;      lsp-ui-sideline-show-symbol t
;;      lsp-ui-sideline-show-hover t
;;      lsp-ui-sideline-showcode-actions t
;;      lsp-ui-sideline-update-mode 'point))

;; (use-package which-key :config (which-key-mode))
;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)
;; ;; (use-package helm-lsp)
;; ;; (use-package helm
;; ;;   :config (helm-mode))
;; (use-package lsp-treemacs)

;; ;; --- CONFIG EXTRAS --- ;;

;; ;; (setq lsp-java-java-path "/usr/lib/jvm/java-11-oracle/bin/java")
;; (setq lsp-java-java-path "/usr/bin/java")

;; add lombok support
;; (setq lombok-version "1.18.16")
;; ;; https://projectlombok.org/
;; (setq lsp-java-vmargs '("-noverify"
;;                         "-Xmx1G"
;;                         "-XX:+UseG1GC"
;;                         "-XX:+UseStringDeduplication"
;;                         ;; (format "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)
;;                         ;; (format "-Xbootclasspath/a:/home/filip/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)
;;                         "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"
;;                         ;; "-Xbootclasspath/a:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"
;;                         ))

;; (add-hook 'java-mode-hook
;;           (lambda () (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)))

;; (add-hook 'java-mode-hook #'enable-paredit-mode)

;; (setq lsp-keep-workspace-alive nil)
