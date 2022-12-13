(require 'quelpa-use-package)
(use-package chatgpt
  :quelpa ((chatgpt :fetcher git :url "https://github.com/joshcho/ChatGPT.el.git") :upgrade t)
  :init
  (setq chatgpt-repo-path (expand-file-name "chatgpt/" quelpa-build-dir))
  :bind ("C-c q" . chatgpt-query))
