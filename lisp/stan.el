;;; ~/.doom.d/lisp/stan.el -*- lexical-binding: t; -*-

(use-package! stan-mode
  :mode "\\.stan\\'"
  :hook (stan-mode . stan-mode-setup)
  :config
  (setq stan-indentation-offset 2)
  (setq-local c-basic-offset stan-indentation-offset))

(use-package! company-stan
  :hook (stan-mode . company-stan)
  :config
  (setq company-stan-fuzzy nil))

(use-package! eldoc-stan
  :hook (stan-mode . eldoc-stan-setup))

(use-package! flycheck-stan
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup)))
