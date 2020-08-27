;;; ~/.doom.d/lisp/julia.el -*- lexical-binding: t; -*-

;; Julia configuration
(use-package! lsp-julia
  :init
  (setq lsp-julia-default-environment "~/.julia/environments/v1.4"
        lsp-julia-package-dir "~/.emacs.d/.local/straight/repos/lsp-julia/languageserver")
  :hook ((julia-mode . lsp-deferred)))

(use-package! julia-repl
  :config
  (require 'vterm)
  (setq julia-repl-terminal-backend (make-julia-repl--buffer-vterm)))

(use-package! julia-mode
  :mode "\\.jl\\'"
  :config
  (use-package vterm)
  (set-popup-rule! "\\*julia.*\\*$"
    :height 0.3
    :quit nil
    :slot 2
    :select nil))

(defun ans/julia-repl-send-paragraph ()
  "Send paragraph to iterm."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (julia-repl-send-region-or-line)
    (deactivate-mark)))

(defun ans/julia-repl-send-function ()
  "Send paragraph to iterm."
  (interactive)
  (save-excursion
    (mark-defun)
    (julia-repl-send-region-or-line)
    (deactivate-mark)))
