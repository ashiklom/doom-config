;;; ~/.doom.d/lisp/julia.el -*- lexical-binding: t; -*-

;; Julia configuration
(use-package! lsp-julia
  :after julia-mode
  :preface
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6"
        lsp-julia-package-dir "~/src/languageserver"
        lsp-enable-folding t))

(use-package vterm
  :after-call (julia-repl)
    :config
    (map! :map vterm-mode-map
          :i "C-c C-q" #'evil-collection-vterm-toggle-send-escape
          :i "C-c C-z" #'evil-window-mru))

(use-package! julia-repl
  :after julia-mode
  :config
  (julia-repl-set-terminal-backend 'vterm))

(use-package! julia-mode
  :mode "\\.jl\\'"
  :config
  (add-hook! 'julia-mode-hook
             #'electric-pair-local-mode)
  (setq julia-repl-executable-records
        '((default "julia")
          (adapt "~/.local/bin/adapt-julia" :basedir "~")))
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

(defun ans/julia-cd ()
  "Set current directory"
  (interactive)
  (julia-repl--send-string
   (format
    "cd(\"%s\")"
    (if (projectile-project-p)
        (projectile-project-root)
      (file-name-directory (buffer-file-name)))))
  )
