;;; ~/.doom.d/lisp/julia.el -*- lexical-binding: t; -*-

;; Julia configuration
(use-package! lsp-julia
  :init
  (setq lsp-julia-default-environment "~/.julia/environments/v1.4"
        lsp-julia-package-dir "~/.emacs.d/.local/straight/repos/lsp-julia/languageserver")
  :hook ((julia-mode . lsp-deferred)))

(use-package! julia-repl
  :config
  (setq julia-repl-terminal-backend (make-julia-repl--buffer-vterm)))

(defun ans/vterm-send-string (string &optional break)
  "Send STRING to julia vterm buffer. If BREAK, add a linebreak."
  (let* ((sendstring string))
    (if break
        (setq sendstring (concat string "\n")))
    (process-send-string "vterm" sendstring)))

(defun ans/vterm-send-region (start end)
  "Send region from START to END to julia vterm buffer."
  (interactive "r")
  (process-send-region "vterm" start end))

(defun ans/julia-start ()
  "Start Julia in current directory."
  (interactive)
  (process-send-string "vterm" "julia\n"))

(defun ans/vterm-send-line ()
  "Send current line to Julia repl"
  (interactive)
  (ans/vterm-send-string (thing-at-point 'line)))

(defun ans/vterm-send-paragraph ()
  "Send paragraph to iterm."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (ans/vterm-send-region (point) (mark))
    (deactivate-mark)))
