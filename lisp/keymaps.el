;;; ~/.doom.d/lisp/keymaps.el -*- lexical-binding: t; -*-

(setq doom-localleader-key "\\")

(map! :n "o" #'ans/evil-open-below
      :n "O" #'ans/evil-open-above
      :n "go" #'evil-open-below
      :n "gO" #'evil-open-above
      :desc "Cycle multi line" :n "g[" #'multi-line

      :n "gz]" #'evil-mc-skip-and-goto-next-match
      :nv "gz[" #'evil-mc-skip-and-goto-prev-match

      :n "gb" #'evil-next-buffer
      :n "gB" #'evil-prev-buffer

      :desc "Eval defun" :m "g RET" #'eval-defun

      :i "C-l" #'+company/complete
      :i "C-0" (lambda! (sp-slurp-hybrid-sexp) (sp-end-of-sexp))
      :i "C-s" (lambda! (upcase-word -1))
      :i "s-k" #'evil-insert-digraph

      :g "M-0" nil

      :g "s-c" #'simpleclip-copy
      :g "s-v" #'simpleclip-paste
      :g "s-x" #'simpleclip-cut

      (:leader
        :desc "Org capture" "X" #'ans/org-capture
        :desc "Shell command" "!" #'shell-command
        :desc "Redraw frame" "&" (lambda! (redraw-frame))
        :desc "Toggle smartparens" ")" #'smartparens-mode
        :desc "Toggle visual lines" "$" #'visual-line-mode
        "x" nil
        (:prefix-map ("x" . "scratch")
          :desc "Fundamental" "x" #'doom/open-scratch-buffer
          :desc "R" "r" (lambda! (doom/open-scratch-buffer) (R-mode))
          :desc "Lisp interaction" "l" (lambda! (doom/open-scratch-buffer) (lisp-interaction-mode))
          :desc "Python" "p" (lambda! (doom/open-scratch-buffer) (python-mode))
          :desc "Bash" "b" (lambda! (doom/open-scratch-buffer) (sh-mode))))

      (:map evil-window-map
        "o" #'ace-window
        "z" #'doom/window-enlargen
        "-" #'evil-window-split
        "\\" #'evil-window-vsplit
        "+" #'evil-window-increase-height
        "_" #'evil-window-decrease-height
        "d" #'+workspace/close-window-or-workspace
        "w" #'evil-window-mru
        "C-n" #'evil-window-next
        "C-p" #'evil-window-prev
        :desc "Raise popup window" "~" #'+popup/raise)

      (:map doom-leader-project-map
        :desc "Find project file or buffer" "p" #'counsel-projectile
        :desc "Find file in project" "f" #'+ivy/projectile-find-file
        :desc "Switch project" "o" #'counsel-projectile-switch-project)

      (:map doom-leader-workspace-map
        :desc "Switch workspace" "TAB" #'+workspace/switch-to
        :desc "Show tab bar" "." #'+workspace/display)

      (:map doom-leader-notes-map
        :desc "Org-capture" "c" #'ans/org-capture)

      (:map doom-leader-search-map :desc "Deadgrep" "r" #'deadgrep)

      (:map doom-leader-git-map
        :desc "Diff hunk" "d" #'git-gutter:popup-hunk)

      (:map doom-leader-buffer-map
        :desc "Erase buffer" "DEL" #'erase-buffer)

      (:map ctbl:table-mode-map
        :n "q" #'doom/escape
        :n "j" #'ctbl:navi-move-down
        :n "k" #'ctbl:navi-move-up
        :n "l" #'ctbl:navi-move-right
        :n "h" #'ctbl:navi-move-left
        :n "gl" #'ctbl:navi-move-right-most
        :n "gh" #'ctbl:navi-move-left-most
        :n "g/" #'ctbl:navi-jump-to-column
        :n "g?" #'ctbl:navi-goto-cell
        :n "g RET" #'ans/ctbl-sort-current-column)

      (:map outline-minor-mode-map
        :desc "Close all" :n "z0" #'outline-hide-body
        :desc "Open all" :n "z*" #'outline-show-all)

      ;; Mimicing my ESS bindings
      (:map python-mode-map
        (:localleader
          :desc "Run Python" :n "rf" #'+python/open-ipython-repl
          :desc "Send defun" :n "l" #'python-shell-send-defun
          :desc "Send buffer" :n "aa" #'python-shell-send-buffer
          :desc "Prnt symbol" :n "rp" #'ans/python-print-symbol-at-point

          :desc "Send region" :v "ss" #'python-shell-send-region))

      (:map inferior-python-mode-map
        :g "C-c C-z" #'evil-window-mru))
