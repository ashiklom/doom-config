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

      ;; Most recently used workspace
      :n "gt" #'+workspace/other
      :n "gT" #'+workspace/other

      :desc "Eval defun" :m "g RET" #'eval-defun

      :i "C-0" (cmd! (sp-slurp-hybrid-sexp) (sp-end-of-sexp))
      :i "C-s" (cmd! (upcase-word -1))
      :i "s-k" #'evil-insert-digraph

      :i "C-." #'doom/dumb-indent

      :g "M-0" nil

      :g "s-c" #'simpleclip-copy
      :g "s-v" #'simpleclip-paste
      :g "s-x" #'simpleclip-cut

      (:leader
        :desc "Org capture" "X" #'ans/org-capture
        :desc "Shell command" "!" #'shell-command
        :desc "Redraw display" "&" #'redraw-display
        :desc "Toggle smartparens" ")" #'smartparens-mode
        :desc "Toggle visual lines" "$" #'visual-line-mode
        "x" nil
        (:prefix-map ("x" . "scratch")
          :desc "Fundamental" "x" #'doom/open-scratch-buffer
          :desc "R" "r" (cmd! (doom/open-scratch-buffer) (R-mode))
          :desc "Lisp interaction" "l" (cmd! (doom/open-scratch-buffer) (lisp-interaction-mode))
          :desc "Python" "p" (cmd! (doom/open-scratch-buffer) (python-mode))
          :desc "Bash" "b" (cmd! (doom/open-scratch-buffer) (sh-mode))))

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
       :desc "Org-capture" "c" #'ans/org-capture
       :desc "Save all buffers" "w" #'org-save-all-org-buffers)

      (:map doom-leader-search-map
       :desc "Deadgrep" "r" #'deadgrep)

      (:map doom-leader-git-map
        :desc "Diff hunk" "d" #'git-gutter:popup-hunk)

      (:map doom-leader-buffer-map
        :desc "Erase buffer" "DEL" #'erase-buffer
        :desc "Chmod" "c" #'ans/chmod-this-file)

      (:map doom-leader-open-map
       :desc "Open iTerm here" "I" #'+macos/open-in-iterm)

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
          :desc "Send defun" :n "ff" #'python-shell-send-defun
          :desc "Send line" :n "l" (cmd! (python-shell-send-string (thing-at-point 'line t)))
          :desc "Send line and down" :n "d" (cmd! (python-shell-send-string (thing-at-point 'line t)) (next-line))
          :desc "Send statement" :n "L" #'python-shell-send-statement
          :desc "Send buffer" :n "aa" #'python-shell-send-buffer
          :desc "Send start to current line" :n "as" (cmd! (python-shell-send-string (buffer-substring-no-properties 1 (point-at-bol))))
          :desc "Print symbol" :n "rp" #'ans/python-print-symbol-at-point
          :desc "Send paragraph" :n "pp" #'ans/python-send-paragraph
          :desc "Send paragraph" :n "pd" (cmd! (ans/python-send-paragraph) (next-line))
          :desc "Send region" :v "ss" (cmd! (python-shell-send-string (buffer-substring-no-properties (mark) (point)))))
        :desc "Restart Python" :n "C-c C-e r" #'pyvenv-restart-python)

      (:map inferior-python-mode-map
        :g "C-c C-z" #'evil-window-mru)

      (:map doom-leader-notes-map
        :desc "Reveal" "TAB" #'org-reveal
        :desc "Ivy bibtex" "b" #'ivy-bibtex
        :desc "Clock history" "h" #'ans/org-clock-history
        :desc "Punch in" "+" #'ans/punch-in
        :desc "Punch out" "-" #'ans/punch-out
        :desc "Clock out" "DEL" #'org-clock-out)

      (:map org-agenda-mode-map
        :m :desc "Log mode" "@l" #'org-agenda-log-mode
        :m :desc "Day view" "@d" #'org-agenda-day-view
        :m :desc "Week view" "@w" #'org-agenda-week-view
        :m :desc "Month view" "@m" #'org-agenda-month-view)

      (:map (text-mode-map evil-org-mode-map LaTeX-mode-map)
        :nvm "j" #'evil-next-visual-line
        :nvm "k" #'evil-previous-visual-line)

      (:map evil-org-mode-map
        :n "z n" #'org-toggle-narrow-to-subtree)

      ;; Using iTerm
      (:map julia-mode-map
        (:localleader
          :n :desc "Start julia" "rf" #'julia-repl
          :n :desc "Quit" "rq" (cmd! (julia-repl--send-string "exit()"))
          :n :desc "Send line" "l" #'julia-repl-send-line
          :n :desc "Send line and down" "d" (cmd! (julia-repl-send-line) (evil-next-line))
          :n :desc "Send buffer" "aa" (cmd! (julia-repl--send-string (format "include(\"%s\")" (buffer-file-name))))
          :n :desc "Send beginning to here" "as" (cmd! (julia-repl--send-string (buffer-substring-no-properties 1 (point))))
          :n :desc "Help on object" "hh" (cmd! (julia-repl--send-string (format "@doc %s" (thing-at-point 'symbol t))))
          :n :desc "Print object" "rp" (cmd! (julia-repl--send-string (thing-at-point 'symbol t)))
          :n :desc "Size of object" "rs" (cmd! (julia-repl--send-string (format "size(%s)" (thing-at-point 'symbol t))))
          :n :desc "Type of object" "rt" (cmd! (julia-repl--send-string (format "typeof(%s)" (thing-at-point 'symbol t))))
          :n :desc "Send paragraph" "pp" #'ans/julia-repl-send-paragraph
          :n :desc "Send paragraph and down" "pd" (cmd! (ans/julia-repl-send-paragraph) (evil-forward-paragraph))
          :n :desc "Send function" "ff" #'ans/julia-repl-send-function
          :n :desc "Change to file directory" "cd" (cmd! (julia-repl--send-string (format "cd(\"%s\")" (file-name-directory (buffer-file-name)))))
          :n :desc "Change up one directory" "c." (cmd! (julia-repl--send-string "cd(\"..\")"))
          :v :desc "Send region" "ss" #'julia-repl-send-region-or-line))

      (:map doom-leader-insert-map
        :desc "Insert org heading" "RET" #'org-insert-heading))

(use-package! evil-magit
  :config
  (evil-define-key* '(normal visual) magit-mode-map
    "%" #'magit-worktree))
