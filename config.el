;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Alexey Shiklomanov"
      user-mail-address "alexey.shiklomanov@gmail.com")

(setq doom-localleader-key "\\")

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions -1		; Don't delete old versions
      version-control t			; Version control backups
      make-backup-files t
      vc-make-backup-files t		; Backup files even if they are version controlled
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ; Save file name changes

(setq display-line-numbers-type nil)

(map! (:map evil-window-map
        "o" #'ace-window
        "z" #'doom/window-enlargen
        "-" #'evil-window-split
        "\\" #'evil-window-vsplit
        "+" #'evil-window-increase-height
        "_" #'evil-window-decrease-height
        "d" #'+workspace/close-window-or-workspace
        "w" #'evil-window-mru
        "C-n" #'evil-window-next
        "C-p" #'evil-window-prev)

      (:map doom-leader-map
        :desc "Shell command" "!" #'shell-command)

      (:map doom-leader-project-map
        :desc "Find project file or buffer" "p" #'counsel-projectile
        :desc "Find file in project" "f" #'+ivy/projectile-find-file
        :desc "Switch project" "o" #'counsel-projectile-switch-project
        :desc ""
        "/" nil)

      (:map doom-leader-workspace-map
        :desc "Switch workspace" "TAB" #'+workspace/switch-to
        :desc "Show tab bar" "." #'+workspace/display)

      :i "C-l" #'+company/complete
      :n "g RET" #'eval-defun

      :i "C-0" (lambda () (interactive) (sp-slurp-hybrid-sexp) (sp-end-of-sexp))
      :i "C-s" (lambda () (interactive) (upcase-word -1))

      :i "s-k" #'evil-insert-digraph)

;; Some custom functions
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(evil-ex-define-cmd "rename" 'rename-this-buffer-and-file)

(defun ans/delete-file-and-buffer ()
  "Kill the current buffer and delete the associated file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (progn
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))

;; Automatically commenting with "o/O" is sometimes useful, but usually just
;; annoying. So, do it with "go/O", but not with regular "o/O".
(defun ans/evil-open-below (count)
  "`(evil-open-below COUNT)` without the extra advice that adds comments."
  (interactive "p")
  (evil-open-below count))
(defun ans/evil-open-above (count)
  "`(evil-open-below COUNT)` without the extra advice that adds comments."
  (interactive "p")
  (evil-open-above count))
(map! :n "o" #'ans/evil-open-below
      :n "O" #'ans/evil-open-above
      :prefix "g"
      :n "o" #'evil-open-below
      :n "O" #'evil-open-above)

(use-package! markdown-mode
  :mode ((rx ".Rmd" string-end) . gfm-mode))

(use-package! org
  :init
  (defvar ans/organization-task-id "b86713a1-f9db-47c5-860f-6a2aecfec6c9")
  (setq ans/hide-scheduled-tasks t)
  :config
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (setq org-agenda-files '("~/Dropbox/Notes/" "~/Dropbox/references/notes.org")
        org-directory "~/Dropbox/Notes"
        org-tags-exclude-from-inheritance '("_project" "_organize")
        org-tags-match-list-sublevels 'indented
        org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "CANCELED"))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-target-verify-function 'ans/verify-refile-target)
  (defun ans/verify-refile-target ()
    "Exclude TODO keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (defun ans/toggle-show-scheduled-tasks ()
    "Toggle display of scheduled/deadline tasks in agenda."
    (interactive)
    (setq ans/hide-scheduled-tasks (not ans/hide-scheduled-tasks))
    (when (equal major-mode 'org-agenda-mode)
      (org-agenda-redo))
    (message "%s SCHEDULED/DEADLINE tasks" (if ans/hide-scheduled-tasks "Hide" "Show")))
  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Notes to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-REFILE-config-reading_list-_project-_organization/NEXT!"
                       ((org-agenda-overriding-header
                         (concat "Next tasks" (if ans/hide-scheduled-tasks "" " (including scheduled)")))
                        (org-agenda-todo-ignore-scheduled ans/hide-scheduled-tasks)
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags-todo "forte-_project/-NEXT!"
                       ((org-agenda-overriding-header
                         (concat "FoRTE" (if ans/hide-scheduled-tasks "" " (including scheduled)")))
                        (org-agenda-todo-ignore-scheduled ans/hide-scheduled-tasks)
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags-todo "hector-_project/-NEXT!"
                       ((org-agenda-overriding-header
                         (concat "Hector" (if ans/hide-scheduled-tasks "" " (including scheduled)")))
                        (org-agenda-todo-ignore-scheduled ans/hide-scheduled-tasks)
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags-todo "pecan-_project/-NEXT!"
                       ((org-agenda-overriding-header
                         (concat "PEcAn" (if ans/hide-scheduled-tasks "" " (including scheduled)")))
                        (org-agenda-todo-ignore-scheduled ans/hide-scheduled-tasks)
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags-todo "-REFILE-config-reading_list-_project-_organization-forte-hector-pecan/-NEXT!"
                       ((org-agenda-overriding-header (concat "Other tasks"
                                                              (if ans/hide-scheduled-tasks
                                                                  ""
                                                                " (including scheduled)")))
                        (org-agenda-sorting-strategy '(todo-state-down priority-down))
                        (org-agenda-todo-ignore-deadlines ans/hide-scheduled-tasks))))
           nil)
          ("r" "Reading list" todo "TODO|NEXT"
           ((org-agenda-files '("~/Dropbox/references/notes.org"))
            (org-agenda-sorting-strategy '(todo-state-down priority-down))))
          ("c" "Configuration" tags-todo "-_organization-_project"
           ((org-agenda-files '("~/Dropbox/Notes/computers.org"))
            (org-agenda-sorting-strategy '(todo-state-down priority-down))))
          ("p" "Projects" tags-todo "_project"
           ((org-agenda-sorting-strategy '(todo-state-down))))))
  (setq org-capture-templates
        '(("e" "Emacs config" entry
           (file+headline "~/Dropbox/Notes/computers.org" "TODO Emacs configuration")
           "** TODO %?" :clock-in t :clock-resume t)
          ("t" "TODO" entry
           (file "~/Dropbox/Notes/unsorted.org")
           "* TODO %?\nCaptured %U\nFrom file %a\n" :clock-in t :clock-keep t)
          ("l" "Later TODO" entry
           (file "~/Dropbox/Notes/unsorted.org")
           "* TODO %?\nCaptured %U\nFrom file %a\n" :clock-in t :clock-resume t)
          ("u" "Miscellaneous note" entry
           (file "~/Dropbox/Notes/unsorted.org")
           "* %? :NOTE:\nCaptured %U\n%a\n" :clock-in t :clock-resume t)
          ("i" "Interruption" entry (file "~/Dropbox/Notes/unsorted.org")
           "* %? \nCaptured %U" :clock-in t :clock-resume t)
          ("c" "No-clock note" entry
           (file "~/Dropbox/Notes/unsorted.org")
           "* %?")))
  ;; Org clock configuration
  (setq org-clock-in-resume t
        org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t
        org-clock-mode-line-total 'current
        org-clock-clocked-in-display 'mode-line
        org-clock-history-length 10
        org-clock-clocktable-default-properties
        '(:maxlevel 5 :scope agenda-with-archives :block today :link t)
        org-agenda-clockreport-parameter-plist
        '(:maxlevel 5 :link 5))
  (org-clock-persistence-insinuate)
  (defun ans/clock-out-maybe ()
    "Clock parent task, or clock out."
    (when (and ans/keep-clock-running
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (ans/clock-in-parent-task)))
  (defun ans/clock-in-parent-task ()
    "Move point to parent task (if any) and clock in.
  Otherwise, clock in the default task."
    (let ((parent-task))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (not parent-task) (org-up-heading-safe))
            (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
              (setq parent-task (point))))
          (if parent-task
              (org-with-point-at parent-task
                (org-clock-in))
            (when ans/keep-clock-running
              (ans/clock-in-organization-task)))))))
  (add-hook 'org-clock-out-hook #'ans/clock-out-maybe 'append)
  (defun ans/clock-in-organization-task ()
    "Clock in the default organization task."
    (interactive)
    (org-with-point-at (org-id-find ans/organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun ans/punch-in ()
    "Start clocking, and set default task to Organization."
    (interactive)
    (setq ans/keep-clock-running t)
    (ans/clock-in-organization-task))

  (defun ans/punch-out ()
    "End all clocking."
    (interactive)
    (setq ans/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out)))

  (defun ans/org-clock-history ()
    "Clock into task based on history."
    (interactive)
    (org-clock-in-last '(4)))

  (map! :map doom-leader-open-map
        (:prefix-map ("c" . "clock")
          :desc "Punch in" "+" #'ans/punch-in
          :desc "Punch out" "-" #'ans/punch-out
          :desc "Go-to" "g" #'org-clock-goto
          :desc "History" "h" #'ans/org-clock-history
          :desc "Clock out" "DEL" #'org-clock-out))

  (map! :map doom-leader-notes-map
        :desc "Reveal" "TAB" #'org-reveal
        :desc "Helm-bibtex" "b" #'helm-bibtex)

  (map! :map org-agenda-mode-map
        :m :desc "Log mode" "@l" #'org-agenda-log-mode
        :m :desc "Day view" "@d" #'org-agenda-day-view
        :m :desc "Week view" "@w" #'org-agenda-week-view
        :m :desc "Month view" "@m" #'org-agenda-month-view)

  (map! :map evil-org-mode-map
        :n "z n" #'org-toggle-narrow-to-subtree))

;;  Related to bibtex references
(defvar ans/reference-dir (file-name-as-directory "~/Dropbox/references")
  "Root directory for storing my bibliography.")
(setq bibtex-completion-library-path (file-name-as-directory (concat ans/reference-dir "pdfs")))
(setq bibtex-completion-notes-path (concat ans/reference-dir "notes.org"))
(setq bibtex-completion-bibliography (concat ans/reference-dir "library.bib"))
(setq bibtex-autokey-titleword-length 15)
(setq org-ref-default-bibliography (list bibtex-completion-bibliography))

;; CSL locale files. If this doesn't exist, clone them from:
;; https://github.com/citation-style-language/locales
(setq citeproc-org-locales-dir "~/.config/csl-locales")

;; Diary -- mostly, to prevent errors about this file not existing
(setq diary-file "~/Dropbox/Notes/journal/diary")

(use-package! fill-function-arguments
  :commands (fill-function-arguments-dwim))
(map! :desc "Fill function arguments" :n "g [" #'ans/fill-function-arguements-and-indent)

(defun ans/fill-function-arguements-and-indent ()
  "Fill function arguments and indent."
  (interactive)
  (fill-function-arguments-dwim)
  (let ((start (save-excursion (sp-beginning-of-sexp) (point)))
        (end (save-excursion (sp-end-of-sexp) (+ (point) 1))))
    (indent-region start end)))

;; Deadgrep
(use-package! deadgrep
  :commands (deadgrep)
  :config
  (setq deadgrep-project-root-function
        (lambda ()
          (if (projectile-project-p) (projectile-project-root) (deadgrep--project-root)))))

(map! :map doom-leader-search-map :desc "Deadgrep" "r" #'deadgrep)

(use-package! dtrt-indent
  :config
  (setq dtrt-indent-hook-mapping-list (add-to-list 'dtrt-indent-hook-mapping-list
                                                   '(ess-r-mode default ess-indent-offset))))

;; Don't quit the compilation buffer if it's still running
(set-popup-rule!
  (rx string-start (zero-or-more blank)
      "*" (zero-or-more blank) "compilation")
  :select nil
  :quit (lambda (_window) (not compilation-in-progress)))

;; Automatically soft-wrap lines in text modes
(add-hook! 'text-mode-hook :append '(visual-line-mode  turn-off-auto-fill))
(remove-hook! 'markdown-mode-hook 'auto-fill-mode)

(use-package! pandoc-mode
  :hook ((markdown-mode gfm-mode poly-markdown-mode) . pandoc-mode))

;; Override counsel-org-capture. For some reason, just mapping directly fails.
(defun ans/org-capture ()
  "Just org-capture. No counsel."
  (interactive)
  (require 'org-capture)
  (org-capture))

(map! (:map doom-leader-map
        :desc "Org-capture" "X" #'ans/org-capture)
      (:map doom-leader-notes-map
        :desc "Org-capture" "c" #'ans/org-capture))

(map! (:map doom-leader-map
        "x" nil
        (:prefix-map ("x" . "scratch")
          :desc "Fundamental" "x" #'doom/open-scratch-buffer
          :desc "R" "r" (lambda () (interactive) (doom/open-scratch-buffer) (R-mode))
          :desc "Lisp interaction" "l" (lambda () (interactive) (doom/open-scratch-buffer) (lisp-interaction-mode))
          :desc "Python" "p" (lambda () (interactive) (doom/open-scratch-buffer) (python-mode))
          :desc "Bash" "b" (lambda () (interactive) (doom/open-scratch-buffer) (sh-mode)))))
