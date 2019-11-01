;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(use-package! org
  :config
  (defvar ans/organization-task-id "b86713a1-f9db-47c5-860f-6a2aecfec6c9")
  (setq ans/hide-scheduled-tasks t)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (remove-hook 'org-mode-hook #'org-fancy-priorities-mode)
  (setq org-agenda-files '("~/Dropbox/Notes/" "~/Dropbox/references/notes.org")
        org-directory "~/Dropbox/Notes"
        org-tags-exclude-from-inheritance '("_project" "_organize")
        org-tags-match-list-sublevels 'indented
        org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "CANCELED"))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-target-verify-function 'ans/verify-refile-target
        org-startup-indented t
        org-agenda-dim-blocked-tasks nil)
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
        '(("o" "Agenda"
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
            (tags-todo "rcmip_hector-_project/-NEXT!"
                       ((org-agenda-overriding-header
                         (concat "RCMIP-Hector" (if ans/hide-scheduled-tasks "" " (including scheduled)")))
                        (org-agenda-todo-ignore-scheduled ans/hide-scheduled-tasks)
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags-todo "pecan-_project/-NEXT!"
                       ((org-agenda-overriding-header
                         (concat "PEcAn" (if ans/hide-scheduled-tasks "" " (including scheduled)")))
                        (org-agenda-todo-ignore-scheduled ans/hide-scheduled-tasks)
                        (org-agenda-sorting-strategy '(priority-down))))
            (tags-todo "-REFILE-config-reading_list-_project-_organization-forte-hector-rcmip_hector-pecan/-NEXT!"
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

  (defun ans/org-remove-headlines (backend)
    "Remove headlines with :no_title tag."
    (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                     "no_title"))

  (add-hook 'org-export-before-processing-hook #'ans/org-remove-headlines)

  (map! :map doom-leader-open-map
        (:prefix-map ("c" . "clock")
          :desc "Punch in" "+" #'ans/punch-in
          :desc "Punch out" "-" #'ans/punch-out
          :desc "Go-to" "g" #'org-clock-goto
          :desc "History" "h" #'ans/org-clock-history
          :desc "Clock out" "DEL" #'org-clock-out))

  (map! :map doom-leader-notes-map
        :desc "Reveal" "TAB" #'org-reveal
        :desc "Ivy bibtex" "b" #'ivy-bibtex)

  (map! :map org-agenda-mode-map
        :m :desc "Log mode" "@l" #'org-agenda-log-mode
        :m :desc "Day view" "@d" #'org-agenda-day-view
        :m :desc "Week view" "@w" #'org-agenda-week-view
        :m :desc "Month view" "@m" #'org-agenda-month-view)

  (map! :map evil-org-mode-map
        :n "z n" #'org-toggle-narrow-to-subtree))

;; Override counsel-org-capture. For some reason, just mapping directly fails.
(defun ans/org-capture ()
  "Just org-capture. No counsel."
  (interactive)
  (require 'org-capture)
  (org-capture))
