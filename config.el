;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Alexey Shiklomanov"
      user-mail-address "alexey.shiklomanov@gmail.com")

(setq doom-localleader-key "\\")

(setq backup-directory-alist '(("." . "~/.emacs.d/.local/backups"))
      delete-old-versions -1		; Don't delete old versions
      version-control t			; Version control backups
      make-backup-files t
      vc-make-backup-files t		; Backup files even if they are version controlled
      auto-save-file-name-transforms '((".*" "~/.emacs.d/.local/auto-save-list/" t))) ; Save file name changes

(setq display-line-numbers-type nil)

(defvar ans/dropbox-dir (file-name-as-directory "~/Dropbox")
  "Absolute path to Dropbox.")

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
        "C-p" #'evil-window-prev
        :desc "Raise popup window" "~" #'+popup/raise)

      (:map doom-leader-map
        :desc "Shell command" "!" #'shell-command
        :desc "Redraw frame" "&" (lambda! (redraw-frame)))

      (:map doom-leader-project-map
        :desc "Find project file or buffer" "p" #'counsel-projectile
        :desc "Find file in project" "f" #'+ivy/projectile-find-file
        :desc "Switch project" "o" #'counsel-projectile-switch-project)

      (:map doom-leader-workspace-map
        :desc "Switch workspace" "TAB" #'+workspace/switch-to
        :desc "Show tab bar" "." #'+workspace/display)

      :i "C-l" #'+company/complete
      :n "g RET" #'eval-defun

      :i "C-0" (lambda! (sp-slurp-hybrid-sexp) (sp-end-of-sexp))
      :i "C-s" (lambda! (upcase-word -1))

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

(load! "+org")
(load! "+bibtex")

;; CSL locale files. If this doesn't exist, clone them from:
;; https://github.com/citation-style-language/locales
(setq citeproc-org-locales-dir "~/.config/csl-locales")

;; Diary -- mostly, to prevent errors about this file not existing
(setq diary-file (concat ans/dropbox-dir "Notes/journal/diary"))

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
          :desc "R" "r" (lambda! (doom/open-scratch-buffer) (R-mode))
          :desc "Lisp interaction" "l" (lambda! (doom/open-scratch-buffer) (lisp-interaction-mode))
          :desc "Python" "p" (lambda! (doom/open-scratch-buffer) (python-mode))
          :desc "Bash" "b" (lambda! (doom/open-scratch-buffer) (sh-mode)))))

(map! :nv "gz]" #'evil-mc-skip-and-goto-next-match
      :nv "gz[" #'evil-mc-skip-and-goto-prev-match
      :n "gb" #'evil-next-buffer
      :n "gB" #'evil-prev-buffer)

(use-package! projectile
  :config
  ;; Allow this to be set via dir-locals without complaint
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp))
