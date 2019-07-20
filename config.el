;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;;;
;;; TODO Helm (Ivy?) bibtex
;;; TODO Org mode
;;; TODO Snippets
;;; TODO Fill function arguments
;;; TODO Deadgrep
;;; TODO dtrt-indent
;;; TODO Smartparens slurp/barf
;;; TODO Simpleclip

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

;;  Related to bibtex references
(defvar ans/reference-dir (file-name-as-directory "~/Dropbox/references")
  "Root directory for storing my bibliography.")
(defvar ans/reference-dir-pdfs (file-name-as-directory (concat ans/reference-dir "pdfs"))
  "Subdirectory containing PDF files of papers in my bibliography.")
(defvar ans/reference-bibfile (concat ans/reference-dir "library.bib")
  "Full path to my personal bibtex file.")
(defvar ans/reference-notes (concat ans/reference-dir "notes.org")
  "Full path to reference-related notes.")

(map! (:map evil-window-map
        "o" #'ace-window
        "z" #'doom/window-enlargen
        "-" #'evil-window-split
        "\\" #'evil-window-vsplit
        "+" #'evil-window-increase-height
        "_" #'evil-window-decrease-height
        "d" #'+workspace/close-window-or-workspace)

      (:map doom-leader-project-map
        :desc "Find project file or buffer" "p" #'counsel-projectile
        :desc "Find file in project" "f" #'+ivy/projectile-find-file
        :desc "Switch project" "o" #'counsel-projectile-switch-project
        :desc ""
        "/" nil)

      (:map doom-leader-workspace-map
        :desc "Switch workspace" "TAB" #'+workspace/switch-to
        :desc "Show tab bar" "." #'+workspace/display)

      ;; (:map company-active-map
      ;;   :i "C-l" #'+company/complete)

      :i "C-l" #'+company/complete
      :n "g RET" #'eval-defun)


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

(def-package! markdown-mode
  :mode ((rx ".Rmd" string-end) . gfm-mode))

;; Persp-mode bugfix
;; See issue https://github.com/hlissner/doom-emacs/issues/1525
(after! persp-mode
  (persp-def-buffer-save/load
   :tag-symbol 'def-indirect-buffer
   :predicate #'buffer-base-buffer
   :save-function (lambda (buf tag vars)
                    (list tag (buffer-name buf) vars
                          (buffer-name (buffer-base-buffer buf))))
   :load-function (lambda (savelist &rest _rest)
                    (cl-destructuring-bind (buf-name _vars base-buf-name &rest _)
                        (cdr savelist)
                      (push (cons buf-name base-buf-name)
                            +workspaces--indirect-buffers-to-restore)
                      nil))))
