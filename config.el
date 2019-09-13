;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Alexey Shiklomanov"
      user-mail-address "alexey.shiklomanov@gmail.com")

(defvar ans/dropbox-dir (file-name-as-directory "~/Dropbox")
  "Absolute path to Dropbox.")

(setq backup-directory-alist '(("." . "~/.emacs.d/.local/backups"))
      delete-old-versions -1		; Don't delete old versions
      version-control t			; Version control backups
      make-backup-files t
      vc-make-backup-files t		; Backup files even if they are version controlled
      auto-save-file-name-transforms '((".*" "~/.emacs.d/.local/auto-save-list/" t))) ; Save file name changes

(setq display-line-numbers-type nil)

;; Diary -- mostly, to prevent errors about this file not existing
(setq diary-file (concat ans/dropbox-dir "Notes/journal/diary"))

(evil-ex-define-cmd "rename" 'rename-this-buffer-and-file)
(evil-ex-define-cmd "dkill" #'ans/delete-file-and-buffer)

(load! "lisp/utils")
(load! "lisp/keymaps")
(load! "lisp/org")
(load! "lisp/bibtex")

;; Deadgrep
(use-package! deadgrep
  :commands (deadgrep)
  :config
  (setq deadgrep-project-root-function
        (lambda ()
          (if (projectile-project-p) (projectile-project-root) (deadgrep--project-root)))))

(use-package! dtrt-indent
  :config
  (setq dtrt-indent-hook-mapping-list (add-to-list 'dtrt-indent-hook-mapping-list
                                                   '(ess-r-mode default ess-indent-offset))))

(use-package! markdown-mode
  :mode ((rx ".Rmd" string-end) . gfm-mode)
  :config
  (remove-hook! 'markdown-mode-hook 'auto-fill-mode))

(use-package! multi-line
  :config
  (setq-default multi-line-current-strategy
                (multi-line-strategy
                 :respace (multi-line-default-respacers
                           (make-instance multi-line-always-newline)))))

(use-package! pandoc-mode
  :hook ((markdown-mode gfm-mode poly-markdown-mode) . pandoc-mode))

(use-package! projectile
  :config
  ;; Allow this to be set via dir-locals without complaint
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp))

;; Modify popup defaults
(setq +popup-defaults
      (list :side 'bottom
            :height 0.16
            :width 0.3
            :quit t
            :select #'ignore
            ;; "Time to live"
            :ttl 5))

;; Don't quit the compilation buffer if it's still running
(set-popup-rule!
  (rx string-start (zero-or-more blank)
      "*" (zero-or-more blank) "compilation")
  :select nil
  :quit (lambda (_window) (not compilation-in-progress)))

;; Automatically soft-wrap lines in text modes
(add-hook! 'text-mode-hook :append '(visual-line-mode  turn-off-auto-fill))

;; Disable smartparens mode in these modes because of interference with electric pairs mode
(add-hook! '(markdown-mode-hook nxml-mode-hook) (smartparens-mode -1))

(put 'erase-buffer 'disabled nil)
