;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "Alexey Shiklomanov"
      user-mail-address "alexey.shiklomanov@gmail.com")

(defvar ans/dropbox-dir (file-name-as-directory "~/Dropbox")
  "Absolute path to Dropbox.")

(setq make-backup-files t
      ;; Save TRAMP file backups locally
      backup-directory-alist (list (cons "." (concat doom-cache-dir "backup/"))
                                   (cons tramp-file-name-regexp (concat doom-cache-dir "backup/")))
      tramp-backup-directory-alist nil
      vc-make-backup-files t		; Backup files even if they are version controlled
      version-control t		; Version control backups
      delete-old-versions -1		; Don't delete old versions
      delete-by-moving-to-trash nil)

;; (let ((font (font-spec :family "JuliaMono" :size 12)))
;;   (if (find-find font)
;;       (progn
;;         (setq doom-font ans/my-font)
;;         (doom/reload-font))))

(setq display-line-numbers-type nil)

;; This is large to accommodate many ivy actions
;; For some reason, multiple columns of actions doesn't work...
(setq max-mini-window-height 0.75)

;; Diary -- mostly, to prevent errors about this file not existing
(setq diary-file (concat ans/dropbox-dir "Notes/journal/diary"))

(evil-ex-define-cmd "rename" 'rename-this-buffer-and-file)
(evil-ex-define-cmd "dkill" #'ans/delete-file-and-buffer)

(electric-pair-mode -1)

(load! "lisp/utils")
(load! "lisp/keymaps")
(load! "lisp/org")
(load! "lisp/bibtex")
(load! "lisp/popups")
(load! "lisp/iterm")
(load! "lisp/python")
(load! "lisp/julia")
(load! "lisp/stan")
(load! "lisp/ess")

;; Deadgrep
(use-package! deadgrep
  :commands deadgrep
  :config
  (setq deadgrep-project-root-function
        (lambda ()
          (if (projectile-project-p) (projectile-project-root) (deadgrep--project-root))))
  (map! (:map deadgrep-edit-mode-map
         :g "C-c C-c" #'deadgrep-mode)))

(use-package! dtrt-indent
  :config
  (setq dtrt-indent-hook-mapping-list (add-to-list 'dtrt-indent-hook-mapping-list
                                                   '(ess-r-mode default ess-indent-offset))))

(use-package! markdown-mode
  :mode (((rx ".Rmd" string-end) . gfm-mode)
         ((rx ".md" string-end) . gfm-mode))
  :config
  (remove-hook! 'markdown-mode-hook 'auto-fill-mode)
  (map! (:map evil-markdown-mode-map
     :i "C-t" #'evil-shift-right-line
     :i "C-d" #'evil-shift-left-line
     :i "C-?" #'ivy-bibtex
     :nv "j" #'evil-next-visual-line
     :nv "k" #'evil-previous-visual-line)))

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

;; https://github.com/abo-abo/swiper/issues/2397
(use-package! ivy-hydra
  :init
  (autoload #'ivy-hydra-read-action "ivy-hydra"))

(use-package! ivy
  :config
  (setq ivy-read-action-function #'ivy-hydra-read-action))

;; Automatically soft-wrap lines in text modes
(add-hook! 'text-mode-hook :append '(visual-line-mode  turn-off-auto-fill))

(put 'erase-buffer 'disabled nil)

(use-package! simpleclip
  :commands (simpleclip-cut simpleclip-copy simpleclip-paste simpleclip-mode)
  :config
  (simpleclip-mode))

;; CSV mode should not wrap
(add-hook! 'csv-mode-hook (toggle-truncate-lines 1))

(use-package conda
  :init
  (setq conda-env-home-directory "/Users/ashiklom/opt/anaconda3"
        conda-anaconda-home "/Users/ashiklom/opt/anaconda3"))

(after! magit
  :config
  (setq magit-delete-by-moving-to-trash nil))

(after! deft
  :config
  (setq deft-default-extension "md"))

(after! ispell
  :config
  (setq ispell-dictionary "en"))

(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")
