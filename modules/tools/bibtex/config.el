;;; tools/bibtex/config.el -*- lexical-binding: t; -*-

(def-package! helm-bibtex
  :init
  (setq bibtex-autokey-name-case-convert-function 'downcase
        bibtex-autokey-name-year-separator "_"
        bibtex-autokey-year-title-separator "_"
        bibtex-autokey-year-length 4
        bibtex-autokey-titlewords 1
        bibtex-autokey-titleword-ignore nil
        bibtex-autokey-titleword-case-convert-function 'downcase
        bibtex-completion-additional-search-fields '(journal keywords)
        bibtex-completion-display-formats
        '((t . "${author:36} ${journal:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${keywords:20}"))
        bibtex-completion-notes-template-one-file
        (string-join
         '("** TODO ${author-abbrev} (${year}): ${title}"
           ":PROPERTIES:"
           ":Custom_ID: ${=key=}"
           ":AUTHOR: ${author-or-editor}"
           ":FULL_TITLE: ${title}"
           ":JOURNAL: ${journal}"
           ":YEAR: ${year}"
           ":VOLUME: ${volume}"
           ":PAGES: ${pages}"
           ":DOI: ${doi}"
           ":URL: ${url}"
           ":END:"
           "cite:${=key=}") "\n"))
  :commands (ivy-bibtex)
  :config
  (defun bibtex-completion-fallback-candidates ()
    "Custom list of bibtex fallback options. This is the same as the
one that ships with helm-bibtex, except that
`bibtex-completion-fallback-options` comes first."
    (let ((bib-files (bibtex-completion-normalize-bibliography 'main)))
      (-concat
       bibtex-completion-fallback-options
       (--map (cons (s-concat "Create new entry in " (f-filename it))
                    `(lambda (_search-expression) (find-file ,it) (goto-char (point-max)) (newline)))
              bib-files))))
  (advice-add 'bibtex-completion-candidates
              :filter-return 'reverse)
  (map! (:map biblio-selection-mode-map
          :desc "Add to bibfile" "I" #'ans/biblio-selection-insert-end-of-bibfile)
        (:map bibtex-mode-map
          :prefix "C-c"
          :desc "Insert DOI" "i" #'doi-insert-bibtex
          :desc "Insert DOI from clipboard" "I" (lambda () (interactive) (doi-insert-bibtex (x-get-clipboard)))
          :desc "Format entry" "l" (lambda () (interactive) (bibtex-clean-entry 4))
          :desc "Edit notes" "n" (lambda () (interactive) (bibtex-completion-edit-notes (list (bibtex-completion-key-at-point))))
          :desc "Open URL" "u" (lambda () (interactive (bibtex-completion-open-url-or-doi (list (bibtex-completion-key-at-point))))))
        (:map org-mode-map
          :localleader
          (:prefix-map ("\\" . "org-ref")
            :desc "URL" "u" #'org-ref-open-url-at-point)))
  ;; Helm customizations
  (defun ans/hsplit-frame ()
    "Split window entirely below the current frame."
    (split-window (frame-root-window) nil 'below))
  (defun ans/helm-hsplit-frame (buffer &optional _resume)
    "Open new window below frame, switch to it, and open BUFFER."
    (ans/hsplit-frame)
    (evil-window-bottom-right)
    (switch-to-buffer buffer))
  (setq helm-display-function #'ans/helm-hsplit-frame
        helm-autoresize-mode t
        helm-autoresize-max-height 40)
  (map! (:map helm-map
          "TAB" #'helm-execute-persistent-action
          "C-z" #'helm-select-action)))

(def-package! org-ref
  :after org)

(def-package! citeproc-org
  :after org
  :commands citeproc-org-setup
  :init
  (setq citeproc-org-ignore-backends nil))

;; Org-ref configuration from my vanilla config.

;; (def-package! org-ref
;;   :init
;;   (setq org-ref-bibliography-notes ans/reference-notes
;;         reftex-default-bibliography `(,ans/reference-bibfile)
;;         org-ref-default-bibliography `(,ans/reference-bibfile)
;;         org-ref-pdf-directory ans/reference-dir-pdfs
;;         org-ref-bibtex-hydra-key-binding nil
;;         org-ref-note-title-format
;;         "** TODO %2a %y - %T
;; :PROPERTIES:
;; :Custom_ID: %k
;; :AUTHOR: %9a
;; :FULL_TITLE: %t
;; :JOURNAL: %j
;; :YEAR: %y
;; :VOLUME: %v
;; :PAGES: %p
;; :DOI: %D
;; :URL: %U
;; :END:
;; ")
;;   (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
;;   :general
;;   (general-def
;;     :states 'normal
;;     "\\\\" 'org-ref-bibtex-hydra/body)
;;   (general-def
;;     :states 'insert
;;     "C-\\" 'org-ref-bibtex-hydra/body)
;;   (general-def
;;     :keymaps 'org-ref-bibtex-hydra/keymap
;;     "n" '(lambda ()(interactive) (org-ref-open-bibtex-notes) (hydra-keyboard-quit)))
;;   (general-def
;;     :states 'normal
;;     :keymaps 'bibtex-mode-map
;;     :prefix "\\"
;;     "d" 'doi-insert-bibtex
;;     "D" '(lambda () (interactive) (doi-insert-bibtex (simpleclip-get-contents)))))

;; (def-package! helm-bibtex
;;   :after org-ref
;;   :init
;;   (setq bibtex-completion-bibliography ans/reference-bibfile
;;         bibtex-completion-library-path ans/reference-dir-pdfs
;;         bibtex-completion-notes-path ans/reference-notes
;;         bibtex-autokey-name-case-convert-function 'downcase
;;         bibtex-autokey-name-year-separator "_"
;;         bibtex-autokey-year-title-separator "_"
;;         bibtex-autokey-year-length 4
;;         bibtex-autokey-titlewords 1
;;         bibtex-autokey-titleword-length nil
;;         bibtex-autokey-titleword-case-convert-function 'downcase)
;;   (setq bibtex-completion-additional-search-fields '(journal keywords))
;;   (setq bibtex-completion-display-formats
;;         '((t . "${author:36} ${journal:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${keywords:20}")))
;;   (setq bibtex-completion-notes-template-one-file
;; "** TODO ${author-abbrev} (${year}): ${title}
;; :PROPERTIES:
;; :Custom_ID: ${=key=}
;; :AUTHOR: ${author-or-editor}
;; :FULL_TITLE: ${title}
;; :JOURNAL: ${journal}
;; :YEAR: ${year}
;; :VOLUME: ${volume}
;; :PAGES: ${pages}
;; :DOI: ${doi}
;; :URL: ${url}
;; :END:
;; cite:${=key=}
;; ")
;;   :commands (ivy-bibtex)
;;   :config
;;   (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
;;   (helm-add-action-to-source "Edit notes" 'ans/org-ref-notes-function helm-source-bibtex 7)
;;   (defun bibtex-completion-fallback-candidates ()
;;     "Custom list of bibtex fallback options. This is the same as the
;; one that ships with helm-bibtex, except that
;; `bibtex-completion-fallback-options` comes first."
;;     (let ((bib-files (bibtex-completion-normalize-bibliography 'main)))
;;       (-concat
;;        bibtex-completion-fallback-options
;;        (--map (cons (s-concat "Create new entry in " (f-filename it))
;;                     `(lambda (_search-expression) (find-file ,it) (goto-char (point-max)) (newline)))
;;               bib-files))))
;;   (advice-add 'bibtex-completion-candidates
;;               :filter-return 'reverse)
;;   (evil-ex-define-cmd "bib[tex]" 'helm-bibtex)
;;   (general-def
;;     :states 'emacs
;;     :keymaps 'biblio-selection-mode-map
;;     "I" 'ans/biblio-selection-insert-end-of-bibfile))
