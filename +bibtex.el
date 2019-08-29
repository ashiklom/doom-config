;;; ~/.doom.d/+bibtex.el -*- lexical-binding: t; -*-

;;  Related to bibtex references
(defvar ans/reference-dir (file-name-as-directory (concat ans/dropbox-dir "references"))
  "Root directory for storing my bibliography.")
(defvar ans/reference-bibfile (concat ans/reference-dir "library.bib")
  "Main bibliography file.")
(defvar ans/reference-notes (concat ans/reference-dir "notes.org")
  "References notes file.")
(defvar ans/reference-pdfs (file-name-as-directory (concat ans/reference-dir "pdfs"))
  "References PDFs directory.")

(use-package! helm-bibtex
  :config
  (setq bibtex-completion-library-path ans/reference-pdfs
        bibtex-completion-notes-path ans/reference-notes
        bibtex-completion-bibliography ans/reference-bibfile
        bibtex-autokey-titleword-length 15
        org-ref-default-bibliography (list bibtex-completion-bibliography))
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
           "cite:${=key=}") "\n")
        helm-display-function #'ans/helm-hsplit-frame
        helm-autoresize-mode t
        helm-autoresize-mode 40)
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
            :desc "URL" "u" #'org-ref-open-url-at-point))
        (:map helm-map
          "TAB" #'helm-execute-persistent-action
          "C-z" #'helm-select-action)))

(use-package! org-ref
  :config
  (setq org-ref-bibliography-notes ans/reference-notes
        reftex-default-bibliography (list ans/reference-bibfile)
        org-ref-default-bibliography (list ans/reference-bibfile)
        org-ref-pdf-directory ans/reference-pdfs))

(defun ans/hsplit-frame ()
  "Split window entirely below the current frame."
  (split-window (frame-root-window) nil 'below))

(defun ans/helm-hsplit-frame (buffer &optional _resume)
  "Open new window below frame, switch to it, and open BUFFER."
  (ans/hsplit-frame)
  (evil-window-bottom-right)
  (switch-to-buffer buffer))
