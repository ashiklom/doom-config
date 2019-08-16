;;; ~/.doom.d/+bibtex.el -*- lexical-binding: t; -*-

(after! helm-bibtex
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


(defun ans/hsplit-frame ()
  "Split window entirely below the current frame."
  (split-window (frame-root-window) nil 'below))

(defun ans/helm-hsplit-frame (buffer &optional _resume)
  "Open new window below frame, switch to it, and open BUFFER."
  (ans/hsplit-frame)
  (evil-window-bottom-right)
  (switch-to-buffer buffer))
