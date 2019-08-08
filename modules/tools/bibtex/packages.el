;; -*- no-byte-compile: t; -*-
;;; tools/bibtex/packages.el

;; (package! helm-bibtex)
(package! helm-bibtex :recipe (:fetcher github :repo "ashiklom/helm-bibtex" :branch "author-abbrev"))
(package! org-ref)
(package! citeproc)
(package! citeproc-org :recipe (:fetcher github :repo "andras-simonyi/citeproc-org"))
