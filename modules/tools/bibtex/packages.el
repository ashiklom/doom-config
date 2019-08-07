;; -*- no-byte-compile: t; -*-
;;; tools/bibtex/packages.el

(package! helm-bibtex)
(package! org-ref)
(package! citeproc)
(package! citeproc-org :recipe (:fetcher github :repo "andras-simonyi/citeproc-org"))
