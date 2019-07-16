;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(map! (:map (company-mode-map
             "C-l" #'company-complete))
      (:map override
        :n "S" #'save-buffer))
