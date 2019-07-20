;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(map! (:map (company-mode-map
             :i "C-l" #'company-complete))
      (:map evil-window-map
        "o" #'ace-window
        "z" #'doom/window-enlargen))
