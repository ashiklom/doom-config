;;; ~/.doom.d/lisp/popups.el -*- lexical-binding: t; -*-

;; Modify popup defaults
(setq +popup-defaults
      (list :side 'bottom
            :height 0.16
            :width 0.3
            :quit t
            :select #'ignore
            ;; "Time to live"
            :ttl 5))

;; ESS or Python inferior buffers
(set-popup-rule!
  (rx string-start "*" (or "R" "julia" "Python") (any "*" ":" "["))
  ;; Only quit these manually
  :height 0.3
  :quit nil
  :slot 2
  :select nil)

(set-popup-rule!
  (rx string-start (zero-or-more blank)
      "*" (zero-or-more blank) "compilation")
  :select nil
  :width 0.5
  :height 0.16
  :slot 1
  ;; Don't quit if it's still running
  :quit (lambda (_window) (not compilation-in-progress)))
