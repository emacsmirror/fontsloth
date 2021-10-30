(define-package "fontsloth" "0.15.1"
  "the slowest font renderer in the world, written in pure elisp"
  '((cl-lib "0.5")
    (emacs "26.1")
    (f "0.20.0")
    (logito "0.1")
    (pcache "0.5"))
    :keywords
    '("font" "glyph" "ttf" "otf" "parsing" "rasterization")
    :authors
    '(("jo" . "jo.gay@mailfence.com"))
    :maintainer
    '("jo" . "jo.gay@mailfence.com"))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;;; fontsloth-pkg.el ends here
