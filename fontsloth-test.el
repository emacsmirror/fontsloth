;;; fontsloth-test.el --- Fontsloth tests -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.19.1
;; Homepage: https://github.com/jollm/fontsloth
;; Keywords: data, font, ttf, otf

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; fontsloth-test.el:
;; Tests for fontsloth.el
;;

;;; Code:

(require 'ert)
(require 'f)
(require 'fontsloth)
(require 'fontsloth-test-util)

(defvar fontsloth-test--font (fontsloth-test--find-font-path '("DejaVu Sans")))
(defvar fontsloth-test--glyph-id 315) ; should correspond to ?Ź

(defvar fontsloth-test--expected-pixmap
  [0 0 0 0 29 7 0 0 0 0 0 71 181 4 0 0 0 0 0 84 13 0 0 0 62 191 191 191 191 191
   191 104 20 63 63 63 63 109 252 68 0 0 0 0 18 221 117 0 0 0 0 3 187 166 0 0 0
   0 0 142 206 9 0 0 0 0 93 234 30 0 0 0 0 52 242 63 0 0 0 0 23 227 108 0 0 0 0
   0 116 255 254 254 254 254 254 173])

(defvar fontsloth-test--post-invalidate? nil
  "For use in fixtures to ensure the current state of cache is left unchanged.")

;; e.g. emacs -batch -l ert -l <test-file> --eval
;; "(ert-run-tests-batch-and-exit fontsloth-test--order)"
(defvar fontsloth-test--order '(member fontsloth-test-font-load-rasterize
                                       fontsloth-test-font-pcache-rasterize))

(defun fontsloth-test--pre-fixture (body)
  "A fixture to run before BODY."
  (unwind-protect
      (progn (setq fontsloth-test--post-invalidate?
                   (not (fontsloth-cache-get fontsloth-test--font)))
             (funcall body))))

(defun fontsloth-test--post-fixture (body)
  "A fixture to run after BODY."
  (unwind-protect (funcall body)
    (when fontsloth-test--post-invalidate?
      (fontsloth-cache-invalidate fontsloth-test--font))))

(ert-deftest fontsloth-test-font-load-rasterize ()
  "Test loading a font and then rasterizing a glyph."
  (fontsloth-test--pre-fixture
   (lambda ()
     (skip-unless (f-exists-p fontsloth-test--font))
     (pcase-let* ((font (fontsloth-load-font fontsloth-test--font
                                             :cache 'reload))
                  ((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                   (fontsloth-font-rasterize
                    font fontsloth-test--glyph-id 12.0)))
       (should (eq (fontsloth-metrics-width metrics) 8))
       (should (eq (fontsloth-metrics-height metrics) 12))
       (should (equal pixmap fontsloth-test--expected-pixmap))))))

(ert-deftest fontsloth-test-font-pcache-rasterize ()
  "Test rasterizing a glyph from a cached font."
  (fontsloth-test--post-fixture
   (lambda ()
     (skip-unless (f-exists-p fontsloth-test--font))
     (pcase-let* ((font (fontsloth-load-font fontsloth-test--font))
                  ((cl-struct fontsloth-metrics+pixmap metrics pixmap)
                   (fontsloth-font-rasterize
                    font fontsloth-test--glyph-id 12.0)))
       (should (eq (fontsloth-metrics-width metrics) 8))
       (should (eq (fontsloth-metrics-height metrics) 12))
       (should (equal pixmap fontsloth-test--expected-pixmap))))))

(provide 'fontsloth-test)
;;; fontsloth-test.el ends here
