;;; fontsloth-test-util.el --- Utilities for fontsloth tests -*- lexical-binding: t -*-

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

;; fontsloth-test-util.el:
;; Utilities for fontsloth tests.
;;

;;; Code:

(require 'cl-lib)
(require 'seq)

(declare-function font-info "font.c" (name &optional frame))

(cl-defsubst fontsloth-test--find-font-path (font-name-list)
  "Return a path to the first found in FONT-NAME-LIST or nil if none."
  (seq-some (lambda (v) (when v v))
	    (cl-mapcar (lambda (name)
			 (when-let ((fuck (font-info name)))
			   (elt fuck 12)))
		       font-name-list)))

(provide 'fontsloth-test-util)
;;; fontsloth-test-util.el ends here
