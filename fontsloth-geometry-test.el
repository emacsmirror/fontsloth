;;; fontsloth-geometry-test.el --- Fontsloth geometry tests -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.20.0
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

;; fontsloth-geometry-test.el:
;; Tests for fontsloth geometry outliner.
;;

;;; Code:

(require 'ert)
(require 'fontsloth-geometry)

(ert-deftest fontsloth-seg-stack-push-pop ()
  "Check the fixed object pool stack against a linked list stack."
  (let ((fontsloth-seg-stack--size 10)
        (point-a (fontsloth-point-create :x 1.0 :y 2.0))
        (point-c (fontsloth-point-create :x -1.0 :y 4.0)))
    (cl-loop repeat 10
             with stack = (fontsloth-seg-stack-create)
             with check-stack
             for seg = (fontsloth-seg-stack--pop stack)
             for check = (pop check-stack)
             do
             (should (equal check seg))
             (push (fontsloth-segment-create :a point-a :at 0.0
                                             :c point-c :ct 1.0)
                   check-stack)
             (fontsloth-seg-stack--push
              point-a 0.0 point-c 1.0 stack)
             (push (fontsloth-segment-create :a point-c :at 1.0
                                             :c point-a :ct 0.0)
                   check-stack)
             (fontsloth-seg-stack--push
              point-c 1.0 point-a 0.0 stack)
             when seg do
             (push (fontsloth-segment-create
                    :a (fontsloth-segment-c check) :at 1.0
                    :c (fontsloth-segment-a check) :ct 0.0)
                   check-stack)
             (fontsloth-seg-stack--push
              (fontsloth-segment-c seg) 1.0
              (fontsloth-segment-a seg) 0.0 stack)
             finally do
             (should (eq 30 (length (fontsloth-seg-stack-pool stack))))
             (should (eq 20 (length check-stack)))
             (should (equal (cl-loop for seg = (pop check-stack)
                                     while seg collect seg)
                            (cl-loop for seg = (fontsloth-seg-stack--pop stack)
                                     while seg collect seg))))))

(provide 'fontsloth-geometry-test)
;;; fontsloth-geometry-test.el ends here
