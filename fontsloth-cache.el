;;; fontsloth-cache.el --- Fontsloth pcache -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.15.3
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

;; fontsloth-cache.el:
;; Provides cache functionality for fontsloth
;;

;;; Code:

(require 'benchmark)
(require 'eieio)
(require 'pcache)

(defconst fontsloth-cache-internal-version-constant 0)
(defconst fontsloth-cache-version-constant
  (format "%s/fontsloth-%s"
          pcache-version-constant fontsloth-cache-internal-version-constant)
  "Version constant used for cache invalidation.")

(defcustom fontsloth-cache-save-delay 5
  "The delay in seconds for updating the `fontsloth-cache' after a put."
  :type 'integer
  :group 'fontsloth)

(defclass fontsloth-cache-pcache (pcache-repository)
  ((entries :initarg :entries :initform (make-hash-table :test 'equal))))

(oset-default 'fontsloth-cache-pcache version-constant
              fontsloth-cache-version-constant)

;;; TODO: separate caches for fonts and for raster output

(defvar fontsloth-cache-pcache-path-name "fontsloth"
  "The pcache pathname for `fontsloth-cache'.")
(defvar fontsloth-cache nil
  "The instance of `fontsloth-cache', a symbol `pcache-repository'.")

(defun fontsloth-cache-init ()
  "Initialize the cache."
  (setq fontsloth-cache
        (let ((cache
               (make-instance
                'fontsloth-cache-pcache
                :object-name (format "%s" fontsloth-cache-pcache-path-name))))
          (oset cache save-delay fontsloth-cache-save-delay)
          cache)))

(defun fontsloth-cache--watch-save-delay (sym nval oper where)
  "Update the cache save delay when the customization value is set.
SYM symbol
NVAL new value
OPER type of operation
WHERE where it occurs"
  (ignore sym)
  (when (and fontsloth-cache (not where) (eq 'set oper))
    (oset fontsloth-cache save-delay nval)))

(add-variable-watcher 'fontsloth-cache-save-delay
                      #'fontsloth-cache--watch-save-delay)

(provide 'fontsloth-cache)
;;; fontsloth-cache.el ends here
