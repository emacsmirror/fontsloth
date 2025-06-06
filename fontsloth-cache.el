;;; fontsloth-cache.el --- Fontsloth pcache -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.17.2
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

(require 'cl-lib)
(require 'eieio)
(require 'pcache)

(defconst fontsloth-cache-internal-version-constant 2)
(defconst fontsloth-cache-version-constant
  (format "%s/fontsloth-%s"
          pcache-version-constant fontsloth-cache-internal-version-constant)
  "Version constant used for cache invalidation.")

(defcustom fontsloth-cache-idle-save-time 60
  "Idle time in seconds after which cache entries are saved.

A nil value or zero indicates to save immediately."
  :type '(choice natnum (const :tag "Immediate" nil))
  :group 'fontsloth)

(defclass fontsloth-cache-pcache (pcache-repository)
  ((entries :initarg :entries :initform (make-hash-table :test 'equal))))

(oset-default 'fontsloth-cache-pcache version-constant
              fontsloth-cache-version-constant)

;;; TODO: separate caches for fonts and for raster output

(defconst fontsloth-cache--pcache-prefix "fontsloth-"
  "The pcache file name prefix for all fontsloth related repositories.")

(defsubst fontsloth-cache--pcache-path-name (file-path)
  "Given FILE-PATH, generate the path name to use for pcache storage."
  (format "%s%s"
          fontsloth-cache--pcache-prefix
          (replace-regexp-in-string "[/\\.]" "_" file-path)))

(defun fontsloth-cache-put (file-path font)
  "Put `fontsloth-font' FONT in cache using lookup key FILE-PATH."
  (let* ((repo-name (fontsloth-cache--pcache-path-name file-path))
         (cache (or (gethash repo-name *pcache-repositories*)
                    (make-instance 'fontsloth-cache-pcache
                                   :object-name repo-name))))
    (pcache-put cache file-path font)
    ;; fonts objects are read-only, so just try once to save them
    (run-with-idle-timer
     (or fontsloth-cache-idle-save-time 0) nil
     (lambda ()
       (pcache-save cache t)))))

(defun fontsloth-cache-get (file-path)
  "Retrieve a cached `fontsloth-font' corresponding to FILE-PATH, if any.

The value is nil if no entry is found."
  (when-let* ((repo-name (fontsloth-cache--pcache-path-name file-path))
              (cache (gethash repo-name *pcache-repositories*)))
    (pcache-get cache file-path)))

(defun fontsloth-cache-invalidate (file-path)
  "Invalidate a single font corresponding to FILE-PATH."
  (when-let* ((repo-name (fontsloth-cache--pcache-path-name file-path))
              (cache (gethash repo-name *pcache-repositories*)))
    (pcache-invalidate cache file-path)
    (pcache-destroy-repository repo-name)))

(defun fontsloth-cache-clear ()
  "Clear all fontsloth related repositories from pcache."
  (cl-loop for k being the hash-keys of *pcache-repositories*
           when (string-prefix-p fontsloth-cache--pcache-prefix k)
           do (pcache-destroy-repository k)))

(defun fontsloth-cache--avoid-save-on-kill (orig-fun &rest args)
  "Avoid saving fontsloth related pcache repositories when killing Emacs.

The entries are read-only and saved onced on first put. This around
advice should preserve the existing pcache logic for all other
repositories.

ORIG-FUN ::= the function being advised
ARGS ::= any arguments passed to that function"
  (let ((fontsloth-repos
         (cl-loop for k being the hash-keys of *pcache-repositories*
                  when (string-prefix-p fontsloth-cache--pcache-prefix k)
                  collect
                  `(,k . ,(gethash k *pcache-repositories*))
                  and do
                  (remhash k *pcache-repositories*))))
    (apply orig-fun args)
    (cl-loop for (k . v) in fontsloth-repos
             do
             (puthash k v *pcache-repositories*))))

(advice-add 'pcache-kill-emacs-hook :around #'fontsloth-cache--avoid-save-on-kill)

(provide 'fontsloth-cache)
;;; fontsloth-cache.el ends here
