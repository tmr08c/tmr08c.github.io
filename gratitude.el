;;; gratitude.el --- Parse grateful/excited section of my daily roam files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Troy Rosenberg

;; Author: Troy Rosenberg <tmr08c@gmail.com>
;; Keywords: local, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'org-element)

(progn
  (find-file-other-window "~/Dropbox/Notes/org/roam/daily/2022-11-18.org")
  (let ((lists (org-element-map (org-element-parse-buffer) 'plain-list
                 (lambda (list)
                   (map-do (lambda (element) print element) list)
                   ))))
       (other-window 2)
       (goto-char (point-max))
       (insert (format "%s" org-ast))))

(provide 'gratitude)
;;; gratitude.el ends here
