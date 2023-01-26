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

;; START write out each entity somewhere
;;   - tmp buffer?
;;   - simply list, one line per entry?
;;     Ideally, this becomes simple to re-fetch and re-format, so if I want to
;;     say, format it in some way, I can re-run the function. This means I
;;     shouldn't stress too much about this.

(defun extract-gratitude-entries (file) "Get gratitude section of daily files."
       (find-file-other-window file)
       (let* (
              (lists (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (headline)
                         (and (string-equal (org-element-property :raw-value headline) "Grateful or Excited About")
                              (org-element-map headline 'plain-list
                                (lambda (list)
                                  (and
                                   (string-equal (org-element-property :type list) 'ordered)
                                   list)))))))
              (items (flatten-list (org-element-map lists 'item
                                     (lambda (item) (org-element-map (org-element-contents item) 'paragraph
                                                 (lambda (p) (string-trim (org-no-properties (car (org-element-contents p)))))))))))
         items))


;; todo
;; - [ ] try updating window management
;;   - split window ahead of time
;;   - try changing from find-file-other-window
;;   - close the buffer of the file after I read it
(let ((files (directory-files (concat org-roam-directory org-roam-dailies-directory) 'full "2022-01-1.+\.org")))
  (dolist (file files)
    (dolist (entry (extract-gratitude-entries file))
      (switch-to-buffer "*gratitude*")
      (insert entry "\n"))))
;; (provide 'gratitude)
;;; gratitude.el ends here
