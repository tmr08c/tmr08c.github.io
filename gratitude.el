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

(defun extract-gratitude-entries (file) "Get gratitude section of daily files."
       (find-file file)
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
                                     (lambda (item)
                                       (org-element-map (org-element-contents item) 'paragraph

                                         ;; https://www.reddit.com/r/orgmode/comments/7qwmbo/comment/dstsmpw/?utm_source=reddit&utm_medium=web2x&context=3
                                         ;; I wish I knew about interpret sooner
                                                   ;; (string-trim (org-no-properties (car (org-element-contents p))))
                                                 (lambda (p) (string-trim (org-element-interpret-data p)))))))))
         (kill-buffer)
         items))

(defun tr/format-org-date (date)
  (org-timestamp-format date "\[%Y-%02m-%02d %3a %02H:%02M\]"))

(defun tr/time-stamp-to-org-timestamp (ts)
  "Taken from `org-timestamp-from-time` - the original function used
     `decode-time`, which doesn't work with our timestamp, so we use
     `parse-time-string` instead"
  (pcase-let ((`(,_ ,minute ,hour ,day ,month ,year . ,_) (parse-time-string ts)))
    (org-element-create 'timestamp
                        (list :type 'active
                              :year-start year
                              :month-start month
                              :day-start day
                              :hour-start hour
                              :minute-start minute))))

(defun tr/file-creation-time-from-name (fpath)
  "Extract a timestamp from the file name. Relies on files having the format
      'YYYYMMDDHHMMSS-*' (the default org-roam node filename)."
  (let ((filename (file-name-base fpath)))
    (tr/format-org-date
     (tr/time-stamp-to-org-timestamp
      (replace-regexp-in-string
       "\\([[:digit:]]\\{4\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)-.*"
       "\\1-\\2-\\3 \\4:\\5:\\6"
       filename)))))

(let ((output-buffer "*gratitude*")
      (files (directory-files (concat org-roam-directory org-roam-dailies-directory) 'full "2022.+\.org")))
  (split-window)
  (other-window 1)
  (generate-new-buffer output-buffer)
  (set-buffer output-buffer)
  (erase-buffer)
  (insert "|Date|Message|Category|\n|-|-|-|\n")
  (dolist (file files)
    (let ((date (tr/file-creation-time-from-name file)))
      (let ((entries (extract-gratitude-entries file)))
        (set-buffer output-buffer)
        (dolist (entry entries)
          (insert "|" date "|" entry "||\n")))))
  (switch-to-buffer output-buffer)
  (org-mode)
  (org-table-align))
;; (provide 'gratitude)
;;; gratitude.el ends here
