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

(defun gratitude () "Get gratitude section of daily files."
       (find-file-other-window "~/Dropbox/Notes/org/roam/daily/2022-11-18.org")
       (let ((lists
              (org-element-map (org-element-parse-buffer) 'plain-list
                ;; TODO also fiter on parent headling
                ;; alternatively, I may be able to get the headline first and then get the children content
                (lambda (list)
                  ;; (print list)
                  ;; (print (length list))
                  (cl-destructuring-bind (type plist . children) list
                    ;; (print "TYPE:")
                    ;; (print type)
                    ;; (print "==============================")
                    ;; (print "PLIST:")
                    ;; (print plist)
                    ;; (print "==============================")
                    ;; (print "CHILDREN:")
                    ;; (print children)
                    ;; (print "==============================")
                    (and
                     (string-equal (org-element-property :type list) 'ordered)
                     (org-element-map (org-element-property :parent list) 'headline
                       (lambda (headline)
                         (print headline)
                         (and
                          (print (org-element-property :raw-value headline))
                          (string-equal (org-element-property :raw-value headline) "Grateful or Excited About")
                          list)))
                     ;; (print (org-element-property :parent list))
                     list))))))
         (print "hum..")
         (print (format "list has %s elements" (length lists)))
         )
       ;; (other-window 2)
       ;; (goto-char (point-max))
       ;; (insert (format "%s" org-ast))
       )

(defun gratitude-from-headline () "Get gratitude section of daily files."
       (find-file-other-window "~/Dropbox/Notes/org/roam/daily/2022-11-18.org")
       (let* ((lists
              (org-element-map (org-element-parse-buffer) 'headline
                ;; TODO also fiter on parent headling
                ;; alternatively, I may be able to get the headline first and then get the children content
                (lambda (headline)
                  ;; (print list)
                  ;; (print (length list))
                  ;; (cl-destructuring-bind (type plist . children) headline
                  ;; (print "TYPE:")
                  ;; (print type)
                  ;; (print "==============================")
                  ;; (print "PLIST:")
                  ;; (print plist)
                  ;; (print "==============================")
                  ;; (print "CHILDREN:")
                  ;; (print children)
                  ;; (print "==============================")
                  (and
                   (string-equal (org-element-property :raw-value headline) "Grateful or Excited About")
                   (org-element-map headline 'plain-list
                     (lambda (list)
                       (and
                        (string-equal (org-element-property :type list) 'ordered)
                        list
                        )))
                   ;; (print (org-element-property :parent list))
                   ))))
             (items (flatten-list (org-element-map lists 'item
                      (lambda (item) (org-element-map (org-element-contents item) 'paragraph
                                  (lambda (p) (string-trim (org-no-properties (car (org-element-contents p)))))))))))
         ;; )

         (print "hmm..")
         ;; (print lists)
         (print (format "list has %s elements" (length items)))
         (print items)
         )
       ;; (other-window 2)
       ;; (goto-char (point-max))
       ;; (insert (format "%s" org-ast))
       )

(gratitude-from-headline)
;; (provide 'gratitude)
;;; gratitude.el ends here

(gratitude)

;; (defun tr/test ()
;;   "testing function writing"
;;   (with-output-to-temp-buffer "*debug org fun*"
;;     (org-element-map (org-element-parse-buffer) 'headline
;;       (lambda (headline)
;;         (if (string= (org-element-property :raw-value headline) "Grateful or Excited About")
;;             (org-element-map (org-element-contents headline) 'plain-list
;;               (lambda (list)
;;                 (if (eq (org-element-property :parent list) headline)
;;                      (print (org-element-contents list))
;;                      (org-element-map (org-element-contents list) 'item
;;                        (lambda (item)
;;                          (print (org-element-contents item)))))))
;;           )))))

;; (tr/test)
