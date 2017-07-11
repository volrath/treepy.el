;;; treepy.el --- Generic Tree Traversing tools           -*- lexical-binding: t -*-
;; 
;; Filename: treepy.el

;; Description: Generic Tree Traversing Tools
;; Author: Daniel Barreto <daniel.barreto.n@gmail.com>
;; Keywords: tree, node, traversing, walk, zipper
;; Created: Mon Jul 10 15:17:36 2017 (+0200)
;; Version: 1.0
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/volrath/treepy.el
;; Doc URL: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Generic tools for recursive and iterative tree traversing based on
;; clojure.walk and clojure.zip respectively.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'map)


;;; Walk (recursive tree traversing)

(defun treepy-walk (inner outer form)
  (cond
   ((and (listp form) (cdr form) (atom (cdr form))) (funcall outer (cons (funcall inner (car form))
                                                                         (funcall inner (cdr form)))))
   ((listp form) (funcall outer (mapcar inner form)))
   ((vectorp form) (funcall outer (apply #'vector (mapcar inner form))))
   ((hash-table-p form) (funcall outer (map-apply (lambda (k v) (funcall inner (cons k v))) form)))
   (t (funcall outer form))))

(defun treepy-postwalk (f form)
  (treepy-walk (apply-partially #'treepy-postwalk f) f form))

(defun treepy-prewalk (f form)
  (treepy-walk (apply-partially #'treepy-prewalk f) #'identity (funcall f form)))

(defun treepy-postwalk-demo (form)
  (let ((walk nil))
    (treepy-postwalk (lambda (x) (push x walk) x)
                     form)
    (reverse walk)))

(defun treepy-prewalk-demo (form)
  (let ((walk nil))
    (treepy-prewalk (lambda (x) (push x walk) x)
                    form)
    (reverse walk)))

(defun treepy-postwalk-replace (smap form)
  (treepy-postwalk (lambda (x) (if (map-contains-key smap x) (map-elt smap x) x))
                   form))

(defun treepy-prewalk-replace (smap form)
  (treepy-prewalk (lambda (x) (if (map-contains-key smap x) (map-elt smap x) x))
                  form))


;;; Zipper (iterative tree traversing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'treepy)

;;; treepy.el ends here
