;;; treepy-walker-tests.el --- Generic tree traversal tools           -*- lexical-binding: t -*-
;; 
;; Filename: treepy.el-walker-test.el
;;
;; Description: Generic Tree Traversal Tools
;; Author: Daniel Barreto <daniel.barreto.n@gmail.com>
;; Created: Mon Jul 10 15:17:36 2017 (+0200)
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/volrath/treepy.el
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Generic tools for recursive and iterative tree traversal based on
;; clojure.walk and clojure.zip respectively.
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

(require 'ert)
(require 'treepy)

(ert-deftest treepy-identity-walk-test ()
  (let ((colls `((1 2 3)
                 [1 2 3]
                 ((:a . 1) (:b . 2) (:c . 3)))))
    (dolist (coll colls)
      (let ((walked (treepy-walk #'identity #'identity coll)))
        (should (equal coll walked))
        (should (equal (type-of coll) (type-of walked))))))
  (let ((hash (make-hash-table :test #'equal)))
    (puthash :a 1 hash)
    (puthash :b 2 hash)
    (puthash :c 3 hash)
    (should (equal (map-pairs hash)
                   (treepy-walk #'identity #'identity hash)))))

(ert-deftest treepy-walk-test ()
  (let ((colls '((1 2 3)
                 [1 2 3])))
    (dolist (coll colls)
      (let ((walked (treepy-walk #'1+ (lambda (x) (seq-reduce #'+ x 0)) coll)))
        (should (equal (seq-reduce #'+ (mapcar #'1+ coll) 0) walked))
        (should (equal walked 9)))))
  ;; map-like structures
  (let ((al '((:a . 1) (:b . 2) (:c . 3)))
        (hash (make-hash-table :test #'equal)))
    (puthash :a 1 hash)
    (puthash :b 2 hash)
    (puthash :c 3 hash)
    (dolist (map (list al hash))
      (let* ((inc-val (lambda (x) (cons (car x) (1+ (cdr x)))))
             (walked (treepy-walk inc-val (lambda (x) (seq-reduce #'+ (map-values x) 0)) map)))
        (should (equal (seq-reduce #'+ (mapcar #'1+ (map-values map)) 0) walked))
        (should (equal walked 9))))))

(ert-deftest treepy-prewalk-order-test ()
  (let ((hash (make-hash-table :test #'equal)))
    (puthash :b 5 hash)
    (puthash :c 6 hash)
    (let ((tree `(1 2 [3 4] ((:a . ,hash) (:d . 7)))))
      (should (equal (treepy-prewalk-demo tree)
                     `((1 2 [3 4] ((:a . ,hash) (:d . 7)))
                       1
                       2
                       [3 4]
                       3
                       4
                       ((:a . ,hash) (:d . 7))
                       (:a . ,hash)
                       :a
                       ,hash
                       (:b . 5)
                       :b
                       5
                       (:c . 6)
                       :c
                       6
                       (:d . 7)
                       :d
                       7)))
      ;; tree should remain the same
      (should (equal tree `(1 2 [3 4] ((:a . ,hash) (:d . 7))))))))

(ert-deftest treepy-postwalk-order-test ()
  (let ((hash (make-hash-table :test #'equal)))
    (puthash :b 5 hash)
    (puthash :c 6 hash)
    (let ((tree `(1 2 [3 4] ((:a . ,hash) (:d . 7)))))
      (should (equal (treepy-postwalk-demo tree)
                     '(1
                       2
                       3
                       4
                       [3 4]
                       :a
                       :b
                       5
                       (:b . 5)
                       :c
                       6
                       (:c . 6)
                       ((:b . 5) (:c . 6))
                       (:a . ((:b . 5) (:c . 6)))
                       :d
                       7
                       (:d . 7)
                       ((:a . ((:b . 5) (:c . 6))) (:d . 7))
                       (1 2 [3 4] ((:a . ((:b . 5) (:c . 6))) (:d . 7))))))
      ;; tree should remain the same
      (should (equal tree `(1 2 [3 4] ((:a . ,hash) (:d . 7))))))))

(ert-deftest treepy-prewalk-replace-test ()
  (should (equal (treepy-prewalk-replace '((:a . :b)) '(:a (1 2 3 :c :a) ((:a . :c) (:c . :a))))
                 '(:b (1 2 3 :c :b) ((:b . :c) (:c . :b))))))

(ert-deftest treepy-postwalk-replace-test ()
  (should (equal (treepy-postwalk-replace '((:a . :b)) '(:a (1 2 3 :c :a) ((:a . :c) (:c . :a))))
                 '(:b (1 2 3 :c :b) ((:b . :c) (:c . :b))))))

(provide 'treepy-walker-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treepy-walker-tests.el ends here
