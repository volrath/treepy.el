;;; treepy-zipper-tests.el --- Generic tree traversing tools           -*- lexical-binding: t -*-
;; 
;; Filename: treepy.el-zipper-test.el
;;
;; Description: Generic Tree Traversing Tools
;; Author: Daniel Barreto <daniel.barreto.n@gmail.com>
;; Created: Mon Jul 10 15:17:36 2017 (+0200)
;; Version: 1.0
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/volrath/treepy.el
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

(defvar list-tree '((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))
(defvar vector-tree [[a * b] + [c * d]])

(defmacro assert-traversing-with-persistent-zipper (binding-vector &rest body)
  (declare (indent defun)
           (debug ((symbolp form) body)))
  (seq-let [var-name zip-constructor] binding-vector
    `(let ((,var-name ,zip-constructor))
       ,@(mapcar (lambda (assert-pair)
                   (seq-let [navigation expected] assert-pair
                     `(progn
                        (setq ,var-name ,navigation)
                        (let ((loc (car ,var-name)))
                          (should (equal loc ,expected))))))
                 (seq-partition body 2)))))

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result
            (if (sequencep form)
                `(,(car form) ,result ,@(cdr form))
              `(,form ,result))))))

(ert-deftest treepy-roundtrip-test ()
  (let ((lz (treepy-list-zip list-tree)))
    (should (equal list-tree (treepy-root lz)))
    (should (equal list-tree (treepy-node lz)))))

(ert-deftest treepy-navigation ()
  (assert-traversing-with-persistent-zipper [lz (treepy-list-zip list-tree)]
    (-> lz
        treepy-down
        treepy-right
        treepy-right
        treepy-right
        treepy-down)
    '((:a 5) . ((:l . nil)
                (:pnodes . (((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
                (:ppath . ((:l . (4 3 (1 2))) (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))) (:ppath) (:r)))
                (:r . ((:b ((:c 6) (:d 7)))))))
    (-> lz
        treepy-down
        treepy-right)
    '(5 . ((:l . (:a))
           (:pnodes . ((:a 5) ((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
           (:ppath . ((:l . nil)
                      (:pnodes . (((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
                      (:ppath . ((:l . (4 3 (1 2))) (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))) (:ppath) (:r)))
                      (:r . ((:b ((:c 6) (:d 7)))))))
           (:r . nil)))
    (-> lz
        treepy-left)
    '(:a . ((:l . nil)
            (:pnodes . ((:a 5) ((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
            (:ppath . ((:l . nil)
                       (:pnodes . (((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
                       (:ppath . ((:l . (4 3 (1 2))) (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))) (:ppath) (:r)))
                       (:r . ((:b ((:c 6) (:d 7)))))))
            (:r . (5))))
    (-> lz
        treepy-up
        treepy-up
        treepy-left
        treepy-left)
    '(3 . ((:l . ((1 2)))
           (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
           (:ppath . nil)
           (:r . (4 ((:a 5) (:b ((:c 6) (:d 7))))))))
    (-> lz
        treepy-up)
    `(,list-tree . ,nil)))

(ert-deftest treepy-context-test ()
  (let ((vz (treepy-vector-zip vector-tree)))
    (should (equal '(c)
                   (-> vz
                       treepy-down
                       treepy-right
                       treepy-right
                       treepy-down
                       treepy-right
                       treepy-lefts)))
    (should (equal '(d)
                   (-> vz
                       treepy-down
                       treepy-right
                       treepy-right
                       treepy-down
                       treepy-right
                       treepy-rights)))
    (should (equal '([[a * b] + [c * d]] [c * d])
                   (-> vz
                       treepy-down
                       treepy-right
                       treepy-right
                       treepy-down
                       treepy-right
                       treepy-path)))))

(ert-deftest treepy-navigation-and-editing-test ()
  (let ((vz (treepy-vector-zip vector-tree)))
    (should (equal [[a * b] + [c / d]]
                   (-> vz
                       treepy-down
                       treepy-right
                       treepy-right
                       treepy-down
                       treepy-right
                       (treepy-replace '/)
                       treepy-root)))
    (should (equal [["a" * b] / [c * d]]
                   (-> vz
                       treepy-next
                       treepy-next
                       (treepy-edit #'symbol-name)
                       treepy-next
                       treepy-next
                       treepy-next
                       (treepy-replace '/)
                       treepy-root)))
    (should (equal [[a * b] + [c *]]
                   (-> vz
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-remove
                       treepy-root)))
    (should (equal [[a * b] + [c * e]]
                   (-> vz
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-remove
                       (treepy-insert-right 'e)
                       treepy-root)))
    (should (equal [[a * b] + [c * e]]
                   (-> vz
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-next
                       treepy-remove
                       treepy-up
                       (treepy-append-child 'e)
                       treepy-root)))
    (should (equal [[c * d]]
                   (-> vz
                       treepy-next
                       treepy-remove
                       treepy-next
                       treepy-remove
                       treepy-root)))))

(ert-deftest treepy-end-p-test ()
  (should (treepy-end-p (-> (treepy-vector-zip vector-tree)
                            treepy-next
                            treepy-next
                            treepy-next
                            treepy-next
                            treepy-next
                            treepy-next
                            treepy-next
                            treepy-next
                            treepy-next
                            treepy-remove
                            treepy-next))))

(ert-deftest treepy-loop-test ()
  (should (equal [[a / b] + [c / d]]
                 (let ((loc (treepy-vector-zip vector-tree)))
                   (while (not (treepy-end-p loc))
                     (setq loc (treepy-next (if (equal '* (treepy-node loc))
                                                (treepy-replace loc '/)
                                              loc))))
                   (treepy-root loc))))
  (should (equal [[a b] + [c d]]
                 (let ((loc (treepy-vector-zip vector-tree)))
                   (while (not (treepy-end-p loc))
                     (setq loc (treepy-next (if (equal '* (treepy-node loc))
                                                (treepy-remove loc)
                                              loc))))
                   (treepy-root loc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treepy-zipper-tests.el ends here
