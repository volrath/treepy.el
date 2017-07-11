;;; treepy.el-test.el --- 
;; 
;; Filename: treepy.el-test.el
;; Description: 
;; Author: Daniel Barreto
;; Maintainer: 
;; Created: Tue Jul 11 17:52:34 2017 (+0200)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
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

(require 'ert)
(require 'treepy)

(defvar list-tree '((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))

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
                          (message "LOC:      %S" loc)
                          (message "EXPECTED: %S" ,expected)
                          (should (equal loc ,expected))))))
                 (seq-partition body 2)))))

(defmacro -> (x form &rest more)
  (cond ((not (null more)) `(-> (-> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(if (null ,x) nil
                  (,(first form) ,x ,@(rest form)))
             `(if (null ,x) nil
                ,(list form x))))))

(ert-deftest treepy-roundtrip-test ()
  (let ((lz (treepy-list-zip list-tree)))
    (should (equal list-tree (treepy-root lz)))
    (should (equal list-tree (treepy-node lz)))))

(ert-deftest treepy-navigation ()
  (assert-traversing-with-persistent-zipper [lz (treepy-list-zipper list-tree)]
    ;; (-> lz
    ;;     treepy-down
    ;;     treepy-right treepy-right treepy-right
    ;;     treepy-down)
    (treepy-down (treepy-right (treepy-right (treepy-right (treepy-down lz)))))
    '((:a 5) . ((:l . nil)
                (:pnodes . (((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
                (:ppath . ((:l . (4 3 (1 2))) (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))) (:ppath) (:r)))
                (:r . ((:b ((:c 6) (:d 7)))))))
    (treepy-right (treepy-down lz))
    '(5 . ((:l . (:a))
           (:pnodes . ((:a 5) ((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
           (:ppath . ((:l . nil)
                      (:pnodes . (((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
                      (:ppath . ((:l . (4 3 (1 2))) (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))) (:ppath) (:r)))
                      (:r . ((:b ((:c 6) (:d 7)))))))
           (:r . nil)))
    (treepy-left lz)
    '(:a . ((:l . nil)
            (:pnodes . ((:a 5) ((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
            (:ppath . ((:l . nil)
                       (:pnodes . (((:a 5) (:b ((:c 6) (:d 7)))) ((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
                       (:ppath . ((:l . (4 3 (1 2))) (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7))))))) (:ppath) (:r)))
                       (:r . ((:b ((:c 6) (:d 7)))))))
            (:r . (5))))
    (treepy-left (treepy-left (treepy-up (treepy-up lz))))
    '(3 . ((:l . ((1 2)))
           (:pnodes . (((1 2) 3 4 ((:a 5) (:b ((:c 6) (:d 7)))))))
           (:ppath . nil)
           (:r . (4 ((:a 5) (:b ((:c 6) (:d 7))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treepy.el-test.el ends here
