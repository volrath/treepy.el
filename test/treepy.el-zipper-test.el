;;; treepy-zipper-tests.el --- Generic tree traversal tools           -*- lexical-binding: t -*-
;; 
;; Filename: treepy.el-zipper-test.el
;;
;; Description: Generic Tree Traversal Tools
;; Author: Daniel Barreto <daniel.barreto.n@gmail.com>
;; Created: Mon Jul 10 15:17:36 2017 (+0200)
;; Version: 0.1.1
;; Package-Requires: ((emacs "25"))
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


;;; List Zipper

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
    `(,list-tree . ,nil))

  ;; Treepy handles non-nil cdr ending lists gracefully.
  (assert-traversing-with-persistent-zipper [lz (treepy-list-zip '(a b . c))]
    (-> lz
        treepy-next
        treepy-next
        treepy-next)
    '(c . ((:l . (b a))
           (:pnodes . ((a b . c)))
           (:ppath . nil)
           (:r . nil)))
    (-> lz
        treepy-prev
        treepy-next)
    '(c . ((:l . (b a))
           (:pnodes . ((a b . c)))
           (:ppath . nil)
           (:r . nil)))
    (-> lz
        treepy-next)
    '((a b . c) . :end)))


;;; Vector Zipper

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


;;; Preorder vs Postorder

(setq numered-tree '(1 . ((2 . (4 5)) 3)))

(defun numered-children (node)
  (cdr node))

(defun numered-make-node (node children)
  `(,node . ,children))

(defun traverse-tree (order)
  (let ((zp (treepy-zipper #'listp #'numered-children #'numered-make-node numered-tree))
        (visited nil))
    (when (equal order :postorder)
      (setq zp (treepy-leftmost-descendant zp)))
    (while (not (treepy-end-p zp))
      (let ((node (treepy-node zp)))
        (push (if (listp node) (car node) node) visited))
      (setq zp (treepy-next zp order)))
    (reverse visited)))

(ert-deftest treepy-test-next-order ()
  (should (equal (traverse-tree :preorder)
                 '(1 2 4 5 3)))
  (should (equal (traverse-tree :postorder)
                 '(4 5 2 3 1))))


;;; Custom Zipper
;; Taken from Alex Miller's article: Tree visitors in Clojure
;; https://www.ibm.com/developerworks/library/j-treevisit/

(defun expected-keys-p (map expected-key-set)
  (null (seq-difference (map-keys map) expected-key-set)))

(defmacro defnode (node-type fields)
  "Create a constructor function for a typed map and a well-known
set of fields (which are validation checked). Constructor will
be (defn new-&node-type> [field-map])."
  (let ((constructor-name (intern (concat "new-" (symbol-name node-type)))))
    `(defun ,constructor-name (nv-map)
       (if (expected-keys-p nv-map (list ,@fields))
           (cons '(:type . ,(intern (concat ":" (symbol-name node-type)))) nv-map)
         (error "Wrong node specification")))))

(defun node-type (ast-node)
  (if (listp ast-node)
      (map-elt ast-node ':type)
    ':leaf))

(defnode root (':children))
(defnode compare-criteria (':left ':right))
(defnode concat (':args))
(defnode value (':val))

(defvar value-1 (new-value '((:val . 1))))
(defvar concat-1 (new-concat '((:args . ("a" "b")))))
(defvar concat-2 (new-concat `((:args . ("c" ,value-1)))))
(defvar concat-3 (new-concat '((:args . ("t" "r" "u" "e")))))
(defvar concat-4 (new-concat '((:args . ("t" "r" "u" "e")))))
(defvar cc-1 (new-compare-criteria `((:left . ,concat-1)
                                     (:right . ,concat-2))))
(defvar cc-2 (new-compare-criteria `((:left . ,concat-3)
                                     (:right . ,concat-4))))
(defvar custom-tree (new-root `((:children . ,(list cc-1 cc-2)))))

;;                         root
;;                          +
;;             +------------+------------+
;;     compare-criteria           compare-criteria
;;             +                          +
;;      +------+-------+           +------+-------+
;;   concat          concat     concat          concat
;;      +              +           +              +
;;   +--+-+         +--+--+   +----+---+     +----+---+
;;   a    b         c    val  t  r  u  e     t  r  u  e
;;                        +
;;                        +
;;                        1
;; ((:type . :root)
;;  (:children ((:type . :compare-criteria)
;;              (:left (:type . :concat) (:args "a" "b"))
;;              (:right (:type . :concat) (:args "c" ((:type . :value)
;;                                                    (:val . 1)))))
;;             ((:type . :compare-criteria)
;;              (:left (:type . :concat) (:args "t" "r" "u" "e"))
;;              (:right (:type . :concat) (:args "t" "r" "u" "e")))))

(defun custom-branch-p (node)
  (and (mapp node)
       (map-contains-key node ':type)))

(defun custom-children (node)
  (cl-case (node-type node)
    (':root (map-elt node ':children))
    (':compare-criteria (list (map-elt node ':left) (map-elt node ':right)))
    (':concat (map-elt node ':args))
    (':value (list (map-elt node ':val)))))

(defun custom-make-node (node children)
  (let* ((type (node-type node))
         (constructor (intern (concat "new-" (substring (symbol-name type) 1 nil)))))
    (funcall constructor
             (cl-case type
               (':root `((:children . ,children)))
               (':compare-criteria `((:left . ,(car children))
                                     (:right . ,(cadr children))))
               (':concat `((:args . ,children)))
               (':value `((:val . ,(car children))))))))

(setq custom-zipper (treepy-zipper #'custom-branch-p #'custom-children #'custom-make-node custom-tree))

(defun can-simplify-concat? (node)
  (and (equal ':concat (node-type node))
       (seq-every-p #'stringp (map-elt node ':args))))

(defun can-simplify-cc? (node)
  (and (equal ':compare-criteria (node-type node))
       (and (not (listp (map-elt node ':left)))
            (not (listp (map-elt node ':right))))))

(defun simplify-concat (node)
  (apply #'concat (map-elt node ':args)))

(defun simplify-cc (node)
  (equal (map-elt node ':left) (map-elt node ':right)))

(defun simplify-tree (zipper)
  (let ((loc (treepy-leftmost-descendant zipper)))
    (if (treepy-end-p loc)
        (treepy-root loc)
      (progn
        (while (not (treepy-end-p (treepy-next loc ':postorder)))
          (let ((node (treepy-node loc)))
            (setq loc (treepy-next
                       (cond
                        ((can-simplify-concat? node) (treepy-edit loc #'simplify-concat))
                        ((can-simplify-cc? node) (treepy-edit loc #'simplify-cc))
                        (t loc))
                       ':postorder))))
        (treepy-root loc)))))

;; Test reduction to
;;                         root
;;                          +
;;             +------------+------------+
;;     compare-criteria                  t
;;             +
;;      +------+-------+
;;     "ab"          concat
;;                     +
;;                  +--+--+
;;                  c    val
;;                        +
;;                        +
;;                        1

(ert-deftest treepy-custom-tree-test ()
  (should (equal '((:type . :root)
                   (:children . (((:type . :compare-criteria)
                                  (:left . "ab")
                                  (:right . ((:type . :concat)
                                             (:args "c" ((:type . :value)
                                                         (:val . 1))))))
                                 t ;; Second node was replaced by/simplified for `t'
                                 )))
                 (simplify-tree custom-zipper))))


;;; Custom Zipper based on Parseclj
;; AST representation for the following clojure code

(defvar clj-ast-root
  '((:node-type . :root)
    (:position . 1)
    (:children ((:node-type . :list)
                (:position . 1)
                (:children ((:node-type . :symbol)
                            (:position . 2)
                            (:form . "defn")
                            (:value . defn))
                           ((:node-type . :symbol)
                            (:position . 7)
                            (:form . "a-test")
                            (:value . a-test))
                           ((:node-type . :vector)
                            (:position . 14)
                            (:children ((:node-type . :symbol)
                                        (:position . 15)
                                        (:form . "x")
                                        (:value . x))))
                           ((:node-type . :string)
                            (:position . 20)
                            (:form . "\"fn documentation\"")
                            (:value . "fn documentation"))
                           ((:node-type . :map)
                            (:position . 41)
                            (:children ((:node-type . :keyword)
                                        (:position . 42)
                                        (:form . ":key")
                                        (:value . :key))
                                       ((:node-type . :symbol)
                                        (:position . 47)
                                        (:form . "x")
                                        (:value . x))))))))
  "Parseclj AST representation of:
(defn a-test [x]
  \"fn documentation\"
  {:key x})")


(defun treepy-parseclj--make-node (node children)
  "Helper parseclj function to create nodes 'a la treepy'.
NODE is an AST node.  CHILDREN is a list of AST nodes."
  (mapcar (lambda (pair)
            (if (eql (car pair) :children)
                (cons :children children)
              pair))
          node))


(defun treepy-parseclj--children (node)
  "Return children for the AST NODE."
  (map-elt node :children))


(defun treepy-parseclj--branch-node-p (node)
  "Return t if the given AST NODE is a branch node."
  (not (member (map-elt node :type) '(:symbol :string))))


(defun treepy-parseclj-ast-zip (ast-node)
  "Create a treepy zipper for AST-NODE."
  (treepy-zipper #'treepy-parseclj--branch-node-p
                 #'treepy-parseclj--children
                 #'treepy-parseclj--make-node
                 ast-node))


(defun treepy-parseclj--traverse-tree ()
  (let ((zp (treepy-parseclj-ast-zip clj-ast-root))
        (visited nil))
    (while (not (treepy-end-p zp))
      (let ((node (treepy-node zp)))
        (push (or (map-elt node :value)
                  (map-elt node :node-type))
              visited))
      (setq zp (treepy-next zp)))
    (reverse visited)))


(ert-deftest treepy-parseclj-test-next-order ()
  (should (equal (treepy-parseclj--traverse-tree)
                 '(:root :list defn a-test :vector x "fn documentation" :map :key x))))

(provide 'treepy-zipper-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treepy-zipper-tests.el ends here
