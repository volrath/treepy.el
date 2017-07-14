;;; treepy.el --- Generic tree traversing tools           -*- lexical-binding: t -*-
;; 
;; Filename: treepy.el
;;
;; Description: Generic Tree Traversing Tools
;; Author: Daniel Barreto <daniel.barreto.n@gmail.com>
;; Keywords: lisp, maint, tools
;; Created: Mon Jul 10 15:17:36 2017 (+0200)
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
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

;; loc structure: ([<node> <context>] <meta-alist>)
;; context structure alist{:l <list of nodes>, :pnodes <list of nodes>
;;                         :ppath <parent context>, :r <list of nodes>}

;; list to vector: (apply #'vector the-list)
;; vector to list: (seq-into the-vector 'list)

(defun treepy--context (loc &optional key)
  (let ((context (cdr (car loc))))
    (if (and context key)
        (map-elt context key)
      context)))

(defun treepy--context-assoc-1 (context k v)
  (if (map-contains-key context k)
      (mapcar (lambda (entry)
                (if (equal (car entry) k)
                    (cons k v)
                  entry))
              context)
    (cons (cons k v) context)))

(defun treepy--context-assoc (context &rest kvs)
  "Immutable map association"
  (seq-reduce (lambda (context kv)
                (seq-let [k v] kv
                  (treepy--context-assoc-1 context k v)))
              (seq-partition kvs 2) context))

(defun treepy--meta (loc &optional key)
  (let ((meta (cdr loc)))
    (if key
        (map-elt meta key)
      meta)))

(defun treepy--with-meta (obj meta)
  (cons obj meta))

(defun treepy--join-children (left-children right-children)
  (append (reverse left-children) right-children))

(defmacro treepy--with-loc (loc &rest body)
  "Binds the variables `node', `context', `l', `pnodes', `ppath',
and `r' for LOC."
  (declare (indent defun))
  `(let* ((node     (treepy-node ,loc))
          (context  (treepy--context ,loc))
          (pnodes   (treepy--context ,loc ':pnodes))
          (ppath    (treepy--context ,loc ':ppath))
          (l        (treepy--context ,loc ':l))
          (r        (treepy--context ,loc ':r))
          (changed? (treepy--context ,loc ':changed?)))
     ,@body))

;; Construction

(defun treepy-zipper (branchp children make-node root)
  (treepy--with-meta
   (cons root nil)
   `((:branchp . ,branchp) (:children . ,children) (:make-node . ,make-node))))

(defun treepy-list-zip (root)
  (let ((make-node (lambda (node children) children)))
    (treepy-zipper #'listp #'identity make-node root)))

(defun treepy-vector-zip (root)
  (let ((make-node (lambda (node children) (apply #'vector children))) ; (treepy--with-meta children (treepy--meta node))
        (children (lambda (cs) (seq-into cs 'list))))
    (treepy-zipper #'vectorp children make-node root)))

;; Context

(defun treepy-node (loc)
  (caar loc))

(defun treepy-branch-p (loc)
  (funcall (treepy--meta loc ':branchp) (treepy-node loc)))

(defun treepy-children (loc)
  (if (treepy-branch-p loc)
      (funcall (treepy--meta loc ':children) (treepy-node loc))
    (error "called children on a leaf node")))

(defun treepy-make-node (loc node children)
  (funcall (treepy--meta loc ':make-node) node children))

(defun treepy-path (loc)
  (reverse (treepy--context loc ':pnodes)))

(defun treepy-lefts (loc)
  (reverse (treepy--context loc ':l)))

(defun treepy-rights (loc)
  (treepy--context loc ':r))

;; Navigation

(defun treepy-down (loc)
  (when (treepy-branch-p loc)
    (let ((children (treepy-children loc)))
      (treepy--with-loc loc
        (seq-let [c &rest cs] children
          (when children
            (treepy--with-meta
             `(,c . ((:l . ,nil)
                     (:pnodes . ,(if context (cons node pnodes) (list node)))
                     (:ppath . ,context)
                     (:r . ,cs)))
             (treepy--meta loc))))))))

(defun treepy-up (loc)
  (treepy--with-loc loc
    (when pnodes
      (let ((pnode (car pnodes)))
        (treepy--with-meta
         (if changed?
             (cons (treepy-make-node loc pnode (treepy--join-children l (cons node r)))
                   (and ppath (treepy--context-assoc ppath ':changed? t)))
           (cons pnode ppath))
         (treepy--meta loc))))))

(defun treepy-root (loc)
  (if (equal ':end (treepy--context loc))
      (treepy-node loc)
    (let ((p loc))
      (while (setq p (treepy-up p))
        (setq loc p))
      (treepy-node loc))))

(defun treepy-right (loc)
  (treepy--with-loc loc
    (seq-let [cr &rest rnext] r
      (when (and context r)
        (treepy--with-meta
         (cons cr
               (treepy--context-assoc context
                                      ':l (cons node l)
                                      ':r rnext))
         (treepy--meta loc))))))

(defun treepy-rightmost (loc)
  (treepy--with-loc loc
    (if (and context r)
        (treepy--with-meta
         (cons (car (last r))
               (treepy--context-assoc context
                                      ':l (treepy--join-children l (cons node (butlast r)))
                                      ':r nil))
         (treepy--meta loc))
      loc)))

(defun treepy-left (loc)
  (treepy--with-loc loc
    (when (and context l)
      (seq-let [cl &rest lnext] l
        (treepy--with-meta
         (cons cl
               (treepy--context-assoc context
                                      ':l lnext
                                      ':r (cons node r)))
         (treepy--meta loc))))))

(defun treepy-leftmost (loc)
  (treepy--with-loc loc
    (if (and context l)
        (treepy--with-meta
         (cons (car (last l))
               (treepy--context-assoc context
                                      ':l []
                                      ':r (treepy--join-children (butlast l) (cons node r))))
         (treepy--meta loc))
      loc)))

;; Modification

(defun treepy-insert-left (loc item)
  (treepy--with-loc loc
    (if (not context)
        (error "Insert at top")
      (treepy--with-meta
       (cons node
             (treepy--context-assoc context
                                    ':l (cons item l)
                                    ':changed? t))
       (treepy--meta loc)))))

(defun treepy-insert-right (loc item)
  (treepy--with-loc loc
    (if (not context)
        (error "Insert at top")
      (treepy--with-meta
       (cons node
             (treepy--context-assoc context
                                    ':r (cons item r)
                                    ':changed? t))
       (treepy--meta loc)))))

(defun treepy-replace (loc node)
  (let ((context (treepy--context loc)))
    (treepy--with-meta
     (cons node
           (treepy--context-assoc context
                                  ':changed? t))
     (treepy--meta loc))))

(defun treepy-edit (loc f &rest args)
  (treepy-replace loc (apply f (treepy-node loc) args)))

(defun treepy-insert-child (loc item)
  (treepy-replace loc (treepy-make-node loc (treepy-node loc) (cons item (treepy-children loc)))))

(defun treepy-append-child (loc item)
  (treepy-replace loc (treepy-make-node loc (treepy-node loc) (append (treepy-children loc) `(,item)))))  ;; TODO: check performance

(defun treepy-remove (loc)
  (treepy--with-loc loc
    (if (not context)
        (error "Remove at top")
      (if (> (length l) 0)
          (let ((nloc (treepy--with-meta (cons (car l)
                                               (treepy--context-assoc context
                                                                      ':l (cdr l)
                                                                      ':changed? t))
                       (treepy--meta loc))))
            (while (setq child (and (treepy-branch-p nloc) (treepy-children nloc)))
              (setq nloc (treepy-rightmost child)))
            nloc)
        (treepy--with-meta
         (cons (treepy-make-node loc (car pnodes) r)
               (and ppath (treepy--context-assoc context ':changed? t)))
         (treepy--meta loc))))))

;; Enumeration

(defun treepy-next (loc)
  (if (equal :end (treepy--context loc))
      loc
    (or
     (and (treepy-branch-p loc) (treepy-down loc))
     (treepy-right loc)
     (let ((p loc))
       (while (and (treepy-up p) (not (setq pr (treepy-right (treepy-up p)))))
         (setq p (treepy-up p)))
       (or pr (cons (cons (treepy-node p) ':end) nil))))))

(defun treepy-prev (loc)
  (let ((lloc (treepy-left loc)))
    (if lloc
        (progn
          (while (setq child (and (treepy-branch-p lloc) (treepy-children lloc)))
            (setq lloc (treepy-rightmost child)))
          lloc)
      (treepy-up loc))))

(defun treepy-end-p (loc)
  (equal :end (treepy--context loc)))

(provide 'treepy)

;;; treepy.el ends here
