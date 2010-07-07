;; A functional balanced tree data structure, with a rather low level
;; interface. It is intended to be used as a base to implement data
;; structures like maps, sets and priority queues. It can obviously
;; also be used to implement sorting, removal of duplicate elements in
;; lists and things like that. The implementation is based on the
;; algorithms described in
;; http://groups.csail.mit.edu/mac/users/adams/BB/
;;
;; A function whose name begins with %% is unsafe; it doesn't check
;; its arguments. It might or might not segfault, but other functions
;; might do it later on since the data structure can become bogus
;; unless you give it proper arguments. They are not exported, so
;; those issues are taken care of internally.
;;
;; Copyright (c) 2010 Per Eckerdal
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;  
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;  
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(cond-expand
 (black-hole
  (export tree?
          empty-tree
          tree-size
          tree-min
          tree-max
          tree-fold
          
          tree-search
          tree-member?
          
          tree-add
          tree-delete
          tree-delete-min
          tree-delete-max
          
          tree-split<
          tree-split>
          tree-union
          tree-difference
          tree-intersection
          
          tree-rank
          tree-index

          list->tree
          tree->list))
 
 (else
  #f))

(define *tree-weight* 5)

(define (check-tree . params)
  (if (let loop ((lst params))
        (if (null? lst)
            #f
            (or (not (tree? (car lst)))
                (loop (cdr lst)))))
      (error "Invalid parameters" params)))

(define (id x) x)

(define-type tree
  constructor: make-tree/internal
  predicate: tree/internal?
  (element read-only: unprintable:)
  (count read-only: unprintable:)
  (left-subtree read-only: unprintable:)
  (right-subtree read-only: unprintable:))

(define (tree? x)
  (or (eq? x empty-tree)
      (tree/internal? x)))

(define empty-tree (list 'empty-tree))

(define-macro (%%tree-element t)
  `(##vector-ref ,t 1))

(define-macro (%%tree-count t)
  `(##vector-ref ,t 2))

(define-macro (%%tree-left-subtree t)
  `(##vector-ref ,t 3))

(define-macro (%%tree-right-subtree t)
  `(##vector-ref ,t 4))

(define-macro (%%tree-size t)
  (let ((gs (gensym)))
    `(let ((,gs ,t))
       (if (eq? ,gs empty-tree)
           0
           (%%tree-count ,gs)))))

(define-macro (%%< a b)
  `(##< ,a ,b))

(define-macro (%%> a b)
  `(##< ,b ,a))

;; The constructors for the tree datatype are in different layers of
;; abstraction:
;;
;; make-tree/internal is the most low-level tree constructor
;;
;; make-tree-nonbalancing keeps track of the count field
;;
;; make-tree is used when the original tree was in balance and one of
;; l or r have changed size by at most one element, as in insertion or
;; deletion of a single element.
;;
;; %%tree-join is used for joining an element and two trees of
;; arbitrary sizes, where the every element of the left tree is < the
;; element and every element of the right tree is > the element.

(define (%%make-tree-nonbalancing elm left right)
  (make-tree/internal elm
                      (+ 1
                         (%%tree-size left)
                         (%%tree-size right))
                      left
                      right))

(define (%%tree-rotate-single-left parent-elm left right)
  (%%make-tree-nonbalancing
   (%%tree-element right)
   (%%make-tree-nonbalancing parent-elm
                             left
                             (%%tree-left-subtree right))
   (%%tree-right-subtree right)))

(define (%%tree-rotate-single-right parent-elm left right)
  (%%make-tree-nonbalancing
   (%%tree-element left)
   (%%tree-left-subtree left)
   (%%make-tree-nonbalancing parent-elm
                             (%%tree-right-subtree left)
                             right)))

(define (%%tree-rotate-double-left parent-elm left right)
  (let ((right-left (%%tree-left-subtree right)))
    (%%make-tree-nonbalancing
     (%%tree-element right-left)
     (%%make-tree-nonbalancing
      parent-elm
      left
      (%%tree-left-subtree right-left))
     (%%make-tree-nonbalancing
      (%%tree-element right)
      (%%tree-right-subtree right-left)
      (%%tree-right-subtree right)))))

(define (%%tree-rotate-double-right parent-elm left right)
  (let ((left-right (%%tree-right-subtree left)))
    (%%make-tree-nonbalancing
     (%%tree-element left-right)
     (%%make-tree-nonbalancing
      (%%tree-element left)
      (%%tree-left-subtree left)
      (%%tree-left-subtree left-right))
     (%%make-tree-nonbalancing
      parent-elm
      (%%tree-right-subtree left-right)
      right))))

(define (%%make-tree elm left right)
  (let ((left-size (%%tree-size left))
        (right-size (%%tree-size right)))
  (cond
   ((%%< (+ left-size right-size)
         2)
    ;; The tree is too small to be balanced
    (%%make-tree-nonbalancing elm left right))

   ((%%> right-size
         (* *tree-weight* left-size))
    ;; Right side is too heavy
    (let ((right-left-size (%%tree-size
                            (%%tree-left-subtree right)))
          (right-right-size (%%tree-size
                             (%%tree-right-subtree right))))
      (if (%%< right-left-size
               right-right-size)
          (%%tree-rotate-single-left elm left right)
          (%%tree-rotate-double-left elm left right))))

   ((%%> left-size
         (* *tree-weight* right-size))
    ;; Left side is too heavy
    (let ((left-right-size (%%tree-size
                            (%%tree-right-subtree left)))
          (left-left-size (%%tree-size
                           (%%tree-left-subtree left))))
      (if (%%< left-right-size
               left-left-size)
          (%%tree-rotate-single-right elm left right)
          (%%tree-rotate-double-right elm left right))))

   (else
    ;; The tree doesn't need to be balanced
    (%%make-tree-nonbalancing elm left right)))))

;; The function %%tree-join is used to join two trees with an element
;; that is between the values in the left tree and the values in the
;; right tree. If the left and right arguments would make a balanced
;; tree then they can be joined immediately. If one tree is
;; significantly larger then it is scanned to find the largest subtree
;; on the side 'facing' the smaller tree that is small enough to
;; balance with the smaller tree. The tree is joined at this position
;; and the higher levels are rebalanced if necessary.
(define (%%tree-join elm left right <?)
  (let loop ((elm elm)
             (left left)
             (right right))
    (cond
     ((eq? left empty-tree) (%%tree-add right elm <?))
     ((eq? right empty-tree) (%%tree-add left elm <?))
     (else
      (let ((left-elm (%%tree-element left))
            (left-size (%%tree-count left))
            (left-left (%%tree-left-subtree left))
            (left-right (%%tree-right-subtree left))
            
            (right-elm (%%tree-element right))
            (right-size (%%tree-count right))
            (right-left (%%tree-left-subtree right))
            (right-right (%%tree-right-subtree right)))
        (cond
         ((%%> right-size
               (* *tree-weight* left-size))
          ;; Right side is too heavy
          (%%make-tree right-elm
                       (loop elm left right-left)
                       right-right))
         
         ((%%> left-size
               (* *tree-weight* right-size))
          ;; Left side is too heavy
          (%%make-tree left-elm
                       left-left
                       (loop elm left-right right)))
         
         (else
          ;; Tree doesn't need to be balanced
          (%%make-tree-nonbalancing elm left right))))))))

;; Concatenates two trees. Every element in left should be < every
;; element in right.
(define (%%tree-concat left right <?)
  (let loop ((left left)
             (right right))
    (cond
     ((eq? left empty-tree)
      right)
     
     ((eq? right empty-tree)
      left)
     
     (else
      (let ((left-elm (%%tree-element left))
            (left-size (%%tree-count left))
            (left-left (%%tree-left-subtree left))
            (left-right (%%tree-right-subtree left))
            
            (right-elm (%%tree-element right))
            (right-size (%%tree-count right))
            (right-left (%%tree-left-subtree right))
            (right-right (%%tree-right-subtree right)))
        (cond
         ((%%> right-size
               (* *tree-weight* left-size))
          ;; Right side is too heavy
          (%%make-tree right-elm
                       (loop left
                             right-left)
                       right-right))
         
         ((%%> left-size
               (* *tree-weight* right-size))
          ;; Left side is too heavy
          (%%make-tree left-elm
                       left-left
                       (loop left-right
                             right)))
         
         (else
          ;; Tree doesn't need to be balanced
          (%%make-tree (tree-min right)
                       left
                       (tree-delete-min right)))))))))

;; The already-there function can possibly throw an error (or not
;; return in another way), and then no trees will be allocated. If
;; already-there is not supplied, the tree with that element is
;; replaced.
(define (%%tree-add tree elm <? #!key (already-there (lambda () #f)))
  (let loop ((tree tree))
    (if (eq? tree empty-tree)
        (make-tree/internal elm 1 empty-tree empty-tree)
        (let ((tree-elm (%%tree-element tree))
              (tree-left (%%tree-left-subtree tree))
              (tree-right (%%tree-right-subtree tree)))
          (cond
           ((<? elm tree-elm)
            (%%make-tree tree-elm
                         (loop tree-left)
                         tree-right))

           ((<? tree-elm elm)
            (%%make-tree tree-elm
                         tree-left
                         (loop tree-right)))

           (else
            (already-there)
            (%%make-tree elm
                         tree-left
                         tree-right)))))))

(define (tree-add tree elm <? #!key (already-there (lambda () #f)))
  (check-tree tree)
  (%%tree-add tree elm <? already-there: already-there))

(define (tree-delete-min tree)
  (check-tree tree)
  (if (eq? tree empty-tree)
      (error "Can't delete empty tree"))

  (let loop ((tree tree))
    (let ((left (%%tree-left-subtree tree))
          (right (%%tree-right-subtree tree)))
      (if (eq? empty-tree left)
          right
          (%%make-tree (%%tree-element tree)
                       (tree-delete-min left)
                       right)))))

(define (tree-delete-max tree)
  (check-tree tree)
  (if (eq? tree empty-tree)
      (error "Can't delete empty tree"))

  (let loop ((tree tree))
    (let ((left (%%tree-left-subtree tree))
          (right (%%tree-right-subtree tree)))
      (if (eq? right empty-tree)
          left
          (%%make-tree (%%tree-element tree)
                       left
                       (tree-delete-max right))))))

;; This is a utility function for tree-delete
;;
;; left should be (tree-left-subtree parent)
;; right should be (tree-right-subtree parent)
(define (tree-delete-root parent left right <?)
  (cond
   ((eq? right empty-tree)
    left)

   ((eq? left empty-tree)
    right)

   (else
    (let ((min-elm (tree-min right)))
      (%%make-tree min-elm
                   left
                   (tree-delete-min right))))))

(define (tree-delete tree elm <?
                     #!key (not-found
                            (lambda ()
                              (error "Not found"))))
  (check-tree tree)
  (let loop ((tree tree))
    (if (eq? tree empty-tree)
        (not-found)
        (let ((tree-elm (%%tree-element tree))
              (left (%%tree-left-subtree tree))
              (right (%%tree-right-subtree tree)))
          (cond
           ((<? elm tree-elm)
            (%%make-tree tree-elm
                         (loop left)
                         right))
           
           ((<? tree-elm elm)
            (%%make-tree tree-elm
                         left
                         (loop right)))
           
           (else
            (tree-delete-root tree left right <?)))))))

(define (tree-size tree)
  (if (eq? tree empty-tree)
      0
      (tree-count tree)))

(define (tree-search tree elm <? fail found)
  (check-tree tree)
  
  (let loop ((tree tree))
    (if (eq? tree empty-tree)
        (fail)
        (let ((tree-elm (%%tree-element tree)))
          (cond
           ((<? elm tree-elm)
            (loop (%%tree-left-subtree tree)))
           ((<? tree-elm elm)
            (loop (%%tree-right-subtree tree)))
           (else
            (found tree-elm)))))))

(define (tree-member? tree elm <?)
  (tree-search tree
               elm
               <?
               (lambda () #f)
               (lambda (tree) #t)))

(define-macro (define-tree-search-min/max name fn)
  `(define (,name tree fail found)
     (check-tree tree)
     (if (eq? tree empty-tree)
         (fail)
         (let loop ((tree tree))
           (let ((subtree (,fn tree)))
             (if (eq? subtree empty-tree)
                 (found (%%tree-element tree))
                 (loop subtree)))))))

(define-tree-search-min/max tree-search-min %%tree-left-subtree)
(define-tree-search-min/max tree-search-max %%tree-right-subtree)

(define (tree-min tree)
  (tree-search-min
   tree
   (lambda () (error "Tree doesn't have a minimum value"))
   (lambda (value) value)))

(define (tree-max tree)
  (tree-search-max
   tree
   (lambda () (error "Tree doesn't have a maximum value"))
   (lambda (value) value)))

(define (tree-fold fn base tree)
  (check-tree tree)

  (let loop ((base base)
             (tree tree))
    (if (eq? tree empty-tree)
        base
        (let ((elm (%%tree-element tree))
              (left (%%tree-left-subtree tree))
              (right (%%tree-right-subtree tree)))
          (loop (fn elm
                    (loop base right))
                left)))))

(define (tree-split< tree elm <?)
  (check-tree tree)
  (let loop ((tree tree))
    (if (eq? tree empty-tree)
        empty-tree
        (let ((tree-elm (%%tree-element tree))
              (tree-left (%%tree-left-subtree tree))
              (tree-right (%%tree-right-subtree tree)))
          (cond
           ((<? elm tree-elm)
            (loop tree-left))
           ((<? tree-elm elm)
            (%%tree-join tree-elm
                       tree-left
                       (loop tree-right)
                       <?))
           (else
            tree-left))))))


(define (tree-split> tree elm <?)
  (check-tree tree)
  (let loop ((tree tree))
    (if (eq? tree empty-tree)
        empty-tree
        (let ((tree-elm (%%tree-element tree))
              (tree-left (%%tree-left-subtree tree))
              (tree-right (%%tree-right-subtree tree)))
          (cond
           ((<? elm tree-elm)
            (%%tree-join tree-elm
                       (loop tree-left)
                       tree-right
                       <?))
           ((<? tree-elm elm)
            (loop tree-right))
           (else
            tree-right))))))

;; This function is here for testing purposes. The tree-union function
;; is faster but more complex (and thus more bug-prone).
(define (tree-union-slow tree1 tree2 <?)
  (check-tree tree1 tree2)
  (let loop ((tree1 tree1)
             (tree2 tree2))
    (cond
     ((eq? tree1 empty-tree)
      tree2)
     
     ((eq? tree2 empty-tree)
      tree1)
     
     (else
      (let* ((tree2-elm (%%tree-element tree2))
             (tree2-left (%%tree-left-subtree tree2))
             (tree2-right (%%tree-right-subtree tree2))

             (tree1<elm (tree-split< tree1 tree2-elm <?))
             (tree1>elm (tree-split> tree1 tree2-elm <?)))
        (%%tree-join tree2-elm
                   (loop tree1<elm
                         tree2-left)
                   (loop tree1>elm
                         tree2-right)
                   <?))))))

(define (tree-union tree1 tree2 <?)
  (define (trim lo hi tree)
    (if (eq? tree empty-tree)
        empty-tree
        (let ((tree-elm (%%tree-element tree))
              (tree-left (%%tree-left-subtree tree))
              (tree-right (%%tree-right-subtree tree)))
          (if (<? lo tree-elm)
              (if (<? tree-elm hi)
                  tree
                  (trim lo hi tree-left))
              (trim lo hi tree-right)))))
             
  (define (uni-bd tree1 tree2 lo hi)
    (cond
     ((eq? tree2 empty-tree)
      tree1)

     ((eq? tree1 empty-tree)
      (let ((tree-elm (%%tree-element tree2))
            (tree-left (%%tree-left-subtree tree2))
            (tree-right (%%tree-right-subtree tree2)))
        (%%tree-join tree-elm
                   (tree-split> tree-left lo <?)
                   (tree-split< tree-right hi <?)
                   <?)))

     (else
      (let ((tree1-elm (%%tree-element tree1))
            (tree1-left (%%tree-left-subtree tree1))
            (tree1-right (%%tree-right-subtree tree1)))
        ;; Invariant lo < tree1-elm < hi
        (%%tree-join tree1-elm
                     (uni-bd tree1-left
                             (trim lo tree1-elm tree2)
                             lo
                             tree1-elm)
                     (uni-bd tree1-right
                             (trim tree1-elm hi tree2)
                             tree1-elm
                             hi)
                     <?)))))

  ;; All the other versions of uni and trim are specializations of the
  ;; above two functions with lo=-infinity and/or hi=+infinity

  (define (trim-lo lo tree)
    (if (eq? tree empty-tree)
        empty-tree
        (if (<? lo (%%tree-element tree))
            tree
            (trim-lo lo (%%tree-right-subtree tree)))))
  (define (trim-hi hi tree)
    (if (eq? tree empty-tree)
        empty-tree
        (if (<? (%%tree-element tree) hi)
            tree
            (trim-hi hi (%%tree-left-subtree tree)))))
  
  (define (uni-hi tree1 tree2 hi)
    (cond
     ((eq? tree2 empty-tree)
      tree1)

     ((eq? tree1 empty-tree)
      (%%tree-join (%%tree-element tree2)
                   (%%tree-left-subtree tree2)
                   (tree-split< (%%tree-right-subtree tree2)
                                hi
                                <?)
                   <?))
     
     (else
      (let ((tree1-elm (%%tree-element tree1))
            (tree1-left (%%tree-left-subtree tree1))
            (tree1-right (%%tree-right-subtree tree1)))
      (%%tree-join tree1-elm
                   (uni-hi tree1-left
                           (trim-hi tree1-elm tree2)
                           tree1-elm)
                   (uni-bd tree1-right
                           (trim tree1-elm hi tree2)
                           tree1-elm
                           hi)
                   <?)))))
  (define (uni-lo tree1 tree2 lo)
    (cond
     ((eq? tree2 empty-tree)
      tree1)

     ((eq? tree1 empty-tree)
      (%%tree-join (%%tree-element tree2)
                   (tree-split> (%%tree-left-subtree tree2)
                                lo
                                <?)
                   (%%tree-right-subtree tree2)
                   <?))
     
     (else
      (let ((tree1-elm (%%tree-element tree1))
            (tree1-left (%%tree-left-subtree tree1))
            (tree1-right (%%tree-right-subtree tree1)))
        (%%tree-join tree1-elm
                     (uni-bd tree1-left
                             (trim lo tree1-elm tree2)
                             lo
                             tree1-elm)
                     (uni-lo tree1-right
                             (trim-lo tree1-elm tree2)
                             tree1-elm)
                     <?)))))
  
  (check-tree tree1 tree2)
  (cond
   ((eq? tree1 empty-tree)
    tree2)
   
   ((eq? tree2 empty-tree)
    tree1)
   
   (else
    (let ((tree1-elm (%%tree-element tree1))
          (tree1-left (%%tree-left-subtree tree1))
          (tree1-right (%%tree-right-subtree tree1)))
      (%%tree-join tree1-elm
                   (uni-hi tree1-left
                           (trim-hi tree1-elm tree2)
                           tree1-elm)
                   (uni-lo tree1-right
                           (trim-lo tree1-elm tree2)
                           tree1-elm)
                   <?)))))

(define (tree-difference tree1 tree2 <?)
  (check-tree tree1 tree2)
  (let loop ((tree1 tree1) (tree2 tree2))
    (cond
     ((eq? tree1 empty-tree)
      empty-tree)

     ((eq? tree2 empty-tree)
      tree1)

     (else
      (let ((tree2-elm (%%tree-element tree2))
            (tree2-left (%%tree-left-subtree tree2))
            (tree2-right (%%tree-right-subtree tree2)))
        (%%tree-concat (loop (tree-split< tree1 tree2-elm <?)
                             tree2-left)
                       (loop (tree-split> tree1 tree2-elm <?)
                             tree2-right)
                       <?))))))

(define (tree-intersection tree1 tree2 <?)
  (check-tree tree1 tree2)
  (let loop ((tree1 tree1) (tree2 tree2))
    (if (or (eq? tree1 empty-tree)
            (eq? tree2 empty-tree))
        empty-tree
        (let* ((tree2-elm (%%tree-element tree2))
               (tree2-left (%%tree-left-subtree tree2))
               (tree2-right (%%tree-right-subtree tree2))

               (tree1<tree2-elm (tree-split< tree1 tree2-elm <?))
               (tree1>tree2-elm (tree-split> tree1 tree2-elm <?)))
          (if (tree-member? tree1 tree2-elm <?)
              (%%tree-join tree2-elm
                         (loop tree1<tree2-elm
                               tree2-left)
                         (loop tree1>tree2-elm
                               tree2-right)
                         <?)
              (%%tree-concat (loop tree1<tree2-elm
                                   tree2-left)
                             (loop tree1>tree2-elm
                                   tree2-right)
                             <?))))))

(define (tree-rank tree elm <?
                   #!key
                   (fail
                    (lambda ()
                      (error "Tree doesn't contain" elm))))
  (check-tree tree)

  (let loop ((tree tree) (accum 0))
    (if (eq? tree empty-tree)
        (fail)
        (let ((tree-elm (%%tree-element tree))
              (tree-left (%%tree-left-subtree tree))
              (tree-right (%%tree-right-subtree tree)))
          (cond
           ((<? elm tree-elm)
            (loop tree-left accum))
           ((<? tree-elm elm)
            (loop tree-right
                  (+ accum
                     1
                     (%%tree-size tree-left))))
           (else
            (+ accum
               (%%tree-size tree-left))))))))

(define (tree-index tree idx
                    #!key
                    (fail
                     (lambda ()
                       (error "Tree has no element with index" idx))))
  (check-tree tree)
  (if (not (fixnum? idx))
      (error "Invalid parameter" idx))

  (let loop ((tree tree) (idx idx))
    (if (eq? tree empty-tree)
        (fail)
        (let* ((tree-left (%%tree-left-subtree tree))
               (left-size (%%tree-size tree-left)))
          (cond
           ((%%< idx left-size)
            (loop tree-left idx))
           ((%%> idx left-size)
            (loop (%%tree-right-subtree tree)
                  (- idx left-size 1)))
           (else
            (%%tree-element tree)))))))

(define (list->tree list <?)
  (let loop ((list list) (accum empty-tree))
    (if (null? list)
        accum
        (loop (cdr list)
              (tree-add accum
                        (car list)
                        <?)))))

(define (tree->list tree)
  (tree-fold cons '() tree))




#;(begin
  ;; Some testing stuff
  (define (make-count from to)
    (if (> from to)
        empty-tree
        (tree-add (make-count (+ from 1) to)
                  from
                  <)))

  (define (list->tree list <?)
    (let loop ((list list) (accum empty-tree))
      (if (null? list)
          accum
          (loop (cdr list)
                (tree-add accum
                          (car list)
                          <?)))))
  
  (define (pt tree)
    (let loop ((tree tree))
      (if (eq? empty-tree tree)
          empty-tree
          (let ((elm (tree-element tree))
                (l (tree-left-subtree tree))
                (r (tree-right-subtree tree)))
            (if (and (eq? l empty-tree)
                     (eq? r empty-tree))
                elm
                (list (loop l)
                      elm
                      (loop r)))))))
  
  (define (tp list)
    (if (pair? list)
        (%%make-tree-nonbalancing (cadr list)
                                  (tp (car list))
                                  (tp (caddr list)))
        (if (eq? list empty-tree)
            empty-tree
            (%%make-tree-nonbalancing list empty-tree empty-tree))))
  
  (define (rm tree)
    (list->tree
     (tree->list tree)
     <))
  
  (tree->list (make-count 11 20))
  (tree->list (make-count 0 9))
  (tree->list
   (%%tree-join 10
              (make-count 0 10)
              (make-count 11 12)
              <))
  (tree->list (tree-split> (make-count 1 20) 10 <))
  (tree->list (tree-split< (make-count 1 20) 10 <))
  
  (tree->list
   (tree-union (tree-split> (make-count 1 20) 10 <)
               (tree-split< (make-count 1 20) 10 <)
               <))
  
  (tree->list
   (tree-difference (make-count 1 10)
                    (make-count 4 8)
                    <))
  
  (tree->list
   (tree-intersection (make-count 1 5)
                      (make-count 4 8)
                      <))
  
  (tree->list
   (tree-union (make-count 1 8)
               (make-count 12 20)
               <)))
