;;; "wttree.scm" Weight balanced trees			-*-Scheme-*-
;;; Copyright (c) 1993-1994 Stephen Adams
;;;
;;; References:
;;;
;;;   Stephen Adams, Implemeting Sets Efficiently in a Functional
;;;      Language, CSTR 92-10, Department of Electronics and Computer
;;;      Science, University of Southampton, 1992
;;;
;;;
;;; Copyright (c) 1993-94 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy and modify
;;; this software, to redistribute either the original software or a
;;; modified version, and to use this software for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warranty or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Weight Balanced Binary Trees
;;
;;
;;
;;  This file has been modified from the MIT-Scheme library version to
;;  make it more standard. The main changes are
;;
;;   . The whole thing has been put in a LET as R4RS Scheme has no module
;;     system.
;;   . The MIT-Scheme define structure operations have been written out by
;;     hand.
;;
;;  It has been tested on MIT-Scheme, scheme48 and scm4e1
;;
;;  If your system has a compiler and you want this code to run fast, you
;;  should do whatever is necessary to inline all of the structure accessors.
;;
;;  This is MIT-Scheme's way of saying that +, car etc should all be inlined.
;;
;;(declare (usual-integrations))

(declare (block)
         (mostly-fixnum)
         (standard-bindings)
         (extended-bindings))

(export make-wt-tree-type
        make-wt-tree
        singleton-wt-tree
        alist->wt-tree
        wt-tree/empty?
        wt-tree/size
        wt-tree/add
        wt-tree/delete
        wt-tree/add!
        wt-tree/delete!
        wt-tree/member?
        wt-tree/lookup
        wt-tree/split<
        wt-tree/split>
        wt-tree/union
        wt-tree/intersection
        wt-tree/difference
        wt-tree/subset?
        wt-tree/set-equal?
        wt-tree/fold
        wt-tree/for-each
        wt-tree/index
        wt-tree/index-datum
        wt-tree/index-pair
        wt-tree/rank
        wt-tree/min
        wt-tree/min-datum
        wt-tree/min-pair
        wt-tree/delete-min
        wt-tree/delete-min!
        number-wt-type
        string-wt-type)

(define slib:error error)

;; We use the folowing MIT-Scheme operation on fixnums (small
;; integers).  R4RS compatible (but less efficient) definitions.
;; You should replace these with something that is efficient in your
;; system.

;; EDIT: Changed these to the gambit names (Per Eckerdal)

(define fix:fixnum? fixnum?)
(define fix:+ fx+)
(define fix:- fx-)
(define fix:< fx<)
(define fix:<= fx<=)
(define fix:> fx>)
(define fix:* fx*)

;;  A TREE-TYPE is a collection of those procedures that depend on the
;;  ordering relation.

;; MIT-Scheme structure definition
;;(define-structure
;;    (tree-type
;;     (conc-name tree-type/)
;;     (constructor %make-tree-type))
;;  (key<?       #F read-only true)
;;  (alist->tree #F read-only true)
;;  (add         #F read-only true)
;;  (insert!     #F read-only true)
;;  (delete      #F read-only true)
;;  (delete!     #F read-only true)
;;  (member?     #F read-only true)
;;  (lookup      #F read-only true)
;;  (split-lt    #F read-only true)
;;  (split-gt    #F read-only true)
;;  (union       #F read-only true)
;;  (intersection #F read-only true)
;;  (difference  #F read-only true)
;;  (subset?     #F read-only true)
;;  (rank        #F read-only true)
;;)

;; EDIT: Using gambit-style define-type
(define-type tree-type
  (key<?        read-only: unprintable:)
  (alist->tree  read-only: unprintable:)
  (add          read-only: unprintable:)
  (insert!      read-only: unprintable:)
  (delete       read-only: unprintable:)
  (delete!      read-only: unprintable:)
  (member?      read-only: unprintable:)
  (lookup       read-only: unprintable:)
  (split-lt     read-only: unprintable:)
  (split-gt     read-only: unprintable:)
  (union        read-only: unprintable:)
  (intersection read-only: unprintable:)
  (difference   read-only: unprintable:)
  (subset?      read-only: unprintable:)
  (rank         read-only: unprintable:))

;;  User level tree representation.
;;
;;  WT-TREE is a wrapper for trees of nodes.
;;
;;MIT-Scheme:
;;(define-structure
;;    (wt-tree
;;     (conc-name tree/)
;;     (constructor %make-wt-tree))
;;  (type  #F read-only true)
;;  (root  #F read-only false))

;; EDIT: Using gambit-style define-type
(define-type wt-tree
  constructor: %make-wt-tree
  (type read-only: unprintable:)
  (root unprintable:))

;;  Nodes are the thing from which the real trees are built.  There are
;;  lots of these and the uninquisitibe user will never see them, so
;;  they are represented as untagged to save the slot that would be
;;  used for tagging structures.
;;  In MIT-Scheme these were all DEFINE-INTEGRABLE

(define (make-node k v l r w) (vector w l k r v))
(define (node/k node) (vector-ref node 2))
(define (node/v node) (vector-ref node 4))
(define (node/l node) (vector-ref node 1))
(define (node/r node) (vector-ref node 3))
(define (node/w node) (vector-ref node 0))

(define empty  'empty)
(define (empty? x) (eq? x 'empty))

(define (node/size node)
  (if (empty? node) 0  (node/w node)))

(define (node/singleton k v) (make-node k v empty empty 1))

(define (with-n-node node receiver)
  (receiver (node/k node) (node/v node) (node/l node) (node/r node)))

;;
;;  Constructors for building node trees of various complexity
;;

(define (n-join k v l r)
  (make-node k v l r (fix:+ 1 (fix:+ (node/size l) (node/size r)))))

(define (single-l a_k a_v x r)
  (with-n-node r
    (lambda (b_k b_v y z) (n-join b_k b_v (n-join a_k a_v x y) z))))

(define (double-l a_k a_v x r)
  (with-n-node r
    (lambda (c_k c_v r_l z)
      (with-n-node r_l
        (lambda (b_k b_v y1 y2)
          (n-join b_k b_v
                  (n-join a_k a_v x y1)
                  (n-join c_k c_v y2 z)))))))

(define (single-r b_k b_v l z)
  (with-n-node l
    (lambda (a_k a_v x y) (n-join a_k a_v x (n-join b_k b_v y z)))))

(define (double-r c_k c_v l z)
  (with-n-node l
    (lambda (a_k a_v x l_r)
      (with-n-node l_r
        (lambda (b_k b_v y1 y2)
          (n-join b_k b_v
                  (n-join a_k a_v x y1)
                  (n-join c_k c_v y2 z)))))))

;; (define-integrable wt-tree-ratio 5)
(define wt-tree-ratio 5)

(define (t-join k v l r)
  (define (simple-join) (n-join k v l r))
  (let ((l_n  (node/size l))
        (r_n  (node/size r)))
    (cond ((fix:< (fix:+ l_n r_n) 2)   (simple-join))
          ((fix:> r_n (fix:* wt-tree-ratio l_n))
           ;; right is too big
           (let ((r_l_n  (node/size (node/l r)))
                 (r_r_n  (node/size (node/r r))))
             (if (fix:< r_l_n r_r_n)
                 (single-l k v l r)
                 (double-l k v l r))))
          ((fix:> l_n (fix:* wt-tree-ratio r_n))
           ;; left is too big
           (let ((l_l_n  (node/size (node/l l)))
                 (l_r_n  (node/size (node/r l))))
             (if (fix:< l_r_n l_l_n)
                 (single-r k v l r)
                 (double-r k v l r))))
          (else
           (simple-join)))))
;;
;;  Node tree procedures that are independent of key<?
;;

(define (node/min node)
  (cond  ((empty? node)          (error:empty 'min))
         ((empty? (node/l node)) node)
         (else                   (node/min (node/l node)))))

(define (node/delmin node)
  (cond ((empty? node)           (error:empty 'delmin))
        ((empty? (node/l node))  (node/r node))
        (else   (t-join (node/k node) (node/v node)
                        (node/delmin (node/l node)) (node/r node)))))

(define (node/concat2 node1 node2)
  (cond ((empty? node1)   node2)
        ((empty? node2)   node1)
        (else
         (let ((min-node (node/min node2)))
           (t-join (node/k min-node) (node/v min-node)
                   node1 (node/delmin node2))))))

(define (node/inorder-fold procedure base node)
  (define (fold base node)
    (if (empty? node)
        base
        (with-n-node node
          (lambda (k v l r)
            (fold (procedure k v (fold base r)) l)))))
  (fold base node))

(define (node/for-each procedure node)
  (if (not (empty? node))
      (with-n-node node
        (lambda (k v l r)
          (node/for-each procedure l)
          (procedure k v)
          (node/for-each procedure r)))))

(define (node/height node)
  (if (empty? node)
      0
      (+ 1 (max (node/height (node/l node))
                (node/height (node/r node))))))

(define (node/index node index)
  (define (loop node index)
    (let ((size_l  (node/size (node/l node))))
      (cond ((fix:< index size_l)  (loop (node/l node) index))
            ((fix:> index size_l)  (loop (node/r node)
                                         (fix:- index (fix:+ 1 size_l))))
            (else                  node))))
  (let ((bound  (node/size node)))
    (if (or (< index 0)
            (>= index bound)
            (not (fix:fixnum? index)))
        (slib:error 'bad-range-argument index 'node/index)
        (loop node index))))

(define (error:empty owner)
  (slib:error "Operation requires non-empty tree:" owner))


(define (local:make-wt-tree-type key<?)
  
  ;; MIT-Scheme definitions:
  ;;(declare (integrate key<?))
  ;;(define-integrable (key>? x y)  (key<? y x))
  
  (define (key>? x y)  (key<? y x))
  
  (define (node/find k node)
    ;; Returns either the node or #f.
    ;; Loop takes D comparisons where D is the depth of the tree
    ;; rather than the traditional compare-low, compare-high which
    ;; takes on average 1.5(D-1) comparisons
    (define (loop this best)
      (cond ((empty? this)  best)
            ((key<? k (node/k this))   (loop (node/l this) best))
            (else (loop (node/r this) this))))
    (let ((best (loop node #f)))
      (cond ((not best)               #f)
            ((key<? (node/k best) k)  #f)
            (else                     best))))
  
  (define (node/rank k node rank)
    (cond ((empty? node)             #f)
          ((key<? k (node/k node))  (node/rank k (node/l node) rank))
          ((key>? k (node/k node))
           (node/rank k (node/r node)
                      (fix:+ 1 (fix:+ rank (node/size (node/l node))))))
          (else                     (fix:+ rank (node/size (node/l node))))))
  
  (define (node/add node k v)
    (if (empty? node)
        (node/singleton k v)
        (with-n-node node
          (lambda (key val l r)
            (cond ((key<? k key)   (t-join key val (node/add l k v) r))
                  ((key<? key k)   (t-join key val l (node/add r k v)))
                  (else            (n-join key v   l r)))))))
  
  (define (node/delete x node)
    (if (empty? node)
        empty
        (with-n-node node
          (lambda (key val l r)
            (cond ((key<? x key)   (t-join key val (node/delete x l) r))
                  ((key<? key x)   (t-join key val l (node/delete x r)))
                  (else            (node/concat2 l r)))))))
  
  (define (node/concat tree1 tree2)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
           (let ((min-node (node/min tree2)))
             (node/concat3 (node/k min-node) (node/v min-node) tree1
                           (node/delmin tree2))))))
  
  (define (node/concat3 k v l r)
    (cond ((empty? l)   (node/add r k v))
          ((empty? r)   (node/add l k v))
          (else
           (let ((n1  (node/size l))
                 (n2  (node/size r)))
             (cond ((fix:< (fix:* wt-tree-ratio n1) n2)
                    (with-n-node r
                      (lambda (k2 v2 l2 r2)
                        (t-join k2 v2 (node/concat3 k v l l2) r2))))
                   ((fix:< (fix:* wt-tree-ratio n2) n1)
                    (with-n-node l
                      (lambda (k1 v1 l1 r1)
                        (t-join k1 v1 l1 (node/concat3 k v r1 r)))))
                   (else
                    (n-join k v l r)))))))
  
  (define (node/split-lt node x)
    (cond ((empty? node)  empty)
          ((key<? x (node/k node))
           (node/split-lt (node/l node) x))
          ((key<? (node/k node) x)
           (node/concat3 (node/k node) (node/v node) (node/l node)
                         (node/split-lt (node/r node) x)))
          (else (node/l node))))
  
  (define (node/split-gt node x)
    (cond ((empty? node)  empty)
          ((key<? (node/k node) x)
           (node/split-gt (node/r node) x))
          ((key<? x (node/k node))
           (node/concat3 (node/k node) (node/v node)
                         (node/split-gt (node/l node) x) (node/r node)))
          (else (node/r node))))
  
  (define (node/union tree1 tree2)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
           (with-n-node tree2
             (lambda (ak av l r)
               (let ((l1  (node/split-lt tree1 ak))
                     (r1  (node/split-gt tree1 ak)))
                 (node/concat3 ak av (node/union l1 l) (node/union r1 r))))))))
  
  (define (node/difference tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   tree1)
          (else
           (with-n-node tree2
             (lambda (ak av l r)
               (let ((l1  (node/split-lt tree1 ak))
                     (r1  (node/split-gt tree1 ak)))
                 av
                 (node/concat (node/difference l1 l)
                              (node/difference r1 r))))))))
  
  (define (node/intersection tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   empty)
          (else
           (with-n-node tree2
             (lambda (ak av l r)
               (let ((l1  (node/split-lt tree1 ak))
                     (r1  (node/split-gt tree1 ak)))
                 (if (node/find ak tree1)
                     (node/concat3 ak av (node/intersection l1 l)
                                   (node/intersection r1 r))
                     (node/concat (node/intersection l1 l)
                                  (node/intersection r1 r)))))))))
  
  (define (node/subset? tree1 tree2)
    (or (empty? tree1)
        (and (fix:<= (node/size tree1) (node/size tree2))
             (with-n-node tree1
               (lambda (k v l r)
                 v
                 (cond ((key<? k (node/k tree2))
                        (and (node/subset? l (node/l tree2))
                             (node/find k tree2)
                             (node/subset? r tree2)))
                       ((key>? k (node/k tree2))
                        (and (node/subset? r (node/r tree2))
                             (node/find k tree2)
                             (node/subset? l tree2)))
                       (else
                        (and (node/subset? l (node/l tree2))
                             (node/subset? r (node/r tree2))))))))))
  
  
    ;;; Tree interface: stripping off or injecting the tree types
  
  (define (tree/map-add tree k v)
    (%make-wt-tree (wt-tree-type tree)
                   (node/add (wt-tree-root tree) k v)))
  
  (define (tree/insert! tree k v)
    (wt-tree-root-set! tree (node/add (wt-tree-root tree) k v)))
  
  (define (tree/delete tree k)
    (%make-wt-tree (wt-tree-type tree)
                   (node/delete k (wt-tree-root tree))))
  
  (define (tree/delete! tree k)
    (wt-tree-root-set! tree (node/delete k (wt-tree-root tree))))
  
  (define (tree/split-lt tree key)
    (%make-wt-tree (wt-tree-type tree)
                   (node/split-lt (wt-tree-root tree) key)))
  
  (define (tree/split-gt tree key)
    (%make-wt-tree (wt-tree-type tree)
                   (node/split-gt (wt-tree-root tree) key)))
  
  (define (tree/union tree1 tree2)
    (%make-wt-tree (wt-tree-type tree1)
                   (node/union (wt-tree-root tree1) (wt-tree-root tree2))))
  
  (define (tree/intersection tree1 tree2)
    (%make-wt-tree (wt-tree-type tree1)
                   (node/intersection (wt-tree-root tree1) (wt-tree-root tree2))))
  
  (define (tree/difference tree1 tree2)
    (%make-wt-tree (wt-tree-type tree1)
                   (node/difference (wt-tree-root tree1) (wt-tree-root tree2))))
  
  (define (tree/subset? tree1 tree2)
    (node/subset? (wt-tree-root tree1) (wt-tree-root tree2)))
  
  (define (alist->tree alist)
    (define (loop alist node)
      (cond ((null? alist)  node)
            ((pair? alist)  (loop (cdr alist)
                                  (node/add node (caar alist) (cdar alist))))
            (else
             (slib:error 'wrong-type-argument alist "alist" 'alist->tree))))
    (%make-wt-tree my-type (loop alist empty)))
  
  (define (tree/get tree key default)
    (let ((node  (node/find key (wt-tree-root tree))))
      (if node
          (node/v node)
          default)))
  
  (define (tree/rank tree key)  (node/rank key (wt-tree-root tree) 0))
  
  (define (tree/member? key tree)
    (and (node/find key (wt-tree-root tree))
         #t))
  
  (define my-type #f)
  
  (set! my-type
        (make-tree-type ;; EDIT Removed % prefix
         key<?                        ;  key<?
         alist->tree                  ;  alist->tree
         tree/map-add                 ;  add
         tree/insert!                 ;  insert!
         tree/delete                  ;  delete
         tree/delete!                 ;  delete!
         tree/member?                 ;  member?
         tree/get                     ;  lookup
         tree/split-lt                ;  split-lt
         tree/split-gt                ;  split-gt
         tree/union                   ;  union
         tree/intersection            ;  intersection
         tree/difference              ;  difference
         tree/subset?                 ;  subset?
         tree/rank                    ;  rank
         ))
  
  my-type)

(define (guarantee-tree tree procedure)
  (if (not (wt-tree? tree))
      (slib:error 'wrong-type-argument
                  tree "weight-balanced tree" procedure)))

(define (guarantee-tree-type type procedure)
  (if (not (tree-type? type))
      (slib:error 'wrong-type-argument
                  type "weight-balanced tree type" procedure)))

(define (guarantee-compatible-trees tree1 tree2 procedure)
  (guarantee-tree tree1 procedure)
  (guarantee-tree tree2 procedure)
  (if (not (eq? (wt-tree-type tree1) (wt-tree-type tree2)))
      (slib:error "The trees" tree1 'and tree2 'have 'incompatible 'types
                  (wt-tree-type tree1) 'and (wt-tree-type tree2))))

;;;______________________________________________________________________
;;;
;;;  Export interface
;;;
(define make-wt-tree-type local:make-wt-tree-type)

(define (make-wt-tree tree-type)
  (%make-wt-tree tree-type empty))

(define (singleton-wt-tree type key value)
  (guarantee-tree-type type 'singleton-wt-tree)
  (%make-wt-tree type (node/singleton key value)))

(define (alist->wt-tree type alist)
  (guarantee-tree-type type 'alist->wt-tree)
  ((tree-type-alist->tree type) alist))

(define (wt-tree/empty? tree)
  (guarantee-tree tree 'wt-tree/empty?)
  (empty? (wt-tree-root tree)))

(define (wt-tree/size tree)
  (guarantee-tree tree 'wt-tree/size)
  (node/size (wt-tree-root tree)))

(define (wt-tree/add tree key datum)
  (guarantee-tree tree 'wt-tree/add)
  ((tree-type-add (wt-tree-type tree)) tree key datum))

(define (wt-tree/delete tree key)
  (guarantee-tree tree 'wt-tree/delete)
  ((tree-type-delete (wt-tree-type tree)) tree key))

(define (wt-tree/add! tree key datum)
  (guarantee-tree tree 'wt-tree/add!)
  ((tree-type-insert! (wt-tree-type tree)) tree key datum))

(define (wt-tree/delete! tree key)
  (guarantee-tree tree 'wt-tree/delete!)
  ((tree-type-delete! (wt-tree-type tree)) tree key))

(define (wt-tree/member? key tree)
  (guarantee-tree tree 'wt-tree/member?)
  ((tree-type-member? (wt-tree-type tree)) key tree))

(define (wt-tree/lookup tree key default)
  (guarantee-tree tree 'wt-tree/lookup)
  ((tree-type-lookup (wt-tree-type tree)) tree key default))

(define (wt-tree/split< tree key)
  (guarantee-tree tree 'wt-tree/split<)
  ((tree-type-split-lt (wt-tree-type tree)) tree key))

(define (wt-tree/split> tree key)
  (guarantee-tree tree 'wt-tree/split>)
  ((tree-type-split-gt (wt-tree-type tree)) tree key))

(define (wt-tree/union tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/union)
  ((tree-type-union (wt-tree-type tree1)) tree1 tree2))

(define (wt-tree/intersection tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)
  ((tree-type-intersection (wt-tree-type tree1)) tree1 tree2))

(define (wt-tree/difference tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/difference)
  ((tree-type-difference (wt-tree-type tree1)) tree1 tree2))

(define (wt-tree/subset? tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/subset?)
  ((tree-type-subset? (wt-tree-type tree1)) tree1 tree2))

(define (wt-tree/set-equal? tree1 tree2)
  (and (wt-tree/subset? tree1 tree2)
       (wt-tree/subset? tree2 tree1)))

(define (wt-tree/fold combiner-key-datum-result init tree)
  (guarantee-tree tree 'wt-tree/fold)
  (node/inorder-fold combiner-key-datum-result
                     init
                     (wt-tree-root tree)))

(define (wt-tree/for-each action-key-datum tree)
  (guarantee-tree tree 'wt-tree/for-each)
  (node/for-each action-key-datum (wt-tree-root tree)))

(define (wt-tree/index tree index)
  (guarantee-tree tree 'wt-tree/index)
  (let ((node  (node/index (wt-tree-root tree) index)))
    (and node (node/k node))))

(define (wt-tree/index-datum tree index)
  (guarantee-tree tree 'wt-tree/index-datum)
  (let ((node  (node/index (wt-tree-root tree) index)))
    (and node (node/v node))))

(define (wt-tree/index-pair tree index)
  (guarantee-tree tree 'wt-tree/index-pair)
  (let ((node  (node/index (wt-tree-root tree) index)))
    (and node (cons (node/k node) (node/v node)))))

(define (wt-tree/rank tree key)
  (guarantee-tree tree 'wt-tree/rank)
  ((tree-type-rank (wt-tree-type tree)) tree key))

(define (wt-tree/min tree)
  (guarantee-tree tree 'wt-tree/min)
  (node/k (node/min (wt-tree-root tree))))

(define (wt-tree/min-datum tree)
  (guarantee-tree tree 'wt-tree/min-datum)
  (node/v (node/min (wt-tree-root tree))))

(define (wt-tree/min-pair tree)
  (guarantee-tree tree 'wt-tree/min-pair)
  (let ((node  (node/min (wt-tree-root tree))))
    (cons (node/k node) (node/v node))))

(define (wt-tree/delete-min tree)
  (guarantee-tree tree 'wt-tree/delete-min)
  (%make-wt-tree (wt-tree-type tree)
                 (node/delmin (wt-tree-root tree))))

(define (wt-tree/delete-min! tree)
  (guarantee-tree tree 'wt-tree/delete-min!)
  (wt-tree-root-set! tree (node/delmin (wt-tree-root tree))))

;; < is a lexpr. Many compilers can open-code < so the lambda is faster
;; than passing <.
(define number-wt-type (local:make-wt-tree-type (lambda (u v) (< u v))))
(define string-wt-type (local:make-wt-tree-type string<?))

;;; Local Variables:
;;; eval: (put 'with-n-node 'scheme-indent-function 1)
;;; eval: (put 'with-n-node 'scheme-indent-hook 1)
;;; End:
