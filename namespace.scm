;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                 Namespace choosing functionality                 ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;


(##define-syntax get-path
  (lambda (a)
    (vector-ref a 2)))

(define ns-file (path-expand "ns.dat"
                             (path-expand
                              (path-directory
                               (get-path)))))
(define ns-table #f)
(define ns-table-timestamp #f)
(define *ns-table-dont-read-file*
  (make-parameter #f))

(define (ns-file-timestamp)
  (time->seconds
   (if ns-file
       (file-info-last-modification-time
        (file-info ns-file))
       (current-time))))

(define (ns-table-up-to-date?)
  (or (not ns-file)
      (and ns-table-timestamp
           (>= ns-table-timestamp
               (ns-file-timestamp)))))

(define (read-ns-table)
  (if (and ns-file
           (file-exists? ns-file))
      (let* ((size (file-info-size
                    (file-info ns-file)))
             (vec (make-u8vector size)))
        (with-input-from-file
            ns-file
          (lambda ()
            (read-subu8vector vec 0 size)))

        ;; This is here to fix an odd bug that put table-set! into an
        ;; infinite loop sometimes.
        (list->table
         (table->list
          (u8vector->object vec))))
      (make-table)))

(define (save-ns-table tbl)
  (if (not tbl)
      (error "Cannot save non-existent ns-table"))
  (if ns-file
      (with-output-to-file
          ns-file
        (lambda ()
          (let ((vect (object->u8vector tbl)))
            (write-subu8vector vect
                               0
                               (u8vector-length vect))))))
  (set! ns-table-timestamp
        (ns-file-timestamp)))

(define (ns-table-cleanup tbl)
  (let ((updated #f))
    (table-for-each
     (lambda (k v)
       (if (not (file-exists? k))
           (begin
             (table-set! tbl k)
             (set! updated #t))))
     tbl)

    (if updated
        (save-ns-table tbl))))

(define (get-ns-table)
  (cond
   ((or (*ns-table-dont-read-file*)
        (and ns-table
             (ns-table-up-to-date?)))
    ns-table)

   (else
    (let ((tbl (read-ns-table)))
      ;; If this is the first time ns-table is loaded, do a cleanup
      (if (not ns-table)
          (ns-table-cleanup tbl))
      
      (set! ns-table tbl)
      tbl))))

(define (suspend-ns-table-changes thunk)
  (if (*ns-table-dont-read-file*)
      (thunk)
      (begin
        (get-ns-table)
        (parameterize
         ((*ns-table-dont-read-file* #t))
         (dynamic-wind
             (lambda ()
               #f)
             thunk
             (lambda ()
               (if (eq? (*ns-table-dont-read-file*)
                        'changed)
                   (save-ns-table ns-table))))))))

(define (update-ns-table name path)
  (table-set! ns-table path name)
  (if (*ns-table-dont-read-file*)
      (*ns-table-dont-read-file* 'changed)
      (save-ns-table ns-table)))

(define (namespace-rename-reserved str)
  (cond
   ((or (string->number str)
        (string-contains str #\~)
        (equal? str "module")
        (equal? str "c"))
    (string-append str "_"))

   (else
    str)))

(define (namespace-choose-unique mod)
  (or (module-ns mod)
      (let ((ns
             (let ((abs-path (module-file mod))
                   (loader (module-loader mod)))
               (get-ns-table)
               
               (or (table-ref ns-table abs-path #f)
                   (let ((ns-no-reserved
                          (namespace-rename-reserved
                           ((loader-module-name loader) mod))))
                     (let loop ((i 0))
                       (let* ((name
                               (if (eq? i 0)
                                   ns-no-reserved
                                   (string-append
                                    ns-no-reserved
                                    "_"
                                    (number->string i))))
                              (found #f))
                         
                         (table-for-each
                          (lambda (k v)
                            (if (equal? v name)
                                (set! found #t)))
                          ns-table)
                         
                         (if found
                             (loop (+ 1 i))
                             (begin
                               (update-ns-table name abs-path)
                               name)))))))))
        (module-ns-set! mod ns)
        ns)))
