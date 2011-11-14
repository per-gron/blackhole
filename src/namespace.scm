;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                 Namespace choosing functionality                 ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;

(define ns-file (path-expand "ns.dat"
                             *blackhole-work-dir*))
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
      (with-input-from-file
          ns-file
        (lambda ()
          (list->table
           (read))))
      (make-table)))

(define (save-ns-table tbl)
  (if (not tbl)
      (error "Cannot save non-existent ns-table"))

  (create-dir-unless-exists (path-directory ns-file))

  (if ns-file
      (with-output-to-file
          ns-file
        (lambda ()
          (write (table->list tbl)))))
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
        (equal? str "bh")
        (equal? str "c"))
    (string-append str "_"))

   (else
    str)))

(define (namespace-choose-unique module-name real-path)
  (get-ns-table)
  (or (table-ref ns-table real-path #f)
      (let ((ns-no-reserved
             (namespace-rename-reserved module-name)))
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
                  (update-ns-table name real-path)
                  name)))))))
