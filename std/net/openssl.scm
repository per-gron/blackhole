;
; Bindings to the OpenSSL SSL/TLS library
; Mikael Möre September 2008
;
; The perhaps most distinguishing feature of this FFI is the application-layer
; OpenSSL u8vector port implementation, accessed through the procedures
; open-ssl-tcp-client and call-with-ssl-tcp-client .
;
; For examples, see openssl-example-app below.
;
; Started as a Gambit-port of the OpenSSL Egg for Chicken Scheme v 1.1.7 by Thomas Chust,
; which in turn is a port of the OpenSSL functionality found in PLT Scheme.
;
; Various sources have been meditated upon in order to get the non-blocking functionality
; going. While there are numerous question its doability, most admit that it can really
; be done.
;
; One source fully taken into account in the development is lib/ssluse.c of libCurl 7.19.0.
;
; A revealing example of how this library should be used can be found in the OpenSSL
; source distribution's demmos/ssl/cli.cpp .
;
; An OpenSSL interface that's similar in functionality to this one is Python's.
;
; ## To build on Windows:
; http://www.devside.net/guides/windows/openssl instructs. Essentially:
;  - Download and install ActivePerl. Add it to PATH environment variable.
;  - Download and uncompress OpenSSL source files.
;  - Open a command prompt, go to OpenSSL's root dir.
;  - Run perl Configure VC-WIN32 then nmake -f ms\ntdll.mak then nmake -f ms\ntdll.mak test
;  - .lib files are put in OpenSSL's out32 dir. 

(syntax-begin
 (import ../string/util))

(export OpenSSL-Error-NONE
        OpenSSL-Error-SSL
        OpenSSL-Error-WANT-READ
        OpenSSL-Error-WANT-WRITE
        OpenSSL-Error-WANT-X509-LOOKUP
        OpenSSL-Error-SYSCALL
        OpenSSL-Error-ZERO-RETURN
        OpenSSL-Error-WANT-CONNECT
        OpenSSL-Error-WANT-ACCEPT
        OpenSSL-OP-ALL
        OpenSSL-OP-NO-SSLv2
        OpenSSL-OP-NO-SSLv3
        OpenSSL-OP-NO-TLSv1
        OpenSSL-BIO-SET-NBIO
        open-ssl-tcp-client
        call-with-ssl-tcp-client
        sockets-ctx)

(compile-options ld-options-prelude: "-lssl -lcrypto"
                 force-compile: #t)

(c-declare #<<c-declare-end

#ifdef _WIN32
#pragma comment(lib,"ws2_32")
#pragma comment(lib,"libeay32")
#pragma comment(lib,"ssleay32")
#endif

c-declare-end
)

(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (mostly-fixnum))

(use tcpip
     ../string/util)

(define openssl-example-app
  `(

    (use (common /openssl /tcpip /util))
    (set! util#trc-display-directly #t)

    ; EXAMPLE 1
    (call-with-ssl-tcp-client
     "www.ft.com" 443
     (lambda (port oport flush)
       (display "Sending request to server.\n")
       (thread-sleep! 5)
       (display "GET / HTTP/1.1\nHost: www.ft.com\n\n" port)
       (force-output port)
       (display "Reading response from server.\n")
       (thread-sleep! 5)
       (display " *** Have read:\n ** ")

       (let loop ()
         (let ;((c (read-u8 port)))
             ((v (make-u8vector 1)))
           (read-subu8vector v 0 1 port)
           (let ((c (u8vector-ref v 0)))
             (if (not (eq? #!eof c))
                 (begin
                   (display c)
                   (loop))))))
       (display " (end)\n")
       (thread-sleep! 5)))

    ; EXAMPLE 2
    (define port (open-ssl-tcp-client (list server-address: "www.ft.com" port-number: 443)))

    (display "GET / HTTP/1.1\r\nHost: www.ft.com\r\n\r\n" port) (force-output port)

    (let loop ()
      (let ;((c (read-u8 port)))
          ((v (make-u8vector 1)))
        (read-subu8vector v 0 1 port)
        (let ((c (u8vector-ref v 0)))
          (if (not (eq? #!eof c))
              (begin
                (display (integer->char c))
                (loop))))))
    (display "Out.")

    ))

(define verbose #f)

(define-macro (if-verbose . code)
  `(if verbose
       (begin
         ,@code)))

(c-declare #<<c-declare-end

#include <errno.h>
#ifdef _WIN32
  #ifdef _MSC_VER
    #include <winsock2.h>
  #else
    #include <ws2tcpip.h>
  #endif

  #include <openssl/rand.h>
#else
  #define closesocket     close
#endif

#ifdef ECOS
  #include <sys/sockio.h>
#else
  #include <unistd.h>
#endif

#include <unistd.h>

#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/bio.h>

// We have copies of this one all over the place. Centralize.
int create_u8vector_of_buffer(const void* buf, size_t bytes) {
     int r = ___EXT(___alloc_scmobj)(___sU8VECTOR,bytes,___MOVABLE0);
     ___EXT(___release_scmobj) (r);

     // Determine where in RAM the u8vector is (0 is the offset into u8vector)
     void *u8vectorptr = ___CAST(void*,&___FETCH_U8(___BODY(r),___INT(0)));

     // Copy gotten data to it
     memcpy(u8vectorptr,buf,(int) bytes);

     return r;
}

c-declare-end
           )

(c-initialize #<<c-declare-end

ERR_load_crypto_strings();
SSL_load_error_strings();
SSL_library_init();

#ifdef _WIN32
  RAND_screen();
#endif

c-declare-end
              )

(c-define-type SSL* (pointer "SSL" SSL* ; "release_SSL_star"
                             ))
(c-define-type SSL-CTX* (pointer "SSL_CTX" SSL-CTX* ; "release_SSL_CTX_star"
                                 ))
(c-define-type CTX* (pointer "CTX" CTX* ; "release_CTX_star"
                             ))
(c-define-type SSL-METHOD* (pointer "SSL_METHOD" SSL-METHOD* ; "release_SSL_CTX_star"
                                    ))
(define-macro (define-constants prefix c-prefix type . names)
  `(begin
     ,@(map
        (lambda (sym)
          `(define ,(symbol-append prefix sym)
             ((c-lambda
               ()
               ,type
               ,(string-append
                 "___result = "
                 c-prefix
                 (string-replace-char (symbol->string sym) #\- #\_)
                 ";")))))
        names)))

(define-constants OpenSSL-Error- "SSL_ERROR_" long
                  NONE SSL WANT-READ WANT-WRITE WANT-X509-LOOKUP
                  SYSCALL ; look at error stack/return value/errno
                  ZERO-RETURN WANT-CONNECT WANT-ACCEPT)

(define-constants OpenSSL-OP- "SSL_OP_" long
                  ALL

                  NO-SSLv2 NO-SSLv3 NO-TLSv1
                  )

(define-constants OpenSSL-BIO- "BIO_C_" long
                  SET-NBIO)

;; TODO This is redundant with all the code commented out
;;(define (##ssl#unwrap-client-context ctx) ctx)

;;; support routines

;; TODO This is never used
;;(define strerror (c-lambda (int) char-string "___result = strerror(___arg1);"))

;; TODO This is never used
;;(define (##net#close-socket fd)
;;  (if ((c-lambda (int) bool "closesocket") fd)
;;      ;(##sys#update-errno)
;;      ;(##sys#signal-hook
;;      ; network-error: '##net#close-socket
;;      ; (##sys#string-append "can not close socket - " strerror)
;;      ; fd)
;;      (error (dumps "can not close socket " fd "."))))

;; TODO This is never used
;;(define (##net#unwrap-tcp-ports tcp-in tcp-out)
;;  (let ((fd (tcp-socket-port->fd tcp-in)))
;;    ; (tcp-abandon-port tcp-in)
;;    ; (tcp-abandon-port tcp-out)
;;    fd))

(define (##ssl#abort loc sym . args)
  (let ((err ((c-lambda () unsigned-long "ERR_get_error"))))
    (error
     ; (make-composite-condition
     ; (make-property-condition
     ;(string-append
     (dumps
      (if sym
          (symbol->string sym)
          "error")
      ": library="
      (or
       ((c-lambda (unsigned-long)
                  char-string
                  "___result = (char*) ERR_lib_error_string(___arg1);")
        err)
       "<unknown>")
      ", function="
      (or
       ((c-lambda (unsigned-long)
                  char-string
                  "___result = (char*) ERR_func_error_string(___arg1);")
        err)
       "<unknown>")
      ", reason="
      (or
       ((c-lambda (unsigned-long)
                  char-string
                  "___result = (char*) ERR_reason_error_string(___arg1);")
        err)
       "<unknown>")
      ;)
      ;'location
      ;loc
      'arguments
      args
      ;(make-property-condition
      ; 'i/o)
      ;(make-property-condition
      ; 'net)
      ;(make-property-condition
      ; 'openssl
      ; 'status
      sym))))

;; TODO This is never used
;;(define ##ssl#ctx-free (c-lambda (SSL-CTX*) void "SSL_CTX_free"))

(define (##ssl#ctx-new protocol server)
  (let ((ctx
         ((c-lambda (SSL-METHOD*) SSL-CTX* #<<c-lambda-end
                    SSL_CTX *ctx;
                    if ((ctx = SSL_CTX_new((SSL_METHOD *)___arg1)))
                      SSL_CTX_set_mode(ctx, SSL_MODE_ENABLE_PARTIAL_WRITE);
                    ___result_voidstar = ctx;
c-lambda-end
                    )
          (case protocol
            ((sslv2-or-v3)
             (if server
                 ((c-lambda () SSL-METHOD* "SSLv23_server_method"))
                 ((c-lambda () SSL-METHOD* "SSLv23_client_method"))))
            ((sslv2)
             (if server
                 ((c-lambda () SSL-METHOD* "SSLv2_server_method"))
                 ((c-lambda () SSL-METHOD* "SSLv2_client_method"))))
            ((sslv3)
             (if server
                 ((c-lambda () SSL-METHOD* "SSLv3_server_method"))
                 ((c-lambda () SSL-METHOD* "SSLv3_client_method"))))
            ((tls)
             (if server
                 ((c-lambda () SSL-METHOD* "TLSv1_server_method"))
                 ((c-lambda () SSL-METHOD* "TLSv1_client_method"))))
            (else
             (error
              (dumps
               "invalid SSL/TLS connection protocol"
               protocol)))))))
    (if (not ctx) (##ssl#abort '##ssl#ctx-new #f))
    ; (set-finalizer! ctx ##ssl#ctx-free)
    ctx))

(define ##ssl#ctx-set-options (c-lambda (SSL-CTX* long) long "SSL_CTX_set_options"))

(define ##ssl#ctx-ctrl (c-lambda (SSL-CTX* int long) long "___result = SSL_CTX_ctrl(___arg1,___arg2,___arg3,NULL);"))

(define (##ssl#new ctx)
  (cond
   (((c-lambda (SSL-CTX*) SSL* "SSL_new") ctx)
    => values)
   (else
    (##ssl#abort '##ssl#new #f))))

(define ##ssl#free (c-lambda (SSL*) void "SSL_free"))

(define ##ssl#SSL_get_error (c-lambda (SSL* int) int "SSL_get_error"))

(define (##ssl#result-or-abort loc ssl ret allow-i/o? . args)
  (call/cc
   (lambda (q)
     (let* ((v (##ssl#SSL_get_error ssl ret))
            ; (tmpx (dbg "##ssl#result-or-abort: Got " v " for " ssl " " ret "."))
            (sym
             (cond
              ((eq? v OpenSSL-Error-NONE)
               (q ret))
              ((eq? v OpenSSL-Error-ZERO-RETURN)
               'zero-return)
              ((eq? v OpenSSL-Error-WANT-READ)
               (if allow-i/o?
                   (q 'want-read)
                   'want-read))
              ((eq? v OpenSSL-Error-WANT-WRITE)
               (if allow-i/o?
                   (q 'want-write)
                   'want-write))
              ((eq? v OpenSSL-Error-WANT-CONNECT)
               'want-connect)
              ((eq? v OpenSSL-Error-WANT-ACCEPT)
               'want-accept)
              ((eq? v OpenSSL-Error-WANT-X509-LOOKUP)
               'want-X509-lookup)
              ((eq? v OpenSSL-Error-SYSCALL)
               'syscall)
              ((eq? v OpenSSL-Error-SSL)
               'ssl)
              (else
               #f))))
       (apply ##ssl#abort loc sym args)))))

(define (##ssl#result ssl ret)
  (let* ((v (##ssl#SSL_get_error ssl ret)))
    (cond
     ((eq? v OpenSSL-Error-NONE)
      ret)
     ((eq? v OpenSSL-Error-ZERO-RETURN)
      'zero-return)
     ((eq? v OpenSSL-Error-WANT-READ)
      'want-read)
     ((eq? v OpenSSL-Error-WANT-WRITE)
      'want-write)
     ((eq? v OpenSSL-Error-WANT-CONNECT)
      'want-connect)
     ((eq? v OpenSSL-Error-WANT-ACCEPT)
      'want-accept)
     ((eq? v OpenSSL-Error-WANT-X509-LOOKUP)
      'want-X509-lookup)
     ((eq? v OpenSSL-Error-SYSCALL)
      'syscall)
     ((eq? v OpenSSL-Error-SSL)
      'ssl)
     (else
      #f))))

(define ##ssl#set-fd!-internal (c-lambda (SSL* int) int "SSL_set_fd"))

(define (##ssl#set-fd! ssl fd)
  (##ssl#result-or-abort '##ssl#set-fd! ssl (##ssl#set-fd!-internal ssl fd) #f
                                                                            fd)
  (void))

(define ##ssl#connect-internal (c-lambda (SSL*) int "SSL_connect"))

(define (##ssl#connect ssl)
  (##ssl#result-or-abort
        '##ssl#connect ssl (##ssl#connect-internal ssl) #t))

(define (##ssl#accept ssl)
  (##ssl#result-or-abort
        '##ssl#accept ssl
                      ((c-lambda (SSL*) int "SSL_accept") ssl) #t))

(define (##ssl#shutdown ssl)
  (let ((r ((c-lambda (SSL*) int
                      "SSL_shutdown") ssl)))
    (cond ((eq? r 0) #f)
          ((eq? r 1) #t)
          (else (##ssl#result-or-abort '##ssl#shutdown ssl r #t)))))

(define (##ssl#get-char ssl)
  (let ((ret
         ((c-lambda (SSL*) int #<<c-lambda-end
                     char ch;
                     int ret;
                     ret = SSL_read((SSL *)___arg1, &ch, 1); // Reintroduce some removed code here to get working. 
c-lambda-end
                    )
          ssl)))
    (if (fixnum? ret)
        (##ssl#result-or-abort '##ssl#get-char ssl ret #t)
        ret)))

(define (##ssl#read ssl)
  ((c-lambda (SSL*) scheme-object #<<c-lambda-end

// fprintf(stderr,"SSL_read: SSL_pending() gives %i for this SSL.\n",SSL_pending(___arg1));

char buf[10240];
int readbytes = SSL_read(___arg1,buf,sizeof(buf));

if ((readbytes > 0) && (readbytes != -1) /* why is this one even necessary. */) {
   // fprintf(stderr,"SSL_read: Returned success %i.\n",readbytes);
   ___result = create_u8vector_of_buffer(buf,readbytes);
} else {
   // fprintf(stderr,"SSL_read: Returned failure %i.\n",readbytes);
   ___result = ___FIX(readbytes);
}

// fprintf(stderr,"SSL_read: Returning %i.\n",___result);

c-lambda-end
             ) ssl))

(define ##ssl#write-private (c-lambda (SSL* scheme-object int int) int #<<c-lambda-end

    // fprintf(stderr,"ssl#write: Initiated.\n");
    void* buf = ___CAST(void*,&___FETCH_U8(___BODY(___arg2),___INT(0)));
    // fprintf(stderr,"ssl#write: Writing to SSL* ptr %i from ptr %i offset %i %i bytes.\n",(int) ___arg1,(int) ___arg2,___arg3,___arg4);
    ___result = SSL_write(___arg1, (char *) buf + ___arg3,___arg4);

c-lambda-end
                                      ))

(define (##ssl#write ssl buffer offset size)
  (##ssl#result-or-abort
        '##ssl#write
              ssl
              (##ssl#write-private
                    ssl buffer offset size)
              #t))


(define (##ssl#write-string ssl s)
  (let ((v (string->utf8-u8vector s)))
    (##ssl#write ssl v 0 (u8vector-length v))))

(define ##ssl#get-cipher (c-lambda (SSL*) char-string "___result = (char*) SSL_get_cipher(___arg1);"))

; Prepare SSL object to work in client mode
(define ##ssl#SSL-set-connect-state (c-lambda (SSL*) void "SSL_set_connect_state"))

; Prepare SSL object to work in server mode
(define ##ssl#SSL-set-accept-state (c-lambda (SSL*) void "SSL_set_accept_state"))

; The custom OpenSSL port implementation.
; Gives the caller an u8vector port using which to access OpenSSL.
(define (##ssl#make-i/o-ports ctx tcp-socket-port ssl)
  ;; note that the ctx parameter is never used but it is passed in order
  ;; to be present in the closure data of the various port functions
  ;; so it isn't garbage collected before the ports are all gone
  (let ((to-beneficiary-thread #f)
        (from-beneficiary-thread #f)
        (read-mutex (make-mutex))
        (shutdown-mutex (make-mutex))
        (has-read #f)
        (has-written #f))
    (call-with-values
     open-u8vector-pipe
     (lambda (our-port beneficiary-port)

       (define (shutdown)
         (mutex-lock! shutdown-mutex) ; If this procedure would be active in parallell, we would have a mess.
         (if-verbose (dbg "##ssl#make-i/o-ports: Shutting down."))
         (if (and from-beneficiary-thread to-beneficiary-thread)
             (thread-terminate! (if (equal? (current-thread) to-beneficiary-thread) from-beneficiary-thread to-beneficiary-thread)))
         (set! ctx #f) ;; ensure that this reference is lost
         (dynamic-wind
          void
          (lambda ()
            (let loop ()
              (case (##ssl#shutdown ssl)
                ((want-read)
                 (port-wait-for-input tcp-socket-port)
                 (thread-yield!)
                 (loop))
                ((want-write)
                 (port-wait-for-output tcp-socket-port)
                 (thread-yield!)
                 (loop)))))
          (lambda ()
            (##ssl#free ssl)
            (close-port tcp-socket-port)
            (if-verbose (dbg "##ssl#make-i/o-ports: Shut down. (Terminating port's last thread.)"))
            (thread-terminate! (current-thread))
            )))

       (define (handle-syscall-response ret caller)
         (cond
          ((eq? ret 0)
           ; According to http://groups.google.com/group/mailing.openssl.users/browse_thread/thread/1e652cb1a03be017/4f6e295c6789f901?lnk=st&q=SSL_ERROR_SYSCALL#4f6e295c6789f901 ,
           ; there are instances when we get this error due to broken SSL implementations.
           ; Let's deal with it this way: If we have had successful SSL sending AND reading previously,
           ; let's consider this an OK way to close the connection.

           ;(if (and has-read has-written)
           ;(shutdown)
           (error (dumps "An EOF was observed that violates the protocol (upon " caller ").")
                  ;)
                  ))

          ((eq? ret -1)
           (error (dumps "The underlying BIO reported an I/O error. (upon " caller ". the posix errno might give advice, be careful to pull it before any other system call is made.)")))
          (else
           (error (dumps "Unknown syscall error. (upon " caller ". ret is " ret ".)")))))

       (define (do-read)
         (dynamic-wind
          (lambda () #t)
          (lambda ()
            (mutex-lock! read-mutex)
            (let ((max-read-recursions 3))
              (let read-again ()
                ; (if-verbose (dbg "##ssl#make-i/o-ports: Reading from TCP port."))
                (let ((v (##ssl#read ssl)))
                  (if (u8vector? v) ; Got data?
                      (begin
                        (set! has-read #t)
                        (if-verbose (dbg "##ssl#make-i/o-ports: Writing results to beneficiary (" (u8vector-length v) " bytes)."))
                        (write-subu8vector v 0 (u8vector-length v) our-port) ; Yep, write to beneficiary.
                        (if-verbose (dbg "##ssl#make-i/o-ports: Written to beneficiary."))

                        (if-verbose
                         (trc-nobr "SSL GOT: ")
                         (write-subu8vector v 0 (u8vector-length v) factual-stdout)
                         (trc-nobr " (end)\n"))
                        )
                      ;(if (zero? v)
                      ; Had zero-length result.
                      ;#t

                      ; Had error return value.
                      (begin
                        ; (if-verbose (dbg "##ssl#make-i/o-ports: Something was wrong about reading from TCP port, got non-u8v value " v ". Checking out what."))
                        (let ((meaning (##ssl#result ssl v)))
                          (if-verbose
                           (if (not (eq? meaning 'want-read)) (dbg "##ssl#make-i/o-ports: Read from TCP port error meaning was " meaning ".")))
                          (cond

                           ; zero-return:
                           ; The TLS/SSL connection has been closed. [..] does not necessarily indicate that the underlying transport has been closed. 
                           ((eq? meaning 'zero-return)
                            (shutdown))

                           ; 'want-read:
                           ; According to http://groups.google.com/group/mailing.openssl.dev/msg/cd6fbb747111cb8c ,
                           ; want-read return from SSL_read ( = ##ssl#read) indicates that we should select() ( = port-wait-for-input).
                           ; libCurl's lib/ssluse.c takes an a bit different approach, it says:
                           ; "there's data pending, re-invoke SSL_read()" "basically EWOULDBLOCK".
                           ((eq? meaning 'want-read)
                            ;(set! max-read-recursions (- max-read-recursions 1))
                            ;(if (< 0 max-read-recursions)
                            ;    (read-again)
                            ;    (if-verbose (dbg "Max read recursions reached, returning.")))
                            (thread-yield!)
                            (if-verbose (dbg "SSL_read returned want-read. Going to select()."))
                            #t
                            )

                           ; 'want-write:
                           ; libCurl's lib/ssluse.c says:
                           ; "there's data pending, re-invoke SSL_read()" "basically EWOULDBLOCK".
                           ((eq? meaning 'want-write)
                            (thread-yield!)
                            (if-verbose
                             (dbg "##ssl#make-i/o-ports: What on earth, SSL_READ returned want-write. Uh. Let's just ignore it for now, we'll write when we want to write in any case."))
                            #t)

                           ((eq? meaning 'syscall)
                            (handle-syscall-response v "SSL_read"))

                           (else
                            (error (dumps "Unknown return value to SSL_read " v " " meaning " (error code is " (##ssl#SSL_get_error ssl v) ").")))))))))))
          (lambda ()
            (mutex-unlock! read-mutex))))

       (define from-beneficiary-thread-thunk
         (lambda ()
           (run-dump-exceptions
            (lambda()
              (let loop ()
                ; Wait for there to be data to feed OpenSSL with on the beneficiary side.
                ;(if-verbose (dbg "##ssl#make-i/o-ports: Waiting for input from beneficiary."))
                ;(port-wait-for-input our-port)
                ;(if-verbose (dbg "##ssl#make-i/o-ports: Had input from beneficiary."))

                ; Read it
                (let* ((max-buf 1)
                       (v (make-u8vector max-buf))
                       (tmpx (if-verbose (dbg "##ssl#make-i/o-ports: Going into read from beneficiary.")))
                       (bytes (read-subu8vector v 0 (u8vector-length v) our-port)))
                  (if-verbose (dbg "##ssl#make-i/o-ports: Read from beneficiary. Going into write to ssl " ssl " vector " v " bytes " bytes "."))
                  (if (< 0 bytes)
                      ; Write it
                      (let write-again ()
                        (let ((r (##ssl#write-private ssl v 0 bytes)))
                          (if-verbose (dbg "##ssl#make-i/o-ports: Written."))
                          (cond
                           ((< 0 r)
                            (if (not (= bytes r))
                                (error (dumps "Not the same number of bytes written as intended to be written! (" bytes " vs " r ".)"))
                                (begin
                                  (set! has-written #t)
                                  (loop))))
                           (else
                            (if-verbose (dbg "##ssl#make-i/o-ports: Something about write seems nonworking, value is " r ". Let's see what."))
                            (let ((meaning (##ssl#result ssl r)))
                              (if-verbose (dbg "ssl#make-i/o-ports: The error is " meaning "."))
                              (cond
                               ; 'zero-return:
                               ; The TLS/SSL connection has been closed. [..] does not necessarily indicate that the underlying transport has been closed. 
                               ((eq? meaning 'zero-return)
                                (if-verbose
                                 (dbg "##ssl-make-i/o-port-from-beneficiary-thread: had zero-return status, means connection is closed."))
                                (shutdown))

                               ; 'want-read:
                               ; If I understand libCurl's lib/ssluse.c right here, it interprets both 'want-read and 'want-write
                               ; as blocking signals, only to lead to select().
                               ((eq? meaning 'want-read)
                                (if-verbose
                                 (dbg "##ssl-make-i/o-port-from-beneficiary-thread: got want-read."))
                                (thread-sleep! 1)
                                (write-again))

                               ; 'want-write:
                               ; If I understand libCurl's lib/ssluse.c right here, it interprets both 'want-read and 'want-write
                               ; as blocking signals, only to lead to select().
                               ((eq? meaning 'want-write)
                                (if-verbose
                                 (dbg "##ssl-make-i/o-port-from-beneficiary-thread: got want-write. Waiting for output deblock."))
                                (port-wait-for-output tcp-socket-port)
                                (if-verbose
                                 (dbg "##ssl-make-i/o-port-from-beneficiary-thread: got output deblock. Trying to write again."))
                                (write-again))

                               ((eq? meaning 'syscall)
                                (handle-syscall-response r "SSL_write"))

                               (else
                                (error (dumps "Unknown return value to SSL_write " r " meaning " meaning " (error code is " (##ssl#SSL_get_error ssl r) ")."))))))))))))))))

       (set! to-beneficiary-thread
             (thread-start!
              (make-thread
               (lambda ()
                 (run-dump-exceptions
                  (lambda ()
                    (let loop ()
                      ; On entry: Go into read, to finalize connect phase.
                      ; If a loop: Invoke OpenSSL to process data
                      (do-read)

                      ; If write thread isn't started yet, start.
                      (if (not from-beneficiary-thread)
                          (begin
                            (set! from-beneficiary-thread
                                  (thread-start!
                                   (make-thread
                                    from-beneficiary-thread-thunk)))))

                      ; Wait for there to be data to feed OpenSSL with on the TCP socket side. 
                      (if-verbose
                       (dbg "##ssl#make-i/o-ports: Waiting for input from TCP port."))
                      (port-wait-for-input tcp-socket-port)
                      (if-verbose
                       (dbg "##ssl#make-i/o-ports: Had input from TCP port."))

                      (loop))))))))

       ;(##sys#setslot in 3 "(ssl)")
       ;(##sys#setslot out 3 "(ssl)")
       ;(##sys#setslot in 7 'socket)
       ;(##sys#setslot out 7 'socket)
       ;(##sys#setslot (##sys#port-data in) 0 fd)
       ;(##sys#setslot (##sys#port-data out) 0 fd)
       beneficiary-port))))

;; TODO This code doesn't work
;;(define (##ssl#unwrap-context obj)
;;  (cond
;;   ((ssl-client-context? obj)
;;    (##ssl#unwrap-client-context obj))
;;   ((ssl-listener? obj)
;;    (##ssl#unwrap-listener-context obj))
;;   (else
;;    (abort
;;     (make-property-condition
;;      'exn
;;      'location '##ssl#unwrap-context
;;      'message "expected an ssl-client-context or ssl-listener, got"
;;      'arguments (list obj))
;;     (make-property-condition
;;      'type)))))

;;; exported routines

;; create SSL client context
;; TODO This is never used
;;(define-type ssl-client-context id: ssl-client-context context)

;; TODO This doesn't work
;;(define (ssl-make-client-context #!optional (protocol 'sslv2-or-v3))
;;  (##ssl#wrap-client-context (##ssl#ctx-new protocol #f)))

;; connect to SSL server
;; TODO This doesn't work
;;(define (ssl-connect hostname #!optional port (ctx 'sslv2-or-v3))
;;  (let* ((fd
;;          (call-with-values (cut tcp-connect hostname port)
;;                            ##net#unwrap-tcp-ports))
;;         (ctx
;;          (if (ssl-client-context? ctx)
;;              (##ssl#unwrap-client-context ctx)
;;              (##ssl#ctx-new ctx #f)))
;;         (ssl
;;          (##ssl#new ctx)))
;;    (let ((success? #f))
;;      (dynamic-wind
;;       void
;;       (lambda ()
;;         (##ssl#set-fd! ssl fd)
;;         (let loop ()
;;           (case (##ssl#connect ssl)
;;             ((want-read)
;;              (##sys#thread-block-for-i/o! ##sys#current-thread fd #t)
;;              (thread-yield!)
;;              (loop))
;;             ((want-write)
;;              (##sys#thread-block-for-i/o! ##sys#current-thread fd #f)
;;              (thread-yield!)
;;              (loop))))
;;         (set! success? #t))
;;       (lambda ()
;;         (unless success?
;;           (##ssl#free ssl)
;;           (##net#close-socket fd)))))
;;    (##ssl#make-i/o-ports ctx fd ssl)))

;; create listener/SSL server context
;; TODO This is redundant with all the other code commented out.
;;(define-type ssl-listener id: ssl-listener
;;             context
;;             listener)

;; TODO This doesn't work
;;(define (ssl-listen port #!optional (backlog 4) (hostname #f) (ctx 'sslv2-or-v3))
;;  (##ssl#wrap-listener
;;        (if (ssl-client-context? ctx)
;;            (##ssl#unwrap-client-context ctx)
;;            (##ssl#ctx-new ctx #t))
;;        (tcp-listen port backlog hostname)))

;; shutdown a SSL server
;; TODO This doesn't work
;;(define (ssl-close listener)
;;  (tcp-close (##ssl#unwrap-listener listener)))

;; return the port number this listener is operating on
;; TODO This doesn't work
;;(define (ssl-listener-port listener)
;;  (tcp-listener-port (##ssl#unwrap-listener listener)))

;; get the underlying socket descriptor number for an SSL listener
;; TODO This doesn't work
;;(define (ssl-listener-fileno listener)
;;  (tcp-listener-fileno (##ssl#unwrap-listener listener)))

;; check whether an incoming connection is pending
;; TODO This doesn't work
;;(define (ssl-accept-ready? listener)
;;  (tcp-accept-ready? (##ssl#unwrap-listener listener)))

;; accept a connection from an SSL listener
;; TODO This doesn't work
;;(define (ssl-accept listener)
;;  (let* ((fd
;;          (call-with-values (cut tcp-accept (##ssl#unwrap-listener listener))
;;                            ##net#unwrap-tcp-ports))
;;         (ssl
;;          (##ssl#new (##ssl#unwrap-listener-context listener))))
;;    (let ((success? #f))
;;      (dynamic-wind
;;       void
;;       (lambda ()
;;         (##ssl#set-fd! ssl fd)
;;         (let loop ()
;;           (case (##ssl#accept ssl)
;;             ((want-read)
;;              (##sys#thread-block-for-i/o! ##sys#current-thread fd #t)
;;              (thread-yield!)
;;              (loop))
;;             ((want-write)
;;              (##sys#thread-block-for-i/o! ##sys#current-thread fd #f)
;;              (thread-yield!)
;;              (loop))))
;;         (set! success? #t))
;;       (lambda ()
;;         (unless success?
;;           (##ssl#free ssl)
;;           (##net#close-socket fd)))))
;;    (##ssl#make-i/o-ports (##ssl#unwrap-listener-context listener) fd ssl)))

;; load identifying certificate chain into SSL context
;; TODO This code doesn't work
;;(define (ssl-load-certificate-chain! obj pathname)
;;  (##sys#check-string pathname)
;;  (unless (eq?
;;           ((c-lambda (SSL-CTX* char-string)
;;                      int "SSL_CTX_use_certificate_chain_file")
;;            (##ssl#unwrap-context obj) (##sys#expand-home-path pathname))
;;           1)
;;    (##ssl#abort 'ssl-load-certificate-chain! #f pathname)))

;; load the private key for the identifying certificate chain
;; TODO This doesn't work
;;(define (ssl-load-private-key! obj pathname #!optional (rsa? #t) (asn1? #f))
;;  (##sys#check-string pathname)
;;  (if (not (eq?
;;            ((c-lambda (SSL-CTX* char-string bool bool) int #<<c-lambda-end
;;             if (___arg3)
;;               ___result = SSL_CTX_use_RSAPrivateKey_file(___arg1,___arg2,(___arg4 ? SSL_FILETYPE_ASN1 : SSL_FILETYPE_PEM));
;;             else
;;               ___result = SSL_CTX_use_PrivateKey_file(___arg1,___arg2,(___arg4 ? SSL_FILETYPE_ASN1 : SSL_FILETYPE_PEM));
;;c-lambda-end
;;                       )
;;             (##ssl#unwrap-context obj) (##sys#expand-home-path pathname)
;;             rsa? asn1?)
;;            1))
;;      (##ssl#abort 'ssl-load-private-key! #f pathname rsa? asn1?)))

;; switch verification of peer on or off
;; TODO This doesn't work
;;(define (ssl-set-verify! obj v)
;;  ((c-lambda (SSL-CTX* bool) void #<<c-lambda-end
;;             SSL_CTX_set_verify(___arg1,(___arg2 ? SSL_VERIFY_PEER|SSL_VERIFY_FAIL_IF_NO_PEER_CERT : SSL_VERIFY_NONE),NULL);
;;c-lambda-end
;;             )
;;   (##ssl#unwrap-context obj) v))

;; load trusted root certificates into SSL context
;;(define (ssl-load-verify-root-certificates! obj pathname #!optional (dirname #f))
;;  (if pathname (##sys#check-string pathname))
;;  (if dirname (##sys#check-string dirname))
;;  (unless (eq?
;;           ((c-lambda (SSL-CTX* char-string char-string)
;;                      int "SSL_CTX_load_verify_locations")
;;            (##ssl#unwrap-context obj)
;;            (if pathname (##sys#expand-home-path pathname) #f)
;;            (if dirname (##sys#expand-home-path dirname) #f))
;;           1)
;;    (##ssl#abort 'ssl-load-verify-root-certificates! #f pathname dirname)))

;; load suggested root certificates into SSL context
;(define (ssl-load-suggested-certificate-authorities! obj pathname)
;  (##sys#check-string pathname)
;  (cond
;   (((c-lambda (char-string) SSL* "SSL_load_client_CA_file")
;     (##sys#expand-home-path pathname))
;    => (cut
;        (c-lambda (SSL-CTX* STACK_OF_X509_NAME*) void "SSL_CTX_set_client_CA_list")
;        (##ssl#unwrap-context obj) <>))
;   (else
;    ##ssl#abort 'ssl-load-suggested-certificate-authorities! #f pathname)))

(define (open-ssl-tcp-client port-settings)
  (let* ((tmp1 (if-verbose (dbg "Opening connection.")))
         (port (open-tcp-client port-settings))
         (tmp2 (if-verbose (dbg "Setting up SSL.")))
         (ssl (##ssl#new sockets-ctx)))

    (##ssl#SSL-set-connect-state ssl)

    ; Inspired by Curl_socket_ready in curl's lib/ssluse.c:
    (if-verbose (dbg "Waiting for socket to be ready for output. "
                     "(Don't know if this one is needed.)"))
    (port-wait-for-output port)

    (if-verbose (dbg "Getting FD."))

    (let ((fd (tcp-socket-port->fd port)))

      (if-verbose (dbg "FD is " fd ". Setting."))
      (if (not (##ssl#set-fd! ssl fd))
          (error "Could not set file descriptor on SSL structure."))

      (if-verbose (dbg "Connecting."))
      (let ((r (##ssl#connect ssl)))
        (if (not (or (eq? r 1)
                     (eq? r 'want-read)
                     (eq? r 'want-write)))
            (error (dumps "SSL_connect returned " r
                          " rather than 1 or want-read or "
                          "want-write. Fatal error, for instance "
                          "with handshake."))))

      (if-verbose (dbg "Creating port wrapper."))
      (##ssl#make-i/o-ports sockets-ctx port ssl))))

(define (call-with-ssl-tcp-client hostname port-num proc)
  (let* ((port
          (open-ssl-tcp-client
           (list server-address: hostname
                 port-number: port-num)))
         (result
          (proc port)))
    (close-port port)
    result))

; ## GLOBAL INITIALIZATION ROUTINES 
(define sockets-ctx (##ssl#ctx-new 'sslv2-or-v3 #f))

; From lib/ssluse.c of libCurl:
; OpenSSL contains code to work-around lots of bugs and flaws in various
; SSL-implementations. SSL_CTX_set_options() is used to enabled those
; work-arounds. The man page for this option states that SSL_OP_ALL enables
; all the work-arounds and that "It is usually safe to use SSL_OP_ALL to
; enable the bug workaround options if compatibility with somewhat broken
; implementations is desired."

(##ssl#ctx-set-options sockets-ctx OpenSSL-OP-ALL)

; From lib/ssluse.c of libCurl:
; disable SSLv2 in the default case (i.e. allow SSLv3 and TLSv1)

(##ssl#ctx-set-options sockets-ctx OpenSSL-OP-NO-SSLv2)

; From lib/ssluse.c of libCurl:
; Not sure it's needed to tell SSL_connect() that socket is
; non-blocking. It doesn't seem to care, but just return with
; SSL_ERROR_WANT_x.

(##ssl#ctx-ctrl sockets-ctx OpenSSL-BIO-SET-NBIO 1)






















