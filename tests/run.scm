(import chicken.port)
(import test srfi-1)
(import transmission transmission.utils)

(define-syntax defhandler
  (syntax-rules ()
    ((defhandler (handler-name arguments tag) body ...)
     `(,(symbol->string 'handler-name)
        . ,(lambda (result)
             (with-transmission-result
               result
               (lambda (arguments tag . _) (and tag (begin body ...)))
               (lambda _ #f)))))))

; test-all-values :: (a -> b) -> [a] -> Bool -> [b]
; The Bool parameter is to choose whether to test the default value or not.
(define-syntax test-all-values
  (syntax-rules ()
    ((test-all-values test-maker (val ...))
     (test-all-values test-maker (val ...) #t))

    ((test-all-values test-maker (val ...) #t)
     (begin
       (test-assert (test-maker))
       (test-all-values test-maker (val ...) #f)))

    ((test-all-values test-maker (val ...) #f)
     (begin
       (test-assert (test-maker val))
       ...))))

(define-syntax test-ids
  (syntax-rules ()
    ((test-ids test-maker)
     (test-ids test-maker #t)) ; test-default?

    ((test-ids test-maker test-default?)
     (test-ids test-maker test-default? #t)) ; test-no-ids?

    ((test-ids test-maker test-default? #t)
     (test-all-values
       test-maker
       ('() "recently-active" 42 #f
        "0000000000000000000000000000000000000000"
        '(42 "0000000000000000000000000000000000000000"))
       test-default?))

    ((test-ids test-maker test-default? #f)
     (test-all-values
       test-maker
       ('() "recently-active" 42
        "0000000000000000000000000000000000000000"
        '(42 "0000000000000000000000000000000000000000"))
       test-default?))))

(define-syntax test-group-3.1/4.6
  (syntax-rules ()
    ((test-group-3.1/4.6 function test-maker-name)
     (test-group (symbol->string 'function)
       (let ((test-maker-name (lambda (#!optional (ids 'none))
                                (apply function #:tag (unique-tag!)
                                       (if (eq? ids 'none) '() `(#:ids ,ids))))))
         (test-ids test-maker-name))))))

(define (xor a b)
  (and (or a b)
       (not (and a b))))

(define (false-or pred? obj)
  (or (not obj) (pred? obj)))

(define (fields? obj)
  (and (vector? obj) (every string? (vector->list obj))))

(define (ids? obj)
  (or (id? obj)
      (and (vector? obj) (every id? (vector->list obj)))))

(define handlers
  (let ((test-3.1/4.6
          (lambda (arguments)
            (alist-let/nor arguments (ids)
                           ids))))
    `(
      ,(defhandler (blocklist-update arguments tag)
                   (not arguments))

      ,(defhandler (free-space arguments tag)
                   (alist-let/and arguments (path)
                                  path))

      ,(defhandler (port-test arguments tag)
                   (not arguments))

      ,(defhandler (queue-move-bottom arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (queue-move-down arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (queue-move-top arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (queue-move-up arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (session-close arguments tag)
                   (not arguments))

      ,(defhandler (session-get arguments tag)
                   (alist-let/nor arguments (fields)
                                  (fields? fields)))

      ,(defhandler (session-set arguments tag)
                   (let ((right-type-for-key?
                           (lambda (kv)
                             (let ((key (car kv))
                                   (value (cdr kv)))
                               (case key
                                 ((
                                   alt-speed-enabled
                                   alt-speed-time-enabled
                                   blocklist-enabled
                                   dht-enabled
                                   download-queue-enabled
                                   idle-seeding-limit-enabled
                                   incomplete-dir-enabled
                                   lpd-enabled
                                   peer-port-random-on-start
                                   pex-enabled
                                   port-forwarding-enabled
                                   queue-stalled-enabled
                                   rename-partial-files
                                   script-torrent-done-enabled
                                   seed-queue-enabled
                                   seed-ratio-limited
                                   speed-limit-down-enabled
                                   speed-limit-up-enabled
                                   start-added-torrents
                                   trash-original-torrent-files
                                   utp-enabled)
                                  (boolean? value))

                                 ((
                                   alt-speed-down
                                   alt-speed-time-begin
                                   alt-speed-time-day
                                   alt-speed-time-end
                                   alt-speed-up
                                   cache-size-mb
                                   download-queue-size
                                   idle-seeding-limit
                                   peer-limit-global
                                   peer-limit-per-torrent
                                   peer-port
                                   queue-stalled-minutes
                                   seed-queue-size
                                   seed-ratio-limit
                                   speed-limit-down
                                   speed-limit-up)
                                  (number? value))

                                 ((
                                   blocklist-url
                                   download-dir
                                   encryption
                                   incomplete-dir
                                   script-torrent-done-filename
                                   )
                                  (string? value))

                                 ; NOTE: I don't understand what this argument is
                                 ;       supposed to have, so I'll assume that
                                 ;       whatever value it has, it's right.
                                 ((units) #t)

                                 (else #f))))))
                     (false-or (cute every right-type-for-key?  <>) arguments)))

      ,(defhandler (session-stats arguments tag)
                   (not arguments))

      ,(defhandler (torrent-add arguments tag)
                   (alist-let/and arguments
                                  (
                                   (bandwidth-priority bandwidthPriority)
                                   cookies
                                   download-dir
                                   filename
                                   files-unwanted
                                   files-wanted
                                   metainfo
                                   paused
                                   peer-limit
                                   priority-high
                                   priority-low
                                   priority-normal
                                   )
                                  (and (xor (string? filename) (string? metainfo))
                                       (false-or number? bandwidth-priority)
                                       (false-or string? cookies)
                                       (false-or string? download-dir)
                                       (boolean? paused)
                                       (false-or number? peer-limit)
                                       ; TODO: array
                                       ; files-unwanted
                                       ; files-wanted
                                       ; priority-high
                                       ; priority-low
                                       ; priority-normal
                                       )))

      ,(defhandler (torrent-get arguments tag)
                   (alist-let/and arguments (fields ids format)
                                  (and (fields? fields)
                                       (false-or ids? ids)
                                       (false-or format? format))))

      ,(defhandler (torrent-reannounce arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-remove arguments tag)
                   (alist-let/nor arguments (ids delete-local-data)
                                  (and (false-or ids? ids)
                                       (boolean? delete-local-data))))

      ,(defhandler (torrent-rename-path arguments tag)
                   (alist-let/and arguments (path name ids)
                                  (and (string? path)
                                       (string? name)
                                       (false-or ids? ids))))

      ,(defhandler (torrent-set arguments tag)
                   (let ((right-type-for-key?
                           (lambda (kv)
                             (let ((key (car kv))
                                   (value (cdr kv)))
                               (case key
                                 ((
                                   bandwidthPriority
                                   downloadLimit
                                   peer-limit
                                   queuePosition
                                   seedIdleLimit
                                   seedIdleMode
                                   seedRatioLimit
                                   seedRatioMode
                                   uploadLimit)
                                  (number? value))

                                 ((
                                   files-unwanted
                                   files-wanted
                                   labels
                                   priority-high
                                   priority-low
                                   priority-normal
                                   trackerAdd
                                   trackerRemove
                                   trackerReplace)
                                  #t) ; TODO: array

                                 ((
                                   downloadLimited
                                   honorsSessionLimits
                                   uploadLimited)
                                  (boolean? value))

                                 ((ids) (ids? value))
                                 ((location) (string? value))
                                 (else #f))))))
                     (false-or (cute every right-type-for-key?  <>) arguments)))

      ,(defhandler (torrent-set-location arguments tag)
                   (alist-let/and arguments (ids location move)
                                  (and (string? location)
                                       (false-or ids? ids)
                                       (boolean? move))))

      ,(defhandler (torrent-start arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-start-now arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-stop arguments tag)
                   (test-3.1/4.6 arguments))

      ,(defhandler (torrent-verify arguments tag)
                   (test-3.1/4.6 arguments)))))

(define-constant bool-values '(#f #t))

(set!  http-call
  (lambda (req msg)
    (and-let* ((method (alist-ref 'method msg eq?))
               (handler (alist-ref method handlers string=?)))
      (let ((test-res (handler msg)))
        (unless test-res
          (with-output-to-port
            (current-error-port)
            (lambda ()
              (print "\n\nTHE MESSAGE:\t" (with-output-to-string (cute write msg)) "\n"))))
        test-res))))

(set! make-serialized-message make-message)

; Make sure it doesn't connect to any running transmission daemon.
(*host* "<null>")
(*port* 0)

(test-group "transmission"
  (test-group "high level API"
    (test-assert (blocklist-update #:tag (unique-tag!)))
    (test-assert (free-space "/some/phony/path/" #:tag (unique-tag!)))
    (test-assert (port-test #:tag (unique-tag!)))
    (test-group-3.1/4.6 queue-move-bottom queue-move-bottom/test)
    (test-group-3.1/4.6 queue-move-down queue-move-down/test)
    (test-group-3.1/4.6 queue-move-top queue-move-top/test)
    (test-group-3.1/4.6 queue-move-up queue-move-up/test)
    (test-assert (session-close #:tag (unique-tag!)))

    (test-group "session-get"
      (test-assert (session-get #:tag (unique-tag!)))
      (test-assert (session-get #:fields '("encryption") #:tag (unique-tag!))))

    ; TODO: Test the different key parameters. This is bugging me... I can't
    ;       think of a good way to do it.
    (test-assert (session-set #:tag (unique-tag!)))

    (test-assert (session-stats #:tag (unique-tag!)))

    ; TODO: Test the different key parameters. This is bugging me... I can't
    ;       think of a good way to do it.
    (test-group "torrent-add"
      (test-assert (torrent-add (torrent-source/filename "/some/phony/path/to/file.torrent") #:tag (unique-tag!)))
      (test-assert (torrent-add (torrent-source/metainfo "c29tZSBwaG9ueSB0b3JyZW50IGZpbGUK") #:tag (unique-tag!))))

    (test-group "torrent-get"
      (let ((torrent-get/test
              (lambda (#!optional (ids 'none))
                (apply torrent-get '("id") #:tag (unique-tag!)
                       (if (eq? ids 'none) '() `(#:ids ,ids))))))
        (test-ids torrent-get/test)))
    (test-group-3.1/4.6 torrent-reannounce torrent-reannounce/test)

    (test-group "torrent-remove"
      (let ((torrent-remove/test
              (lambda (#!optional (ids 'none))
                (apply torrent-remove #:tag (unique-tag!)
                       (if (eq? ids 'none) '() `(#:ids ,ids))))))
        (test-ids torrent-remove/test)))

    (test-group "torrent-rename-path"
      (let ((torrent-rename-path/test
              (lambda (#!optional (ids 'none))
                (apply torrent-rename-path
                       "/some/phony/path/"
                       "/some/other/phony/path/"
                       #:tag (unique-tag!)
                       (if (eq? ids 'none) '() `(#:ids ,ids))))))
        (test-ids torrent-rename-path/test #t #f)))

    ; TODO: Test the different key parameters. This is bugging me... I can't
    ;       think of a good way to do it.
    (test-assert (torrent-set #:tag (unique-tag!)))

    (test-group "torrent-set-location"
      (let ((torrent-set-location/test
              (lambda (#!optional (ids 'none))
                (apply torrent-set-location "/some/phony/path/"
                       #:tag (unique-tag!)
                       (if (eq? ids 'none) '() `(#:ids ,ids))))))
        (test-ids torrent-set-location/test #t #f)))

    (test-group-3.1/4.6 torrent-start torrent-start/test)
    (test-group-3.1/4.6 torrent-start-now torrent-start-now/test)
    (test-group-3.1/4.6 torrent-stop torrent-stop/test)
    (test-group-3.1/4.6 torrent-verify torrent-verify/test))

  (test-group "`reply`"
    (define (make-reply result arguments tag)
      `((result . ,result) (arguments . ,arguments) (tag . ,tag)))
    (define success-reply (make-reply "success" 'args #t))
    (define error-reply (make-reply "error" 'args #t))

    (test-assert (fixnum? (unique-tag!)))

    (test-group "reply-success?"
      (test-assert (not (reply-success? error-reply)))
      (test-assert (reply-success? success-reply)))

    (test-group "reply-tag"
      (test-assert (reply-tag error-reply))
      (test-assert (reply-tag success-reply)))

    (test-group "reply-arguments"
      (test 'args (reply-arguments error-reply))
      (test 'args (reply-arguments success-reply)))))

(test-group "transmission.utils"
  (test-group "parse-ids"
    (test #f (parse-ids "all123"))
    (test '() (parse-ids ""))
    (test #f (parse-ids "all"))
    (test "recently-active" (parse-ids "active"))
    (test '(42) (parse-ids "42"))
    (test '(1 2 3) (parse-ids "1,2,3"))
    (test '(1 2 3) (parse-ids "1-3"))
    (test '(1 2 3 5 6 7) (parse-ids "1-3,5-7"))
    (test '("618de4d680fa7c634fbf66c62c6d68a20e17ea2a") (parse-ids "618de4d680fa7c634fbf66c62c6d68a20e17ea2a"))
    (test '(1 2 3 5 6 7 "618de4d680fa7c634fbf66c62c6d68a20e17ea2a") (parse-ids "1-3,5-7,618de4d680fa7c634fbf66c62c6d68a20e17ea2a"))))

(test-exit)
