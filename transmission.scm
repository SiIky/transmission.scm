(module
  transmission
  (
   *host*
   *password*
   *port*
   *session-id*
   *url*
   *username*

   update-request-session-id
   handle-409
   serialize-message
   make-rpc-request

   rpc-call

   make-rpc-call
   define-rpc-call
   export-rpc-call

   filename
   metainfo
   )

  (import
    scheme
    (only chicken.base
          assert
          cut
          fixnum?
          identity
          make-parameter
          print)
    (only chicken.condition
          condition-case
          get-condition-property
          signal)
    (only chicken.module
          export)
    (only chicken.port
          with-output-to-string)
    (only chicken.string
          ->string))

  (import
    (only http-client
          with-input-from-request)
    (only intarweb
          header-values
          headers
          make-request
          response-code
          response-headers
          update-request)
    (only json
          json-read
          json-write)
    (only srfi-1
          every
          filter)
    (only uri-common
          make-uri))

  (define (assert* loc type type?)
    (lambda (x)
      (assert
        (type? x) loc
        (string-append "Expected " type ", but got " (->string x)))
      x))

  (define false? not)

  (define (or? . rest)
    (lambda (x)
      (let loop ((l rest))
        (or (null? l)
            ((car l) x)
            (loop (cdr l))))))

  ;;;
  ;;; RPC Parameters
  ;;; @see https://github.com/transmission/transmission/wiki/Editing-Configuration-Files#rpc
  ;;;

  (define *host* (make-parameter "localhost" (assert* '*host* "a string" string?)))

  ;; rpc-url
  (define *url* (make-parameter '(/ "transmission" "rpc")))

  ;; rpc-port
  (define *port*
    (make-parameter 9091 (assert* '*port* "an integer" fixnum?)))

  ;; rpc-username
  (define *username*
    (make-parameter #f (assert* '*username* "a string or #f" (or? false? string?))))

  ;; rpc-password
  (define *password*
    (make-parameter #f (assert* '*password* "a string or #f" (or? false? string?))))

  ;; 2.3.1 X-Transmission-Session-Id
  ;; @see https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
  (define *session-id* (make-parameter "" (assert* '*session-id* "a string" string?)))

  ;;;
  ;;; Core Procedures
  ;;;

  ;; @brief Create a request object
  ;; @param host The hostname of the RPC server
  ;; @param url See `rpc-url`
  ;; @param port See `rpc-port`
  ;; @param username See `rpc-username`
  ;; @param password See `rpc-password`
  ;; @param session-id The `x-transmission-session-id` header
  ;; @returns A request object
  ;;
  ;; @a username and @a password are only required if
  ;;   `rpc-authentication-required` is enabled
  (define (make-rpc-request host url port username password #!optional (session-id (*session-id*)))
    (make-request
      #:method 'POST
      #:uri (make-uri
              #:scheme 'http
              #:host host
              #:port port
              #:path url
              #:username username
              #:password password)
      #:headers (headers `((x-transmission-session-id ,session-id)))))

  ;; @brief Serialize a message, according to the spec
  ;; @param method The `method` field
  ;; @param arguments The `arguments` field
  ;; @param tag The `tag` field
  ;; @returns A string of the serialized JSON message
  ;; @see Section 2.1
  (define (serialize-message method arguments tag)
    (define (mkmsg method arguments tag)
      (let ((optional (filter cdr `((arguments . ,arguments) (tag . ,tag)))))
        (list->vector `((method . ,method) . ,optional))))
    (with-output-to-string
      (cut json-write (mkmsg method arguments tag))))

  ;; @brief Update a request's `x-transmission-session-id` header
  ;; @param request The request
  ;; @param session-id The new session ID
  ;; @returns The new request
  (define (update-request-session-id request #!optional (session-id (*session-id*)))
    (update-request request #:headers (headers `((x-transmission-session-id ,session-id)))))

  ;; @brief Handle 409, according to the spec
  ;; @param condition The condition object
  ;; @param request The request
  ;; @param message The RPC message
  ;; @returns The deserialized response of the RPC call
  ;;
  ;; If the condition is caused by anything other than 409, the exception is
  ;;   propagated. In case of 409, the new session ID is read from the response
  ;;   headers and we try again. If this second try results in an error, it is
  ;;   propagated.
  (define (handle-409 condition request message)
    (let ((response (get-condition-property condition 'client-error 'response)))

      ; See 2.3.1 for how to handle 409
      (cond

        ; The condition object may not have a response property, in which
        ;   case, it is not a 409
        ((and response
              (= (response-code response) 409))
         (let ((session-id (car (header-values 'x-transmission-session-id (response-headers response)))))
           ;(print "GOT A 409!")
           (*session-id* session-id)
           (let ((req (update-request-session-id request session-id)))
             (with-input-from-request req message json-read))))

        ; Rethrow any other errors
        (else
          ;(print "NOT A 409!")
          (signal condition)))))

  ;; @brief Make an RPC call to a Transmission daemon
  ;; @param method A string naming an RPC method
  ;; @param arguments The arguments of this method
  ;; @param tag The tag for this call
  ;; @returns A response object read with json-read, or #f in case of wrong
  ;;          parameters
  ;;
  ;; Throws exceptions for HTTP errors, except for 409, which is handled
  ;;   according to 2.3.1; see http-client for other HTTP errors.
  ;;
  ;; `rpc-call` returns #f if parameters are wrong. Some parameters are
  ;; mandatory (with defaults provided): `*host*`, `*url*`, `*port*`.
  ;; Others are optional: `*session-id*`, `*password*`, `*username*`.
  ;; `*password*` and `*username*` are, however, optional "together"; they must
  ;; both be #f or a string.
  ;;
  ;; See the json egg for how to encode arguments and decode responses.
  (define (rpc-call method #!key (arguments #f) (tag #f))
    (define (call host url port username password method arguments tag)
      (let ((req (make-rpc-request host url port username password))
            (message (serialize-message method arguments tag)))
        (condition-case
          (with-input-from-request req message json-read)
          (condition (exn http client-error) ; 409 is in this condition kind
                     (handle-409 condition req message)))))

    (let ((host (*host*))
          (url (*url*))
          (port (*port*))
          (username (*username*))
          (password (*password*))
          (arguments (and arguments
                          (positive? (vector-length arguments))
                          arguments)))
      (and host url port
           (or (and username password)
               (not (or username password)))
           (call host url port username password method arguments tag))))

  ;;;
  ;;; General & Common Utilities
  ;;;

  (define (just x)     `(just . ,x))
  (define nothing      'nothing)
  (define (just? x)    (and (pair? x) (eq? (car x) 'just)))
  (define (nothing? x) (eq? x nothing))
  (define (maybe? x)   (or (nothing? x) (just? x)))
  (define (unwrap x)   (cdr x))

  (define (->maybe b)
    (cond ((or (false? b) (nothing? b)) nothing)
          ((just? b) b)
          (else (just b))))

  (define (maybe f x)
    ((assert* 'maybe "a maybe" maybe?) x)
    (if (just? x) (f (unwrap x)) nothing))

  (define (maybe-do . procs)
    (lambda (x)
      (let loop ((x x) (procs procs))
        (if (or (null? procs) (nothing? x))
            x
            (loop (maybe (car procs) x) (cdr procs))))))

  ;; For `torrent-add`
  (define (filename str) `(filename . ,str))
  (define (metainfo str) `(metainfo . ,str))
  (define (filename? ts) ((torrent-source-with-tag? 'filename) ts))
  (define (metainfo? ts) ((torrent-source-with-tag? 'metainfo) ts))
  (define (torrent-source-with-tag? tag)
    (lambda (ts) (and ts (pair? ts) (eq? tag (car ts)) (string? (cdr ts)))))
  (define (torrent-source? ts)
    (or ((torrent-source-with-tag? 'filename) ts) ((torrent-source-with-tag? 'metainfo) ts)))

  ;; @brief Make a function that returns #f or an argument pair
  ;; @param key The argument's key
  ;; @param proc Function that processes an input value
  ;; @returns A function that, from an input value, returns #f or an argument pair
  ;;
  ;; @a proc must return a Maybe. If the result of proc is Nothing, it returns
  ;;   #f, and if the result is Just, it returns an argument pair.
  (define (make-*->arguments key proc)
    (lambda (x)
      (let ((x (proc x)))
        (and (just? x) `(,key . ,(unwrap x))))))

  (define (id? id)
    (or (fixnum? id)
        (and (string? id)
             (or (= (string-length id) 40) ; SHA1
                 (string=? id "recently-active")))))

  ;; @brief Pre-process an ID
  ;; @param id An ID
  ;; @returns A Maybe
  (define (proc-id id)
    (->maybe (and id (id? id) id)))

  ;; @brief Pre-process a list of IDs
  ;; @param ids A list of IDs
  ;; @returns A Maybe
  ;;
  ;; @a ids can be:
  ;;   * '() meaning no torrent (the default);
  ;;   * #f meaning all torrents;
  ;;   * "recently-active" meaning the recently active torrents;
  ;;   * a single ID (fixnum);
  ;;   * a single hash (string);
  ;;   * a list of torrent IDs and hashes
  (define (proc-ids ids)
    ((maybe-do
       ; Handle "recently-active" and list of strings (hash) and integers (ID)
       (lambda (ids)
         (just (cond ((id? ids) (if (string? ids) ids `(,ids)))
                     ((list? ids) ids)
                     (else '()))))
       (lambda (ids)
         (if (id? ids) (just ids)
             (let ((ids (map proc-id ids)))
               (if (every just? ids) (just (map unwrap ids)) nothing)))))
     (->maybe ids)))

  (define (proc-array array)
    ; The json egg serializes scheme lists as JSON arrays
    (cond
      ((vector? array) (just (vector->list array)))
      ((list? array) (just array))
      (else nothing)))

  (define (proc-list-of-strings strs)
    (->maybe
      ((assert*
         'proc-list-of-strings
         "a list of strings or #f"
         (or? false? (cut every string? <>)))
       strs)))

  (define proc-fields proc-list-of-strings)

  ;; TODO: Strict version
  (define proc-object ->maybe)

  (define (proc-bool b) (just (and (just? b) (unwrap b))))
  (define (proc-string str) (->maybe (and str (string? str) str)))
  (define (proc-number n) (->maybe (and n (number? n) n)))

  (define ids->arguments (make-*->arguments 'ids proc-ids))
  (define fields->arguments (make-*->arguments 'fields proc-fields))
  (define (make-number->arguments key) (make-*->arguments key proc-number))
  (define (make-string->arguments key) (make-*->arguments key proc-string))
  (define (make-bool->arguments key) (make-*->arguments key proc-bool))
  (define (make-array->arguments key) (make-*->arguments key proc-array))
  (define (torrent-source->arguments ts)
    (assert* 'torrent-source->arguments "a filename or metainfo" torrent-source?)
    ts)

  ;;;
  ;;; Method Definitions
  ;;;

  ;; @brief Create an RPC procedure
  ;; @param method The name of the RPC method
  ;; @param required A required parameter
  ;; @param required-handler A handler for a required parameter
  ;; @param key A key parameter
  ;; @param default The default value for a key parameter
  ;; @param key-handler A handler for a key parameter
  ;; @returns An RPC procedure
  ;;
  ;; This macro defines and exports an RPC procedure described by the given
  ;;   parameters.
  ;;
  ;; On top of the given key parameters, every RPC procedure has the `tag` key
  ;;   parameter, as per the RPC spec.
  ;;
  ;; All handlers (both required and key) must return either #f or a pair. This
  ;;   pair is the key and associated value of an RPC call argument.
  ;;
  ;; Examples:
  ;;
  ;; A call of no arguments (other than `tag`)
  ;;   (make-rpc-call some-method)
  ;;
  ;; A call with one required argument `fields` and one key argument `ids`
  ;;   (make-rpc-call (some-method (fields fields->arguments)) (ids '() ids->arguments))
  (define-syntax make-rpc-call
    (syntax-rules ()
      ((make-rpc-call (method (required required-handler) ...) (key default key-handler) ...)
       (let ((method-str (symbol->string 'method)))
         (lambda (required ... #!key (tag #f) (key default) ...)
           (let ((required (required-handler required)) ... (key (key-handler key)) ...)
             (let ((arguments (list->vector (filter identity `(,required ... ,key ...)))))
               (rpc-call method-str #:arguments arguments #:tag tag))))))
      ((make-rpc-call method (key default key-handler) ...)
       (make-rpc-call (method) (key default key-handler) ...))))

  ;; Like `make-rpc-call` but defines the created procedure
  (define-syntax define-rpc-call
    (syntax-rules ()
      ((define-rpc-call (method required ...) key ...)
       (define method (make-rpc-call (method required ...) key ...)))
      ((define-rpc-call method key ...)
       (define-rpc-call (method) key ...))))

  ;; Like `define-rpc-call` but exports the defined procedure
  (define-syntax export-rpc-call
    (syntax-rules ()
      ((export-rpc-call (method required ...) key ...)
       (begin
         (export method)
         (define-rpc-call (method required ...) key ...)))
      ((export-rpc-call method key ...)
       (export-rpc-call (method) key ...))))

  ;; Export RPC procedures of sections 3.1 and 4.6. These procedures have a
  ;;   single optional parameter `ids`.
  (define-syntax export-3.1/4.6
    (syntax-rules ()
      ((export-3.1/4.6 method)
       (export-rpc-call method (ids '() ids->arguments)))))

  (export-3.1/4.6 torrent-start)
  (export-3.1/4.6 torrent-start-now)
  (export-3.1/4.6 torrent-stop)
  (export-3.1/4.6 torrent-verify)
  (export-3.1/4.6 torrent-reannounce)

  (export-rpc-call
    torrent-set
    (bandwidth-priority    #f      (make-number->arguments 'bandwidthPriority))
    (download-limit        #f      (make-number->arguments 'downloadLimit))
    (download-limited      nothing (make-bool->arguments   'downloadLimited))
    (files-wanted          #f      (make-array->arguments  'files-wanted))
    (files-unwanted        #f      (make-array->arguments  'files-unwanted))
    (honors-session-limits nothing (make-bool->arguments   'honorsSessionLimits))
    (ids                   '()     ids->arguments)
    (labels                #f      (make-array->arguments  'labels))
    (location              #f      (make-string->arguments 'location))
    (peer-limit            #f      (make-number->arguments 'peer-limit))
    (priority-high         #f      (make-array->arguments  'priority-high))
    (priority-low          #f      (make-array->arguments  'priority-low))
    (priority-normal       #f      (make-array->arguments  'priority-normal))
    (queue-position        #f      (make-number->arguments 'queuePosition))
    (seed-idle-limit       #f      (make-number->arguments 'seedIdleLimit))
    (seed-idle-mode        #f      (make-number->arguments 'seedIdleMode))
    (seed-ratio-limit      #f      (make-number->arguments 'seedRatioLimit))
    (seed-ratio-mode       #f      (make-number->arguments 'seedRatioMode))
    (tracker-add           #f      (make-array->arguments  'trackerAdd))
    (tracker-remove        #f      (make-array->arguments  'trackerRemove))
    (tracker-replace       #f      (make-array->arguments  'trackerReplace))
    (upload-limit          #f      (make-number->arguments 'uploadLimit))
    (upload-limited        nothing (make-bool->arguments   'uploadLimited)))

  (export-rpc-call (torrent-get (fields fields->arguments)) (ids '() ids->arguments))

  ;; `source` must be a filename or metainfo, as described in section 3.4,
  ;;   and is constructed like so:
  ;;     (torrent-add (filename "/path/to/file.torrent") ...)
  ;;     (torrent-add (metainfo "<base64 torrent file>") ...)
  ;; Magnets go in `filename`.
  (export-rpc-call
    (torrent-add
      (source torrent-source->arguments))
    (cookies            #f      (make-string->arguments 'cookies))
    (download-dir       #f      (make-string->arguments 'download-dir))
    (paused             nothing (make-bool->arguments   'paused))
    (peer-limit         #f      (make-number->arguments 'peer-limit))
    (bandwidth-priority #f      (make-number->arguments 'bandwidthPriority))
    (files-wanted       #f      (make-array->arguments  'files-wanted))
    (files-unwanted     #f      (make-array->arguments  'files-unwanted))
    (priority-high      #f      (make-array->arguments  'priority-high))
    (priority-low       #f      (make-array->arguments  'priority-low))
    (priority-normal    #f      (make-array->arguments  'priority-normal)))

  (export-rpc-call
    (torrent-remove (ids ids->arguments))
    (delete-local-data nothing (make-bool->arguments 'delete-local-data)))

  (export-rpc-call
    (torrent-set-location
      (ids ids->arguments)
      (location (make-string->arguments 'location)))
    (move nothing (make-bool->arguments 'move)))

  (export-rpc-call
    (torrent-rename-path
      (ids ids->arguments)
      (path (make-string->arguments 'path))
      (name (make-string->arguments 'name))))

  (export-rpc-call
    session-set
    (alt-speed-down               #f      (make-number->arguments 'alt-speed-down))
    (alt-speed-enabled            nothing (make-bool->arguments   'alt-speed-enabled))
    (alt-speed-time-begin         #f      (make-number->arguments 'alt-speed-time-begin))
    (alt-speed-time-day           #f      (make-number->arguments 'alt-speed-time-day))
    (alt-speed-time-enabled       nothing (make-bool->arguments   'alt-speed-time-enabled))
    (alt-speed-time-end           #f      (make-number->arguments 'alt-speed-time-end))
    (alt-speed-up                 #f      (make-number->arguments 'alt-speed-up))
    (blocklist-enabled            nothing (make-bool->arguments   'blocklist-enabled))
    (blocklist-url                #f      (make-string->arguments 'blocklist-url))
    (cache-size-mb                #f      (make-number->arguments 'cache-size-mb))
    (dht-enabled                  nothing (make-bool->arguments   'dht-enabled))
    (download-dir                 #f      (make-string->arguments 'download-dir))
    (download-queue-enabled       nothing (make-bool->arguments   'download-queue-enabled))
    (download-queue-size          #f      (make-number->arguments 'download-queue-size))
    (encryption                   #f      (make-string->arguments 'encryption))
    (idle-seeding-limit           #f      (make-number->arguments 'idle-seeding-limit))
    (idle-seeding-limit-enabled   nothing (make-bool->arguments   'idle-seeding-limit-enabled))
    (incomplete-dir               #f      (make-string->arguments 'incomplete-dir))
    (incomplete-dir-enabled       nothing (make-bool->arguments   'incomplete-dir-enabled))
    (lpd-enabled                  nothing (make-bool->arguments   'lpd-enabled))
    (peer-limit-global            #f      (make-number->arguments 'peer-limit-global))
    (peer-limit-per-torrent       #f      (make-number->arguments 'peer-limit-per-torrent))
    (peer-port                    #f      (make-number->arguments 'peer-port))
    (peer-port-random-on-start    nothing (make-bool->arguments   'peer-port-random-on-start))
    (pex-enabled                  nothing (make-bool->arguments   'pex-enabled))
    (port-forwarding-enabled      nothing (make-bool->arguments   'port-forwarding-enabled))
    (queue-stalled-enabled        nothing (make-bool->arguments   'queue-stalled-enabled))
    (queue-stalled-minutes        #f      (make-number->arguments 'queue-stalled-minutes))
    (rename-partial-files         nothing (make-bool->arguments   'rename-partial-files))
    (script-torrent-done-enabled  nothing (make-bool->arguments   'script-torrent-done-enabled))
    (script-torrent-done-filename #f      (make-string->arguments 'script-torrent-done-filename))
    (seed-queue-enabled           nothing (make-bool->arguments   'seed-queue-enabled))
    (seed-queue-size              #f      (make-number->arguments 'seed-queue-size))
    (seed-ratio-limit             #f      (make-number->arguments 'seedRatioLimit))
    (seed-ratio-limited           nothing (make-bool->arguments   'seedRatioLimited))
    (speed-limit-down             #f      (make-number->arguments 'speed-limit-down))
    (speed-limit-down-enabled     nothing (make-bool->arguments   'speed-limit-down-enabled))
    (speed-limit-up               #f      (make-number->arguments 'speed-limit-up))
    (speed-limit-up-enabled       nothing (make-bool->arguments   'speed-limit-up-enabled))
    (start-added-torrents         nothing (make-bool->arguments   'start-added-torrents))
    (trash-original-torrent-files nothing (make-bool->arguments   'trash-original-torrent-files))
    (units                        #f      (make-*->arguments      'units proc-object))
    (utp-enabled                  nothing (make-bool->arguments   'utp-enabled)))

  (export-rpc-call session-get (fields #f fields->arguments))

  (export-rpc-call session-stats)
  (export-rpc-call blocklist-update)
  (export-rpc-call port-test)
  (export-rpc-call session-close)

  (export-3.1/4.6 queue-move-top)
  (export-3.1/4.6 queue-move-up)
  (export-3.1/4.6 queue-move-down)
  (export-3.1/4.6 queue-move-bottom)

  (export-rpc-call (free-space (path (make-string->arguments 'path)))))
