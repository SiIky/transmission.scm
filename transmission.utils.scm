(module transmission.utils
  (
   reply-ref-path

   status/check
   status/check-wait
   status/download
   status/download-wait
   status/seed
   status/seed-wait
   status/stopped

   priority/high
   priority/low
   priority/normal

   alist-keep-keys
   unique-tag!

   alist-let
   alist-let/and
   alist-let/nor

   parse-ids
   )

  (import
    (except scheme
            member)
    (only chicken.base
          add1
          constantly
          cute
          fixnum?
          o))

  (import
    (only srfi-1
          concatenate
          filter
          iota
          member)
    (only scheme.base
          vector-map))

  (import
    (only transmission
          reply-ref
          result-ref))

  ; NOTE(alist-let):
  ;
  ; The common part, of both `alist-let/and` and `alist-let/nor`, of expanding
  ; the "let-list", is now in `alist-let`. With the "normal entry point" for
  ; `alist-let`, `alist-let/and` and `alist-let/nor` would result in a double
  ; unnecessary `let`; e.g.
  ;
  ; (alist-let/and alist (k1 k2) (print k1 k2))
  ;
  ; ==>
  ;
  ; (let ((%alist alist))
  ;   (and %alist
  ;        (alist-let %alist (k1 k2) (print k1 k2))))
  ;
  ; ==>
  ;
  ; (let ((%alist alist))
  ;   (and %alist
  ;        (let ((%alist %alist))
  ;          (let ((k1 (alist-ref 'k1 %alist))
  ;                (k2 (alist-ref 'k2 %alist)))
  ;            (print k1 k2)))))
  ;
  ; To avoid this, both of them use one of the internal "states" ("rec") of
  ; `alist-let`.
  ;
  ; That same expression now expands to
  ;
  ; (let ((%alist alist))
  ;   (and %alist
  ;        (let ((k1 (alist-ref 'k1 %alist))
  ;              (k2 (alist-ref 'k2 %alist)))
  ;          (print k1 k2))))
  ;
  ; which is the same as what the original `alist-let/and` expanded to.
  ;
  ; TODO:
  ; - [ ] Allow different types of keys (other than symbols);
  ; - [ ] Allow different equality predicates;

  (define-syntax alist-let
    (syntax-rules ()
      ((alist-let "rec" let-list alist () body ...)
       (let let-list
         body ...))

      ((alist-let "rec" let-list
                  alist ((variable-name key) . let-tail)
                  body ...)
       (alist-let "rec" ((variable-name (reply-ref 'key alist)) . let-list)
                  alist let-tail
                  body ...))

      ((alist-let "rec" let-list
                  alist (key . let-tail)
                  body ...)
       (alist-let "rec" ((key (reply-ref 'key alist)) . let-list)
                  alist let-tail
                  body ...))

      ((alist-let alist (key ...)
                  body ...)
       (let ((%alist alist))
         (alist-let "rec" ()
                    %alist (key ...)
                    body ...)))))



  (define-syntax alist-let/and
    (syntax-rules ()
      ((alist-let/and alist (key ...) body ...)
       (let ((%alist alist))
         (and %alist
              ; NOTE: See NOTE(alist-let) above.
              (alist-let "rec" () %alist (key ...) body ...))))))

  (define-syntax alist-let/nor
    (syntax-rules ()
      ((alist-let/nor alist (key ...) body ...)
       (let ((%alist alist))
         (or (not %alist)
             ; NOTE: See NOTE(alist-let) above.
             (alist-let "rec" () %alist (key ...) body ...))))))

  (define unique-tag!
    (let ((n 0))
      (lambda (#!optional (new-n #f))
        (if (fixnum? new-n)
            (begin
              (set! n new-n)
              (unique-tag!))
            (let ((ret n))
              (set! n (add1 n))
              ret)))))

  (define (alist-keep-keys alist . keys)
    (filter (lambda (kv) (member (car kv) keys eq?)) alist))

  (define (reply-ref-path reply path #!optional (==? equal?))
    (cond
      ((null? path)
       reply)

      ((list? reply) ; table?
       (let ((phead (car path))
             (ptail (cdr path)))
         (let ((branch (reply-ref phead reply ==?)))
           (reply-ref-path branch ptail ==?))))

      ((vector? reply) ; array?
       (vector-map (cute reply-ref-path <> path ==?) reply))

      (else #f)))

  ;; tr_torrent_activity from libtransmission/transmission.h
  (define status/stopped       0)
  (define status/check-wait    1)
  (define status/check         2)
  (define status/download-wait 3)
  (define status/download      4)
  (define status/seed-wait     5)
  (define status/seed          6)

  ;; tr_priority_t from libtransmission/transmission.h
  (define priority/low   -1) ; TR_PRI_LOW
  (define priority/normal 0) ; TR_PRI_NORMAL
  (define priority/high   1) ; TR_PRI_HIGH

  ;;; IDs := IDs-group | IDs-list
  ;;;
  ;;; IDs-group := "" | "active" | "all"
  ;;;
  ;;; IDs-list := ID ("," ID)*
  ;;;
  ;;; ID := number-range | <number> | <hash>
  ;;;
  ;;; number-range := <number> "-" <number>
  ;;;
  ;;; TODO: How to distinguish failure from "all" (#f)? Maybe a final `bind` to
  ;;;       wrap a successful result. Especially important because `#f` means
  ;;;       "all" torrents.
  ;;; TODO: How to put this behind a feature, so that comparse and srfi-14 are
  ;;;       optional?
  (define (parse-ids str)
    (import
      comparse
      (only srfi-14
            char-set:digit
            char-set:hex-digit))

    (define (as-number parser)
      (bind (as-string parser) (o result string->number)))

    (define (as-list parser)
      (bind parser (o result list)))

    (define <hash> (as-string (repeated (in char-set:hex-digit) 40)))
    (define <number> (as-number (one-or-more (in char-set:digit))))
    (define number-range
      (sequence* ((n1 <number>) (n2 (preceded-by (is #\-) <number>)))
                 (and (< n1 n2)
                      (result (iota (add1 (- n2 n1)) n1)))))

    (define ID (any-of number-range (as-list <hash>) (as-list <number>)))
    (define IDs-list (sequence* ((h ID) (t (zero-or-more (preceded-by (is #\,) ID))))
                                ; TODO: Is there a way to improve this?
                                (result (append h (concatenate t)))))

    (define <active> (bind (char-seq "active") (constantly (result "recently-active"))))
    (define <all> (bind (char-seq "all") (constantly (result #f))))
    (define <none> (bind (char-seq "") (constantly (result '()))))
    (define IDs-group (any-of <active> <all> <none>))

    ; TODO: Why does order matter here?
    (define IDs (any-of IDs-list IDs-group))

    (parse (followed-by IDs end-of-input) (->parser-input str))))
