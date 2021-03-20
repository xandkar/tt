#lang racket

(require openssl/sha1)
(require racket/date)

(require http-client)
(require rfc3339-old)

(module+ test
  (require rackunit))

(struct msg  (ts_epoch ts_rfc3339 nick uri text))
(struct feed (nick uri))

(define (concurrent-filter-map num_workers f xs)
  ; TODO preserve order of elements OR communicate that reorder is expected
  ; TODO switch from mailboxes to channels
  (define (make-worker id f)
    (define parent (current-thread))
    (λ ()
       (define self (current-thread))
       (define (work)
         (thread-send parent (cons 'next self))
         (match (thread-receive)
           ['done          (thread-send parent (cons 'exit id))]
           [(cons 'unit x) (begin
                             (define y (f x))
                             (when y (thread-send parent (cons 'result y)))
                             (work))]))
       (work)))
  (define (dispatch ws xs ys)
    (if (empty? ws)
        ys
        (match (thread-receive)
          [(cons 'exit w)   (dispatch (remove w ws =) xs ys)]
          [(cons 'result y) (dispatch ws xs (cons y ys))]
          [(cons 'next thd) (match xs
                              ['()         (begin
                                             (thread-send thd 'done)
                                             (dispatch ws xs ys))]
                              [(cons x xs) (begin
                                             (thread-send thd (cons 'unit x))
                                             (dispatch ws xs ys))])])))
  (define workers (range num_workers))
  (define threads (map (λ (id) (thread (make-worker id f))) workers))
  (define results (dispatch workers xs '()))
  (for-each thread-wait threads)
  results)

(module+ test
  (let* ([f        (λ (x) (if (even? x) x #f))]
         [xs       (range 11)]
         [actual   (sort (concurrent-filter-map 10 f xs) <)]
         [expected (sort (           filter-map    f xs) <)])
    (check-equal? actual expected "concurrent-filter-map")))

(define msg-print
  (let* ([colors (vector 36 33)]
         [n      (vector-length colors)])
    (λ (out-format color-i msg)
       (let ([color (vector-ref colors (modulo color-i n))]
             [nick  (msg-nick msg)]
             [uri   (msg-uri  msg)]
             [text  (msg-text msg)])
         (match out-format
           ['single-line
            (printf "~a  \033[1;37m<~a>\033[0m  \033[0;~am~a\033[0m~n"
                    (parameterize ([date-display-format 'iso-8601])
                                  (date->string (seconds->date [msg-ts_epoch msg]) #t))
                    nick color text)]
           ['multi-line
            (printf "~a~n\033[1;37m<~a ~a>\033[0m~n\033[0;~am~a\033[0m~n~n"
                    (parameterize ([date-display-format 'rfc2822])
                                  (date->string (seconds->date [msg-ts_epoch msg]) #t))
                    nick uri color text)])))))

; TODO Implement rfc3339->epoch and remove dependency on rfc3339-old

(define str->msg
  (let ([re (pregexp "^([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}(:[0-9]{2})?)(\\.[0-9]+)?([^\\s\t]*)[\\s\t]+(.*)$")])
    (λ (nick uri str)
       (with-handlers*
         ([exn:fail?
            (λ (e)
               (log-error "Failed to parse msg: ~v, from: ~v, at: ~v, because: ~v" str nick uri e)
               #f)])
         (match (regexp-match re str)
           [(list _wholething ts s _fractional tz text)
            (let*
              ([ts_rfc3339 (string-append ts (if s "" ":00") (if tz tz ""))]
               [t          (string->rfc3339-record ts_rfc3339)]
               [s          (rfc3339-record:second t)]
               ; TODO handle tz offset
               [ts_epoch (find-seconds [if s s 0]
                                       [rfc3339-record:minute t]
                                       [rfc3339-record:hour   t]
                                       [rfc3339-record:mday   t]
                                       [rfc3339-record:month  t]
                                       [rfc3339-record:year   t])])
              (msg ts_epoch ts_rfc3339 nick uri text))]
           [_
             (log-debug "Non-msg line from nick:~a, line:~a" nick str)
             #f])))))

(module+ test
  (let* ([tzs (for*/list ([d '("-" "+")]
                          [h '("5" "05")]
                          [m '("00" ":00" "57" ":57")])
                         (string-append d h m))]
         [tzs (list* "" "Z" tzs)])
    (for* ([n   '("fake-nick")]
           [u   '("fake-uri")]
           [s   '("" ":10")]
           [f   '("" ".1337")]
           [z   tzs]
           [sep (list "\t" " ")]
           [txt '("foo bar baz" "'jaz poop bear giraffe / tea" "@*\"``")])
          (let* ([ts (string-append "2020-11-18T22:22"
                                    (if (non-empty-string? s) s ":00")
                                    z)]
                 [m  (str->msg n u (string-append ts sep txt))])
            (check-not-false m)
            (check-equal? (msg-nick m) n)
            (check-equal? (msg-uri m) u)
            (check-equal? (msg-text m) txt)
            (check-equal? (msg-ts_rfc3339 m) ts (format "Given: ~v" ts))
            )))

  (let* ([ts       "2020-11-18T22:22:09-0500"]
         [tab      "	"]
         [text     "Lorem ipsum"]
         [nick     "foo"]
         [uri      "bar"]
         [actual   (str->msg nick uri (string-append ts tab text))]
         [expected (msg 1605756129 ts nick uri text)])
    ; FIXME re-enable after handling tz offset
    ;(check-equal?
    ;  (msg-ts_epoch actual)
    ;  (msg-ts_epoch expected)
    ;  "str->msg ts_epoch")
    (check-equal?
      (msg-ts_rfc3339 actual)
      (msg-ts_rfc3339 expected)
      "str->msg ts_rfc3339")
    (check-equal?
      (msg-nick actual)
      (msg-nick expected)
      "str->msg nick")
    (check-equal?
      (msg-uri actual)
      (msg-uri expected)
      "str->msg uri")
    (check-equal?
      (msg-text actual)
      (msg-text expected)
      "str->msg text")))

(define (str->lines str)
  (string-split str (regexp "[\r\n]+")))

(module+ test
  (check-equal? (str->lines "abc\ndef\n\nghi") '("abc" "def" "ghi")))

(define (str->msgs nick uri str)
  (filter-map (λ (line) (str->msg nick uri line)) (str->lines str)))

(define (hash-sha1 str)
  (define in (open-input-string str))
  (define digest (sha1 in))
  (close-input-port in)
  digest)

(define (uri-read-cached uri)
  (define cache-file-path
    (expand-user-path (string-append "~/.tt/cache/" (hash-sha1 uri))))
  (if (file-exists? cache-file-path)
      (file->string cache-file-path)
      (begin
        (log-warning "Cache file not found for URI: ~a" uri)
        "")))

; uri-download : String -> Void
(define (uri-download uri)
  (define cache-file-path
    (expand-user-path (string-append "~/.tt/cache/" (hash-sha1 uri))))
  (log-info "uri-download ~a" uri)
  ; TODO Timeout. Currently hangs on slow connections.
  (let* ([resp   (http-get uri)]
         [status (http-response-code resp)]
         [body   (http-response-body resp)])
    (log-debug "finished GET ~a status:~a  body length:~a"
               uri status (string-length body))
    ; TODO Handle redirects
    (if (= status 200)
        (display-to-file body cache-file-path #:exists 'replace)
        ; TODO A more-informative exception
        (raise status))))

(define (timeline-print out-format timeline)
  (void (foldl (match-lambda**
                 [((and m (msg _ _ nick _ _)) (cons prev-nick i))
                  (let ([i (if (string=? prev-nick nick) i (+ 1 i))])
                    (msg-print out-format i m)
                    (cons nick i))])
               (cons "" 0)
               timeline)))

; feed->msgs : Feed -> (Listof Msg)
(define (feed->msgs feed)
  (log-info "Reading feed nick:~a uri:~a"
            (feed-nick feed)
            (feed-uri feed))
  (define uri (feed-uri feed))
  (str->msgs (feed-nick feed) uri (uri-read-cached uri)))

; feed-download : Feed -> Void
(define (feed-download feed)
  (log-info "Downloading feed nick:~a uri:~a"
            (feed-nick feed)
            (feed-uri feed))
  (with-handlers
    ([exn:fail:network?
       (λ (e)
          (log-error "Network error nick:~a uri:~a  exn:~a"
                     (feed-nick feed)
                     (feed-uri feed)
                     e)
          #f)]
     [integer?
       (λ (status)
          (log-error "HTTP error nick:~a uri:~a  status:~a"
                     (feed-nick feed)
                     (feed-uri feed)
                     status)
          #f)])
    (uri-download (feed-uri feed))))

; timeline-download : Integer -> (Listof Feed) -> Void
(define (timeline-download num_workers feeds)
  ; TODO No need for map - can just iter
  (void (concurrent-filter-map num_workers feed-download feeds)))

; TODO timeline contract : time-sorted list of messages
; timeline-read : (U 'old->new 'new->old) -> (Listof Feeds) -> (Listof Msg)
(define (timeline-read order feeds)
  (define cmp (match order
                ['old->new <]
                ['new->old >]))
  (sort (append* (filter-map feed->msgs feeds))
        (λ (a b) (cmp (msg-ts_epoch a) (msg-ts_epoch b)))))

(define (str->feed str)
  ; TODO validation
  (define toks (string-split str))
  (apply feed toks))

(define (filter-comments lines)
  (filter-not (λ (line) (string-prefix? line "#")) lines))

(define (str->feeds str)
  (map str->feed (filter-comments (str->lines str))))

(define (file->feeds filename)
  (str->feeds (file->string filename)))

(define (user-agent prog-name prog-version)
  (let*
    ([prog-uri       "https://github.com/xandkar/tt"]
     [user-feed-file (expand-user-path "~/twtxt-me.txt")]
     [user
       (if (file-exists? user-feed-file)
           (let ([user (first (file->feeds user-feed-file))])
             (format "+~a; @~a" (feed-uri user) (feed-nick user)))
           (format "+~a" prog-uri))])
    (format "~a/~a (~a)" prog-name prog-version user)))

(define (start-logger level)
  (let* ([logger       (make-logger #f #f level #f)]
         [log-receiver (make-log-receiver logger level)])
    (void (thread (λ ()
                     (parameterize
                       ([date-display-format 'iso-8601])
                       (let loop ()
                         (define data  (sync log-receiver))
                         (define level (vector-ref data 0))
                         (define msg   (vector-ref data 1))
                         (define ts    (date->string (current-date) #t))
                         (eprintf "~a [~a] ~a~n" ts level msg)
                         (loop))))))
    (current-logger logger)))

(module+ main
  (require (prefix-in info: setup/getinfo))

  (let ([log-level 'info])
    (command-line
      #:program
      "tt"
      #:once-each
      [("-d" "--debug")
       "Enable debug log level."
       (set! log-level 'debug)]
      #:help-labels
      ""
      "and <command> is one of"
      "r, read i   : Read the timeline."
      "d, download : Download the timeline."
      "u, upload   : Upload your twtxt file (alias to execute ~/.tt/upload)."
      ""
      #:args (command . args)
      (start-logger log-level)
      (current-command-line-arguments (list->vector args))
      (match command
        [(or "d" "download")
         (let ([num_workers 15]) ; 15 was fastest out of the tried: 1, 5, 10, 20.
           (command-line
             #:program
             "tt download"
             #:once-each
             [("-j" "--jobs")
              njobs "Number of concurrent jobs."
              (set! num_workers (string->number njobs))]

             #:args (filename)

             (current-http-client/response-auto #f)
             (let* ([prog-name    "tt"]
                    [prog-version ((info:get-info (list prog-name)) 'version)]
                    [user-agent   (user-agent prog-name prog-version)])
               (current-http-client/user-agent user-agent))
             (timeline-download num_workers (file->feeds filename))
             ))]
        [(or "u" "upload")
         (command-line
             #:program
             "tt upload"
             #:args ()
             (if (system (path->string (expand-user-path "~/.tt/upload")))
               (exit 0)
               (exit 1)))]
        [(or "r" "read")
         (let ([out-format 'multi-line]
               [order      'old->new])
           (command-line
             #:program
             "tt read"
             #:once-each
             [("-r" "--rev")
              "Reverse displayed timeline order."
              (set! order 'new->old)]
             #:once-any
             [("-s" "--short")
              "Short output format"
              (set! out-format 'single-line)]
             [("-l" "--long")
              "Long output format"
              (set! out-format 'multi-line)]
             #:args (filename)
             (timeline-print out-format (timeline-read order (file->feeds filename)))))]
        ))))
