#lang typed/racket/no-check

(require openssl/sha1)
(require racket/date)
(require
  net/http-client
  net/url-string
  net/url-structs)

(require (prefix-in info: "info.rkt"))

(module+ test
  (require rackunit))

(define-type Url
  net/url-structs:url)

(define-type Out-Format
  (U 'single-line
     'multi-line))

(define-type Timeline-Order
  (U 'old->new
     'new->old))

(struct msg
        ([ts_epoch   : Integer]
         [ts-orig    : String]
         [nick       : String]
         [uri        : Url]
         [text       : String])
        #:type-name Msg)

(struct feed
        ([nick : String]
         [uri  : Url])
        #:type-name Feed)

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

(: msg-print (-> Out-Format Integer Msg Void))
(define msg-print
  (let* ([colors (vector 36 33)]
         [n      (vector-length colors)])
    (λ (out-format color-i msg)
       (let ([color (vector-ref colors (modulo color-i n))]
             [nick  (msg-nick msg)]
             [uri   (url->string (msg-uri msg))]
             [text  (msg-text msg)])
         (match out-format
           ['single-line
            (printf "~a  \033[1;37m<~a>\033[0m  \033[0;~am~a\033[0m~n"
                    (parameterize ([date-display-format 'iso-8601])
                                  (date->string (seconds->date [msg-ts_epoch msg]) #t))
                    nick color text)]
           ['multi-line
            (printf "~a (~a)~n\033[1;37m<~a ~a>\033[0m~n\033[0;~am~a\033[0m~n~n"
                    (parameterize ([date-display-format 'rfc2822])
                                  (date->string (seconds->date [msg-ts_epoch msg]) #t))
                    (msg-ts-orig msg)
                    nick uri color text)])))))

(: rfc3339->epoch (-> String (Option Nonnegative-Integer)))
(define rfc3339->epoch
  (let ([re (pregexp "^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2})(:([0-9]{2}))?(\\.[0-9]+)?(Z|([+-])([0-9]{1,2}):?([0-9]{2}))?$")])
    (λ (ts)
       (match (regexp-match re ts)
         [(list _wholething yyyy mm dd HH MM _:SS SS _fractional tz-whole tz-sign tz-HH tz-MM)
          (let*
            ([tz-offset
               (match* (tz-whole tz-sign tz-HH tz-MM)
                 [("Z" #f #f #f)
                  0]
                 [(_  (or "-" "+") (? identity h)  (? identity m))
                  (let ([h (string->number h)]
                        [m (string->number m)]
                        ; Reverse to get back to UTC:
                        [op (match tz-sign ["+" -] ["-" +])])
                    (op 0 (+ (* 60 m) (* 60 (* 60 h)))))]
                 [(a b c d)
                  (log-warning "Impossible TZ string: ~v, components: ~v ~v ~v ~v" tz-whole a b c d)
                  0])]
             [ts-orig ts]
             [local-time? #f]
             [ts-epoch (find-seconds (if SS (string->number SS) 0)
                                     (string->number MM)
                                     (string->number HH)
                                     (string->number dd)
                                     (string->number mm)
                                     (string->number yyyy)
                                     local-time?)])
            (+ ts-epoch tz-offset))]
         [_
           (log-error "Invalid timestamp: ~v" ts)
           #f]))))

(: str->msg (-> String Url String (Option Msg)))
(define str->msg
  (let ([re (pregexp "^([^\\s\t]+)[\\s\t]+(.*)$")])
    (λ (nick uri str)
       (with-handlers*
         ([exn:fail?
            (λ (e)
               (log-error
                 "Failed to parse msg: ~v, from: ~v, at: ~v, because: ~v"
                 str nick (url->string uri) e)
               #f)])
         (match (regexp-match re str)
           [(list _wholething ts-orig text)
            (let ([ts-epoch (rfc3339->epoch ts-orig)])
              (if ts-epoch
                  (msg ts-epoch ts-orig nick uri text)
                  (begin
                    (log-error
                      "Msg rejected due to invalid timestamp: ~v, nick:~v, uri:~v"
                      str nick (url->string uri))
                    #f)))]
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
            (check-equal? (msg-ts-orig m) ts (format "Given: ~v" ts))
            )))

  (let* ([ts       "2020-11-18T22:22:09-0500"]
         [tab      "	"]
         [text     "Lorem ipsum"]
         [nick     "foo"]
         [uri      "bar"]
         [actual   (str->msg nick uri (string-append ts tab text))]
         [expected (msg 1605756129 ts nick uri text)])
    (check-equal?
      (msg-ts_epoch actual)
      (msg-ts_epoch expected)
      "str->msg ts_epoch")
    (check-equal?
      (msg-ts-orig actual)
      (msg-ts-orig expected)
      "str->msg ts-orig")
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

(: str->lines (-> String (Listof String)))
(define (str->lines str)
  (string-split str (regexp "[\r\n]+")))

(module+ test
  (check-equal? (str->lines "abc\ndef\n\nghi") '("abc" "def" "ghi")))

(: str->msgs (-> String Url String (Listof Msg)))
(define (str->msgs nick uri str)
  (filter-map (λ (line) (str->msg nick uri line)) (filter-comments (str->lines str))))

(: hash-sha1 (-> String String))
(define (hash-sha1 str)
  (define in (open-input-string str))
  (define digest (sha1 in))
  (close-input-port in)
  digest)

(: url->cache-file-path (-> Url Path-String))
(define (url->cache-file-path uri)
  ; TODO Replace hashing with encoding
  (expand-user-path (string-append "~/.tt/cache/" (hash-sha1 (url->string uri)))))

; TODO Return Option
(: uri-read-cached (-> Url String))
(define (uri-read-cached uri)
  (define path (url->cache-file-path uri))
  (if (file-exists? path)
      (file->string path)
      (begin
        (log-warning "Cache file not found for URI: ~a" (url->string uri))
        "")))

(: str->feed (String (Option Feed)))
(define (str->feed str)
  (log-debug "Parsing feed string: ~v" str)
  (match (string-split str)
    [(list nick u)
     (with-handlers*
       ([exn:fail?
          (λ (e)
             (log-error "Invalid URI: ~v, exn: ~v" u e)
             #f)])
       (feed nick (string->url u)))]
    [_
      (log-error "Invalid feed string: ~v" str)
      #f]))

(: filter-comments (-> (Listof String) (Listof String)))
(define (filter-comments lines)
  (filter-not (λ (line) (string-prefix? line "#")) lines))

(: str->feeds (-> String (Listof Feed)))
(define (str->feeds str)
  (filter-map str->feed (filter-comments (str->lines str))))

(: file->feeds (-> Path-String (Listof Feed)))
(define (file->feeds filename)
  (str->feeds (file->string filename)))

(: user-agent String)
(define user-agent
  (let*
    ([prog-name      "tt"]
     [prog-version   (info:#%info-lookup 'version)]
     [prog-uri       "https://github.com/xandkar/tt"]
     [user-feed-file (expand-user-path "~/twtxt-me.txt")]
     [user
       (if (file-exists? user-feed-file)
           (let ([user (first (file->feeds user-feed-file))])
             (format "+~a; @~a" (url->string (feed-uri user)) (feed-nick user)))
           (format "+~a" prog-uri))])
    (format "~a/~a (~a)" prog-name prog-version user)))

(: uri-download (-> Url Void))
(define (uri-download u)
  (define cache-file-path (url->cache-file-path u))
  (log-debug "uri-download ~v into ~v" u cache-file-path)
  (match* ((url-scheme u) (url-host u) (url-port u))
    [(s h p)
     #:when (and s h)
     (define ssl? (string=? s "https"))
     (define-values (status-line headers body-input)
       ; TODO Timeout. Currently hangs on slow connections.
       (http-sendrecv
         h
         (url->string (struct-copy url u [scheme #f] [host #f]))
         #:ssl? ssl?
         #:port (cond [p p] [ssl? 443] [else 80])
         #:headers (list (format "User-Agent: ~a" user-agent))
         ))
     (log-debug "headers: ~v" headers)
     (log-debug "status-line: ~v" status-line)
     (define status
       (string->number (second (string-split (bytes->string/utf-8 status-line)))))
     (log-debug "status: ~v" status)
     ; TODO Handle redirects
     (if (= 200 status)
         (call-with-output-file cache-file-path
                                (λ (cache-output)
                                   (copy-port body-input cache-output))
                                #:exists 'replace)
         (raise status))]
    [(_ _ _)
     (log-error "Invalid URI: ~v" u)]))

(: timeline-print (-> Out-Format (Listof Msg) Void))
(define (timeline-print out-format timeline)
  (void (foldl (match-lambda**
                 [((and m (msg _ _ nick _ _)) (cons prev-nick i))
                  (let ([i (if (string=? prev-nick nick) i (+ 1 i))])
                    (msg-print out-format i m)
                    (cons nick i))])
               (cons "" 0)
               timeline)))

(: feed->msgs (-> Feed (Listof Msg)))
(define (feed->msgs f)
  (match-define (feed nick uri) f)
  (log-info "Reading feed nick:~a uri:~v" nick (url->string uri))
  (str->msgs nick uri (uri-read-cached uri)))

(: feed-download (-> Feed Void))
(define (feed-download f)
  (match-define (feed nick uri) f)
  (log-info "Downloading feed nick:~a uri:~a" nick (url->string uri))
  (with-handlers
    ([exn:fail?
       (λ (e)
          (log-error "Network error nick:~a uri:~v  exn:~v" nick uri e)
          #f)]
     [integer?
       (λ (status)
          (log-error "HTTP error nick:~a uri:~a  status:~a" nick uri status)
          #f)])
    (uri-download uri)))

(: timeline-download (-> Integer (Listof Feed) Void))
(define (timeline-download num_workers feeds)
  ; TODO No need for map - can just iter
  (void (concurrent-filter-map num_workers feed-download feeds)))

; TODO timeline contract : time-sorted list of messages
(: timeline-read (-> Timeline-Order (Listof Feed) (Listof Msg)))
(define (timeline-read order feeds)
  (define cmp (match order
                ['old->new <]
                ['new->old >]))
  (sort (append* (filter-map feed->msgs feeds))
        (λ (a b) (cmp (msg-ts_epoch a) (msg-ts_epoch b)))))

(: start-logger (-> Log-Level Void))
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
             (timeline-download num_workers (file->feeds filename))))]
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
