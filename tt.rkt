#lang typed/racket/no-check

(require openssl/sha1)
(require racket/date)
(require
  net/head
  net/uri-codec
  net/url)

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

(struct Msg
        ([ts-epoch   : Integer]
         [ts-orig    : String]
         [nick       : (Option String)]
         [uri        : Url]
         [text       : String]
         [mentions   : (Listof Peer)]))

(struct Peer
        ([nick : (Option String)]
         [uri  : Url])
        #:transparent)

(: tt-home-dir Path-String)
(define tt-home-dir (build-path (expand-user-path "~") ".tt"))

(: concurrent-filter-map (∀ (α β) (-> Natural (-> α β) (Listof α) (Listof β))))
(define (concurrent-filter-map num-workers f xs)
  ; TODO preserve order of elements OR communicate that reorder is expected
  ; TODO switch from mailboxes to channels
  (define (make-worker id f)
    (define parent (current-thread))
    (λ ()
       (define self : Thread (current-thread))
       (: work (∀ (α) (-> α)))
       (define (work)
         (thread-send parent (cons 'next self))
         (match (thread-receive)
           ['done          (thread-send parent (cons 'exit id))]
           [(cons 'unit x) (begin
                             (define y (f x))
                             (when y (thread-send parent (cons 'result y)))
                             (work))]))
       (work)))
  (: dispatch (∀ (α β) (-> (Listof Nonnegative-Integer) (Listof α) (Listof β))))
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
  (define workers (range num-workers))
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
             [nick  (Msg-nick msg)]
             [uri   (url->string (Msg-uri msg))]
             [text  (Msg-text msg)]
             [mentions (Msg-mentions msg)])
         (match out-format
           ['single-line
            (let ([nick (if nick nick uri)])
              (printf "~a  \033[1;37m<~a>\033[0m  \033[0;~am~a\033[0m~n"
                      (parameterize
                        ([date-display-format 'iso-8601])
                        (date->string (seconds->date (Msg-ts-epoch msg)) #t))
                      nick color text))]
           ['multi-line
            (let ([nick (if nick (string-append nick " ") "")])
              (printf "~a (~a)~n\033[1;37m<~a~a>\033[0m~n\033[0;~am~a\033[0m~n~n"
                      (parameterize
                        ([date-display-format 'rfc2822])
                        (date->string (seconds->date (Msg-ts-epoch msg)) #t))
                      (Msg-ts-orig msg)
                      nick uri color text))])))))

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

(: str->msg (-> (Option String) Url String (Option Msg)))
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
                  (let ([mentions
                          (filter-map
                            (λ (m) (match (regexp-match #px"@<([^>]+)>" m)
                                     [(list _wholething nick-uri)
                                      (str->peer nick-uri)]))
                            (regexp-match* #px"@<[^\\s]+([\\s]+)?[^>]+>" text))])
                    (Msg ts-epoch ts-orig nick uri text mentions))
                  (begin
                    (log-error
                      "Msg rejected due to invalid timestamp: ~v, nick:~v, uri:~v"
                      str nick (url->string uri))
                    #f)))]
           [_
             (log-debug "Non-msg line from nick:~v, line:~a" nick str)
             #f])))))

(module+ test
  ; TODO Test for when missing-nick case
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
            (check-equal? (Msg-nick m) n)
            (check-equal? (Msg-uri m) u)
            (check-equal? (Msg-text m) txt)
            (check-equal? (Msg-ts-orig m) ts (format "Given: ~v" ts))
            )))

  (let* ([ts       "2020-11-18T22:22:09-0500"]
         [tab      "	"]
         [text     "Lorem ipsum"]
         [nick     "foo"]
         [uri      "bar"]
         [actual   (str->msg nick uri (string-append ts tab text))]
         [expected (Msg 1605756129 ts nick uri text '())])
    (check-equal?
      (Msg-ts-epoch actual)
      (Msg-ts-epoch expected)
      "str->msg ts-epoch")
    (check-equal?
      (Msg-ts-orig actual)
      (Msg-ts-orig expected)
      "str->msg ts-orig")
    (check-equal?
      (Msg-nick actual)
      (Msg-nick expected)
      "str->msg nick")
    (check-equal?
      (Msg-uri actual)
      (Msg-uri expected)
      "str->msg uri")
    (check-equal?
      (Msg-text actual)
      (Msg-text expected)
      "str->msg text")))

(: str->lines (-> String (Listof String)))
(define (str->lines str)
  (string-split str (regexp "[\r\n]+")))

(module+ test
  (check-equal? (str->lines "abc\ndef\n\nghi") '("abc" "def" "ghi")))

(: str->msgs (-> (Option String) Url String (Listof Msg)))
(define (str->msgs nick uri str)
  (filter-map (λ (line) (str->msg nick uri line)) (filter-comments (str->lines str))))

(: cache-dir Path-String)
(define cache-dir (build-path tt-home-dir "cache"))

(define cache-object-dir (build-path cache-dir "objects"))

(: url->cache-file-path-v1 (-> Url Path-String))
(define (url->cache-file-path-v1 uri)
  (define (hash-sha1 str) : (-> String String)
    (define in (open-input-string str))
    (define digest (sha1 in))
    (close-input-port in)
    digest)
  (build-path cache-object-dir (hash-sha1 (url->string uri))))

(: url->cache-file-path-v2 (-> Url Path-String))
(define (url->cache-file-path-v2 uri)
  (build-path cache-object-dir (uri-encode (url->string uri))))

(define url->cache-object-path url->cache-file-path-v2)

(define (url->cache-etag-path uri)
  (build-path cache-dir "etags" (uri-encode (url->string uri))))

(define (url->cache-lmod-path uri)
  (build-path cache-dir "lmods" (uri-encode (url->string uri))))

; TODO Return Option
(: uri-read-cached (-> Url String))
(define (uri-read-cached uri)
  (define path-v1 (url->cache-file-path-v1 uri))
  (define path-v2 (url->cache-file-path-v2 uri))
  (when (file-exists? path-v1)
    (rename-file-or-directory path-v1 path-v2 #t))
  (if (file-exists? path-v2)
      (file->string path-v2)
      (begin
        (log-warning "Cache file not found for URI: ~a" (url->string uri))
        "")))

(: uri? (-> String Boolean))
(define (uri? str)
  (regexp-match? #rx"^[a-z]+://.*" (string-downcase str)))

(: str->peer (String (Option Peer)))
(define (str->peer str)
  (log-debug "Parsing peer string: ~v" str)
  (with-handlers*
    ([exn:fail?
       (λ (e)
          (log-error "Invalid URI in string: ~v, exn: ~v" str e)
          #f)])
    (match (string-split str)
      [(list u)   #:when (uri? u) (Peer #f  (string->url u))]
      [(list n u) #:when (uri? u) (Peer  n  (string->url u))]
      [_
        (log-error "Invalid peer string: ~v" str)
        #f])))


(: filter-comments (-> (Listof String) (Listof String)))
(define (filter-comments lines)
  (filter-not (λ (line) (string-prefix? line "#")) lines))

(: str->peers (-> String (Listof Peer)))
(define (str->peers str)
  (filter-map str->peer (filter-comments (str->lines str))))

(: peers->file (-> (Listof Peers) Path-String Void))
(define (peers->file peers path)
  (display-lines-to-file
    (map (match-lambda
           [(Peer n u)
            (format "~a~a" (if n (format "~a " n) "") (url->string u))])
         peers)
    path
    #:exists 'replace))

(: file->peers (-> Path-String (Listof Peer)))
(define (file->peers file-path)
  (if (file-exists? file-path)
      (str->peers (file->string file-path))
      (begin
        (log-warning "File does not exist: ~v" (path->string file-path))
        '())))

(define re-rfc2822
  #px"^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), ([0-9]{2}) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) ([0-9]{4}) ([0-2][0-9]):([0-6][0-9]):([0-6][0-9]) GMT")

(: b->n (-> Bytes (Option Number)))
(define (b->n b)
  (string->number (bytes->string/utf-8 b)))

(: mon->num (-> Bytes Natural))
(define/match (mon->num mon)
  [(#"Jan")  1]
  [(#"Feb")  2]
  [(#"Mar")  3]
  [(#"Apr")  4]
  [(#"May")  5]
  [(#"Jun")  6]
  [(#"Jul")  7]
  [(#"Aug")  8]
  [(#"Sep")  9]
  [(#"Oct") 10]
  [(#"Nov") 11]
  [(#"Dec") 12])

(: rfc2822->epoch (-> Bytes (Option Nonnegative-Integer)))
(define (rfc2822->epoch timestamp)
  (match (regexp-match re-rfc2822 timestamp)
    [(list _ _ dd mo yyyy HH MM SS)
     #:when (and dd mo yyyy HH MM SS)
     (find-seconds (b->n SS)
                   (b->n MM)
                   (b->n HH)
                   (b->n dd)
                   (mon->num mo)
                   (b->n yyyy)
                   #f)]
    [_
      #f]))

(: user-agent String)
(define user-agent
  (let*
    ([prog-name      "tt"]
     [prog-version   (info:#%info-lookup 'version)]
     [prog-uri       "https://github.com/xandkar/tt"]
     [user-peer-file (build-path tt-home-dir "me")]
     [user
       (if (file-exists? user-peer-file)
           (match (first (file->peers user-peer-file))
             [(Peer #f u) (format "+~a"      (url->string u)  )]
             [(Peer  n u) (format "+~a; @~a" (url->string u) n)])
           (format "+~a" prog-uri))])
    (format "~a/~a (~a)" prog-name prog-version user)))

(: header-get (-> (Listof Bytes) Bytes (Option Bytes)))
(define (header-get headers name)
  (match (filter-map (curry extract-field name) headers)
    [(list val) val]
    [_           #f]))

(: uri-download (-> Url Void))
(define (uri-download u)
  (define cached-object-path (url->cache-object-path u))
  (define cached-etag-path (url->cache-etag-path u))
  (define cached-lmod-path (url->cache-lmod-path u))
  (log-debug "uri-download ~v into ~v" u cached-object-path)
  (define-values (status-line headers body-input)
    ; TODO Timeout. Currently hangs on slow connections.
    (http-sendrecv/url u #:headers (list (format "User-Agent: ~a" user-agent))))
  (log-debug "headers: ~v" headers)
  (log-debug "status-line: ~v" status-line)
  (define status
    (string->number (second (string-split (bytes->string/utf-8 status-line)))))
  (log-debug "status: ~v" status)
  ; TODO Handle redirects
  (match status
    [200
      (let* ([etag      (header-get headers #"ETag")]
             [lmod      (header-get headers #"Last-Modified")]
             [lmod-curr (if lmod (rfc2822->epoch lmod) #f)]
             [lmod-prev (if (file-exists? cached-lmod-path)
                            (rfc2822->epoch (file->bytes cached-lmod-path))
                            #f)])
        (log-debug "lmod-curr:~v lmod-prev:~v" lmod-curr lmod-prev)
        (unless (or (and etag
                         (file-exists? cached-etag-path)
                         (bytes=? etag (file->bytes cached-etag-path))
                         (begin
                           (log-info "ETags match, skipping the rest of ~v" (url->string u))
                           #t))
                    (and lmod-curr
                         lmod-prev
                         (<= lmod-curr lmod-prev)
                         (begin
                           (log-info "Last-Modified <= current skipping the rest of ~v" (url->string u))
                           #t)))
          (begin
            (log-info
              "Downloading the rest of ~v. ETag: ~a, Last-Modified: ~v"
              (url->string u) etag lmod)
            (make-parent-directory* cached-object-path)
            (make-parent-directory* cached-etag-path)
            (make-parent-directory* cached-lmod-path)
            (call-with-output-file cached-object-path
                                   (curry copy-port body-input)
                                   #:exists 'replace)
            (when etag
              (display-to-file etag cached-etag-path #:exists 'replace))
            (when lmod
              (display-to-file lmod cached-lmod-path #:exists 'replace))))
        (close-input-port body-input))]
    [_
      (raise status)]))

(: timeline-print (-> Out-Format (Listof Msg) Void))
(define (timeline-print out-format timeline)
  (void (foldl (match-lambda**
                 [((and m (Msg _ _ nick _ _ _)) (cons prev-nick i))
                  (let ([i (if (equal? prev-nick nick) i (+ 1 i))])
                    (msg-print out-format i m)
                    (cons nick i))])
               (cons "" 0)
               timeline)))

(: peer->msgs (-> Peer (Listof Msg)))
(define (peer->msgs f)
  (match-define (Peer nick uri) f)
  (log-info "Reading peer nick:~v uri:~v" nick (url->string uri))
  (str->msgs nick uri (uri-read-cached uri)))

(: peer-download (-> Peer Void))
(define (peer-download f)
  (match-define (Peer nick uri) f)
  (define u (url->string uri))
  (log-info "Downloading peer uri:~a" u)
  (with-handlers
    ([exn:fail?
       (λ (e)
          (log-error "Network error nick:~v uri:~v  exn:~v" nick u e)
          #f)]
     [integer?
       (λ (status)
          (log-error "HTTP error nick:~v uri:~a  status:~a" nick u status)
          #f)])
    (define-values (_result _tm-cpu-ms tm-real-ms _tm-gc-ms)
      (time-apply uri-download (list uri)))
    (log-info "Peer downloaded in ~a seconds, uri: ~a" (/ tm-real-ms 1000.0) u)))

(: timeline-download (-> Integer (Listof Peer) Void))
(define (timeline-download num-workers peers)
  ; TODO No need for map - can just iter
  (void (concurrent-filter-map num-workers peer-download peers)))

(: uniq (∀ (α) (-> (Listof α) (Listof α))))
(define (uniq xs)
  (set->list (list->set xs)))

(: peers->timeline (-> (listof Peer) (listof Msg)))
(define (peers->timeline peers)
  (append* (filter-map peer->msgs peers)))

(: timeline-sort (-> (listof Msg) timeline-order (Listof Msgs)))
(define (timeline-sort msgs order)
  (define cmp (match order
                ['old->new <]
                ['new->old >]))
  (sort msgs (λ (a b) (cmp (Msg-ts-epoch a)
                           (Msg-ts-epoch b)))))

(: paths->peers (-> (Listof String) (Listof Peer)))
(define (paths->peers paths)
  (let* ([paths (match paths
                  ['()
                   (let ([peer-refs-file (build-path tt-home-dir "peers")])
                     (log-debug
                       "No peer ref file paths provided, defaulting to ~v"
                       (path->string peer-refs-file))
                     (list peer-refs-file))]
                  [paths
                    (log-debug "Peer ref file paths provided: ~v" paths)
                    (map string->path paths)])]
         [peers (append* (map file->peers paths))])
    (log-info "Read-in ~a peers." (length peers))
    (uniq peers)))

(: log-writer-stop (-> Thread Void))
(define (log-writer-stop log-writer)
  (log-message (current-logger) 'fatal 'stop "Exiting." #f)
  (thread-wait log-writer))

(: log-writer-start (-> Log-Level Thread))
(define (log-writer-start level)
  (let* ([logger
           (make-logger #f #f level #f)]
         [log-receiver
           (make-log-receiver logger level)]
         [log-writer
           (thread
             (λ ()
                (parameterize
                  ([date-display-format 'iso-8601])
                  (let loop ()
                    (match-define (vector level msg _ topic) (sync log-receiver))
                    (unless (equal? topic 'stop)
                      (eprintf "~a [~a] ~a~n" (date->string (current-date) #t) level msg)
                      (loop))))))])
    (current-logger logger)
    log-writer))

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
      "r, read     : Read the timeline (offline operation)."
      "d, download : Download the timeline."
      ; TODO Add path dynamically
      "u, upload   : Upload your twtxt file (alias to execute ~/.tt/upload)."
      "c, crawl    : Discover new peers mentioned by known peers (offline operation)."
      ""
      #:args (command . args)
      (define log-writer (log-writer-start log-level))
      (current-command-line-arguments (list->vector args))
      (match command
        [(or "d" "download")
         ; Initially, 15 was fastest out of the tried: 1, 5, 10, 20.  Then I
         ; started noticing significant slowdowns. Reducing to 5 seems to help.
         (let ([num-workers 5])
           (command-line
             #:program
             "tt download"
             #:once-each
             [("-j" "--jobs")
              njobs "Number of concurrent jobs."
              (set! num-workers (string->number njobs))]
             #:args file-paths
             (let ([peers (paths->peers file-paths)])
               (define-values (_res _cpu real-ms _gc)
                 (time-apply timeline-download (list num-workers peers)))
               (log-info "Downloaded timelines from ~a peers in ~a seconds."
                         (length peers)
                         (/ real-ms 1000.0)))))]
        [(or "u" "upload")
         (command-line
           #:program
           "tt upload"
           #:args ()
           (if (system (path->string (build-path tt-home-dir "upload")))
               (exit 0)
               (exit 1)))]
        [(or "r" "read")
         (let ([out-format 'multi-line]
               [order      'old->new]
               [ts-min     #f]
               [ts-max     #f])
           (command-line
             #:program
             "tt read"
             #:once-each
             [("-r" "--rev")
              "Reverse displayed timeline order."
              (set! order 'new->old)]
             [("-m" "--min")
              m "Earliest time to display (ignore anything before it)."
              (set! ts-min (rfc3339->epoch m))]
             [("-x" "--max")
              x "Latest time to display (ignore anything after it)."
              (set! ts-max (rfc3339->epoch x))]
             #:once-any
             [("-s" "--short")
              "Short output format"
              (set! out-format 'single-line)]
             [("-l" "--long")
              "Long output format"
              (set! out-format 'multi-line)]
             #:args file-paths
             (let* ([peers
                      (paths->peers file-paths)]
                    [timeline
                      (timeline-sort (peers->timeline peers) order)]
                    [timeline
                      (filter (λ (m) (and (if ts-min (>= (Msg-ts-epoch m)
                                                         ts-min)
                                              #t)
                                          (if ts-max (<= (Msg-ts-epoch m)
                                                         ts-max)
                                              #t)))
                              timeline)])
               (timeline-print out-format timeline))))]
        [(or "c" "crawl")
         (command-line
           #:program
           "tt crawl"
           #:args file-paths
           (let* ([peers-sort
                    (λ (peers) (sort peers (match-lambda**
                                             [((Peer n1 _) (Peer n2 _))
                                              (string<? (if n1 n1 "")
                                                        (if n2 n2 ""))])))]
                  [peers-all-file
                    (build-path tt-home-dir "peers-all")]
                  [peers-mentioned-file
                    (build-path tt-home-dir "peers-mentioned")]
                  [peers
                    (paths->peers
                      (match file-paths
                        ; TODO Refactor such that path->string not needed
                        ['() (list (path->string peers-all-file))]
                        [_   file-paths]))]
                  [timeline
                    (peers->timeline peers)]
                  [peers-mentioned-curr
                    (uniq (append* (map Msg-mentions timeline)))]
                  [peers-mentioned-prev
                    (file->peers peers-mentioned-file)]
                  [peers-mentioned
                    (peers-sort (uniq (append peers-mentioned-prev
                                              peers-mentioned-curr)))]
                  [peers-all-prev
                    (file->peers peers-all-file)]
                  [peers-all
                    (list->set (append peers
                                       peers-mentioned
                                       peers-all-prev))]
                  [n-peers-discovered
                    (set-count (set-subtract peers-all
                                             (list->set peers-all-prev)))]
                  [peers-all
                    (peers-sort (set->list peers-all))])
             (log-info "Discovered ~a new peers." n-peers-discovered)
             (peers->file peers-mentioned
                          peers-mentioned-file)
             (peers->file peers-all
                          peers-all-file)))]
        [command
          (eprintf "Error: invalid command: ~v\n" command)
          (eprintf "Please use the \"--help\" option to see a list of available commands.\n")
          (exit 1)])
      (log-writer-stop log-writer))))
