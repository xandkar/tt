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

(define-type Result
  (∀ (α β) (U (cons 'ok α)
              (cons 'error β))))

(struct Hist
        ([freq : Nonnegative-Integer]
         [last : Nonnegative-Integer])
        #:transparent)

(define-type Nick-Hist
  (Immutable-HashTable Url (Immutable-HashTable (Option String) Hist)))

(struct User
        ([uri  : Url]
         [nick : (Option String)]))

(struct User-Agent
        ([user : User]
         [prog : Prog]))

(struct Prog
        ([name    : String]
         [version : String]))

(struct Msg
        ([ts-epoch   : Integer]
         [ts-orig    : String]
         [from       : Peer]
         [text       : String]
         [mentions   : (Listof Peer)]))

(struct Peer
        ([nick    : (Option String)]
         [uri     : Url]
         [uri-str : String]
         [comment : (Option String)])
        #:transparent)

(: prog Prog)
(define prog
  (Prog "tt" (info:#%info-lookup 'version)))

(: user-default User)
(define user-default
  (User (string->url "https://github.com/xandkar/tt") #f))

(: user->str (-> User String))
(define (user->str user)
  (match-define (User u0 n) user)
  (define u (url->string u0))
  (if n
      (format "+~a; @~a" u n)
      (format "+~a"      u  )))

(: user-agent->str (-> User-Agent String))
(define (user-agent->str ua)
  (match-define (User-Agent u p) ua)
  (format "~a/~a (~a)" (Prog-name p) (Prog-version p) (user->str u)))

(: user->user-agent User)
(define (user->user-agent user)
  (User-Agent user prog))

(: user-agent-str String)
(define user-agent-str
  (user-agent->str (user->user-agent user-default)))

(: set-user-agent-str (-> Path-String Void))
(define (set-user-agent-str filename)
  (set! user-agent-str (user-agent->str (user->user-agent (file->user filename))))
  (log-info "User-Agent string is now set to: ~v" user-agent-str))

(: file->user (-> Path-String User))
(define (file->user filename)
  (if (file-exists? filename)
      (match (file->peers filename)
        [(list p)
         (log-info
           "User-Agent. Found one peer in file: ~v. Using the found peer: ~a"
           filename
           (peer->str p))
         (peer->user p)]
        [(list* p _)
         (log-warning
           "User-Agent. Multiple peers in file: ~v. Picking arbitrary: ~a"
           filename
           (peer->str p))
         (peer->user p)]
        ['()
         (log-warning
           "User-Agent. No peers found in file: ~v. Using the default user: ~a"
           filename
           user-default)
         user-default])
      (begin
        (log-warning
          "User-Agent. File doesn't exist: ~v. Using the default user: ~a"
          filename
          user-default)
        user-default)))

(: peer->user (-> Peer User))
(define (peer->user p)
  (match-define (Peer n u _ _) p)
  (User u n))

(: peers-equal? (-> Peer Peer Boolean))
(define (peers-equal? p1 p2)
  (equal? (Peer-uri-str p1)
          (Peer-uri-str p2)))

(: peer-hash (-> Peer Fixnum))
(define (peer-hash p)
  (equal-hash-code (Peer-uri-str p)))

(define-custom-set-types peers
  #:elem? Peer?
  peers-equal?
  peer-hash)
; XXX Without supplying above explicit hash procedure, we INTERMITTENTLY get
;     the following contract violations:
;
;         custom-elem-contents: contract violation
;           expected: custom-elem?
;           given: #f
;           context...:
;            /usr/share/racket/collects/racket/private/set-types.rkt:104:0: custom-set->list
;            /home/siraaj/proj/pub/tt/tt.rkt:716:0: crawl
;            /usr/share/racket/collects/racket/cmdline.rkt:191:51
;            body of (submod "/home/siraaj/proj/pub/tt/tt.rkt" main)
;
; TODO Investigate why and make a minimal reproducible test case.

(: peers-merge (-> (Listof Peer) * (Listof Peer)))
(define (peers-merge . peer-sets)
  (define groups
    (foldl
      (λ (p groups)
         (hash-update groups (Peer-uri-str p) (λ (group) (cons p group)) '()))
      (hash)
      (append* peer-sets)))
  (define (merge peers)
    (match peers
      ['() (raise 'impossible)]
      [(list p) p]
      [(list* p1 p2 ps)
       (let* ([n1 (Peer-nick p1)]
              [n2 (Peer-nick p2)]
              [p (cond
                   ; TODO Try to pick from nicks db: preferred, otherwise seen
                   [(and (not n1) (not n2)) p1] ; TODO update with most-common nick
                   [(and      n1       n2 ) p1] ; TODO compare which is more-common
                   [(and      n1  (not n2)) p1]
                   [(and (not n1)      n2)  p2]
                   [else
                     (raise 'impossible)])])
         (merge (cons p ps)))]))
  (sort (map merge (hash-values groups))
        (match-lambda**
          [((Peer _ _ u1 _) (Peer _ _ u2 _)) (string<? u1 u2)])))

(module+ test
  (let* ([u1 "http://foo/bar"]
         [u2 "http://baz/quux"]
         [p1 (Peer #f (string->url u1) u1 #f)]
         [p2 (Peer "a" (string->url u1) u1 #f)]
         [p3 (Peer "b" (string->url u2) u2 #f)]
         [s1 (list p1)]
         [s2 (list p2 p3)])
    (check-equal? (list p3 p2) (peers-merge s1 s2))
    (check-equal? (list p3 p2) (peers-merge s2 s1))))

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
             [nick  (Peer-nick (Msg-from msg))]
             [uri   (Peer-uri-str (Msg-from msg))]
             [text  (Msg-text msg)])
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
           (log-debug "Invalid timestamp: ~v" ts)
           #f]))))

(: str->msg (-> Peer String (Option Msg)))
(define str->msg
  (let ([re (pregexp "^([^\\s\t]+)[\\s\t]+(.*)$")])
    (λ (from str)
       (define from-str (peer->str from))
       (define str-head (substring str 0 (min 100 (string-length str))))
       (with-handlers*
         ([exn:fail?
            (λ (e)
               (log-debug
                 "Failed to parse msg: ~v, from: ~v, at: ~v, because: ~v"
                 str-head from-str e)
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
                    (Msg ts-epoch ts-orig from text mentions))
                  (begin
                    (log-debug
                      "Msg rejected due to invalid timestamp. From:~v. Line:~v"
                      from-str str-head)
                    #f)))]
           [_
             (log-debug "Non-msg line. From:~v. Line:~v" from-str str-head)
             #f])))))

(module+ test
  ; TODO Test for when missing-nick case
  (let* ([tzs (for*/list ([d '("-" "+")]
                          [h '("5" "05")]
                          [m '("00" ":00" "57" ":57")])
                         (string-append d h m))]
         [tzs (list* "" "Z" tzs)])
    (for* ([n   '("fake-nick")]
           [u   '("http://fake-uri")]
           [p   (list (Peer n (string->url u) u #f))]
           [s   '("" ":10")]
           [f   '("" ".1337")]
           [z   tzs]
           [sep (list "\t" " ")]
           [txt '("foo bar baz" "'jaz poop bear giraffe / tea" "@*\"``")])
          (let* ([ts (string-append "2020-11-18T22:22"
                                    (if (non-empty-string? s) s ":00")
                                    z)]
                 [m  (str->msg p (string-append ts sep txt))])
            (check-not-false m)
            (check-equal? (Msg-from m) p)
            (check-equal? (Msg-text m) txt)
            (check-equal? (Msg-ts-orig m) ts (format "Given: ~v" ts))
            )))

  (let* ([ts       "2020-11-18T22:22:09-0500"]
         [tab      "	"]
         [text     "Lorem ipsum"]
         [nick     "foo"]
         [uri      "http://bar/"]
         [peer     (Peer nick (string->url uri) uri #f)]
         [actual   (str->msg peer (string-append ts tab text))]
         [expected (Msg 1605756129 ts peer text '())])
    (check-equal?
      (Msg-ts-epoch actual)
      (Msg-ts-epoch expected)
      "str->msg ts-epoch")
    (check-equal?
      (Msg-ts-orig actual)
      (Msg-ts-orig expected)
      "str->msg ts-orig")
    (check-equal?
      (Peer-nick (Msg-from actual))
      (Peer-nick (Msg-from expected))
      "str->msg nick")
    (check-equal?
      (Peer-uri (Msg-from actual))
      (Peer-uri (Msg-from expected))
      "str->msg uri")
    (check-equal?
      (Peer-uri-str (Msg-from actual))
      (Peer-uri-str (Msg-from expected))
      "str->msg uri-str")
    (check-equal?
      (Msg-text actual)
      (Msg-text expected)
      "str->msg text")))

(: str->lines (-> String (Listof String)))
(define (str->lines str)
  (string-split str (regexp "[\r\n]+")))

(module+ test
  (check-equal? (str->lines "abc\ndef\n\nghi") '("abc" "def" "ghi")))

; TODO Should return 2 things: 1) msgs; 2) metadata parsed from comments
; TODO Update peer nick based on metadata?
(: str->msgs (-> Peer String (Listof Msg)))
(define (str->msgs peer str)
  (filter-map (λ (line) (str->msg peer line))
              (filter-comments (str->lines str))))

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

(define url->cache-object-path
  url->cache-file-path-v2)

(define (url->cache-etag-path uri)
  (build-path cache-dir "etags" (uri-encode (url->string uri))))

(define (url->cache-lmod-path uri)
  (build-path cache-dir "lmods" (uri-encode (url->string uri))))

(: uri-read-cached (-> Url (Option String)))
(define (uri-read-cached uri)
  (define path-v1 (url->cache-file-path-v1 uri))
  (define path-v2 (url->cache-file-path-v2 uri))
  (when (file-exists? path-v1)
    (rename-file-or-directory path-v1 path-v2 #t))
  (if (file-exists? path-v2)
      (file->string path-v2)
      (begin
        (log-debug "Cache file not found for URI: ~a" (url->string uri))
        #f)))

(: str->url (-> String (Option String)))
(define (str->url s)
  (with-handlers*
    ([exn:fail? (λ (e) #f)])
    (string->url s)))

(: peer->str (-> Peer String))
(define (peer->str peer)
  (match-define (Peer n _ u c) peer)
  (format "~a~a~a"
          (if n (format "~a " n) "")
          u
          (if c (format " # ~a" c) "")))

(: str->peer (-> String (Option Peer)))
(define (str->peer str)
  (log-debug "Parsing peer string: ~v" str)
  (match
    (regexp-match
      #px"(([^\\s\t]+)[\\s\t]+)?([a-zA-Z]+://[^\\s\t]*)[\\s\t]*(#\\s*(.*))?"
      str)
    [(list _wholething
           _nick-with-space
           nick
           url
           _comment-with-hash
           comment)
     (match (str->url url)
       [#f
         (log-error "Invalid URI in peer string: ~v" str)
         #f]
       [url
         (Peer nick url (url->string url) comment)])]
    [_
      (log-debug "Invalid peer string: ~v" str)
      #f]))

(module+ test
  (check-equal?
    (str->peer "foo http://bar/file.txt # some rando")
    (Peer "foo" (str->url "http://bar/file.txt") "http://bar/file.txt" "some rando"))
  (check-equal?
    (str->peer "http://bar/file.txt # some rando")
    (Peer #f (str->url "http://bar/file.txt") "http://bar/file.txt" "some rando"))
  (check-equal?
    (str->peer "http://bar/file.txt #")
    (Peer #f (str->url "http://bar/file.txt") "http://bar/file.txt" ""))
  (check-equal?
    (str->peer "http://bar/file.txt#") ; XXX URLs can have #s
    (Peer #f (str->url "http://bar/file.txt#") "http://bar/file.txt#" #f))
  (check-equal?
    (str->peer "http://bar/file.txt")
    (Peer #f (str->url "http://bar/file.txt") "http://bar/file.txt" #f))
  (check-equal?
    (str->peer "foo http://bar/file.txt")
    (Peer "foo" (str->url "http://bar/file.txt") "http://bar/file.txt" #f))
  (check-equal?
    (str->peer "foo bar # baz")
    #f)
  (check-equal?
    (str->peer "foo bar://baz # quux")
    (Peer "foo" (str->url "bar://baz") "bar://baz" "quux"))
  (check-equal?
    (str->peer "foo bar//baz # quux")
    #f))

(: filter-comments (-> (Listof String) (Listof String)))
(define (filter-comments lines)
  (filter-not (λ (line) (string-prefix? line "#")) lines))

(: str->peers (-> String (Listof Peer)))
(define (str->peers str)
  (filter-map str->peer (filter-comments (str->lines str))))

(: peers->file (-> (Listof Peers) Path-String Void))
(define (peers->file peers path)
  (display-lines-to-file
    (map peer->str
         (sort peers
               (match-lambda**
                 [((Peer n1 _ _ _) (Peer n2 _ _ _))
                  (string<? (if n1 n1 "")
                            (if n2 n2 ""))])))
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

(: header-get (-> (Listof Bytes) Bytes (Option Bytes)))
(define (header-get headers name)
  (match (filter-map (curry extract-field name) headers)
    [(list val) val]
    [_           #f]))

(: uri-download-from-port
   (-> Url (Listof (U Bytes String)) Input-Port
       (U 'skipped-cached 'downloaded-new))) ; TODO 'ok|'error ?
(define (uri-download-from-port u headers body-input)
   ; TODO Update message db from here? or where?
   ; - 1st try can just be an in-memory set that gets written-to
   ;   and read-from disk as a whole.
  (define u-str (url->string u))
  (log-debug "uri-download-from-port ~v into ~v" u-str cached-object-path)
  (define cached-object-path (url->cache-object-path u))
  (define cached-etag-path (url->cache-etag-path u))
  (define cached-lmod-path (url->cache-lmod-path u))
  (define etag      (header-get headers #"ETag"))
  (define lmod      (header-get headers #"Last-Modified"))
  (define lmod-curr (if lmod (rfc2822->epoch lmod) #f))
  (define lmod-prev (if (file-exists? cached-lmod-path)
                        (rfc2822->epoch (file->bytes cached-lmod-path))
                        #f))
  (log-debug "lmod-curr:~v lmod-prev:~v" lmod-curr lmod-prev)
  (define cached?
    (or (and etag
             (file-exists? cached-etag-path)
             (bytes=? etag (file->bytes cached-etag-path))
             (begin
               (log-debug "ETags match, skipping the rest of ~v" u-str)
               #t))
        (and lmod-curr
             lmod-prev
             (<= lmod-curr lmod-prev)
             (begin
               (log-debug "Last-Modified <= current skipping the rest of ~v" u-str)
               #t))))
  (if (not cached?)
      (begin
        (log-debug
          "Downloading the rest of ~v. ETag: ~a, Last-Modified: ~v"
          u-str etag lmod)
        (make-parent-directory* cached-object-path)
        (make-parent-directory* cached-etag-path)
        (make-parent-directory* cached-lmod-path)
        (call-with-output-file cached-object-path
                               (curry copy-port body-input)
                               #:exists 'replace)
        (when etag
          (display-to-file etag cached-etag-path #:exists 'replace))
        (when lmod
          (display-to-file lmod cached-lmod-path #:exists 'replace))
        'downloaded-new)
      'skipped-cached))

(: uri-download
   (-> Positive-Float Url
       (Result (U 'skipped-cached 'downloaded-new)
               Any))) ; TODO Maybe more-precise error type?
(define (uri-download timeout u)
  (define u-str (url->string u))
  (define timeout-chan (make-channel))
  (define result-chan (make-channel))
  (define timeout-thread
    (thread (λ ()
               ; Doing this instead of sync/timeout to distinguish error values,
               ; rather than just have #f to work with.
               (sleep timeout)
               (channel-put timeout-chan '(error . timeout)))))
  (define result-thread
    (thread (λ ()
               (define result
                 (with-handlers
                   ; TODO Maybe name each known errno? (exn:fail:network:errno-errno e)
                   ([exn:fail:network?
                      (λ (e) `(error . (net-error . ,e)))]
                    [exn?
                      (λ (e) `(error . (other . ,e)))])
                   (define-values (status-line headers body-input)
                     (http-sendrecv/url
                       u
                       #:headers (list (format "User-Agent: ~a" user-agent-str))))
                   (log-debug "headers: ~v" headers)
                   (log-debug "status-line: ~v" status-line)
                   (define status
                     (string->number (second (string-split (bytes->string/utf-8 status-line)))))
                   (log-debug "status: ~v" status)
                   (let ([result
                           ; TODO Handle redirects.
                           ; TODO Should a redirect update a peer URL?
                           (match status
                             [200
                               `(ok . ,(uri-download-from-port u headers body-input))]
                             [_
                               `(error . (http-not-ok . ,status))])])
                     (close-input-port body-input)
                     result)))
               (channel-put result-chan result))))
  (define result (sync timeout-chan result-chan))
  (kill-thread result-thread)
  (kill-thread timeout-thread)
  result)

(: timeline-print (-> Out-Format (Listof Msg) Void))
(define (timeline-print out-format timeline)
  (match timeline
    ['()
     (void)]
    [(cons first-msg _)
     (void (foldl (match-lambda**
                    [((and m (Msg _ _ from _ _)) (cons prev-from i))
                     (let ([i (if (peers-equal? prev-from from) i (+ 1 i))])
                       (msg-print out-format i m)
                       (cons from i))])
                  (cons (Msg-from first-msg) 0)
                  timeline))]))

(: peer->msgs (-> Peer (Listof Msg)))
(define (peer->msgs peer)
  (match-define (Peer nick uri uri-str _) peer)
  (log-debug "Reading peer nick:~v uri:~v" nick uri-str)
  (define msgs-data (uri-read-cached uri))
  ; TODO Expire cache
  (if msgs-data
      (str->msgs peer msgs-data)
      '()))

(: peer-download
   (-> Positive-Float Peer
       (Result (U 'skipped-cached 'downloaded-new)
               Any)))
(define (peer-download timeout peer)
  (match-define (Peer nick uri u _) peer)
  (log-info "Download BEGIN URL:~a" u)
  (define-values (results _tm-cpu-ms tm-real-ms _tm-gc-ms)
    (time-apply uri-download (list timeout uri)))
  (define result (car results))
  (log-info "Download END in ~a seconds, URL:~a, result:~s"
            (/ tm-real-ms 1000.0)
            u
            result)
  result)

(: timeline-download (-> Integer Positive-Float (Listof Peer) Void))
(define (timeline-download num-workers timeout peers)
  (define results
    (concurrent-filter-map num-workers
                           (λ (p) (cons p (peer-download timeout p)))
                           peers))
  (define peers-ok
    (filter-map (match-lambda
                  [(cons p (cons 'ok _)) p]
                  [(cons _ (cons 'error e)) #f])
                results))
  (define peers-err
    (filter-map (match-lambda
                  [(cons _ (cons 'ok _))
                   #f]
                  [(cons p (cons 'error e))
                   (struct-copy Peer p [comment (format "~s" e)])])
                results))
  (peers->file peers-ok (build-path tt-home-dir "peers-last-downloaded-ok"))
  (peers->file peers-err (build-path tt-home-dir "peers-last-downloaded-err")))

(: peers->timeline (-> (Listof Peer) (Listof Msg)))
(define (peers->timeline peers)
  (append* (filter-map peer->msgs peers)))

(: timeline-sort (-> (Listof Msg) timeline-order (Listof Msgs)))
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
         [peers (apply peers-merge (map file->peers paths))])
    (log-info "Read-in ~a peers." (length peers))
    peers))

(: cache-filename->peer (-> Path-String (Option Peer)))
(define (cache-filename->peer filename)
  (define nick #f) ; TODO Look it up in the nick-db when it exists.
  (define url-str (uri-decode (path->string filename))) ; TODO Can these crash?
  (match (str->url url-str)
    [#f #f]
    [url (Peer nick url url-str #f)]))

(: peers-cached (-> (Listof Peer)))
(define (peers-cached)
  ; TODO Expire cache?
  (filter-map cache-filename->peer (directory-list cache-object-dir)))

(: peers-mentioned (-> (Listof Msg) (Listof Peer)))
(define (peers-mentioned msgs)
  (append* (map Msg-mentions msgs)))

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

(: msgs->nick-hist (-> (Listof Msg) Nick-Hist))
(define (msgs->nick-hist msgs)
  (foldl
    (λ (msg url->nick->hist)
       (match-define (Msg curr _ from _ mentions) msg)
       (foldl
         (λ (peer url->nick->hist)
            (match-define (Peer nick url _ _) peer)
            (if nick
                (hash-update url->nick->hist
                             url
                             (λ (nick->hist)
                                (hash-update nick->hist
                                             nick
                                             (match-lambda
                                               [(Hist freq prev)
                                                (Hist (+ 1 freq) (max prev curr))])
                                             (Hist 0 0)))
                             (hash))
                url->nick->hist))
         url->nick->hist
         (cons from mentions)))
    (hash)
    msgs))

(: update-nicks-history-files (-> Nick-Hist Void))
(define (update-nicks-history-files nick-hist)
  (hash-for-each
    nick-hist
    (λ (url nick->hist)
       (define path (build-path tt-home-dir "nicks" "seen" (uri-encode (url->string url))))
       (make-parent-directory* path)
       (display-lines-to-file
         (map (match-lambda
                [(cons nick (Hist freq last))
                 (format "~a ~a ~a" nick freq last)])
              (sort (hash->list nick->hist)
                    (match-lambda**
                      [((cons _ (Hist a _)) (cons _ (Hist b _)))
                       (> a b)])))
         path
         #:exists 'replace))))

(: nick-hist-most-by (-> Nick-Hist Url (-> Hist Nonnegative-Integer) (Option String)))
(define (nick-hist-most-by url->nick->hist url by)
  (match (hash-ref url->nick->hist url #f)
    [#f #f]
    [nick->hist
      (match (sort (hash->list nick->hist)
                   (λ (a b) (> (by (cdr a))
                               (by (cdr b)))))
        ['() #f]
        [(cons (cons nick _) _) nick])]))

(: nick-hist-latest (-> Nick-Hist Url (Option String)))
(define (nick-hist-latest nick-hist url)
  (nick-hist-most-by nick-hist url Hist-last))

(: nick-hist-common (-> Nick-Hist Url (Option String)))
(define (nick-hist-common nick-hist url)
  (nick-hist-most-by nick-hist url Hist-freq))

(: peers-update-nick-to-common (-> Nick-Hist (Listof Peer) (Listof Peer)))
(define (peers-update-nick-to-common nick-hist peers)
  (map
    (λ (p)
       (match (nick-hist-common nick-hist (Peer-uri p))
         [#f p]
         [n (struct-copy Peer p [nick n])]))
    peers))

(module+ test
  (let* ([url-str  "http://foo"]
         [url      (string->url url-str)]
         [nick1    "a"]
         [nick2    "b"]
         [nick3    "c"]
         [ts-str-1 "2021-11-29T23:29:08-0500"]
         [ts-str-2 "2021-11-29T23:30:00-0500"]
         [ts-1     (rfc3339->epoch ts-str-1)]
         [ts-2     (rfc3339->epoch ts-str-2)]
         [msgs
           (map (match-lambda
                  [(cons ts-str nick)
                   (str->msg (str->peer "test http://test")
                             (string-append ts-str "	Hi @<" nick " " url-str ">"))])
                (list (cons ts-str-2 nick1)
                      (cons ts-str-1 nick2)
                      (cons ts-str-1 nick2)
                      (cons ts-str-1 nick3)
                      (cons ts-str-1 nick3)
                      (cons ts-str-1 nick3)))]
         [hist
           (msgs->nick-hist msgs)])
    (check-equal? (hash-ref (hash-ref hist url) nick1) (Hist 1 ts-2))
    (check-equal? (hash-ref (hash-ref hist url) nick2) (Hist 2 ts-1))
    (check-equal? (hash-ref (hash-ref hist url) nick3) (Hist 3 ts-1))
    (check-equal? (nick-hist-common hist url) nick3)
    (check-equal? (nick-hist-latest hist url) nick1)))

(: crawl (-> Void))
(define (crawl)
  ; TODO Test the non-io parts of crawling
  (let* ([peers-all-file
           (build-path tt-home-dir "peers-all")]
         [peers-mentioned-file
           (build-path tt-home-dir "peers-mentioned")]
         [peers-parsed-file
           (build-path tt-home-dir "peers-parsed")]
         [peers-cached-file
           (build-path tt-home-dir "peers-cached")]
         [peers-cached
           (peers-cached)]
         [cached-timeline
           (peers->timeline peers-cached)]
         [nick-hist
           (msgs->nick-hist cached-timeline)]
         [peers-mentioned-curr
           (peers-mentioned cached-timeline)]
         [peers-mentioned-prev
           (file->peers peers-mentioned-file)]
         [peers-all-prev
           (file->peers peers-all-file)]
         [peers-mentioned
           (peers-merge peers-mentioned-prev
                        peers-mentioned-curr)]
         [peers-all
           (peers-update-nick-to-common
             nick-hist
             (peers-merge peers-mentioned
                          peers-all-prev
                          peers-cached))]
         [peers-discovered
           (set->list (set-subtract (make-immutable-peers peers-all)
                                    (make-immutable-peers peers-all-prev)))]
         [peers-parsed
           (filter (λ (p) (> (length (peer->msgs p)) 0)) peers-all)])
    ; TODO Deeper de-duping
    (log-info "Known peers cached ~a" (length peers-cached))
    (log-info "Known peers mentioned: ~a" (length peers-mentioned))
    (log-info "Known peers parsed ~a" (length peers-parsed))
    (log-info "Known peers total: ~a" (length peers-all))
    (log-info "Discovered ~a new peers:~n~a"
              (length peers-discovered)
              (pretty-format (map
                               (match-lambda
                                 [(Peer n _ u c) (list n u c)])
                               peers-discovered)))
    (update-nicks-history-files nick-hist)
    (peers->file peers-cached
                 peers-cached-file)
    (peers->file peers-mentioned
                 peers-mentioned-file)
    (peers->file peers-parsed
                 peers-parsed-file)
    (peers->file peers-all
                 peers-all-file)))

(: read (-> (Listof String) Number Number Timeline-Order Out-Format Void))
(define (read file-paths ts-min ts-max order out-format)
  (let* ([peers
           (paths->peers file-paths)]
         [msgs
           (timeline-sort (peers->timeline peers) order)]
         [include?
           (λ (m)
              (and (or (not ts-min) (>= (Msg-ts-epoch m) ts-min))
                   (or (not ts-max) (<= (Msg-ts-epoch m) ts-max))))])
    (timeline-print out-format (filter include? msgs))))

(: upload (-> Void))
(define (upload)
  ; FIXME Should not exit from here, but only after cleanup/logger-stoppage.
  (if (system (path->string (build-path tt-home-dir "hooks" "upload")))
      (exit 0)
      (exit 1)))

(: download (-> (Listof String) Positive-Integer Positive-Float Void))
(define (download file-paths num-workers timeout)
  (let ([peers (paths->peers file-paths)])
    (define-values (_res _cpu real-ms _gc)
      (time-apply timeline-download (list num-workers timeout peers)))
    (log-info "Downloaded timelines from ~a peers in ~a seconds."
              (length peers)
              (/ real-ms 1000.0))))

(: dispatch (-> String Void))
(define (dispatch command)
  (match command
    [(or "d" "download")
     ; Initially, 15 was fastest out of the tried: 1, 5, 10, 20.  Then I
     ; started noticing significant slowdowns. Reducing to 5 seems to help.
     (let ([num-workers 5]
           [timeout     10.0])
       (command-line
         #:program "tt download"
         #:once-each
         [("-j" "--jobs")
          njobs "Number of concurrent jobs."
          (set! num-workers (string->number njobs))]
         [("-t" "--timeout")
          seconds "Timeout seconds per request."
          (set! timeout (string->number seconds))]
         #:args file-paths
         (download file-paths num-workers timeout)))]
    [(or "u" "upload")
     (command-line
       #:program "tt upload" #:args () (upload))]
    [(or "r" "read")
     (let ([out-format 'multi-line]
           [order      'old->new]
           [ts-min     #f]
           [ts-max     #f])
       (command-line
         #:program "tt read"
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
         (read file-paths ts-min ts-max order out-format)))]
    [(or "c" "crawl")
     (command-line
       #:program "tt crawl" #:args () (crawl))]
    [command
      (eprintf "Error: invalid command: ~v\n" command)
      (eprintf "Please use the \"--help\" option to see a list of available commands.\n")
      (exit 1)]))

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
      "u, upload   : Upload your twtxt file (alias to execute ~/.tt/hooks/upload)."
      "c, crawl    : Discover new peers mentioned by known peers (offline operation)."
      ""
      #:args (command . args)
      (define log-writer (log-writer-start log-level))
      (current-command-line-arguments (list->vector args))
      (set-user-agent-str (build-path tt-home-dir "me"))
      ; TODO dispatch should return status with which we should exit after cleanups
      (dispatch command)
      (log-writer-stop log-writer))))
