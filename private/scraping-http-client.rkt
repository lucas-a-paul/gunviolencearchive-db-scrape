#lang racket/base

(require
  racket/match
  racket/port
  racket/string
  net/http-client
  html-parsing)

(provide make-scraping-http-client
         scraping-http-client-get-xexp)

(struct scraping-http-client (host port ssl? timeout connection (killer-thread #:mutable)))

(define-logger httpclient)

;; Create a thread that will close the given connection after a timeout
(define (make-killer-thread timeout connection)
  (log-httpclient-debug "Creating killer thread")
  (thread (lambda () (sleep timeout)
            (log-httpclient-info "Killer thread closing connection")
            (http-conn-close! connection))))

;; Make a new scraping-http-client instance for the given host.
; If use-immediately? is true, open the connection immediately.
(define (make-scraping-http-client host [port 443] [ssl? #t] [timeout 5] [use-immediately? #t])
  (define connection (http-conn))
  (when use-immediately?
    (http-conn-open! connection host ssl? port #t))
  (define killer-thread
    (make-killer-thread timeout connection))
  (scraping-http-client host port ssl? timeout connection killer-thread))

(define (scraping-http-client-get
         client
         uri
         #:version [version #"1.1"]
         #:method [method #"GET"]
         #:headers [headers '()]
         #:data [data #f])
  (kill-thread (scraping-http-client-killer-thread client))
  (set-scraping-http-client-killer-thread!
   client
   (make-killer-thread (scraping-http-client-timeout client) (scraping-http-client-connection client)))
  (http-conn-sendrecv! (scraping-http-client-connection client)
                       uri #:version version #:method method #:headers headers #:data data))

;; Get the given URI as an xexp, or #f if the request fails.
; Assumes the URI points to an HTML document encoded in UTF-8.
; Uses a connection that can be reused for subsequent requests if they are made within the timeout period.
; If the timeout expires, the connection is closed, but making another request will make use of
; Racket net/http-client's auto-reconnect feature.
(define (scraping-http-client-get-xexp
         client
         uri
         #:version [version #"1.1"]
         #:method [method #"GET"]
         #:headers [headers '()]
         #:data [data #f])
  (match (bytes->string/utf-8 (findf (lambda (header) (string-prefix? (bytes->string/utf-8 header) "Accept-Charset:")) headers))
    [#f
     #;=> (set! headers (cons #"Accept-Charset: utf-8" headers))]
    [(? (lambda (accept-charset-string) (string-contains? accept-charset-string "utf-8")) _)
     ; no action necessary
     (void)]
    [charset
     (error "Unsupported charset header: ~a" charset)])
  (match (bytes->string/utf-8 (findf (lambda (header) (string-prefix? (bytes->string/utf-8 header) "Accept:")) headers))
    [#f
     #;=> (set! headers (cons #"Accept: text/html" headers))]
    [(? (lambda (accept-string) (string-contains? accept-string "text/html")) _)
     ; no action necessary
     (void)]
    [accept
     (error "Unsupported Accept header: ~a" accept)])
  (define-values (status rheaders body-port)
    (scraping-http-client-get client uri #:version version #:method method #:headers headers #:data data))
  (match (cons status rheaders)
    [(list #"200 OK" ... #"Content-Type: text/html; charset=utf-8" _)
     #;=> (html->xexp (port->string body-port))]
    [_
     (log-httpclient-error "Failed to retrieve HTML - status: ~a, headers: ~a" status rheaders)
     #;=> #f]))
