#lang racket/base

(require net/url-structs
         "./attrs.rkt")
(provide sniff-encoding)

(define (sniff-encoding/bytes bytestr)
  (define blen (bytes-length bytestr))
  (define initial-segment-length (min 1024 blen))
  (define initial-segment (subbytes bytestr
                                    0
                                    initial-segment-length))
  (define (encoding-from-meta pos)
    "iso-8859-1")
  (define (sniff pos)
    (let ([meta-match (regexp-match "^<[mM][eE][tT][aA][ /](.*)"
                                    initial-segment
                                    pos)])
      (if (and (list? meta-match)
               (= (length meta-match) 2)
               (bytes? (list-ref meta-match 1)))
          (values (encoding-from-meta (+ pos 7))
                  'certain)
          (values "utf-8"
                  'certain))))
  (sniff 0))

(define (sniff-encoding/url url)
  (values "utf-8"
          'certain))

(define (sniff-encoding/port port)
  (values "utf-8"
          'certain))

(define (sniff-encoding/override encoding)
  (values encoding
          'certain))

(define (sniff-encoding thing #:encoding [encoding #f])
  (cond ((not (eq? encoding #f))
         (unless (string? encoding)
           (raise-arguments-error 'sniff-encoding
                                  "If an encoding is provided, it should be a string."
                                  "encoding"
                                  encoding))
         (sniff-encoding/override encoding))
        ((bytes? thing)
         (sniff-encoding/bytes thing))
        ((url? thing)
         (sniff-encoding/url thing))
        ((port? thing)
         (sniff-encoding/port thing))
        (#t
         (raise-arguments-error 'sniff-encoding
                                "Don't know what to do; argument is neither a byte string nor a URL."
                                "thing"
                                thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (define (test-html input-html input-encoding expected-encoding expected-confidence)
    (let-values ([(encoding confidence) (sniff-encoding input-html
                                                        #:encoding input-encoding)])
    (check-equal? encoding expected-encoding)
    (check-equal? confidence expected-confidence))))

(module+ test
  (check-exn exn:fail?
             (lambda ()
               (sniff-encoding "" #:encoding 5)))
  (test-html #f
             "iso-8859-1"
             "iso-8859-1"
             'certain)
  (test-html #""
             #f
             "utf-8"
             'tentative)
  (test-html #"<!--<meta charset=\"iso-8859-1\"/>"
             #f
             "iso-8859-1"
             'certain)
  (test-html #"<html><body><p>hi"
             #f
             "utf-8"
             'certain)
  (test-html #""
             #f
             "utf-8"
             'certain)
  (test-html #"<meta charset=\"iso-8859-4\"/>"
             #f
             "iso-8859-4"
             'certain)

  ;; The next group of tests check whether we correctly handle encoding detection
  ;; when the character after "<meta" is not a simple space.
  (test-html (bytes-append #"<meta"
                           (string->bytes/utf-8 (format "~c" #\u0009)) ;; tab
                           #"charset=\"iso-8859-9\"/>")
             #f
             "iso-8859-9"
             'certain)
  (test-html (bytes-append #"<meta"
                           (string->bytes/utf-8 (format "~c" #\u000A)) ;; line feed
                           #"charset=\"iso-885-3\"/>")
             #f
             "iso-885-3"
             'certain)
  (test-html (bytes-append #"<meta"
                           (string->bytes/utf-8 (format "~c" #\u000C)) ;; form feed
                           #"charset=\"iso-884-3\"/>")
             #f
             "iso-884-3"
             'certain)
  (test-html (bytes-append #"<meta"
                           (string->bytes/utf-8 (format "~c" #\u000D)) ;; carriage return
                           #"charset=\"iso-894-3\"/>")
             #f
             "iso-894-3"
             'certain)
  ;; here's a whitespace character that, officially, is not supported
  (test-html (bytes-append #"<meta"
                           (string->bytes/utf-8 (format "~c" #\u000B)) ;; vertical tab
                           #"charset=\"iso-894-3\"/>")
             #f
             "utf-8"
             'tentative))
