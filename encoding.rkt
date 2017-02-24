#lang racket/base

(require net/url-structs)
(provide sniff-encoding)

(define </byte #\u003C) ;; <
(define >/byte #\u003C) ;; >
(define !/byte #\u0021) ;; !
(define -/byte #\u002D) ;; -
(define m/byte #\u004D) ;; m
(define M/byte #\u006D) ;; M
(define e/byte #\u0045) ;; e
(define E/byte #\u0065) ;; E
(define t/byte #\u0054) ;; t
(define T/byte #\u0074) ;; T
(define a/byte #\u0041) ;; a
(define A/byte #\u0061) ;; A
(define tab/byte #\u0009) ;; tab
(define lf/byte #\u000A) ;; line feed
(define ff/byte #\u000C) ;; form feed
(define cr/byte #\u000D) ;; carriage return
(define sp/byte #\u0020) ;; space
(define slash/byte #\u002F) ;; slash
(define whitespace-bytes (list tab/byte
                               lf/byte
                               ff/byte
                               cr/byte
                               sp/byte))

(define (sniff-encoding/bytes bytestr)
  (define length (bytes-length bytestr))
  (define initial-segment-length (min 1024 length))
  (define initial-segment (subbytes bytestr
                                    0
                                    initial-segment-length))
  (define (get-attribute pos)
    (values #"q" #"frob" (+ pos 1)))
  (define (encoding-from-meta pos)
    (define got-pragma? #f)
    (define need-pragma? #f)
    (define finished? #f)
    (define encoding #f)
    (define (foo foo-pos)
      (let-values ([(name value next-pos) (get-attribute foo-pos)])
        (if (and (eq? name #f)
                 (eq? value #f))
            (if (eq? need-pragma? #f)
                (sniff (+ pos 1))
                (if (eq? got-pragma? #f)
                    (sniff (+ pos 1))
                    (sniff (+ pos 1))))
            (cond ((bytes=? name #"http-equiv")
                   (set! got-pragma? #t))
                  ((bytes=? name #"content")
                   (let ([encoding-from-meta (encoding-from-meta value)])
                     (when encoding-from-meta
                       (unless encoding
                         (set! encoding encoding-from-meta)
                         (set! need-pragma? #t)))))
                  ((bytes=? name #"charset")
                   (set! encoding value)
                   (set! need-pragma? #f))))))
    (foo pos))
  (define (sniff pos)
    (let ([meta-match (regexp-match "<[mM][eE][tT][aA][ /](.*)" initial-segment pos)])
      (if (and (list? meta-match)
               (= (length meta-match) 2)
               (bytes? (list-ref meta-match 2)))
          (encoding-from-meta (+ pos 7))
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
  ;; (test-html #f
  ;;            "iso-8859-1"
  ;;            "iso-8859-1"
  ;;            'certain)
  ;; (test-html #""
  ;;            #f
  ;;            "utf-8"
  ;;            'tentative)
  ;; (test-html #"<!--<meta charset=\"iso-8859-1\"/>"
  ;;            #f
  ;;            "iso-8859-1"
  ;;            'certain)
  (test-html #"<html><body><p>hi"
             #f
             "utf-8"
             'certain)
  (test-html #""
             #f
             "utf-8"
             'certain)
  ;; (test-html #"<meta charset=\"iso-8859-4\"/>"
  ;;            #f
  ;;            "iso-8859-4"
  ;;            'certain)
  )
