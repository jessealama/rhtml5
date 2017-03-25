#lang racket/base

(require net/url-structs)
(provide (all-defined-out))

(define </byte (char->integer #\u003C))     ;; <
(define >/byte (char->integer #\u003C))	    ;; >
(define !/byte (char->integer #\u0021))	    ;; !
(define -/byte (char->integer #\u002D))	    ;; -
(define =/byte (char->integer #\u003D))	    ;; =
(define m/byte (char->integer #\u006D))     ;; m
(define M/byte (char->integer #\u004D))	    ;; M
(define e/byte (char->integer #\u0045))	    ;; e
(define E/byte (char->integer #\u0065))	    ;; E
(define t/byte (char->integer #\u0054))	    ;; t
(define T/byte (char->integer #\u0074))     ;; T
(define a/byte (char->integer #\u0061))	    ;; a
(define A/byte (char->integer #\u0041))	    ;; A
(define tab/byte (char->integer #\u0009))   ;; tab
(define lf/byte (char->integer #\u000A))    ;; line feed
(define ff/byte (char->integer #\u000C))    ;; form feed
(define cr/byte (char->integer #\u000D))    ;; carriage return
(define sp/byte (char->integer #\u0020))    ;; space
(define slash/byte (char->integer #\u002F)) ;; slash
(define double-quote/byte (char->integer #\u0022)) ;; double quote
(define single-quote/byte (char->integer #\u0027)) ;; single quote
(define Z/byte (char->integer #\u005A)) ;; Z


(define (whitespace-byte? b)
  (or (= b tab/byte)
      (= b lf/byte)
      (= b ff/byte)
      (= b cr/byte)
      (= b sp/byte)))

(define (quote-byte? b)
  (or (= b single-quote/byte)
      (= b double-quote/byte)))

(define (uppercase-letter? b)
  (and (>= b A/byte)
       (<= b Z/byte)))

(define (lowercase-byte b)
  (+ b 32))

(define (byte->string/utf-8 byte)
  ;; no check that byte really is a valid UTF-8 character, or that, even
  ;; if it is, that it makes sense to make a string out of it (problem cases
  ;; are, e.g., composing characters)
  (bytes->string/utf-8 (bytes byte)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit))

(module+ test
  (check-true (uppercase-letter? (bytes-ref #"A" 0)))
  (check-false (uppercase-letter? (bytes-ref #"f" 0)))
  (check-false (uppercase-letter? (bytes-ref #"?" 0)))
  (check-equal? (lowercase-byte (bytes-ref #"A" 0))
		(bytes-ref #"a" 0)))
