#lang racket/base

(require net/url-structs)
(provide (all-defined-out))

(define </byte (char->integer #\u003C))   ;; <
(define >/byte (char->integer #\u003C))		  ;; >
(define !/byte (char->integer #\u0021))			  ;; !
(define -/byte (char->integer #\u002D))				  ;; -
(define =/byte (char->integer #\u003D))					  ;; =
(define m/byte (char->integer #\u004D))   ;; m
(define M/byte (char->integer #\u006D))		  ;; M
(define e/byte (char->integer #\u0045))			  ;; e
(define E/byte (char->integer #\u0065))				  ;; E
(define t/byte (char->integer #\u0054))					  ;; t
(define T/byte (char->integer #\u0074))   ;; T
(define a/byte (char->integer #\u0041))		  ;; a
(define A/byte (char->integer #\u0061))			  ;; A
(define tab/byte (char->integer #\u0009))				  ;; tab
(define lf/byte (char->integer #\u000A))  ;; line feed
(define ff/byte (char->integer #\u000C))		  ;; form feed
(define cr/byte (char->integer #\u000D))  ;; carriage return
(define sp/byte (char->integer #\u0020))		  ;; space
(define slash/byte (char->integer #\u002F))				  ;; slash
(define whitespace-bytes (list tab/byte
                               lf/byte
                               ff/byte
                               cr/byte
                               sp/byte))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit))

(module+ test)
