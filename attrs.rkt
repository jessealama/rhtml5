#lang racket/base

(require net/url-structs)
(require "bytes.rkt")
(provide attr-name+value)

(define (consume-spaces bstr pos)
  (define blen (bytes-length bstr))
  (if (<= blen pos)
      pos
      (let ([b (bytes-ref bstr pos)])
        (cond ([whitespace-byte? b]
               (consume-spaces bstr (+ pos 1)))
              ((not (= b =/byte))
               pos)
              (else
               (+ pos 1))))))

(define (extract-value bstr pos)
  (define blen (bytes-length bstr))
  (define (extract-value-helper pos quote-char attr-value)
    (if (<= pos blen)
	attr-value
	(let ([b (bytes-ref bstr pos)])
	  (cond ([whitespace-byte? b]
		 (extract-value-helper (+ pos 1) quote-char attr-value))
		([(quote-byte? b)]
		 (if (byte? quote-char)
		     (values attr-value (+ pos 1))
		     (extract-value-helper (+ pos 1) b attr-value)))
		([uppercase-letter? b]
		 (extract-value-helper (+ pos 1) quote-char (format "~a~a" attr-value (lowercase-byte b))))
		(else
		 (extract-value-helper (+ pos 1) quote-char (format "~a~a" attr-value b)))))))
  (extract-value-helper pos #f ""))

(define (extract-name bstr pos)
  (define (name-helper pos attr-name)
    (if (<= (bytes-length bstr) pos)
	(values attr-name pos)
	(let ([b (bytes-ref bstr pos)])
	  (cond ((and (= b =/byte)
		      (not (string=? attr-name "")))
		 (values attr-name (+ pos 1)))
		((whitespace-byte? b)
		 (let ([new-pos (consume-spaces bstr (+ pos 1))])
		   (values attr-name new-pos)))
		((or (= b slash/byte)
		     (= b >/byte))
		 (values attr-name (+ pos 1)))
		((and (>= b 65) (<= b 90))
		 (name-helper (+ pos 1)
			      (format "~a~a"
				      attr-name
				      (byte->string/utf-8 (+ b 32)))))
		(else
		 (name-helper (+ pos 1)
			      (format "~a~a"
				      attr-name
				      (byte->string/utf-8 b))))))))
  (name-helper pos ""))

(define (attr-name+value bstr pos)
  (let-values ([(attr after-name-pos) (extract-name bstr pos)])
    (let-values ([(value after-value-pos) (extract-value bstr after-name-pos)])
      (values attr value after-value-pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (define (test-attrs input-bytes position expected-attr-name expected-attr-value expected-position)
    (let-values ([(n v pos) (attr-name+value input-bytes position)])
      (check-equal? n expected-attr-name)
      (check-equal? v expected-attr-value)
      (check-equal? pos expected-position))))

(module+ test
  (define bstr-1 #"a=b")
  (test-attrs bstr-1 0 "a" "b" 3)
  (test-attrs bstr-1 1 #f #f 3)
  (define bstr-2 #" xy = z")
  (test-attrs bstr-2 0 "xy" "z" 7)
  (test-attrs bstr-2 1 "xy" "z" 7)
)
