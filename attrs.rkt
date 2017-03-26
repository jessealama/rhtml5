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

;; byte-string integer -> string|false string|false integer
(define (attr-name+value bstr pos)
  (define len (bytes-length bstr))
  (define (extract-quoted-value pos quote-char attr-value)
    (values "blug" pos))
  ;; integer string -> string integer
  (define (extract-value pos quote-char attr-value)
    (if (<= len pos)
	(values "" pos)
	(let ([b (bytes-ref bstr pos)])
	  (cond
	   ((whitespace-byte? b)
	    (extract-value (+ pos 1) quote-char attr-value))
	   ((quote-byte? b)
	    (extract-quoted-value b attr-value))
	   (else
	    (values "whatever" (+ pos 1)))))))
  ;; integer -> string string integer
  (define (extract-name+value pos attr-name)
    (if (<= len pos)
	(values attr-name "" pos)
	(let ([b (bytes-ref bstr pos)])
	  (cond
	   ([= b >/byte]
	    (values #f #f pos))
	   ((and (= b =/byte)
		 (string? attr-name)
		 (not (string=? attr-name "")))
	    (let-values ([(val pos) (extract-value (+ pos 1) #f "")])
	      (values attr-name val pos)))
	   ((whitespace-byte? b)
	    (extract-name+value (+ pos 1) attr-name))
	   ((or (= b slash/byte)
		(= b >/byte))
	    (values attr-name "" (+ pos 1)))
	   (else
	    (extract-name+value (+ pos 1)
				(format "~a~a"
					attr-name
					(byte->string/utf-8 (if (uppercase-letter? b)
								(+ b 32)
								b)))))))))
  (extract-name+value pos ""))

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
