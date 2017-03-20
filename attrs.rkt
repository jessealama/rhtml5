#lang racket/base

(require net/url-structs)
(require "bytes.rkt")
(provide attr-name+value)

(define (consume-spaces bstr pos)
  (define blen (bytes-length bstr))
  (if (> (+ pos 1) blen)
      #f
      (let ([b (bytes-ref bstr pos)])
        (cond ((or (= b tab/byte)
		   (= b lf/byte)
		   (= b ff/byte)
		   (= b cr/byte)
		   (= b sp/byte))
               (consume-spaces bstr (+ pos 1)))
              ((not (= b =/byte))
               #f)
              (else
               (+ pos 1))))))

(define (attr-name+value bstr pos)
  (define blen (bytes-length bstr))
  (define (value-helper pos attr-value)
    "blog")
  (define (name-helper pos attr-name)
    (if (> (+ pos 1) blen)
        (values attr-name "")
        (let ([b (bytes-ref bstr pos)])
          (cond ((and (= b =/byte)
                      (not (string=? attr-name "")))
                 (values attr-name
			 (value-helper (+ pos 1) "")))
                ((whitespace-byte? b)
                 (let ([new-pos (consume-spaces bstr (+ pos 1))])
                   (values attr-name
                           (if (integer? new-pos)
                               (value-helper (+ new-pos 1) "")
                               ""))))
                ((or (= b slash/byte)
                     (= b >/byte))
                 (values attr-name ""))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (define (test-attrs input-bytes position expected-attr-name expected-attr-value)
    (let-values ([(n v) (attr-name+value input-bytes position)])
      (check-equal? n expected-attr-name)
      (check-equal? v expected-attr-value))))

(module+ test
  (define bstr-1 #"a=b")
  (test-attrs bstr-1 0 "a" "b")
  (test-attrs bstr-1 1 #f #f)
  (define bstr-2 #" xy = z")
  (test-attrs bstr-2 0 "xy" "z")
  (test-attrs bstr-2 1 "xy" "z")
)
