#lang racket/base

(module+ test
  (require rackunit
	   (file "./encoding.rkt")))

(module+ test
  ;; Tests to be run with raco test
  (check-exn exn:fail?
             (lambda ()
               (sniff-encoding "" #:encoding 5)))
  (let-values ([(encoding confidence) (sniff-encoding #f #:encoding "iso-8859-1")])
    (check-equal? encoding "iso-8859-1")
    (check-equal? confidence 'certain))
  (let-values ([(encoding confidence) (sniff-encoding #"")])
    (check-equal? encoding "utf-8")
    (check-equal? confidence 'tentative))
  (let-values ([(encoding confidence) (sniff-encoding #"<!--<meta charset=\"iso-8859-1\"/>")])
    (check-equal? encoding "iso-8859-1")
    (check-equal? confidence 'certain))
  (define html #"<html><body><p>hi")
  (let-values ([(encoding confidence) (sniff-encoding html)])
    (check-equal? encoding "utf-8")
    (check-equal? confidence 'certain))
  (let-values ([(encoding confidence) (sniff-encoding #"")])
    (check-equal? encoding "utf-8")
    (check-equal? confidence 'certain))

  (let-values ([(encoding confidence) (sniff-encoding #"<meta charset=\"iso-8859-4\"/>")])
    (check-equal? encoding "iso-8859-4")
    (check-equal? confidence 'certain))
)
