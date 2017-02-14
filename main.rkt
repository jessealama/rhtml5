#lang racket/base

(require net/url-structs)

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(define (sniff-encoding/bytes bytestr)
  (values "utf-8"
          'certain))

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

(module+ test
  ;; Tests to be run with raco test
  (check-exn exn:fail?
             (lambda ()
               (sniff-encoding "" #:encoding 5)))
  (let-values ([(encoding confidence) (sniff-encoding #f #:encoding "iso-8859-1")])
    (check-equal? encoding "iso-8859-1")
    (check-equal? confidence 'certain))
  (define html #"<html><body><p>hi")
  (let-values ([(encoding confidence) (sniff-encoding html)])
    (check-equal? encoding "utf-8")
    (check-equal? confidence 'certain))
  (let-values ([(encoding confidence) (sniff-encoding #"")])
    (check-equal? encoding "utf-8")
    (check-equal? confidence 'certain)))

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
