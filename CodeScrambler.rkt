;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname CodeScrambler) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Make code that appears same to SPD handin but different to TAs

(require 2htdp/batch-io)

;; Test code

(define test-code-quote
  (quote (string-append PREFIX "_" SUFFIX)))


;; Option 1: Use Racket's metaprogramming features to do everything



;; Option 2: Parse racket yourself
;;(define code0 (read-file "TestProgram.rkt"))
(define code1 "(+ 1 (+ 2 3))")


(define (parse-code code)
  (cond [(string=? (string-ith code 0) "(")
          [local [(define next (parse-code (substring code 1)))]
            (cons (list (first next)) (second next))]]
        [else [local [(define (nextstop token code)
                       (if (string-contains? "() " (string-ith code 0))
                           (list token code)
                           (nextstop (string-append token (string-ith code 0))
                                     (substring code 1))))
                      (define token (first (nextstop "" code)))
                      (define rest (second (nextstop "" code)))
                      (define next (parse-code rest))]
                (cons (list token (first next)) (second next))]]))
(parse-code code1)