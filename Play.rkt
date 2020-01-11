;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Play) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (fibonacci n)
  (local [(define (fibonacci n acc)
            (if (< n 2)
                acc
                (fibonacci (- n 1) (cons (+ (first acc) (second acc)) acc))))]
    (fibonacci n '(1 0))))

(fibonacci 3)

((lambda (x) (+ x 2)) 2)
"hi"
(display #\newline)
"yo"