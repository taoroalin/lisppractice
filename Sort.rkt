;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Sorting


;; Merge Sort

(define (merge-sort ls) (cond [(= (length? ls))]))

;; Insertion Sort

(define (insertion-sort ls) (local
                              (define (sort sorted rest)
                                     (local (define (insert val ls)
                                              (