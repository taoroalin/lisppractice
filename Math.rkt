;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Math) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Math 223
(require racket/list)
(require racket/pretty)

(define ex1 (list (list 1 2)))

(define ex2 (list (list 1 2 3)
                  (list 4 5 6)))

(define ex3 (list (list 2 4 6 0)
                  (list 4 5 6 3)
                  (list 7 8 9 6)))

(define ex4 (list (list 0 0 1 -1 -1 4)
                  (list 2 4 2 4 2 4)
                  (list 2 4 3 3 3 4)
                  (list 3 6 6 2 6 6)))

(define ex5 (list (list 1 1 1 0 8)
                  (list 1 1 0 1 1)
                  (list 1 0 1 1 14)
                  (list 0 1 1 1 14)))

(define ex6 (list (list 1 1 1 0 8)
                  (list 1 1 1 0 8)
                  (list 1 1 1 0 8)
                  (list 1 1 1 0 8)))

(define ex7 (list (list 0 0)
                  (list 0 1)))

(define ex8 (list empty
                  empty))

(define ex9 empty)

(define ex10 (list (list 0 0)
                   (list 0 0)))

(define (print-matrix m)
  (for-each
   (lambda (a)
     (begin
       (display (foldr
                 (lambda (c d) (string-append (number->string c) " " d)) "" a))
       (display #\newline)))
   m))

(print-matrix ex5)

(define (permute p m)
  (map (lambda (x) (list-ref m x)) p))

;;(permute (list 1 0) ex2)

(define (transpose m)
  (if (empty? (first m))
      '()
      (cons (map first m)
            (transpose (map rest m)))
      ))

;;(transpose ex3)

;; fn to find location of first nonzero in row
(define (pivot l)
  (local [(define (pivot l c)
            (if (not (= (first l) 0))
                c
                (pivot (rest l) (+ c 1))))]
    (pivot l 0)))

;;(pivot '(0 0 2 3 4 0))

;;(define (gauss-elim m) '()) ; stub

(define (gauss-elim m)
  (if (or (empty? m) (empty? (first m))) ;; Stop if empty
      m
      (if (= (first (first m)) 0) ;; Is the top left zero?
          (if (andmap (lambda (x) (= x 0)) (map first m))
              ;; Is the whole first column zero?
              (map cons (map first m) (gauss-elim (map rest m)))
              ;; Add zero column to REF of rest of columns
              (gauss-elim (sort m
                       (lambda (a b) (> (abs (first a)) (abs (first b)))))))
              ;; Sort rows from highest first value to lowest
          ;; If first not zero 
          (local
            [(define top (map (lambda (x) (/ x (first (first m)))) (first m)))
             ;; Scale top to start with 1
             (define (cancel row top)
               (map - row (map (lambda (x) (* x (first row))) top)))
             ;; Fn to cancel first entry of row with top
             (define bottom (map (lambda (x) (cancel x top)) (rest m)))
             ;; Cancel first column of rest of matrix
             ]
            (cons top (gauss-elim bottom))))))

;;(gauss-elim ex2)
;;(gauss-elim ex3)
;;(gauss-elim ex4)
;;(gauss-elim ex5)

;; (define (jordan-elim m) '()) ; stub

(define (jordan-elim m)
  (if (or (empty? m) (empty? (first m))) ;; Stop if empty
      m
      (if (andmap (lambda (x) (= x 0)) (last m)) ;; if bottom row zeros
          (append (jordan-elim (take m (- (length m) 1))) (list (last m)))
          ;; skip to next row up
          (local
            [(define bottom
               (map (lambda (x) (/ x (list-ref (last m) (pivot (last m)))))
                    (last m)))
             (define (cancel row bottom)
               (map - row
                    (map (lambda (x) (* x (list-ref row (pivot bottom))))
                         bottom)))
             ;; Fn to cancel column of pivot
             (define top
               (map (lambda (x) (cancel x bottom)) (take m (- (length m) 1))))
             ;; Cancel first column of rest of matrix
             ]
            (append (jordan-elim top) (list bottom))))))

(define (row-reduce m) (jordan-elim (gauss-elim m)))

(row-reduce ex2)
(row-reduce ex5)
(row-reduce ex6)
(row-reduce ex7)
(row-reduce ex8)
(row-reduce ex9)
(row-reduce ex10)




      