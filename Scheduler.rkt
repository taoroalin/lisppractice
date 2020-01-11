;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Scheduler) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct ta (name slots))

(define-struct slot (name needed))


(define allslots (list (make-slot "a" 1) (make-slot "b" 1)))

(define tas (list (make-ta "albert" (list "b")) (make-ta "beth" (list "a" "b"))))


(check-expect (assign tas allslots) (list (list "beth" "a") (list "albert" "b")))


(define (assign tas slots)
  
  [local (define (assign assigned unassigned filled)
           [local
             (define (available left)
                    (cond [(empty? left) left]
                        [(string=? (ta-name (first unassigned)) (slot-name (first left)))
                         (slot-name (first left))]
                        [else (available (rest left))]))
             (define choice (available filled))
             (cond [(empty? choice)
                    (assign (cons (cons (ta-name (first unassigned)) choice assigned)]
                   [else 
             ]       
           )
    
    (car (assign '() tas slots))
    
    ]
  )

              
(+ 1 (+ 1 2))

(define (naturals n) (if (=? n 1) '(1) (naturals (- n 1))))

(naturals 3)