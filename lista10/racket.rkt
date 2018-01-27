#lang racket
;https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Pairs.html#Pairs

; ZADANIE 2

;(count-atoms (cons 1 2)) = 2
;(count-atoms '(cons (#\c . x) "napis")) = 4
;@(1 @(2 3))

(define count-atoms
  (lambda (p)
    (if (pair? p)
        (+ (count-atoms (car p)) (count-atoms (cdr p)))
        (if (null? p)
            0
            1))))


; ZADANIE 3

(define (mk-mobile left right) (cons left right))
(define (mk-branch length struct) (cons length struct))

;left-branch, right-branch dla konstrukcji
(define (left-branch struct) (car struct))
(define (right-branch struct) (cdr struct))

;branch-length, branch-struct dla ramienia
(define (branch-length branch) (car branch))
(define (branch-struct branch) (cdr branch))

(define (weight struct)
  (if (number? struct)
      struct
      (+ (weight (branch-struct (left-branch struct))) (weight (branch-struct (right-branch struct))))))

(define (balanced struct)
  (if (number? struct)
      #t
      (and (and (balanced (branch-struct (left-branch struct))) (balanced (branch-struct (right-branch struct))))
           (= (* (weight (branch-struct (left-branch struct))) (branch-length (left-branch struct)))
              (* (weight (branch-struct (right-branch struct))) (branch-length (right-branch struct)))))))

(require racket/draw)
(require racket/gui)

 (define draw-bottom
   (lambda (dc x y)
     (send dc draw-line
           x y
           x (- y 10))))

(define draw-left-line
  (lambda (dc x y l)
    (send dc draw-line
          x y
          (- x l) y)))

(define draw-right-line
  (lambda (dc x y l)
    (send dc draw-line
           x y
           (+ x l) y)))

(define (draw-single-struct dc struct x y)
  (draw-bottom dc x y)
  (draw-left-line dc x (- y 10) (branch-length (left-branch struct)))
  (draw-right-line dc x (- y 10) (branch-length (right-branch struct))))

(define (draw-structs dc struct x y)
  (draw-single-struct dc struct x y)
  (draw-struct dc (branch-struct (left-branch struct)) (- x (branch-length (left-branch struct))) (- y 10))
  (draw-struct dc (branch-struct (right-branch struct)) (+ x (branch-length (right-branch struct))) (- y 10)))
     

(define (draw-struct dc struct x y)
  (if (number? struct)
      (void)
      (draw-structs dc struct x y)))


(define test-struct
  (mk-mobile (mk-branch 20 (mk-mobile (mk-branch 50 2) (mk-branch 5 20))) (mk-branch 50 (mk-mobile (mk-branch 10 2) (mk-branch 5 30)))))


(define (draw-struct-wrapper struct)
  (define target (make-bitmap 400 400))
  (define dc (new bitmap-dc% [bitmap target]))
  (begin (draw-struct dc struct 200 400)) (make-object image-snip% target))
   


