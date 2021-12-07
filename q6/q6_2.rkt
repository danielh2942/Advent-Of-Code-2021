#lang typed/racket
(require math/matrix)
(require racket/vector)
(define mat (matrix [[0 1 0 0 0 0 0 0 0] ;;The Next State works as follows
                     [0 0 1 0 0 0 0 0 0] ;;Subtract each day by 1
                     [0 0 0 1 0 0 0 0 0] ;;Insert any entities that were at zero (1 in the matrix), at 6 (7)
                     [0 0 0 0 1 0 0 0 0] ;;And add duplicates at position 8 (9 in the matrix)
                     [0 0 0 0 0 1 0 0 0]
                     [0 0 0 0 0 0 1 0 0]
                     [1 0 0 0 0 0 0 1 0]
                     [0 0 0 0 0 0 0 0 1]
                     [1 0 0 0 0 0 0 0 0]]))

(: string_list->num_list (-> (Listof String) (Listof Integer)))
(define (string_list->num_list input)
  (if (null? input) '() (cons (assert (string->number(string-trim (car input))) exact-integer?) (string_list->num_list (cdr input)))))

(: mat-pow (-> (Matrix Number) Integer (Matrix Number)))
(define (mat-pow mat pow )
  (define p mat)
  (if (= pow 1)
      mat
      (begin
        (set! p (mat-pow mat (floor (/ pow 2))))
        (if (= (modulo pow 2) 0)
            (matrix* p p)
            (matrix* p p mat)))))

(: count_list_to_vector (-> (Vectorof Integer) (Listof Integer) (Vectorof Integer)))
(define (count_list_to_vector vec nums)
  (if (null? nums)
      vec
      (begin
        (vector-set! vec (car nums) (+ (vector-ref vec (car nums)) 1))
        (count_list_to_vector vec (cdr nums)))))

(define vec (make-vector 9 0))
(define data (string_list->num_list (string-split (car (file->lines "input.txt")) ",")))
(set! vec (count_list_to_vector vec data))
(define vecm (vector->matrix 9 1 vec)) ;; I was getting the wrong values for a while then I realized all I needed to do was re-orient this matrix by 90deg
(apply + (matrix->list (matrix* (mat-pow mat 80) vecm)))
(apply + (matrix->list (matrix* (mat-pow mat 256) vecm)))