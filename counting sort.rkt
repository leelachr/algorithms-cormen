#lang racket

; notes/todo:
; - use vectors instead of arrays for 1-d arrays.
;   (if they're mutable.)
; - is it fundamentally different to build a list
;   and use an upper bound as a starting criteria
;   rather than zero. (build from a natural num.)
; - use hungarian prefix to show where i am using
;   lists v. arrays v. mutable arrays, etc.
; - test suite, rename example data.
; - note: rule is that (for consistency), whenever
;   i turn a value from unsorted (A) into an index
;   i must sub1 from the unsorted value.
; - difference between settable-array and mutable-
;   array in racket?

(require math/array)
(require rackunit)
(require (planet williams/describe/describe)) ; can use (describe x) to get type information.

(define A (array #[3 6 4 1 3 4 1 4])) ; i.e. unsorted
(define B (array #[1 1 3 3 4 4 4 6])) ; i.e. sorted
(define EMPTY-C (mutable-array #[0 0 0 0 0 0])) ; i.e. empty auxiliary array
(define C (array #[2 0 2 3 0 1]))
(define SHIFTED-C (array #[2 2 4 7 7 8])) ; i.e. auxiliary array

; the flow of this program:
; 1. create empty 

; Array -> Mutable-Array
; create an array C up to the max integer that will
; occur in the unsorted input A.
(define (make-auxiliary-array unsorted)
  (local ((define size-for-aux (array-ref (array-axis-max unsorted 0)
                                          #())))
    (array->mutable-array (make-array (vector size-for-aux) 0))))

;(check-equal? (make-auxiliary-array A) EMPTY-C) 

; Array -> Mutable-Array
; go through input A once, incrementing the entry in
; C whose index is the value of the A entry.
(define (count-instances unsorted)
  (local ((define auxiliary (make-auxiliary-array unsorted)))
    (array-map (λ (u) (array-set! auxiliary
                                  (vector u)
                                  (add1 (array-ref auxiliary (vector u)))))
               unsorted)
    auxiliary)) ; interesting note: the fn body has two expressions in a row, which are evaluated sequentially--first the mutating map, then returning the value.

;(check-equal? (count-instances A) C) 

; sum-prev : Mutable-Array -> Mutable-Array
; make each value in C the sum of those that came
; before it.

; Array -> Array
; creates array which is same length as old array.
; each element i in new array is sum of ith and i-1th
; elements in old array. the zeroth element just keeps
; its value, as there is no -1th value.
; this procedure could easily be modified to combine
; the two previous elements in any way and to accept
; any base case.
(define (double-map array)
    (local
      ((define list (array->list array))
       (define init-result (cons (car list) empty))
       (define init-prev (car list))
       ; List -> List
       ; "result" and "prev" arguments are accumulators.
       (define (double-map/a list prev)
         (cond
           [(empty? list) empty]
           [else (cons (+ prev (car list))
                       (double-map/a (cdr list) (+ prev (car list)) ))])))
      ; double-map FUNCTION BODY
      (cons (car list) (double-map/a (cdr list) init-prev))))

(list->array (double-map C))

; Array Array -> Array
; starting at the highest index of unsorted array A,
; take its value and go to the position in C with
; that index. the C value tells you how many numbers
; smaller than the A value there are. place the A
; value in a position that leaves room before it for
; all those smaller values.
(define (put-in-place arr-unsorted shifted-aux)
  (local ((define marr-sorted (array->mutable-array
                               (make-array (vector (array-size arr-unsorted)) 0)))
          (define lst-unsorted (reverse (array->list arr-unsorted)))
          (define marr-aux (array->mutable-array shifted-aux))
          ; List Array Mutable-Array -> Mutable-Array
          (define (put-in-place/a lst-unsorted shifted-aux marr-sorted) ; iterate through a
            (cond
              [(empty? lst-unsorted) marr-sorted]
              [else (array-set! marr-sorted ; mutate marr-sorted
                                (vector (sub1 (array-ref shifted-aux
                                                   (vector (sub1 (car lst-unsorted))))))
                                (car lst-unsorted))
                    (array-set! marr-aux
                                (vector (sub1 (car lst-unsorted)))
                                (sub1 (array-ref marr-aux (vector (sub1 (car lst-unsorted))))))
                    (put-in-place/a (cdr lst-unsorted) ; use marr-sorted
                                    marr-aux
                                    marr-sorted)])))
    ; put-in-place FUNCTION BODY
    (put-in-place/a lst-unsorted shifted-aux marr-sorted)))
 
(put-in-place A SHIFTED-C)

; Array -> Mutable-Array
; sorts array of integers from 1 (not 0!) to k.
(define (linear-sort arr-unsorted)
  (put-in-place arr-unsorted
                (count-instances arr-unsorted)))