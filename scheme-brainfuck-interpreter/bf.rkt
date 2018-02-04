#lang racket

(require racket/dict)
(require racket/cmdline)

; Total amount of one memory cell in bits
(define mem-cell-size-bits 8)
; Total amount of memory in bytes
(define mem-size-bytes 256)

;; Simple increment/decrement functions
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

;; Calculate modulo n for memory operations
(define (calc-mem-cell-size) (expt 2 mem-cell-size-bits))
(define (calc-mem-size) (* mem-size-bytes (calc-mem-cell-size)))

;; Increment/decrement the value stored in memory cell
(define (inc-mem-value k) (modulo (inc k) (calc-mem-cell-size)))
(define (dec-mem-value k) (modulo (dec k) (calc-mem-cell-size)))

;; Increment/decrement current memory pointer
(define (inc-mem-pointer k) (modulo (inc k) (calc-mem-size)))
(define (dec-mem-pointer k) (modulo (dec k) (calc-mem-size)))

;; Stack operations
(define (stack-push stack value) (cons value stack))
(define (stack-peek stack) (if (empty? stack) '() (car stack)))
(define (stack-pop stack) (if (empty? stack) '() (cdr stack)))

;; Creates jump table for input
;; Jump table is just a dict that stores
;; (<from input position> . <to input position>) pairs
(define (jump-table-create input)
  (define (iter i s stack res)
    (if (empty? s) res
        (let ([car-s (car s)]
              [cdr-s (cdr s)]
              [inc-i (inc i)])
          (cond
            [(eq? car-s #\[)
             (iter inc-i cdr-s (stack-push stack i) res)]
            [(eq? car-s #\]) 
             (let ([_stack-peek (stack-peek stack)])
               (iter inc-i cdr-s (stack-pop stack) (dict-set (dict-set res _stack-peek i) i _stack-peek)))]
            [else
             (iter inc-i cdr-s stack res)]))))
  (iter 0 (string->list input) '() '()))

;; Memory operations
(define (mem-create mem-state cur-pointer) (list cur-pointer mem-state))
(define (mem-create-empty) (mem-create '() 0))
(define (mem-pointer mem) (car mem))
(define (mem-state mem) (cadr mem))
(define (mem-get mem) (dict-ref (mem-state mem) (mem-pointer mem) 0))

(define (mem-value-op mem op)
  (let* ([_mem-state (mem-state mem)]
         [_mem-pointer (mem-pointer mem)]
         [cur (dict-ref _mem-state _mem-pointer 0)])
    (mem-create (dict-set _mem-state _mem-pointer (op cur)) _mem-pointer)))
(define (mem-value-inc mem) (mem-value-op mem inc-mem-value))
(define (mem-value-dec mem) (mem-value-op mem dec-mem-value))

(define (mem-pointer-op mem op) (mem-create (mem-state mem) (op (mem-pointer mem))))
(define (mem-pointer-inc mem) (mem-pointer-op mem inc-mem-pointer))
(define (mem-pointer-dec mem) (mem-pointer-op mem dec-mem-pointer))

(define (mem-output mem) (integer->char (mem-get mem)))
(define (mem-input mem input)
  (let ([cur-pointer (mem-pointer mem)])
    (mem-create
     (dict-set (mem-state mem) cur-pointer (char->integer input))
     cur-pointer)))

;; Brainfuck interpreter
(define (bf-process-str str)
  (define (list-tail s k) (string->list (substring s k (string-length s))))
  (define (bf-tick mem j-table i input)
    (cond
      [(eq? input #\>) (list '() (mem-pointer-inc mem))]
      [(eq? input #\<) (list '() (mem-pointer-dec mem))]
      [(eq? input #\+) (list '() (mem-value-inc mem))]
      [(eq? input #\-) (list '() (mem-value-dec mem))]
      [(eq? input #\.) (list (mem-output mem) mem)]
      [(eq? input #\,) (list '() (mem-input mem))]
      [else (list '() mem)]))
  (let ([j-table (jump-table-create str)])
    (define (iter i s mem res)
      (if (empty? s) res
          (let ([car-s (car s)]
                [cdr-s (cdr s)]
                [inc-i (inc i)])
            (define (process-lb-or-rb cond)
              (let ([new-i (inc (dict-ref j-table i 0))])
                (if cond
                    (iter new-i (list-tail str new-i) mem res)
                    (iter inc-i cdr-s mem res))))
            (cond
              [(eq? car-s #\[) (process-lb-or-rb (eq? (mem-get mem) 0))]
              [(eq? car-s #\]) (process-lb-or-rb (not (eq? (mem-get mem) 0)))]
              [else
               (let* ([tick-res (bf-tick mem j-table i (car s))]
                      [new-mem (cadr tick-res)]
                      [output (car tick-res)])
                 (iter inc-i cdr-s new-mem (if (char? output) (append res (list output)) res)))]))))
    (list->string (iter 0 (string->list str) (mem-create-empty) '()))))

;; Entrypoint
(with-input-from-file (vector-ref (current-command-line-arguments) 0)
  (lambda () (bf-process-str (read-line))))
