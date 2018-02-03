#lang racket

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (inc-in-group k n) (modulo (inc k) n))
(define (dec-in-group k n) (modulo (dec k) n))

(define mem-value-size 256)
(define mem-size 65536)

(define (inc-mem-value k) (inc-in-group k mem-value-size))
(define (dec-mem-value k) (dec-in-group k mem-value-size))
(define (inc-mem-pointer k) (inc-in-group k mem-size))
(define (dec-mem-pointer k) (dec-in-group k mem-size))

(define (stack-push s value) (append s (list value)))
(define (stack-pop s)
  (if (empty? s) (cons '() '())
      (list (last s) (reverse (cdr (reverse s))))))

(define (map-get map-state key)
  (let ([get (filter
               (lambda (pair) (eq? (car pair) key))
               map-state)])
    (if (empty? get) '() (cdr (first get)))))

(define (map-get-or-default map-state key default)
  (let ([get (map-get map-state key)])
    (if (empty? get) default get)))

(define (map-remove map-state key)
  (filter
   (lambda (pair) (not (eq? (car pair) key)))
   map-state))

(define (map-remove-if map-state condition)
  (filter
   (lambda (pair) (not (condition (car pair))))
   map-state))

(define (map-set map-state key value)
  (append (map-remove map-state key) (list (cons key value))))

(define (mem-create mem-state cur-pointer) (list cur-pointer mem-state))
(define (mem-create-empty) (mem-create '() 0))
(define (mem-pointer mem) (car mem))
(define (mem-state mem) (cadr mem))
(define (mem-get mem) (map-get-or-default (mem-state mem) (mem-pointer mem) 0))

(define (mem-value-op mem op)
  (let* ([_mem-state (mem-state mem)]
         [_mem-pointer (mem-pointer mem)]
         [cur (map-get-or-default _mem-state _mem-pointer 0)])
    (mem-create (map-set _mem-state _mem-pointer (op cur)) _mem-pointer)))
(define (mem-value-inc mem) (mem-value-op mem inc-mem-value))
(define (mem-value-dec mem) (mem-value-op mem dec-mem-value))

(define (mem-pointer-op mem op) (mem-create (mem-state mem) (op (mem-pointer mem))))
(define (mem-pointer-inc mem) (mem-pointer-op mem inc-mem-pointer))
(define (mem-pointer-dec mem) (mem-pointer-op mem dec-mem-pointer))

(define (mem-output mem) (integer->char (mem-get mem)))
(define (mem-input mem input)
  (let ([cur-pointer (mem-pointer mem)])
    (mem-create
     (map-set (mem-state mem) cur-pointer (char->integer input))
     cur-pointer)))

(define (memout> mem) (mem-pointer-inc mem))
(define (memout< mem) (mem-pointer-dec mem))
(define (memout+ mem) (mem-value-inc mem))
(define (memout- mem) (mem-value-dec mem))
(define (memout-p mem) (mem-output mem))
(define (memout-c mem) (mem-input mem))

(define (bf-tick mem j-table i input)
  (cond
    [(eq? input #\>) (list '() (memout> mem))]
    [(eq? input #\<) (list '() (memout< mem))]
    [(eq? input #\+) (list '() (memout+ mem))]
    [(eq? input #\-) (list '() (memout- mem))]
    [(eq? input #\.) (list (memout-p mem) mem)]
    [(eq? input #\,) (list '() (memout-c mem))]
    [else (list '() mem)]))

(define (jump-table-create input)
  (define (iter i s stack res)
    (if (empty? s) res
        (let* ([car-s (car s)]
               [ps (if (eq? car-s #\]) (stack-pop stack) '())]
               [popped (if (empty? ps) '() (car ps))]
               [popped-stack (if (empty? ps) '() (cadr ps))])
          (iter
           (inc i)
           (cdr s)
           (cond
             [(eq? car-s #\[) (stack-push stack i)]
             [(eq? car-s #\]) popped-stack]
             [else stack])
           (cond
             [(eq? car-s #\[) res]
             [(eq? car-s #\]) (map-set (map-set res i popped) popped i)]
             [else res])))))
  (iter 0 (string->list input) '() '()))

(define (string-tail s k)
  (substring s k (string-length s)))

(define (bf-process-str str)
  (let ([j-table (jump-table-create str)])
    (define (iter i s mem res)
      (if (empty? s) res
          (cond
            [(eq? (car s) #\[)
             (if (eq? (mem-get mem) 0)
                 (let ([new-i (inc (map-get j-table i))])
                   (iter new-i (string->list (string-tail str new-i)) mem res))
                 (iter (inc i) (cdr s) mem res))]
            [(eq? (car s) #\])
             (if (not (eq? (mem-get mem) 0))
                 (let ([new-i (inc (map-get j-table i))])
                   (iter new-i (string->list (string-tail str new-i)) mem res))
                 (iter (inc i) (cdr s) mem res))]
            [else
             (let* ([tick-res (bf-tick mem j-table i (car s))]
                    [new-mem (cadr tick-res)]
                    [output (car tick-res)])
            (iter (inc i) (cdr s) new-mem (if (char? output) (append res (list output)) res)))])))
    (list->string (iter 0 (string->list str) (mem-create-empty) '()))))

(bf-process-str "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
