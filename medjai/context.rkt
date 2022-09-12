#lang rosette
; this module stores all context related components, including:
;   |- RunContext
(require (prefix-in tokamak: lib-symexec/tokamak)
         (prefix-in memory: lib-symexec/memory)
         (prefix-in instruction: "./instruction.rkt"))
(provide (all-defined-out))

; ============================ ;
; ======== RunContext ======== ;
; ============================ ;
; (RunContext)
; (memory) memory
(struct context
        (mem pc
             ap
             fp ; rv / int
             prime) ; int
  #:mutable
  #:transparent
  #:reflection-name 'context)

; raw constructor
(define (new-context #:mem mem #:pc pc #:ap ap #:fp fp #:prime prime)
  (context mem pc ap fp prime))
; constructor
(define (make-context #:mem mem #:pc pc #:ap ap #:fp fp #:prime prime)
  (new-context #:mem mem #:pc pc #:ap ap #:fp fp #:prime prime))

(define (get-instruction-encoding p)
  (let ([mem (context-mem p)] [pc (context-pc p)] [prime (context-prime p)])
    (define instruction-encoding (memory:memory-ref mem pc))
    (assert (integer? instruction-encoding)
            (format "instruction should be an int, got: ~a." instruction-encoding))
    (define imm-addr (memory:rvmod (memory:rvadd pc 1) prime))
    (define optional-imm
      (let ([imm0 (memory:memory-ref mem imm-addr)]) (if (integer? imm0) imm0 null)))
    (values instruction-encoding optional-imm)))

(define (compute-dst-addr p instruction)
  (define base-addr
    (let ([sym (instruction:instruction-dst instruction)])
      (cond
        [(equal? 'ap sym) (context-ap p)]
        [(equal? 'fp sym) (context-fp p)]
        [else (tokamak:error "invalid dst-register value.")])))
  (memory:rvmod (memory:rvadd base-addr (instruction:instruction-off0 instruction))
                (context-prime p)))

(define (compute-op0-addr p instruction)
  (define base-addr
    (let ([sym (instruction:instruction-op0 instruction)])
      (cond
        [(equal? 'ap sym) (context-ap p)]
        [(equal? 'fp sym) (context-fp p)]
        [else (tokamak:error "invalid op0-register value.")])))
  (memory:rvmod (memory:rvadd base-addr (instruction:instruction-off1 instruction))
                (context-prime p)))

(define (compute-op1-addr p instruction op0)
  (define base-addr
    (let ([sym (instruction:instruction-op1 instruction)])
      (tokamak:log "compute-op1-addr sym: ~a" sym)
      (cond
        [(equal? 'ap sym) (context-ap p)]
        [(equal? 'fp sym) (context-fp p)]
        [(equal? 'imm sym)
         (assert (equal? 1 (instruction:instruction-off2 instruction))
                 "in immediate mode, off2 should be 1.")
         (context-pc p)]
        [(equal? 'op0 sym)
         (assert op0 "op0 must be known in double dereference.")
         op0]
        [else (tokamak:error "invalid op1-register value.")])))
  (tokamak:log "compute-op1-addr base-addr: ~a" base-addr)
  (tokamak:log "compute-op1-addr off2: ~a" (instruction:instruction-off2 instruction))
  (memory:rvmod (memory:rvadd base-addr (instruction:instruction-off2 instruction))
                (context-prime p)))
