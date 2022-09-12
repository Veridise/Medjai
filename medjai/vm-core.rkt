#lang rosette
; this module stores all vm related components, including:
;   |- VirtualMachine
(require (prefix-in tokamak: lib-symexec/tokamak)
         (prefix-in context: "./context.rkt")
         (prefix-in memory: lib-symexec/memory)
         (prefix-in program: "./program.rkt")
         (prefix-in encode: "./encode.rkt")
         (prefix-in instruction: "./instruction.rkt")
         (prefix-in hint: "./hint.rkt")
         lib-symexec/sym
         "./vm.rkt"
         math/number-theory)

(provide (all-defined-out))

; constructor
(define (make-vm #:prog prog
                 #:cntx cntx
                 #:hlocals hlocals
                 #:slocals [slocals null]
                 #:track-revert [track-revert #f]
                 #:brunners [brunners null]
                 #:pbase [pbase null])
  (define cntx0 cntx)
  (define pbase0 (if (null? pbase) (context:context-pc cntx) pbase))
  (define brunners0 (if (null? brunners) (make-hash) brunners))
  ; base init starts ==>
  (define prime0 (program:program-prime prog))
  ; brunners is fine
  (define excopes0 null)
  ; (fixme) call enter_scope

  (flush-output)
  (define hint-pcs
    (map (curry memory:rv 0)
         (sort (map (compose string->number ~a) (hash-keys (program:program-hints prog))) <)))
  (tokamak:log "hints pcs: ~a" hint-pcs)
  (define hints0 (make-hash))
  (define hintspcs (make-hash))
  (for ([hint-pc hint-pcs])
    (let* (;; TODO should we find the scope from the hints struct?
           [scope (program:closest-scope hint-pc prog)]
           [hint-func (hint:pop-hint-from-scope scope)])
      (hash-set! hintspcs hint-pc #t)
      (when hint-func
        (tokamak:log "Assigning hint ~a for ~a at pc ~a" hint-func hint-pc scope)
        (hash-set! hints0 hint-pc hint-func))))

  (define hpi0 (make-hash))
  (define idi0 (make-hash))
  (define dfc0 (make-hash))
  (define ema0 null)
  (define prog0 prog)
  (define mem0 (context:context-mem cntx))
  ; (fixme) tell apart StrippedProgram and Program
  (when (program:program? prog0)
    (load-program prime0 prog0 pbase0))
  (define autodd0 (lambda (seg) empty))
  (define slocals0
    (if (! (null? slocals))
        slocals
        (make-hash (list (cons 'PRIME prime0)
                         (cons 'fadd (lambda (a b p) (modulo (+ a b) p)))
                         ; (fixme) add the remainings
                         ))))
  ; <== base init ends
  (define acaddrs0 null)
  (define trace0 null)
  (define currstep0 0)
  (define skipiexec0 #f)
  (new-vm #:prime prime0
          #:brunners brunners0
          #:excopes excopes0
          #:hints hints0
          #:hintspcs hintspcs
          #:hpi hpi0
          #:idi idi0
          #:dfc dfc0
          #:ema ema0
          #:prog prog0
          #:mem mem0
          #:autodd autodd0
          #:slocals slocals0
          #:track-revert track-revert
          #:return-vals empty
          #:cntx cntx0
          #:acaddrs acaddrs0
          #:trace trace0
          #:currstep currstep0
          #:skipiexec skipiexec0))

; (VirtualMachineBase.load_program)
; yeah the vm also has a load_program method
(define (load-program self.prime program program-base)
  (assert (equal? self.prime (program:program-prime program))
          (format "unexpected prime for loaded program: ~a != ~a."
                  (program:program-prime program)
                  self.prime)))

(define (step p #:execute-hints [exec-hints? #t])
  (tokamak:log "vm step, exec-hints: ~a" exec-hints?)
  (tokamak:log "context-pc: ~a" (context:context-pc (vm-cntx p)))
  (tokamak:log "scope: ~a" (program:closest-scope (context:context-pc (vm-cntx p)) (vm-prog p)))
  (tokamak:log "return size: ~a"
               (program:get-return-size
                (vm-prog p)
                (program:closest-scope (context:context-pc (vm-cntx p)) (vm-prog p))))
  ; (fixme) hint execution is skipped
  (tokamak:log "noop?: ~a" (should-treat-as-nop p))
  (let* ([new-pc (context:context-pc (vm-cntx p))] [hint-func (hash-ref (vm-hints p) new-pc #f)])
    (when (and exec-hints? hint-func)
      (hint-func p)))
  (define instruction (decode-current-instruction p))
  (tokamak:log "instruction is: ~a" instruction)
  (run-instruction p instruction)
  (tokamak:log "context-pc after: ~a" (context:context-pc (vm-cntx p))))

(define (decode-current-instruction p)
  (let ([cntx (vm-cntx p)])
    (define-values (instruction-encoding imm) (context:get-instruction-encoding cntx))
    (define instruction (decode-instruction p instruction-encoding #:imm imm))
    instruction))

(define (decode-instruction p encoded-inst #:imm [imm null])
  (encode:decode-instruction encoded-inst #:imm imm))

(define (should-treat-as-nop p)
  (define pc (context:context-pc (vm-cntx p)))
  (define is-storage-pc? (program:is-storage-var-pc? (vm-prog p) pc))
  (cond
    [is-storage-pc? #t]
    [else #f]))

(define (opcode-assertions p instruction operands)
  (when (and (equal? (instruction:instruction-opcode instruction) 'assert-eq)
             (not (should-treat-as-nop p)))
    (let ([dst (instruction:operands-dst operands)] [res (instruction:operands-res operands)])
      (tokamak:log "adding assertion: ~a = ~a" dst res)
      (cond
        [(vm-track-revert p)
         (tokamak:log "adding revert test before: ~a" memory:revert-var)
         (when (not (and res (equal? (memory:torv dst) (memory:torv res))))
           (memory:set-revert-var!))
         (tokamak:log "adding revert test after: ~a" memory:revert-var)]
        [else
         (assume res)
         (assume (equal? (memory:torv dst) (memory:torv res)))]))))

(define (handle-verification p instruction operands)
  (tokamak:log "dst is: ~a" (instruction:operands-dst operands))
  (let ([opcode (instruction:instruction-opcode instruction)]
        [val (memory:rv-off (memory:rvmod (instruction:operands-dst operands)
                                          (context:context-prime (vm-cntx p))))]
        [opval (memory:fromrv (instruction:operands-op1 operands))])
    (cond
      [(equal? opcode 'verify-eq)
       (tokamak:log "verify equal: (= ~a ~a)" val (instruction:operands-op1 operands))
       (assert (equal? opval val))]
      [(equal? opcode 'verify-neq)
       (tokamak:log "verify neq: (!= ~a ~a)" val (instruction:operands-op1 operands))
       (assert (not (equal? opval val)))]
      [(equal? opcode 'verify-geq)
       (tokamak:log "verify geq: (>= ~a ~a)" val (instruction:operands-op1 operands))
       (assert (>= val opval))]
      [(equal? opcode 'verify-lt)
       (tokamak:log "verify lt: (< ~a ~a)" val (instruction:operands-op1 operands))
       (assert (< val opval))])))

(define (run-instruction p instruction)
  ; (fixme) will call compute-operands
  (define-values (operands operands-mem-addresses) (compute-operands p instruction))

  (opcode-assertions p instruction operands)
  (handle-verification p instruction operands)

  ; (fixme) skipped a lot here
  ; update registers
  (handle-medjai-ver-calls p instruction operands)
  (handle-bitwise p instruction operands)
  (handle-storage p instruction operands)
  (handle-returns p instruction operands)
  (tokamak:log "done handling storage")
  (update-registers p instruction operands)
  (set-vm-currstep! p (+ 1 (vm-currstep p))))

(define (isMask v)
  (cond
    [(! (symbolic? v)) (equal? (bitwise-and (+ v 1) v) 0)]
    [else #f]))

(define (isPow2 v)
  (cond
    [(! (symbolic? v)) (equal? (bitwise-and (- v 1) v) 0)]
    [else #f]))

(define (perform-bitwise-and p fp ap)
  (define (handle-pow2-and op pow2 ap)
    ; assumes pow2 is a power of 2
    (cond
      [(equal? pow2 0) 0]
      [(equal? (modulo (quotient op pow2) 2) 1) pow2]
      [else 0]))
  (let* ([mem (vm-mem p)]
         [op1 (memory:fromrv (memory:memory-ref mem (memory:rvsub fp 3)))]
         [op2 (memory:fromrv (memory:memory-ref mem (memory:rvsub fp 4)))])
    (tokamak:log "OP1 and OP2: ~a ~a" op1 op2)
    (cond
      [(and (symbolic? op1) (symbolic? op2))
       (tokamak:log "ERROR PERFORMING BITWISE OP ON TWO SYMBOLICS")]
      [(isPow2 op1)
       (begin
         (tokamak:log "writing ~a to ~a" (memory:rvsub ap 1) (handle-pow2-and op2 op1 ap))
         (memory:memory-set! mem (memory:rvsub ap 1) (handle-pow2-and op2 op1 ap)))]
      [(isPow2 op2)
       (begin
         (tokamak:log "writing ~a to ~a" (memory:rvsub ap 1) (handle-pow2-and op1 op2 ap))
         (memory:memory-set! mem (memory:rvsub ap 1) (handle-pow2-and op1 op2 ap)))]
      [(isMask op1)
       (begin
         (tokamak:log "writing ~a to ~a" (memory:rvsub ap 1) (modulo op2 (+ 1 op1)))
         (memory:memory-set! mem (memory:rvsub ap 1) (modulo op2 (+ 1 op1))))]
      [(isMask op2)
       (begin
         (tokamak:log "writing ~a to ~a" (memory:rvsub ap 1) (modulo op1 (+ 1 op2)))
         (memory:memory-set! mem (memory:rvsub ap 1) (modulo op1 (+ 1 op2))))]
      [else
       (tokamak:log "WARNING WILL ROBINSON PERFORMING BITWISE AND WITH SOMETHING ELSE: ~a ~a"
                    op1
                    op2)])))

(define (handle-medjai-ver-calls p instruction operands)
  (let ([pc (context:context-pc (vm-cntx p))]
        [pc-update (instruction:instruction-pc instruction)]
        [fp (context:context-fp (vm-cntx p))]
        [opcode (instruction:instruction-opcode instruction)]
        [ap (context:context-ap (vm-cntx p))])
    (cond
      [(and (equal? 'jump pc-update) (equal? (instruction:instruction-opcode instruction) 'ret))
       (tokamak:log "Medjai CALL: ~a" instruction)
       (when (null? (instruction:operands-res operands))
         (tokamak:error "Res.UNCONSTRAINED cannot be used with PcUpdate.JUMP"))
       (define scope (program:closest-scope pc (vm-prog p)))
       (tokamak:log "MEDJAI CALL: ~a" scope)
       (cond
         [(program:is-medjai-assume-or-assert? scope)
          (let ([op1 (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 4)))]
                [op2 (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3)))])
            ; Assumptions
            (when (program:is-assume-le? scope)
              (assume (<= op1 op2)))
            (when (program:is-assume-ge? scope)
              (assume (>= op1 op2)))
            (when (program:is-assume-lt? scope)
              (assume (< op1 op2)))
            (when (program:is-assume-gt? scope)
              (assume (> op1 op2)))
            (when (program:is-assume-eq? scope)
              (assume (equal? op1 op2)))
            (when (program:is-assume-neq? scope)
              (assume (! (equal? op1 op2))))
            ; Assertions
            (when (program:is-assert-le? scope)
              (assert (<= op1 op2)))
            (when (program:is-assert-ge? scope)
              (assert (>= op1 op2)))
            (when (program:is-assert-lt? scope)
              (assert (< op1 op2)))
            (when (program:is-assert-gt? scope)
              (assert (> op1 op2)))
            (when (program:is-assert-eq? scope)
              (assert (equal? op1 op2)))
            (when (program:is-assert-neq? scope)
              (assert (! (equal? op1 op2)))))]
         [(program:is-make-symbolic-felt? scope)
          (memory:memory-set! (vm-mem p) (memory:rvsub ap 1) (symint (vm-prime p)))])])))

(define (handle-bitwise p instruction operands)
  (let ([pc (context:context-pc (vm-cntx p))]
        [pc-update (instruction:instruction-pc instruction)]
        [fp (context:context-fp (vm-cntx p))]
        [opcode (instruction:instruction-opcode instruction)]
        [ap (context:context-ap (vm-cntx p))])
    (cond
      [(and (equal? 'jump pc-update) (equal? (instruction:instruction-opcode instruction) 'ret))
       (tokamak:log "BITWISE leinstruction: ~a" instruction)
       (when (null? (instruction:operands-res operands))
         (tokamak:error "Res.UNCONSTRAINED cannot be used with PcUpdate.JUMP"))
       (define scope (program:closest-scope pc (vm-prog p)))
       (tokamak:log "BITWISE instruction: ~a" scope)
       (when (program:is-bitwise-and? scope)
         (perform-bitwise-and p fp ap))
       (when (program:is-bitwise-or? scope)
         (tokamak:error "Cannot handle bitwise or"))
       (when (program:is-bitwise-xor? scope)
         (tokamak:error "Cannot handle bitwise xor"))
       (when (program:is-bitwise-not? scope)
         (tokamak:error "Cannot handle bitwise not"))])))

(define (get-builtin-refs mem ap size)
  (cond
    [(> size 1)
     (let ([ref (memory:memory-ref mem (memory:rvsub ap size))])
       (cond
         [(and (memory:rv? ref)
               (! (symbolic? (memory:rv-seg ref)))
               (>= (memory:rv-seg ref) 2)
               (<= (memory:rv-seg ref) 5))
          (cons ref (get-builtin-refs mem ap (+ 1 size)))]
         [else empty]))]
    [else empty]))

(define (get-return-vals mem ap size)
  ; Get the explicit return values
  (append (for/list ([i size])
            (memory:memory-ref mem (memory:rvsub ap (+ 1 i))))
          ; Look for updated builtin references and add them
          (get-builtin-refs mem ap (+ 1 size))))

(define (handle-returns p instruction operands)
  (let ([pc (context:context-pc (vm-cntx p))]
        [pc-update (instruction:instruction-pc instruction)]
        [fp (context:context-fp (vm-cntx p))]
        [opcode (instruction:instruction-opcode instruction)]
        [ap (context:context-ap (vm-cntx p))]
        [prog (vm-prog p)])
    (cond
      [(and (equal? 'jump pc-update) (equal? (instruction:instruction-opcode instruction) 'ret))
       (let* ([scope (program:closest-scope pc prog)]
              [return-size (program:get-return-size prog scope)])
         (tokamak:log "RETURN SIZE: ~a" return-size)
         (tokamak:log "GETTING RETURN VALS: ~a" (get-return-vals (vm-mem p) ap return-size))
         (set-vm-return-vals! p (get-return-vals (vm-mem p) ap return-size)))])))

(define (handle-storage p instruction operands)
  (let ([pc (context:context-pc (vm-cntx p))]
        [pc-update (instruction:instruction-pc instruction)]
        [fp (context:context-fp (vm-cntx p))]
        [opcode (instruction:instruction-opcode instruction)]
        [ap (context:context-ap (vm-cntx p))])
    (cond
      [(and (equal? 'jump pc-update) (equal? (instruction:instruction-opcode instruction) 'ret))
       (tokamak:log "instruction: ~a" instruction)
       (when (null? (instruction:operands-res operands))
         (tokamak:error "Res.UNCONSTRAINED cannot be used with PcUpdate.JUMP"))
       (define scope (program:closest-scope pc (vm-prog p)))
       (when (program:is-storage-write? scope)
         (define kv-tuple (program:get-key-val-size (vm-prog p) scope))
         (tokamak:log "scope: ~a" scope)
         (tokamak:log "storage write kv args: ~a" kv-tuple)
         (perform-write (get-storage-id scope) p (car kv-tuple) (cdr kv-tuple) fp))
       (when (program:is-storage-read? scope)
         (define kv-tuple (program:get-key-val-size (vm-prog p) scope))
         (tokamak:log "read, write size: ~a ~a" (car kv-tuple) (cdr kv-tuple))
         (perform-read (get-storage-id scope) p (car kv-tuple) (cdr kv-tuple) fp ap))])))

(define (get-next-fp p instruction operands)
  (let ([fp (context:context-fp (vm-cntx p))]
        [fp-update (instruction:instruction-fp instruction)]
        [ap (context:context-ap (vm-cntx p))])
    (cond
      [(equal? 'ap+2 fp-update) (memory:rvadd ap 2)]
      [(equal? 'dst fp-update) (instruction:operands-dst operands)]
      [(equal? 'regular fp-update) fp]
      [else (tokamak:error "invalid fp_update value")])))

(define (update-registers p instruction operands)
  ; update fp
  (context:set-context-fp! (vm-cntx p) (get-next-fp p instruction operands))

  ; update ap
  (let ([ap (context:context-ap (vm-cntx p))] [ap-update (instruction:instruction-ap instruction)])
    (cond
      [(equal? 'add ap-update)
       (when (null? (instruction:operands-res operands))
         (tokamak:error "Res.UNCONSTRAINED cannot be used with ApUpdate.ADD"))
       (tokamak:log "first rvmod")
       (context:set-context-ap!
        (vm-cntx p)
        (memory:rvmod (memory:rvadd ap (instruction:operands-res operands)) (vm-prime p)))]
      [(equal? 'add1 ap-update) (context:set-context-ap! (vm-cntx p) (memory:rvadd ap 1))]
      [(equal? 'add2 ap-update) (context:set-context-ap! (vm-cntx p) (memory:rvadd ap 2))]
      [(! (equal? 'regular ap-update)) (tokamak:error "invalid ap_update value")]))
  (tokamak:log "second rvmod")
  (context:set-context-ap! (vm-cntx p) (memory:rvmod (context:context-ap (vm-cntx p)) (vm-prime p)))

  ; update pc
  (let ([pc (context:context-pc (vm-cntx p))] [pc-update (instruction:instruction-pc instruction)])
    (cond
      [(equal? 'regular pc-update)
       (context:set-context-pc! (vm-cntx p)
                                (memory:rvadd pc (instruction:instruction-size instruction)))]
      [(equal? 'jump pc-update)
       (when (null? (instruction:operands-res operands))
         (tokamak:error "Res.UNCONSTRAINED cannot be used with PcUpdate.JUMP"))
       (context:set-context-pc! (vm-cntx p) (instruction:operands-res operands))]
      [(equal? 'jump-rel pc-update)
       (when (null? (instruction:operands-res operands))
         (tokamak:error "Res.UNCONSTRAINED cannot be used with PcUpdate.JUMP_REL"))
       (context:set-context-pc!
        (vm-cntx p)
        (memory:rvadd pc (memory:fromrv (instruction:operands-res operands))))]
      [(equal? 'jnz pc-update)
       (if (is-zero p (instruction:operands-dst operands))
           (context:set-context-pc! (vm-cntx p)
                                    (memory:rvadd pc (instruction:instruction-size instruction)))
           (context:set-context-pc! (vm-cntx p)
                                    (memory:rvadd pc (instruction:operands-op1 operands))))]
      [else (tokamak:error "invalid pc_update value")]))
  (cond
    [(union? (context:context-pc (vm-cntx p)))
     (tokamak:log "union contents: ~a" (union-contents (context:context-pc (vm-cntx p))))])
  (tokamak:log "third rvmod, context-pc: ~a" (context:context-pc (vm-cntx p)))
  (context:set-context-pc! (vm-cntx p)
                           (memory:rvmod (context:context-pc (vm-cntx p)) (vm-prime p))))

(define (is-zero p value)
  (tokamak:log "is-zero value: ~a" value)
  ; the method itself has implicit assertions of value
  (cond
    [(integer? value) (equal? value 0)]
    ;; TODO/NOTE: the following case is not in the actual cairo-lang interpreter
    ;; but it's necessary here because we don't distinguish between rv and int
    ;; as the result of computation.
    ;; If a Cairo program actually did a jnz on a value of (rv 0 0) instead of 0,
    ;; our interpreter would behave differently
    [(and (memory:rv? value) (equal? (memory:rv-seg value) 0)) (equal? (memory:rv-off value) 0)]
    [(and (memory:rv? value) (memory:rvge (memory:rv-off value) 0)) #f]
    [else (tokamak:error "PureValueError: jmp != 0")]))

(define (compute-res p instruction op0 op1)
  (let ([res (instruction:instruction-res instruction)])
    (cond
      [(equal? 'op1 res) op1]
      [(equal? 'add res) (memory:rvmod (memory:rvadd op0 op1) (vm-prime p))]
      [(equal? 'mul res)
       (let ([op0 (if (memory:rv? op0) (memory:rv-off op0) op0)]
             [op1 (if (memory:rv? op1) (memory:rv-off op1) op1)])
         (modulo (* op0 op1) (vm-prime p)))]
      [(equal? 'symbolic res)
       (let ([sym (symint (vm-prime p))])
         ;; TODO better to just return (modulo sym (vm-prime p)) and let solver pick any int?
         (tokamak:log "Creating fresh symbolic constant: ~a" sym)
         ;(assume (>= sym 0))
         ;(assume (< sym (vm-prime p)))
         sym)]
      [(equal? 'unconstrained res) #f]
      [else (tokamak:error "invalid res value")])))

(define (get-vals fp p num-vals)
  (tokamak:log "vals before reverse: ~a" (get-vals-rev fp p num-vals))
  (reverse (get-vals-rev fp p num-vals)))

; Gets list (fp, fp+1, fp + 2, .., fp + num-vals) or () if num-vals is 0
(define (get-vals-rev fp p num-vals)
  (cond
    [(equal? num-vals 0) empty]
    [else
     (let* ([mem (vm-mem p)] [val (memory:memory-ref mem fp)])
       (cons val (get-vals-rev (memory:rvsub fp 1) p (- num-vals 1))))]))

(define (perform-write storage-id p num-keys num-vals fp)
  (let* ([mem (vm-mem p)]
         [vals (get-vals (memory:rvsub fp 3) p num-vals)]
         [keys (get-vals (memory:rvsub fp (+ 3 num-vals)) p num-keys)])
    (memory:write-storage-var mem storage-id keys vals)))

(define (get-return-addrs ap num-vals)
  (tokamak:log "calling get return addrs")
  (cond
    [(equal? num-vals 0) empty]
    [else (cons (memory:rvsub ap num-vals) (get-return-addrs ap (- num-vals 1)))]))

(define (get-storage-id scope)
  (cond
    [(string-contains? scope ".write") (string-replace scope ".write" "")]
    [(string-contains? scope ".read") (string-replace scope ".read" "")]
    [else scope]))

; This implementation assumes that Medjai has finished interpreting a  stub (usually) implementation
; of _read and is about to jump back to the caller
(define (perform-read storage-id p num-keys num-vals fp ap)
  (let* ([mem (vm-mem p)]
         ;[key-addr (memory:rvsub fp 3)] ;; TODO
         ;[keys (memory:memory-ref mem key-addr)]
         [keys (get-vals (memory:rvsub fp 3) p num-keys)]
         [vals (memory:read-storage-var mem storage-id keys num-vals)]
         [_debug (tokamak:log "vals after read: ~a" vals)]
         [return-addrs (get-return-addrs ap (length vals))])
    ;(context:set-context-ap! (vm-cntx p)
    ;                  next-ap)
    (tokamak:log "read ~a from ~a and writing to addrs ~a" vals storage-id return-addrs)
    (map (curry memory:memory-set! mem) return-addrs vals)))

(define (add-auto-deduction-rule p segment rule)
  (define old-autodd (vm-autodd p))
  (define (new-autodd seg)
    (if (equal? seg segment) (cons rule (old-autodd seg)) (old-autodd seg)))

  (set-vm-autodd! p new-autodd))

(define (deduce_memory_cell p addr)
  (tokamak:log "deducing memory cell at: ~a." addr)

  (define seg (memory:rv-seg addr))
  (define rules ((vm-autodd p) seg))

  ;; TODO return null instead of false?
  (ormap (lambda (rule)
           ;; rule should return #f if it does not match
           (rule vm addr))
         rules))

;; compute x / y % prime
(define (div-mod x y prime)
  (let ([yinv (modular-inverse y prime)]) (modulo (* x yinv) prime)))

(define (deduce_op0 p instruction dst op1)
  (tokamak:log "deduce_op0 opcode: ~a, res: ~a"
               (instruction:instruction-opcode instruction)
               (instruction:instruction-res instruction))
  (let ([opcode (instruction:instruction-opcode instruction)]
        [instr-res (instruction:instruction-res instruction)]
        [prime (context:context-prime (vm-cntx p))])
    (tokamak:log "deduce_op0 condition: ~a"
                 (and (equal? opcode 'assert-eq) (equal? instr-res 'add) dst op1))
    (cond
      [(equal? opcode 'call)
       (list (memory:rvadd (context:context-pc (vm-cntx p))
                           (instruction:instruction-size instruction))
             #f)]
      [(and (equal? opcode 'assert-eq) (equal? instr-res 'add) dst op1)
       (list (memory:rvmod (memory:rvsub dst op1) prime) dst)]
      [(and (equal? opcode 'assert-eq)
            (equal? instr-res 'mul)
            (integer? dst)
            (integer? op1)
            (not (equal? 0 op1)))
       (list (div-mod dst op1 prime) dst)]
      [else '(#f #f)])))

(define (deduce_op1 p instruction dst op0)
  (let ([opcode (instruction:instruction-opcode instruction)]
        [instr-res (instruction:instruction-res instruction)]
        [prime (context:context-prime (vm-cntx p))])
    (cond
      ; TODO
      [(and (equal? opcode 'assert-eq) (equal? instr-res 'op1) dst) (list dst dst)]
      [(and (equal? opcode 'assert-eq) (equal? instr-res 'add) dst op0)
       (list (memory:rvmod (memory:rvsub dst op0) prime) dst)]
      [(and (equal? opcode 'assert-eq)
            (equal? instr-res 'mul)
            (integer? dst)
            (integer? op0)
            (not (equal? op0 0)))
       (list (div-mod dst op0 prime) dst)]
      [else '(#f #f)])))

(define (compute-operands p instruction)
  (tokamak:log "Assertions: ~a." (vc-asserts (vc)))
  (tokamak:log "Assumptions: ~a." (vc-assumes (vc)))

  (define dst-addr (context:compute-dst-addr (vm-cntx p) instruction))
  (define dst (memory:memory-ref (vm-mem p) dst-addr))
  (tokamak:log "dst-addr is: ~a." dst-addr)
  (tokamak:log "dst is: ~a." dst)

  (define op0-addr (context:compute-op0-addr (vm-cntx p) instruction))
  (tokamak:log "op0-addr is: ~a." op0-addr)
  (define op0
    (let ([t0 (memory:memory-ref (vm-mem p) op0-addr)])
      (tokamak:log "original op0 is: ~a." t0)
      t0))

  (define op1-addr (context:compute-op1-addr (vm-cntx p) instruction op0))
  (tokamak:log "op1-addr is: ~a." op1-addr)
  (define op1
    (let ([t0 (memory:memory-ref (vm-mem p) op1-addr)])
      (tokamak:log "original op1 is: ~a." t0)
      t0))

  (define res #f)

  (define should-update-dst (not dst))
  (define should-update-op0 (not op0))
  (define should-update-op1 (not op1))

  ;; TODO only (assert #f) when the option to use hints is on
  ;; Set dst to symbolic when it's not infered
  (when (let ([opcode (instruction:instruction-opcode instruction)])
          (and (not dst)
               should-update-dst
               (not (equal? 'assert-eq opcode))
               (not (equal? 'call opcode))))
    (tokamak:log "Making symbolic for dst")
    (assert #f))
  ;(set! dst (symint (vm-prime p))))
  ;(set! dst 0)
  (set! dst (memory:torv dst))
  (when (not op0)
    (let* ([op0+res (deduce_op0 p instruction dst op1)]
           [new-op0 (first op0+res)]
           [new-res (second op0+res)])
      (set! op0 new-op0)
      (when (not res)
        (set! res new-res))))
  ;; Set op0 to symbolic when it's not infered
  (when (and (not op0) should-update-op0)
    (tokamak:log "Making symbolic for op0")
    (assert #f))
  ;(set! op0 (symint (vm-prime p))))
  (set! op0 (memory:torv op0))
  (tokamak:log "op0 is: ~a." op0)

  (when (not op1)
    (let* ([op1+res (deduce_op1 p instruction dst op0)]
           [new-op1 (first op1+res)]
           [new-res (second op1+res)])
      (set! op1 new-op1)
      (when (not res)
        (set! res new-res))))
  ;; Set op1 to symbolic when it's not infered
  (when (and (not op1) should-update-op1)
    (tokamak:log "Making symbolic for op1")
    ;(set! op1 (symint (vm-prime p)))
    (set! op1 0)
    ;(assert #f)
    )
  (set! op1 (memory:torv op1))
  (tokamak:log "op1 is: ~a." op1)

  ; (fixme) res may become not null when you call any deduce methods above
  (when (not res)
    (set! res (compute-res p instruction op0 op1)))
  (tokamak:log "res is: ~a." res)

  ; deduce dst
  (define dst0
    (if dst
        dst
        ; else, update
        (let ([opcode (instruction:instruction-opcode instruction)])
          (cond
            [(and (equal? 'assert-eq opcode) res) res]
            [(equal? 'call opcode) (context:context-fp (vm-cntx p))]
            [else dst]))))

  ; force pulling dst from memory for soundness
  (define dst1 (if dst0 dst0 (memory:memory-ref (vm-mem p) dst-addr)))

  (when should-update-dst
    (memory:memory-set! (vm-mem p) dst-addr dst1))
  (when should-update-op0
    (memory:memory-set! (vm-mem p) op0-addr op0))
  (when should-update-op1
    (memory:memory-set! (vm-mem p) op1-addr op1))

  (values (instruction:make-operands #:dst dst1 #:op0 op0 #:op1 op1 #:res res)
          (list dst-addr op0-addr op1-addr)))
