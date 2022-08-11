#lang rosette
; this module stores all runner related components, including:
;   |- CairoRunner
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in memory: "./memory.rkt")
    (prefix-in config: "./config.rkt")
    (prefix-in program: "./program.rkt")
    (prefix-in context: "./context.rkt")
    (prefix-in instruction: "./instruction.rkt")
    (prefix-in vm: "./vm.rkt")
    (prefix-in vm: "./vm-core.rkt")
    (only-in rosette/base/core/polymorphic ite)
)
(provide (all-defined-out))

; ============================= ;
; ======== CairoRunner ======== ;
; ============================= ;
; (CairoRunner)
(struct runner (
    prog ; (program) program
    layout ; str
    brunners ; (fixme) (builtin_runners) dict[str,BuiltinRunner]
    osteps ; (original_steps) ??
    pfmode ; (proof_mode) bool
    ambs ; (allow_missing_builtins) bool
    mem ; (memory+segments) memory
    soffs ; (segment_offsets) dict[int,int]
    finalpc ; (final_pc) rv / int
    runend ; (_run_ended) bool
    segfin ; (_segments_finalized) bool
    acaddrs ; (fixme) (accessed_addresses) set[rv]

    ; implicitly defined, all initialized to null
    pbase ; (program_base) rv / int
    ebase ; (execution_base) rv / int
    initpc ; (initial_pc) rv / int
    initap ; (initial_ap) rv / int
    initfp ; (initial_fp) rv / int
    vm ; vm
    epm ; (execution_public_memory) list[int]
) #:mutable #:transparent #:reflection-name 'runner)

; raw constructor
(define (new-runner
    #:prog prog #:layout layout #:brunners brunners #:osteps osteps #:pfmode pfmode
    #:ambs ambs #:mem mem #:soffs soffs #:finalpc finalpc #:runend runend
    #:segfin segfin #:acaddrs acaddrs
    #:pbase pbase #:ebase ebase #:initpc initpc #:initap initap #:initfp initfp #:vm vm
    #:epm epm
    )
    ; return
    (runner 
        prog layout brunners osteps pfmode
        ambs mem soffs finalpc runend
        segfin acaddrs
        pbase ebase initpc initap initfp vm
        epm
    )
)

; constructor
(define (make-runner
    #:prog prog #:layout [layout "plain"] #:mem [mem null]
    #:pfmode [pfmode null] #:ambs [ambs null]
    )
    (tokamak:typed prog program:program?)
    (tokamak:typed layout string?)
    (tokamak:typed mem memory:memory? null?)
    (tokamak:typed pfmode boolean? null?)
    (tokamak:typed ambs boolean? null?)
    (define prog0 prog)
    (define layout0 layout)
    (define brunners0 (make-hash))
    (define osteps0 null)
    (define pfmode0 (if (null? pfmode) #f pfmode))
    (define ambs0 (if (null? ambs) #f ambs))
    ; (fixme) here we skip a bit
    (define mem0 (if (null? mem)
        (memory:make-memory #:prime (program:program-prime prog))
        mem
    ))
    (define soffs0 null)
    (define finalpc0 null)
    (define runend0 #f)
    (define segfin0 #f)
    (define acaddrs0 null)
    ; return
    (new-runner
        #:prog prog0 #:layout layout0 #:brunners brunners0 #:osteps osteps0 #:pfmode pfmode0
        #:ambs ambs0 #:mem mem0 #:soffs soffs0 #:finalpc finalpc0 #:runend runend0
        #:segfin segfin0 #:acaddrs acaddrs0
        #:pbase null #:ebase null #:initpc null #:initap null #:initfp null #:vm null
        #:epm null
    )
)

; shortcut for getting self.vm.run_context.pc
(define (get-pc p)
    (tokamak:typed p runner?)
    ; return
    (context:context-pc (vm:vm-cntx (runner-vm p)))
)

(define (topc p lop)
    (tokamak:typed p runner?)
    (tokamak:typed lop string? integer?)
    ; return
    (if (string? lop)
        ; (fixme)
        ; (program:get-label (runner-prog p) lop)
        0
        lop
    )
)

(define (initialize-segments p #:program-base [program-base null])
    (tokamak:typed p runner?)
    (tokamak:typed program-base memory:rv? null?)
    (let ([mem (runner-mem p)])
        ; program segment
        (set-runner-pbase! p
            (if (null? program-base)
                (memory:add-segment mem)
                program-base
            )
        )
        ; execution segment
        (set-runner-ebase! p (memory:add-segment mem))
        ; builtin segment
        ; (fixme) add it later
    )
)

(define (setup-stack p)
    (tokamak:typed p runner?)
    (set-runner-epm! p null)
    (let ([prog (runner-prog p)][mem (runner-mem p)])
        (define arg-size (program:get-total-num-args prog (program:program-mainid prog)))
        (memory:set-rngcheckseg (program:get-range-check-ptr-seg prog (~a (program:program-mainid prog))))
        ;; TODO set bitwise op ptr the same way
        (tokamak:log "rngcheckseg: ~a" (memory:get-rngcheckseg))
        (for/list ([i arg-size]) (memory:add-segment mem))
    )
)



; (fixme) extremely simplified
(define (initialize-main-entrypoint p #:track-revert [track-revert #f])
    (tokamak:typed p runner?)
    (set-runner-epm! p null)
    (let ([prog (runner-prog p)][mem (runner-mem p)])
        ;; TODO read args, how do we handle the pointers?
        ;(setup-stack p)
        ;(define stack (list (memory:add-segment mem) (memory:add-segment mem) (memory:add-segment mem)))
        (define stack (setup-stack p))
        (define return-fp (memory:add-segment mem))
        (define main (program:program-main prog))
        ; return
        (initialize-function-entrypoint p main stack #:return-fp return-fp)
    )
)

(define (initialize-function-entrypoint p entrypoint args #:return-fp [return-fp (memory:rv 0 0)])
    (tokamak:typed p runner?)
    (tokamak:typed entrypoint string? integer?)
    (tokamak:typed args list?)
    (tokamak:typed return-fp memory:rv? integer?)
    (let ([mem (runner-mem p)])
        (define end (memory:add-segment mem))
        (define stack (append args (list return-fp end)))
        (initialize-state p entrypoint stack)
        (set-runner-initfp! p (memory:rvadd (runner-ebase p) (length stack)))
        (set-runner-initap! p (memory:rvadd (runner-ebase p) (length stack)))
        (set-runner-finalpc! p end)
        ; return
        end
    )
)

(define (initialize-state p entrypoint stack)
    (tokamak:typed p runner?)
    (tokamak:typed entrypoint string? integer?)
    (tokamak:typed stack list?)
    (set-runner-initpc! p (memory:rvadd (runner-pbase p) (topc p entrypoint)))
    ;(tokamak:log "program data is: ~a" (program:program-data (runner-prog p)))
    (tokamak:log "stack is: ~a" stack)
    (load-data p (runner-pbase p) (program:program-data (runner-prog p))) ; load program
    (load-data p (runner-ebase p) stack) ; load stack
)

(define (load-data p ptr data)
    (tokamak:typed p runner?)
    (tokamak:typed ptr memory:rv? integer?)
    (tokamak:typed data list?)
    (memory:load-data (runner-mem p) ptr data)
)

(define (initialize-vm p hint-locals #:static-locals [static-locals null] #:track-revert [track-revert #f])
    (tokamak:typed p runner?)
    (tokamak:typed hint-locals hash?)
    (tokamak:typed static-locals hash? null?)
    (define cntx (context:make-context
        #:mem (runner-mem p)
        #:pc (runner-initpc p)
        #:ap (runner-initap p)
        #:fp (runner-initfp p)
        #:prime (program:program-prime (runner-prog p))
    ))
    (define sl (if (null? static-locals) (make-hash) static-locals))
    (define vm (vm:make-vm
        #:prog (runner-prog p) #:cntx cntx #:hlocals hint-locals
        #:slocals sl #:track-revert track-revert #:brunners (runner-brunners p) #:pbase (runner-pbase p)
    ))
    (set-runner-vm! p vm)
    ; (fixme) skipped a few
)

(define (at-call-instr? p)
  (let* ([next-instr (vm:decode-current-instruction (runner-vm p))]
         [opcode (instruction:instruction-opcode next-instr)])
    (equal? 'call opcode)))

(define (get-aps ap l)
    (match* (ap)
    [((expression (== ite) a b c)) (begin (tokamak:log "ite b: ~a, c: ~a d: ~a," b c (append (get-aps b l) (get-aps c l))) (append (get-aps b l) (get-aps c l)))]
    [(_) (begin (tokamak:log "concrete ap: ~a" ap) (cons ap l))]
    )
)

(define (get-new-ap mem ap size)
    (memory:rv 1 (car (sort (get-aps (memory:rv-off ap) empty) >=)))
)

(define (sanitize-return-val v)
    ; The idea with sanitize-return-val is we may want to sanitize the return values produced after the merge 
    ; Maybe apply rewrites or whatever to output. Right now we just return the value ex
    (cond
        ; TODO: FIXME. Right now we concretize the builtins. This should be correct because we essentially model all the 
        ; builtin pointers. However, if we no longer model them then this code should be refactored.
        [(and (memory:rv? v) (>= (memory:rv-seg v) 2) (<= (memory:rv-seg v) 5) (symbolic? (memory:rv-off v)))
            (memory:rv (memory:rv-seg v) (car (sort (get-aps (memory:rv-off v) empty) >=)))]
        [else v]
    )
)

(define (set-return-vals mem ap return-vals)
    (tokamak:log "SETTING RETURN VALS: ~a" (length return-vals))
    (for ([i (length return-vals)])
        (tokamak:log "SETTING RETURN VAL ~a TO ~a " (memory:rvsub ap (+ i 1)) (sanitize-return-val (list-ref return-vals i)))
        (memory:memory-set! mem (memory:rvsub ap (+ i 1)) (sanitize-return-val (list-ref return-vals i)))
    )
)

; (fixme) skipped a few
(define (run-until-pc p addr #:run-resources [run-resources null] #:execute-hints [exec-hints #t] #:track-revert [track-revert #t])
    (tokamak:typed p runner?)
    (tokamak:typed addr memory:rv? integer?)
    ; (fixme) run-resources type unsupported
    ; a for loop definition
    (define (do-step)
      ; each time fetch the current vm from runner
      (let ([vm0 (runner-vm p)])
        (when (! (memory:rveq (get-pc p) addr))
            (for*/all ([pc (memory:rv-off (context:context-pc (vm:vm-cntx (runner-vm p)))) #:exhaustive])
                       ;[ap (memory:rv-off (context:context-ap (vm:vm-cntx (runner-vm p)))) #:exhaustive])
              (begin
                (context:set-context-pc! (vm:vm-cntx (runner-vm p)) (memory:rv 0 pc))
                ;(context:set-context-fp! (vm:vm-cntx (runner-vm p)) (memory:rv 1 fp))
                ;(context:set-context-ap! (vm:vm-cntx (runner-vm p)) (memory:rv 1 ap))
                (tokamak:log "pc is: ~a." (context:context-pc (vm:vm-cntx (runner-vm p))))
                (tokamak:log "ap is: ~a." (context:context-ap (vm:vm-cntx (runner-vm p))))
                (tokamak:log "fp is: ~a." (context:context-fp (vm:vm-cntx (runner-vm p))))
                (let ([my-fp (context:context-fp (vm:vm-cntx (runner-vm p)))])
                  (when (union? my-fp)
                    (for ([c (union-contents my-fp)])
                      (tokamak:log "union fp vc: ~a" (car c))
                      (tokamak:log "union fp val: ~a" (cdr c)))))
                ;(assert ap-rv)
                ;(assert fp-rv)
                (define is-call (at-call-instr? p))
                ;(define old-ap (context:context-ap (vm:vm-cntx (runner-vm p))))
                (let* ([old-ap (context:context-ap (vm:vm-cntx (runner-vm p)))]
                    )
                    (tokamak:log "old ap: ~a" old-ap)
                    (vm-step p #:execute-hints exec-hints #:track-revert track-revert)
                    (when is-call
                    (let* ([return-addr (memory:memory-ref (vm:vm-mem (runner-vm p))
                                                            (memory:rvadd old-ap 1))]
                           [scope (program:closest-scope (context:context-pc (vm:vm-cntx (runner-vm p))) (runner-prog p))]
                           [return-size (program:get-return-size (runner-prog p) scope)]
                          )
                        ;; NOTE: return-addr should never be symbolic
                        (tokamak:log "Return addr for call: ~a" return-addr)
                        (run-until-pc p return-addr
                                    #:run-resources run-resources
                                    #:execute-hints exec-hints)
                        (tokamak:log "RETURN VALS AFTER CALL: ~a" (vm:vm-return-vals (runner-vm p)))
                        (tokamak:log "SCOPE FOR CALL RET: ~a" scope)
                        (tokamak:log "ap is after call: ~a." (context:context-ap (vm:vm-cntx (runner-vm p))))
                        (tokamak:log "return size: ~a" return-size)
                        (let* ( [mem (vm:vm-mem (runner-vm p))]
                                [new-ap (get-new-ap mem (context:context-ap (vm:vm-cntx (runner-vm p))) return-size)])
                            (tokamak:log "NEW AP: ~a" new-ap)
                            (set-return-vals mem new-ap (vm:vm-return-vals (runner-vm p)))
                            (context:set-context-ap! (vm:vm-cntx (runner-vm p)) new-ap)
                        )
                        ;(context:set-context-ap! (vm:vm-cntx (runner-vm p)) (concretize-ap (vm:vm-mem (runner-vm p)) (context:context-ap (vm:vm-cntx (runner-vm p))) return-size))             
                        (tokamak:log "ap after is: ~a." (context:context-ap (vm:vm-cntx (runner-vm p)))))
                    ))
                (do-step))))))    ;) ; TODO
                ;; TODO

    ; start the loop
    (do-step)
    (cond
      [track-revert (assert (equal? memory:revert-var #t))])
    ;; Return final memory state
    (runner-mem p))

(define (vm-step p #:execute-hints exec-hints #:track-revert track-revert)
    (tokamak:typed p runner?)
    (when (memory:rveq (get-pc p) (runner-finalpc p))
        (tokamak:error "execution reached the end of the program."))
    (vm:step (runner-vm p) #:execute-hints exec-hints)
)