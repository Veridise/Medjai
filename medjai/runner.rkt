#lang rosette
; this module stores all runner related components, including:
;   |- CairoRunner
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in memory: "./memory.rkt")
    (prefix-in config: "./config.rkt")
    (prefix-in program: "./program.rkt")
    (prefix-in context: "./context.rkt")
    (prefix-in vm: "./vm.rkt")
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

; (fixme) extremely simplified
(define (initialize-main-entrypoint p)
    (tokamak:typed p runner?)
    (set-runner-epm! p null)
    (let ([prog (runner-prog p)][mem (runner-mem p)])
        ;; TODO read args
        (define stack (list (memory:add-segment mem)))
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
    (tokamak:log "program data is: ~a" (program:program-data (runner-prog p)))
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

(define (initialize-vm p hint-locals #:static-locals [static-locals null])
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
        #:slocals sl #:brunners (runner-brunners p) #:pbase (runner-pbase p)
    ))
    (set-runner-vm! p vm)
    ; (fixme) skipped a few
)

; (fixme) skipped a few
(define (run-until-pc p addr #:run-resources [run-resources null] #:execute-hints [exec-hints #t])
    (tokamak:typed p runner?)
    (tokamak:typed addr memory:rv? integer?)
    ; (fixme) run-resources type unsupported
    ; a for loop definition
    (define (do-step)
      ; each time fetch the current vm from runner
      (let ([vm0 (runner-vm p)])
        (when (! (memory:rveq (get-pc p) addr))
            (for/all ([pc (memory:rv-off (context:context-pc (vm:vm-cntx (runner-vm p)))) #:exhaustive])
              (begin
                (context:set-context-pc! (vm:vm-cntx (runner-vm p)) (memory:rv 0 pc))
                (tokamak:log "pc is: ~a." (context:context-pc (vm:vm-cntx (runner-vm p))))
                (tokamak:log "ap is: ~a." (context:context-ap (vm:vm-cntx (runner-vm p))))
                (tokamak:log "fp is: ~a." (context:context-fp (vm:vm-cntx (runner-vm p))))
                (vm-step p #:execute-hints exec-hints)
                (do-step))))))

    ; start the loop
    (do-step)
)

(define (vm-step p #:execute-hints exec-hints)
    (tokamak:typed p runner?)
    (when (memory:rveq (get-pc p) (runner-finalpc p))
        (tokamak:error "execution reached the end of the program."))
    (vm:step (runner-vm p) #:execute-hints exec-hints)
)