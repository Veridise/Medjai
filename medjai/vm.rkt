#lang rosette

(provide (all-defined-out))

; ================================ ;
; ======== VirtualMachine ======== ;
; ================================ ;
; (VirtualMachineBase) -> (VirtualMachine)
(struct vm (

    ; VirtualMachineBase
    prime ; int
    brunners ; (fixme) (builtin_runners) dict[str,BuiltinRunner]
    excopes ; (exec_scopes) list[dict]
    hints ; (fixme) dict[rv,list[CompiledHint]]
    hintspcs ; hint pcs
    hpi ; (fixme) (hint_pc_and_index) dict[int,tuple[rv,int]]
    idi ; (fixme) (instruction_debug_info) dict[rv,InstructionLocation]
    dfc ; (debug_file_contents) dict[str,str]
    ema ; (fixme) (error_message_attributes) list[VmAttributeScope]
    prog ; (program) program
    mem ; (validated_memory) memory
    autodd ; (fixme) (auto_deduction) dict[int,list[tuple[Rule,tuple]]]
    slocals ; (static_locals) dict[str,Any]
    track-revert; (track-revert) bool
    return-vals; (return vals) list[Any]
    ; VirtualMachine
    cntx ; (run_context) context
    acaddrs ; (fixme) (accessed_addresses) set[rv]
    trace ; (fixme) list[TraceEntry[rv]]
    currstep ; (current_step) int
    skipiexec ; (skip_instruction_execution) bool

) #:mutable #:transparent #:reflection-name 'vm)

; raw constructor
(define (new-vm
    ; VirtualMachineBase
    #:prime prime #:brunners brunners #:excopes excopes #:hints hints #:hintspcs hintspcs
    #:hpi hpi #:idi idi #:dfc dfc #:ema ema
    #:prog prog #:mem mem #:autodd autodd #:slocals slocals #:track-revert track-revert #:return-vals return-vals
    ; VirtualMachine
    #:cntx cntx #:acaddrs acaddrs #:trace trace
    #:currstep currstep #:skipiexec skipiexec
    )
    ; return
    (vm prime brunners excopes hints hintspcs hpi idi dfc ema prog mem autodd slocals track-revert return-vals
        cntx acaddrs trace currstep skipiexec)
)
