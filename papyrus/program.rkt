#lang rosette
; this module stores all program related components, including:
;   |- Program
(require json
    (prefix-in tokamak: "./tokamak.rkt")
)
(provide (all-defined-out))

; ================================= ;
; ======== helping structs ======== ;
; ================================= ;
; (ScopeName)
; (CairoHint)

; ========================= ;
; ======== Program ======== ;
; ========================= ;
; (ProgramBase) -> (Program)
(struct program (
    ; ProgramBase
    prime ; int
    data ; list[int]
    builtins ; list[str]
    main ; int or null

    ; Program
    hints ; dict[int,list[hint]]
    mscope ; (main_scope) scopename
    ids ; (fixme) (identifiers) IdentifierManager
    refmgr ; (fixme) (reference_manager) ReferenceManager
    attrs ; (fixme) (attributes) list[AttributeScope]
    dbg ; (fixme) (debug_info) DebugInfo or null

) #:mutable #:transparent #:reflection-name 'program)

; raw constructor
(define (new-program
    ; ProgramBase
    #:prime prime #:data data #:builtins builtins #:main main
    ; Program
    #:hints hints #:mscope mscope #:ids ids #:refmgr refmgr #:attrs attrs #:dbg dbg
    )
    ; return
    (program prime data builtins main hints mscope ids refmgr attrs dbg)
)

; (cairo_runner.load_program) + (Program.load)
; adapted from marshmarrow's method restoring a program object from json
(define (load-program jspath)
    (tokamak:typed jspath string?)
    (define js0 (string->jsexpr (file->string jspath)))
    ; return
    (new-program
        #:prime (hash-ref js0 'prime)
        #:data (hash-ref js0 'data)
        #:builtins (hash-ref js0 'builtins)
        #:main null ; (fixme) where is this initialized?
        #:hints (hash-ref js0 'hints)
        #:mscope null ; (fixme) need to parse (hash-ref js0 'main_scope)
        #:ids null ; (fixme) need to parse (hash-ref js0 'identifiers)
        #:refmgr null ; (fixme) need to parse (hash-ref js0 'reference_manager)
        #:attrs null ; (fixme) need to parse (hash-ref js0 'attributes)
        #:dbg null ; (fixme) need to parse (hasr-ref js0 'debug_info)
    )
)