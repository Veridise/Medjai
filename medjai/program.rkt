#lang rosette
; this module stores all program related components, including:
;   |- Program
(require json
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in memory: "./memory.rkt")
)
(provide (all-defined-out))

(define prog-prime #f)

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

    ; Program
    hints ; dict[int,list[hint]]
    mscope ; (main_scope) scopename
    ids ; (fixme) (identifiers) IdentifierManager
    storage-var-ids
    refmgr ; (fixme) (reference_manager) ReferenceManager
    attrs ; (fixme) (attributes) list[AttributeScope]
    dbg ; (fixme) (debug_info) DebugInfo or null
    entrypoint ; entrypoint pc
    instlocs ; instruction locations
    identifiers ; program identifiers
    mainid ; (main function identifier)
) #:mutable #:transparent #:reflection-name 'program)

(define (closest-scope pc prog)
  (tokamak:typed pc memory:rv?)
  (tokamak:typed prog program?)
  ;(tokamak:log "~a" (string->symbol (~a 0)))
  ;(tokamak:log "~a" (program-instlocs prog))
  (define key (string->symbol (~a (memory:fromrv pc))))
  (if (hash-has-key? (program-instlocs prog) key)
    (last (hash-ref 
          (hash-ref (program-instlocs prog) key)
          'accessible_scopes 
          )
    ) "empty"
  )
)

(define (is-storage-var-pc? prog pc)
  (tokamak:typed pc memory:rv?)
  (tokamak:typed prog program?)
  (define key (string->symbol (~a (memory:fromrv pc))))
  (if (hash-has-key? (program-instlocs prog) key)
    (string-contains? (hash-ref 
          (hash-ref (hash-ref 
            (hash-ref (program-instlocs prog) key) 'inst
            ) 'input_file
          ) 'filename) "storage_var"
    ) #f
  )
)

; Assumes scope is called with either write or read
(define (get-write scope)
  (string-replace scope "read" "write")
)

(define (get-key-val-size prog scope)
  (define read-size (get-args-size prog (get-read scope)))
  (define write-size (- (get-args-size prog (get-write scope)) read-size))
  (cons read-size write-size)
)

(define (is-storage-write? scope)
  (string-contains? scope ".write")
)

(define (is-storage-read? scope)
  (string-contains? scope ".read")
)

(define (is-bitwise-and? scope)
  (string-contains? scope ".bitwise_and")
)

(define (is-bitwise-or? scope)
  (string-contains? scope ".bitwise_or")
)

(define (is-bitwise-not? scope)
  (string-contains? scope ".bitwise_not")
)

(define (is-bitwise-xor? scope)
  (string-contains? scope ".bitwise_not")
)

(define (get-read scope)
  (string-replace scope "write" "read")
)

(define (get-args-size prog scope)
  (tokamak:typed scope string?)
  (tokamak:typed prog program?)
  (let ([ident (string-append scope ".Args")])
    (hash-ref (hash-ref (program-identifiers prog) (string->symbol ident)) 'size)
  )
)

(define (type-size prog type)
  (tokamak:typed prog program?)
  (tokamak:typed type string?)
  (cond
    [(equal? type "felt*") 1]
    [(equal? type "felt") 1]
    [(equal? type "") 0]
    [else (hash-ref (hash-ref (program-identifiers prog) (string->symbol type)) 'size 0)]
  )
)

(define (get-size prog type_list_as_string)
  (let*
      ([l1 (string-split type_list_as_string ",")]
       [l2 (map (lambda (s) (list-ref (string-split s ":") (- (length (string-split s ":")) 1))) l1)]
       [l3 (map (lambda (s) (string-replace (string-replace s ")" "") "(" "")) l2)] ; remove all left and right parenthesis
       [l4 (map (lambda (s) (string-trim s " ")) l3)]
       [l5 (map (lambda (s) (type-size prog s)) l4)]
      )
      (foldl + 0 l5)
  )
)

(define (get-return-size prog scope)
  (tokamak:typed scope string?)
  (tokamak:typed prog program?)
  (let* ([ident (string-append scope ".Return")]
         [typelist (~a (hash-ref (hash-ref (program-identifiers prog) (string->symbol ident)) 'cairo_type))]
        )
        (get-size prog typelist)
  )
)

(define (get-implicit-args-size prog scope)
  (tokamak:typed scope string?)
  (tokamak:typed prog program?)
  (let* ([ident (string-append scope ".ImplicitArgs")]
         [identifiers (hash-ref (program-identifiers prog) (string->symbol ident) #f)])
         (cond [identifiers (hash-ref identifiers 'size)][else 0])
  )
)

(define (get-total-num-args prog scope)
  (tokamak:typed scope string?)
  (tokamak:typed prog program?)
  (+ (get-args-size prog scope) (get-implicit-args-size prog scope))
)

(define (get-range-check-ptr-seg prog scope)
  (tokamak:typed scope string?)
  (tokamak:typed prog program?)
  (let ([ident (string-append scope ".ImplicitArgs")])
    (define members (hash-ref (hash-ref (program-identifiers prog) (string->symbol ident)) 'members #f))
    (tokamak:log "MEMBERS: ~a" members)
    (cond [members (if (hash-ref members 'range_check_ptr #f) (+ (hash-ref (hash-ref members 'range_check_ptr) 'offset) 2) #f)]
          [else #f])
  )
)

; raw constructor
(define (new-program
    ; ProgramBase
    #:prime prime #:data data #:builtins builtins
    ; Program
    #:hints hints #:mscope mscope #:ids ids #:refmgr refmgr #:attrs attrs #:dbg dbg
    #:storage-var-ids storage-var-ids
    ; Extra
    #:entrypoint entrypoint #:instlocs instlocs #:identifiers identifiers #:mainid mainid
    )
    ; return
    (program prime data builtins hints mscope ids storage-var-ids
             refmgr attrs dbg entrypoint instlocs identifiers mainid)
)

; (cairo_runner.load_program) + (Program.load)
; adapted from marshmarrow's method restoring a program object from json
(define (load-program jspath starknet? entry-identifier)
    (tokamak:typed jspath string?)
    (define js0
      (let ([prog (string->jsexpr (file->string jspath))])
        (if starknet?
          (hash-ref prog 'program)
          prog)))

    ; parse data
    (define data0
      (for/list ([t0 (hash-ref js0 'data)])
        (string->number (substring t0 2) 16))) ; hex remove leading "0x"

    (define storage-var-ids
      (filter
        (lambda (id)
          (let* ([read-id (string->symbol (~a id ".storage_read"))]
                 [write-id (string->symbol (~a id ".storage_write"))]
                 [read-obj (hash-ref (hash-ref js0 'identifiers) read-id #f)]
                 [write-obj (hash-ref (hash-ref js0 'identifiers) write-id #f)])
            (and read-obj
                 write-obj
                 (equal? "alias"
                         (hash-ref read-obj 'type #f))
                 (equal? "starkware.starknet.common.syscalls.storage_read"
                         (hash-ref read-obj 'destination #f))
                 (equal? "alias"
                         (hash-ref write-obj 'type #f))
                 (equal? "starkware.starknet.common.syscalls.storage_write"
                         (hash-ref write-obj 'destination #f)))))
        (hash-keys (hash-ref js0 'identifiers))))

    (define entry-pc
      ;; '__main__.main is the default
      (hash-ref (hash-ref (hash-ref js0 'identifiers) entry-identifier) 'pc))
    ;(tokamak:log "symbol keys? ~a" (symbol? (first (hash-keys (hash-ref js0 'identifiers)))))
    ; return
    (set! prog-prime (string->number (substring (hash-ref js0 'prime) 2) 16))
    (new-program
        #:prime (string->number (substring (hash-ref js0 'prime) 2) 16) ; hex remove leading "0x"
        #:data data0
        #:builtins (hash-ref js0 'builtins)
        #:hints (hash-ref js0 'hints)
        #:mscope null ; (fixme) need to parse (hash-ref js0 'main_scope)
        #:ids null ; (fixme) need to parse (hash-ref js0 'identifiers)
        #:storage-var-ids storage-var-ids
        #:refmgr null ; (fixme) need to parse (hash-ref js0 'reference_manager)
        #:attrs null ; (fixme) need to parse (hash-ref js0 'attributes)
        #:dbg null ; (fixme) need to parse (hasr-ref js0 'debug_info)
        #:entrypoint entry-pc
        #:instlocs (hash-ref (hash-ref js0 'debug_info) 'instruction_locations)
        #:identifiers (hash-ref js0 'identifiers)
        #:mainid (~a entry-identifier)
    )
)

; property: main
(define (program-main p)
    (tokamak:typed p program?)
    (program-entrypoint p)
)