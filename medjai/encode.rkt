#lang rosette
; this module simulates the functionalities as in encode.py
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in utils: "./utils.rkt")
    (prefix-in config: "./config.rkt")
    (prefix-in instruction: "./instruction.rkt")
)
(provide (all-defined-out))

; global constants
(define DST-REG-BIT 0)
(define OP0-REG-BIT 1)
(define OP1-IMM-BIT 2)
(define OP1-FP-BIT 3)
(define OP1-AP-BIT 4)
(define RES-ADD-BIT 5)
(define RES-MUL-BIT 6)
(define PC-JUMP-ABS-BIT 7)
(define PC-JUMP-REL-BIT 8)
(define PC-JNZ-BIT 9)
(define AP-ADD-BIT 10)
(define AP-ADD1-BIT 11)
(define OPCODE-CALL-BIT 12)
(define OPCODE-RET-BIT 13)
(define OPCODE-ASSERT-EQ-BIT 14)
; (define RESERVED-BIT 15)

(define (decode-instruction enc #:imm imm)
    (tokamak:log "decode-instruction | enc: ~a, imm: ~a" enc imm)
    (when (union? enc)
      (for ([c (union-contents enc)])
        (displayln (cdr c))))
    (tokamak:typed enc integer?)
    (tokamak:typed imm integer? null?)
    (let-values ([(flags off0-enc off1-enc off2-enc) (instruction:decode-instruction-values enc)])
        (tokamak:log "    flags: ~a, off0-enc: ~a, off1-enc: ~a, off2-enc: ~a" flags off0-enc off1-enc off2-enc)
        (define dst-register (if (utils:true? (bitwise-and (utils:shift-right flags DST-REG-BIT) 1)) 'fp 'ap))
        (define op0-register (if (utils:true? (bitwise-and (utils:shift-right flags OP0-REG-BIT) 1)) 'fp 'ap))

        ; get op1
        (define op1-addr-pat (list
            (bitwise-and (utils:shift-right flags OP1-IMM-BIT) 1)
            (bitwise-and (utils:shift-right flags OP1-AP-BIT) 1)
            (bitwise-and (utils:shift-right flags OP1-FP-BIT) 1)
        ))
        (define op1-addr (cond
            [(equal? op1-addr-pat (list 1 0 0)) 'imm]
            [(equal? op1-addr-pat (list 0 1 0)) 'ap]
            [(equal? op1-addr-pat (list 0 0 1)) 'fp]
            [(equal? op1-addr-pat (list 0 0 0)) 'op0]
            [else (tokamak:error "unrecognized op1-addr-pat.")]
        ))

        (define imm0 (cond
            [(equal? 'imm op1-addr)
                (assert (! (null? imm)) "op1-addr is Op1Addr.IMM, but no immediate given.")
                imm
            ]
            [else null]
        ))

        ; get pc_update
        (define pc-update-pat (list
            (bitwise-and (utils:shift-right flags PC-JUMP-ABS-BIT) 1)
            (bitwise-and (utils:shift-right flags PC-JUMP-REL-BIT) 1)
            (bitwise-and (utils:shift-right flags PC-JNZ-BIT) 1)
        ))
        (define pc-update (cond
            [(equal? pc-update-pat (list 1 0 0)) 'jump]
            [(equal? pc-update-pat (list 0 1 0)) 'jump-rel]
            [(equal? pc-update-pat (list 0 0 1)) 'jnz]
            [(equal? pc-update-pat (list 0 0 0)) 'regular]
            [else (tokamak:error "unrecognized pc-update-pat")]
        ))

        (tokamak:log "pc update: ~a" pc-update)

        ; get res
        (define res-pat (list
            (bitwise-and (utils:shift-right flags RES-ADD-BIT) 1)
            (bitwise-and (utils:shift-right flags RES-MUL-BIT) 1)
        ))
        (define res (cond
            [(equal? res-pat (list 1 0)) 'add]
            [(equal? res-pat (list 0 1)) 'mul]
            [(equal? res-pat (list 0 0))
                (if (equal? 'jnz pc-update) 'unconstrained 'op1)
            ]
            [(equal? res-pat (list 1 1)) 'symbolic]
            [else (tokamak:error "unrecognized res-pat")]
        ))
        ; JNZ opcode means res must be UNCONSTRAINED
        (when (equal? 'jnz pc-update) (assert (equal? 'unconstrained res)))

        ; get opcode
        (define opcode-pat (list
            (bitwise-and (utils:shift-right flags OPCODE-CALL-BIT) 1)
            (bitwise-and (utils:shift-right flags OPCODE-RET-BIT) 1)
            (bitwise-and (utils:shift-right flags OPCODE-ASSERT-EQ-BIT) 1)
        ))
        (define opcode (cond
            [(equal? opcode-pat (list 1 0 0)) 'call]
            [(equal? opcode-pat (list 0 1 0)) 'ret]
            [(equal? opcode-pat (list 0 0 1)) 'assert-eq]
            [(equal? opcode-pat (list 0 0 0)) 'nop]
            [(equal? opcode-pat (list 0 1 1)) 'verify-lt]
            [(equal? opcode-pat (list 1 0 1)) 'verify-geq]
            [(equal? opcode-pat (list 1 1 0)) 'verify-eq]
            [(equal? opcode-pat (list 1 1 1)) 'verify-neq]
            [else (tokamak:error "unrecognized opcode-pat")]
        ))

        ; get ap_update
        (define ap-update-pat (list
            (bitwise-and (utils:shift-right flags AP-ADD-BIT) 1)
            (bitwise-and (utils:shift-right flags AP-ADD1-BIT) 1)
        ))
        (define ap-update-init (cond
            [(equal? ap-update-pat (list 1 0)) 'add]
            [(equal? ap-update-pat (list 0 1)) 'add1]
            [(equal? ap-update-pat (list 0 0))
            ;; TODO/fixme: this if statement shouldn't be here
                (if (equal? 'call opcode) 'add2 'regular)
            ]
            [else (tokamak:error "unrecognized ap-update-pat")]
        ))
        ; update
        (define ap-update (cond
            [(equal? 'call opcode)
                (assert (equal? 'add2 ap-update-init) "call must have update_ap is ADD2")
                'add2
            ]
            [else ap-update-init]
        ))

        ; get fp_update
        (define fp-update
            (if (equal? 'call opcode)
                'ap+2
                (if (equal? 'ret opcode)
                    'dst
                    'regular
                )
            )
        )

        ; return
        (instruction:make-instruction
            #:off0 (- off0-enc (expt 2 (- instruction:OFFSET-BITS 1)))
            #:off1 (- off1-enc (expt 2 (- instruction:OFFSET-BITS 1)))
            #:off2 (- off2-enc (expt 2 (- instruction:OFFSET-BITS 1)))
            #:imm imm0
            #:dst dst-register
            #:op0 op0-register
            #:op1 op1-addr
            #:res res
            #:pc pc-update
            #:ap ap-update
            #:fp fp-update
            #:opcode opcode
        )
    )
)