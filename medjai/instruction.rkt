#lang rosette
; this module stores all instruction related components, including:
;   |- Instruction
(require (prefix-in tokamak: lib-symexec/tokamak)
         (prefix-in utils: "./utils.rkt")
         (prefix-in config: "./config.rkt"))
(provide (all-defined-out))

; ========================== ;
; ======== Operands ======== ;
; ========================== ;
; (Operands)
(struct operands
        (dst ; rv
         res ; rv or null
         op0 ; rv
         op1) ; rv
  #:mutable
  #:transparent
  #:reflection-name 'operands)

; raw constructor
(define (new-operands #:dst dst #:res res #:op0 op0 #:op1 op1)
  (operands dst res op0 op1))

; constructor
(define (make-operands #:dst dst #:res res #:op0 op0 #:op1 op1)
  (new-operands #:dst dst #:res res #:op0 op0 #:op1 op1))

; ============================= ;
; ======== Instruction ======== ;
; ============================= ;

; global constants
(define OFFSET-BITS 16)
(define N-FLAGS 15)

; (Instruction)
(struct instruction
        (off0
         off1
         off2 ; int
         imm ; int or null
         dst ; (dst_register) Register: 'ap, 'fp
         op0 ; (op0_register) Register: 'ap, 'fp
         op1 ; (op1_addr) Op1Addr: 'imm, 'ap, 'fp, 'op0
         res ; Res: 'op1, 'add, 'mul, 'unconstrained, 'symbolic
         pc ; (pc_update) PcUpdate: 'regular, 'jump, 'jump-rel, 'jnz
         ap ; (ap_update) ApUpdate: 'regular, 'add, 'add1, 'add2
         fp ; (fp_update) FpUpdate: 'regular, 'ap+2, 'dst
         opcode) ; Opcode: 'nop, 'assert-eq, 'call, 'ret, 'verify-lt, 'verify-geq, 'verify-eq, 'verify-lt
  #:mutable
  #:transparent
  #:reflection-name 'instruction)

; raw constructor
(define (new-instruction #:off0 off0
                         #:off1 off1
                         #:off2 off2
                         #:imm imm
                         #:dst dst
                         #:op0 op0
                         #:op1 op1
                         #:res res
                         #:pc pc
                         #:ap ap
                         #:fp fp
                         #:opcode opcode)
  (instruction off0 off1 off2 imm dst op0 op1 res pc ap fp opcode))

; constructor
(define (make-instruction #:off0 off0
                          #:off1 off1
                          #:off2 off2
                          #:imm imm
                          #:dst dst
                          #:op0 op0
                          #:op1 op1
                          #:res res
                          #:pc pc
                          #:ap ap
                          #:fp fp
                          #:opcode opcode)
  (new-instruction #:off0 off0
                   #:off1 off1
                   #:off2 off2
                   #:imm imm
                   #:dst dst
                   #:op0 op0
                   #:op1 op1
                   #:res res
                   #:pc pc
                   #:ap ap
                   #:fp fp
                   #:opcode opcode))

(define (instruction-size p)
  (if (! (null? (instruction-imm p))) 2 1))

(define (decode-instruction-values encoded-instruction)
  (assert (<= 0 encoded-instruction))
  (assert (< encoded-instruction (expt 2 (+ (* 3 OFFSET-BITS) N-FLAGS))))
  (define off0 (bitwise-and encoded-instruction (- (expt 2 OFFSET-BITS) 1)))
  (define off1
    (bitwise-and (utils:shift-right encoded-instruction OFFSET-BITS) (- (expt 2 OFFSET-BITS) 1)))
  (define off2
    (bitwise-and (utils:shift-right encoded-instruction (* 2 OFFSET-BITS))
                 (- (expt 2 OFFSET-BITS) 1)))
  (define flags-val (utils:shift-right encoded-instruction (* 3 OFFSET-BITS)))
  (values flags-val off0 off1 off2))
