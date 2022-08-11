#lang rosette

(require
  (prefix-in tokamak: "./tokamak.rkt")
  (prefix-in memory: "./memory.rkt")
  (prefix-in context: "./context.rkt")
  "./vm.rkt")

(provide pop-hint-from-scope refresh-popable-hints)

(define hint-registry (make-hash))

(define popable-hint-registry (make-hash))
(define (refresh-popable-hints)
  (for ([k (hash-keys hint-registry)])
    (hash-set! popable-hint-registry k (hash-ref hint-registry k))))

(define (pop-hint-from-scope scope)
  (let ([hints (hash-ref popable-hint-registry scope empty)])
    (if (empty? hints)
      #f
      (begin
        (hash-set! popable-hint-registry scope (rest hints))
        (first hints)))))

(define-symbolic caller-addr integer?)
(define (get-caller-address-hint p #:symres? [symres? #f])
 (tokamak:typed p vm?)
 (let* ([fp (context:context-fp (vm-cntx p))]
        [syscall-ptr-adr (memory:rvsub fp 3)]
        [syscall-ptr (memory:memory-ref (vm-mem p) syscall-ptr-adr)]
        [result-ptr (memory:rvadd syscall-ptr 1)]
        [result (if symres? caller-addr 10)]) ;; Use arbitrary number 23 if not symbolic
  (memory:memory-set! (vm-mem p) result-ptr result)))
(hash-set!
  hint-registry
  "starkware.starknet.common.syscalls.get_caller_address"
  (list get-caller-address-hint))

(define (uint256-add-hint p)
   ;%{
   ;    sum_low = ids.a.low + ids.b.low
   ;    ids.carry_low = 1 if sum_low >= ids.SHIFT else 0
   ;    sum_high = ids.a.high + ids.b.high + ids.carry_low
   ;    ids.carry_high = 1 if sum_high >= ids.SHIFT else 0
   ;%}
   ;   Signature of uint256_add is uint256_add{range_check_ptr}(a : Uint256, b : Uint256)
   ;   offset of a is 0, offset of b is 2
   ;   To calculate address of argument, we use fp - (2 + n_args) + arg offset
   ;   a.low = fp - 6
   ;   a.high = fp - 5
   ;   b.low = fp - 4
   ;   b.high = fp - 3
   ;   range_check_ptr = fp - 7
   ;   carry_low = fp + 
   ;   SHIFT = 340282366920938463463374607431768211456
   (tokamak:typed p vm?)
   (define fp (context:context-fp (vm-cntx p)))
   (define a_low (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 6))))
   (define a_high (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 5))))
   (define b_low (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 4))))
   (define b_high (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))))
   (define sum_low (+ a_low b_low))
   (define carry_low (
               if (>= sum_low 340282366920938463463374607431768211456) 1 0))
   (define sum_high (+ a_high b_high carry_low))
   (define carry_high (
               if (>= sum_high  340282366920938463463374607431768211456) 1 0))
   (memory:memory-set! (vm-mem p) (memory:rvadd fp 2) carry_low)
   (memory:memory-set! (vm-mem p) (memory:rvadd fp 3) carry_high))
(hash-set!
  hint-registry
  "starkware.cairo.common.uint256.uint256_add"
  (list uint256-add-hint))

(define (math-is-nn-hint-first p)
   ;   %{ memory[ap] = 0 if 0 <= (ids.a % PRIME) < range_check_builtin.bound else 1 %}
   ;   Signature of is_nn is is_nn{range_check_ptr}(a) -> (res : felt):
   ;   a = fp - 3
   ;   range_check_builtin.bound = 2 ** 128
   ;   SHIFT = 340282366920938463463374607431768211456
   (tokamak:typed p vm?)
   (define fp (context:context-fp (vm-cntx p)))
   (define ap (context:context-ap (vm-cntx p)))
   (define a (memory:fromrv
              (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))
              ))
   (define rcbound 340282366920938463463374607431768211456) ; 2 ** 128
   (define lb (>= (modulo a (vm-prime p)) 0))
   (define ub (< (modulo a (vm-prime p)) rcbound))
   (define res (
               if (and lb ub) 0 1))
   (memory:memory-set! (vm-mem p) ap res))

(define (math-is-nn-hint-second p)
   ;   %{ memory[ap] = 0 if 0 <= ((-ids.a - 1) % PRIME) < range_check_builtin.bound else 1 %}
   ;   a = fp - 3
   ;   range_check_builtin.bound = 2 ** 128
   ;   SHIFT = 340282366920938463463374607431768211456
   (tokamak:typed p vm?)
   (define fp (context:context-fp (vm-cntx p)))
   (define ap (context:context-ap (vm-cntx p)))
   (define a (memory:fromrv
              (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))
              ))
   (define rcbound 340282366920938463463374607431768211456) ; 2 ** 128
   (define suba (modulo (- -1 a) (vm-prime p)))
   (define lb (>= suba 0))
   (define ub (< suba rcbound))
   (define res (
               if (and lb ub) 0 1))
   (memory:memory-set! (vm-mem p) ap res))
(hash-set!
  hint-registry
  "starkware.cairo.common.math_cmp.is_nn"
  (list math-is-nn-hint-first math-is-nn-hint-second))

(define (math-assert-le-felt-hint-49 p)
  #|%{
      from starkware.cairo.common.math_utils import assert_integer
      assert_integer(ids.a)
      assert_integer(ids.b)
      a = ids.a % PRIME
      b = ids.b % PRIME
      assert a <= b, f'a = {a} is not less than or equal to b = {b}.'

      ids.small_inputs = int(
          a < range_check_builtin.bound and (b - a) < range_check_builtin.bound)
  %}|#
  (tokamak:typed p vm?)
  (define fp (context:context-fp (vm-cntx p)))
  (define ids-a (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 4))))
  (define ids-b (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))))
  (define PRIME (vm-prime p))
  (define range-check-builtin-bound 340282366920938463463374607431768211456)

  (define a (modulo ids-a PRIME))
  (define b (modulo ids-b PRIME))
  (assume (<= a b))
  (define res
    (if (and (< a range-check-builtin-bound)
             (< (- b a) range-check-builtin-bound))
      1
      0))
  ;; small-inputs at fp
  (memory:memory-set! (vm-mem p) fp res))
(hash-set!
  hint-registry
  "starkware.cairo.common.math.assert_le_felt"
  (list math-assert-le-felt-hint-49))

(define (math-assert-nn-hint p)
  #| %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.a)
        assert 0 <= ids.a % PRIME < range_check_builtin.bound, f'a = {ids.a} is out of range.'
    %}
  |#
  (tokamak:typed p vm?)
  (define fp (context:context-fp (vm-cntx p)))
  (define range-check-builtin-bound 340282366920938463463374607431768211456)
  (define PRIME (vm-prime p))
  (define a (modulo (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))) PRIME))
  
  (tokamak:log "assert nn a: ~a" a)
  (assume (>= a 0))
  (assume (< a range-check-builtin-bound)))
  ;(memory:memory-set! (vm-mem p) fp res))
(hash-set!
  hint-registry
  "starkware.cairo.common.math.assert_nn"
  (list math-assert-nn-hint))

(define (math-assert-uint256-signed-nn p)
  ;  %{ memory[ap] = 1 if 0 <= (ids.a.high % PRIME) < 2 ** 127 else 0 %}
  (tokamak:typed p vm?)
  (define fp (context:context-fp (vm-cntx p)))
  (define PRIME (vm-prime p))
  (define rcup 170141183460469231731687303715884105728) ; 2**127
  (define a (modulo (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))) PRIME))
  (define ap (context:context-ap (vm-cntx p)))
  (tokamak:log "signed nn a: ~a" a)
  (define lb (>= a 0))
  (define ub (< a rcup))
   (define res (
               if (and lb ub) 1 0))
   (memory:memory-set! (vm-mem p) ap res))
  ;(memory:memory-set! (vm-mem p) fp res))
(hash-set!
  hint-registry
  "starkware.cairo.common.uint256.uint256_signed_nn"
  (list math-assert-uint256-signed-nn))

(define (math-assert-not-equal-hint p)
  #|%{
        from starkware.cairo.lang.vm.relocatable import RelocatableValue
        both_ints = isinstance(ids.a, int) and isinstance(ids.b, int)
        both_relocatable = (
            isinstance(ids.a, RelocatableValue) and isinstance(ids.b, RelocatableValue) and
            ids.a.segment_index == ids.b.segment_index)
        assert both_ints or both_relocatable, \
            f'assert_not_equal failed: non-comparable values: {ids.a}, {ids.b}.'
        assert (ids.a - ids.b) % PRIME != 0, f'assert_not_equal failed: {ids.a} = {ids.b}.'
  %}|#
  (tokamak:typed p vm?)
  (define fp (context:context-fp (vm-cntx p)))
  (define a (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 4))))
  (define b (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))))
  (define PRIME (vm-prime p))

  (tokamak:log "assert ne a: ~a" a)
  (tokamak:log "assert ne b: ~a" b)
  (tokamak:log "adding assumption hint: ~a" (! (equal? (modulo (- a b) PRIME) 0)))
  (assume (! (equal? (modulo (- a b) PRIME) 0))))
  ;(memory:memory-set! (vm-mem p) fp res))
(hash-set!
  hint-registry
  "starkware.cairo.common.math.assert_not_equal"
  (list math-assert-not-equal-hint))

(define (math-assert-not-zero-hint p)
  #| %{
        from starkware.cairo.common.math_utils import assert_integer
        assert_integer(ids.value)
        assert ids.value % PRIME != 0, f'assert_not_zero failed: {ids.value} = 0.'
    %}
  
  |#
  (tokamak:typed p vm?)
  (define fp (context:context-fp (vm-cntx p)))
  (define val (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))))
  (define PRIME (vm-prime p))

  (tokamak:log "assert_not_zero val: ~a" val)
  (assume (! (equal? (modulo val PRIME) 0))))
  ;(memory:memory-set! (vm-mem p) fp res))
(hash-set!
  hint-registry
  "starkware.cairo.common.math.assert_not_zero"
  (list math-assert-not-zero-hint))

(define (uint256-split-64 p)
  #|
    %{
        ids.low = ids.a & ((1<<64) - 1)
        ids.high = ids.a >> 64
    %}
  |#
  (tokamak:typed p vm?)
  (define fp (context:context-fp (vm-cntx p)))
  (define ids-value (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))))

  (define low (modulo ids-value (expt 2 64)))
  (define high (quotient ids-value (expt 2 64)))
  (define low-addr fp)
  (tokamak:log "split-64 hint low addr: ~a" low-addr)
  (define high-addr (memory:rvadd low-addr 1))
  (memory:memory-set! (vm-mem p) low-addr low)
  (memory:memory-set! (vm-mem p) high-addr high))
(hash-set!
  hint-registry
  "starkware.cairo.common.uint256.split_64"
  (list uint256-split-64))

(define (math-split-felt-hint-18 p)
  #|
    %{
        from starkware.cairo.common.math_utils import assert_integer
        assert ids.MAX_HIGH < 2**128 and ids.MAX_LOW < 2**128
        assert PRIME - 1 == ids.MAX_HIGH * 2**128 + ids.MAX_LOW
        assert_integer(ids.value)
        ids.low = ids.value & ((1 << 128) - 1)
        ids.high = ids.value >> 128
    %}
  |#
  (tokamak:typed p vm?)
  (define fp (context:context-fp (vm-cntx p)))
  (define PRIME (vm-prime p))
  (define MAXHIGH (/ PRIME (expt 2 128)))
  (define MAXLOW 0)
  (define ids-value (memory:fromrv (memory:memory-ref (vm-mem p) (memory:rvsub fp 3))))

  (define low (remainder ids-value (expt 2 128)))
  (define high (quotient ids-value (expt 2 128)))
  (define low-addr (memory:memory-ref (vm-mem p) (memory:rvsub fp 4)))
  (tokamak:log "hint 18 low addr: ~a" low-addr)
  (define high-addr (memory:rvadd low-addr 1))
  (memory:memory-set! (vm-mem p) low-addr low)
  (memory:memory-set! (vm-mem p) high-addr high))
(hash-set!
  hint-registry
  "starkware.cairo.common.math.split_felt"
  (list math-split-felt-hint-18))

(define (alloc-alloc-hint p)
  (tokamak:typed p vm?)
  (define ap (context:context-ap (vm-cntx p)))
  (memory:memory-set! (vm-mem p) ap (memory:add-segment (vm-mem p))))
(hash-set!
  hint-registry
  "starkware.cairo.common.alloc.alloc"
  (list alloc-alloc-hint))