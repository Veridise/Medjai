#lang rosette
; this module stores all memory related components, including:
;   |- MemoryDict
;   |- RelocatableValue
;   |- MemorySegmentManager
(require
    (prefix-in tokamak: "./tokamak.rkt")
    (prefix-in config: "./config.rkt")
    "./sym.rkt"
)
(provide (all-defined-out))

; ================================== ;
; ======== RelocatableValue ======== ;
; ================================== ;
(define SEGMENT-BITS 16)
(define OFFSET-BITS 47)
; (RelocatableValue)
(struct rv (
    seg ; (segment_index) int
    off ; (offset) int
) #:transparent #:reflection-name 'rv)

(define (torv i)
  (tokamak:typed i rv? integer?)
  (if (integer? i)
    (rv 0 i)
    ;; When tokamak:typed does nothing, return i for all non-ints
    i))

(define rngchkseg -1)
(define ap-seg-vec (make-vector 10000 #f))
(define (set-rngcheckseg num) (set! rngchkseg num))
(define (get-rngcheckseg) rngchkseg)

; TODO/fixme checks whether an rv belongs to a range_check_ptr segment
; range_check_ptr segment is from (2, >0)
; we should probably do something smarter here to figure out exactly where
; in case there are multiple builtins
(define (israngechk r)
  (tokamak:typed r rv?)
  (tokamak:log "checking for rangechk ~a" r)
  ;; TODO actually figure out which arg is the range_check_ptr
  (equal? (rv-seg r) rngchkseg))

(define (fromrv r)
  (tokamak:typed r rv? integer?)
  (if (integer? r)
    r
    ;; When tokamak:typed does nothing, return i for all non-ints
    (rv-off r)))

(define (rvadd self other)
    ;(tokamak:typed self rv? integer?)
    ;(tokamak:typed other integer?)
    ; return
  (let ([self (torv self)]
        [other (fromrv other)])
    (rv (rv-seg self) (+ (rv-off self) other))))

(define (rvsub self other)
    
    (tokamak:typed self rv? integer?)
    (tokamak:typed other rv? integer?)
    (cond [(union? other)
            (for/all ([v other #:exhaustive])
                (rvsub self v)
            )
          ]
          [else
            (let ([self (torv self)])
                (cond
                    [(integer? other)  (rv (rv-seg self) (- (rv-off self) other))]
                    [else
                    ; other is also rv
                    (assert (equal? (rv-seg self) (rv-seg other)) "rv-sub segs mismatch")
                    (- (rv-off self) (rv-off other))]))
          ]
    ))

(define (rvmod self other)
    (tokamak:typed self rv? integer?)
    (tokamak:typed other integer?)
    ; return
    (let ([self (torv self)])
      (rv (rv-seg self) (modulo (rv-off self) other))))

(define (rveq self other)
    (tokamak:typed self rv?)
    (tokamak:typed other rv?)
    ; return
    (&&
        (equal? (rv-seg self) (rv-seg other))
        (equal? (rv-off self) (rv-off other))
    )
)

(define (rvlt self other)
    (tokamak:typed self rv?)
    (tokamak:typed other rv? integer?)
    (cond
        [(integer? other) #f]
        [else ; rv
            (let ([seg0 (rv-seg self)][seg1 (rv-seg other)]
                  [off0 (rv-off self)][off1 (rv-off other)])
                (if (< seg0 seg1)
                    #t
                    (if (equal? seg0 seg1)
                        (if (< off0 off1) #t #f)
                        #f
                    )
                )
            )
        ]
    )
)

; pass on, no need to check types
(define (rvle self other) (|| (rvlt self other) (rveq self other)))
(define (rvge self other) (! (rvlt self other)))
(define (rvgt self other) (! (rvle self other)))

; ========================================================================= ;
; ======== MemoryDict + MemorySegmentManager + ValidatedMemoryDict ======== ;
; ========================================================================= ;
; (MemoryDict + MemorySegmentManager + ValidatedMemoryDict)
(struct memory (

    ; MemoryDict
    data ; vector of vectors
    storage-var-ids ; list of storage variable names
    storage-vars ; vector of storage variable functions, same order as storage-var-ids
    frozen ; (_frozen) bool
    rrules ; (relocation_rules) dict[int,rv]

    ; MemorySegmentManager
    ; (memory) is data, no need to include
    prime ; int
    nsegs ; (n_segments) int
    ntsegs ; (n_temp_segments) int
    ssizes ; (_segment_sizes) dict[int,int]
    susizes ; (_segment_used_sizes) dict[int,int] or null
    pmoffs ; (public_memory_offsets) dict[int,list[tup[int,int]]]

    ; ValidatedMemoryDict
    vrules ; (fixme) (__validation_rules) dict[int,list[tuple[ValidationRule,tuple]]]
    vaddrs ; (fixme) (__validated_addresses) set[rv]

) #:mutable #:transparent #:reflection-name 'memory)

; raw constructor
(define (new-memory
    ; MemoryDict
    #:data data
    #:storage-var-ids storage-var-ids
    #:storage-vars storage-vars
    #:frozen frozen #:rrules rrules
    ; MemorySegmentManager
    #:prime prime #:nsegs nsegs #:ntsegs ntsegs #:ssizes ssizes
    #:susizes susizes #:pmoffs pmoffs
    ; ValidatedMemoryDict
    #:vrules vrules #:vaddrs vaddrs
    )
    ; return
    (memory data storage-var-ids storage-vars frozen rrules
            prime nsegs ntsegs ssizes susizes pmoffs vrules vaddrs)
)

; constructor
(define (make-memory
    #:values [values null] ; MemoryDict
    #:storage-var-ids storage-var-ids
    #:prime prime ; MemorySegmentManager
    )
    (tokamak:typed values list? null?)
    (tokamak:typed prime integer?)

    ; (fixme) you need a correct way to convert values into data
    (when (! (null? values)) (tokamak:error "not implemented"))
    (define data0 (list->vector (for/list ([_ (range config:segcap)]) null)))

    ; return
    (new-memory
        #:data data0
        ;; TODO don't hard-cdoe
        ;; TODO on read, if returned false, should store new value
        ;;      (of appropriate size) and return that
        #:storage-var-ids storage-var-ids
        #:storage-vars (for/vector ([svid storage-var-ids]) (thunk* #f))
        #:frozen #f
        #:rrules (make-hash)
        #:prime prime
        #:nsegs 0
        #:ntsegs 0
        #:ssizes (make-hash)
        #:susizes null
        #:pmoffs null
        #:vrules (make-hash) ; (fixme)
        #:vaddrs null ; (fixme)
    )
)

(define revert-var #f)

(define set-revert-var! (set! revert-var #t))


(define (make-concrete-memory old-memory mdl)
  (define old-data (memory-data old-memory))
  (define new-data
    (for/vector ([seg old-data])
      (if (empty? seg)
        seg
        (segment
          ;; Access function
          (lambda (off)
            (let ([old-val ((segment-access-function seg) off)])
              (evaluate old-val mdl)))
          ;; keys
          (remove-duplicates (map (curryr evaluate mdl) (segment-keys seg)))))))

  (define svids (memory-storage-var-ids old-memory))
  ;; TODO/NOTE: Unlike memory, storage vars can be overwritten in Cairo.
  ;;            This means that we need to rely on the fact that values go into memory first
  ;;            before being written into a storage var.
  ;;            This is why we use the former over the later initialization for storage-vars.
  (define storage-vars (for/vector ([svid svids]) (thunk* #f)))
  ;(define old-storage-vars (memory-storage-vars old-memory))
  ;(define storage-vars
  ;  (for/vector ([svfunc old-storage-vars])
  ;    (lambda (keys)
  ;      (let ([old-val (svfunc keys)])
  ;        (evaluate old-val mdl)))))
  (new-memory
    #:data new-data
    #:storage-var-ids svids
    #:storage-vars storage-vars
    #:frozen #t
    #:rrules (make-hash)
    #:prime (memory-prime old-memory)
    #:nsegs 0
    #:ntsegs 0
    #:ssizes (make-hash)
    #:susizes null
    #:pmoffs null
    #:vrules (make-hash)
    #:vaddrs null))

;; readfunc is a nested function, # nests = (length keys)
;; if keys is empty, readfunc is a constant
;(define (apply-read readfunc keys)
;  (if (empty? keys)
;    readfunc
;    (apply-read (readfunc (first keys)) (rest keys))))

;; return a new readfunc
;(define (apply-write readfunc keys val)
;  (if (empty? keys)
;    val
;    (let ([subread (apply-write (readfunc (first keys)) (rest keys) val)])
;      (lambda (k)
;        (if (equal? k (first keys))
;          subread
;          (readfunc k))))))

;; TODO which is more efficient? Depends on how many writes to any one storage var happen
(define (apply-write readfunc my-keys val)
  (lambda (keys)
    (if (equal? my-keys keys)
      val
      (readfunc keys))))

(define (write-storage-var p varname k v)
  (tokamak:log "Writing to key ~a, value ~a for var ~a" k v varname)
  (let* ([varname (string->symbol varname)]
         [svs (memory-storage-vars p)]
         [svids (memory-storage-var-ids p)]
         [my-sv-index (index-of svids varname)]
         [my-sv (vector-ref svs my-sv-index)])
    (vector-set! svs my-sv-index (apply-write my-sv k v))))

(define (print-val l)
    (cond 
        [(equal? l #f) (tokamak:log "~a" #f)]
        [(union? l) (tokamak:log "union contents: ~a" (union-contents l))]
        [else (for-each (lambda (e) (cond [(union? e) (tokamak:log "~a" (union-contents e))][else (tokamak:log "~a" e)])) l)]
    )
)

(define (read-storage-var p varname k num-vals)
  (tokamak:log "Reading from key ~a for var ~a" k varname)
  (let* ([varname (string->symbol varname)]
         [svs (memory-storage-vars p)]
         [svids (memory-storage-var-ids p)]
         [my-sv-index (index-of svids varname)]
         [my-sv (vector-ref svs my-sv-index)]
         [val (my-sv k)])
    (tokamak:log "Have setup storage var?: ~a" (not (not my-sv)))
    (if val
      val
      (let ([new-val (build-list num-vals (thunk* (symint (memory-prime p))))])
        (write-storage-var p (~a varname) k new-val)
        new-val))))

(define (segment-print seg port mode)
  (let* ([print-func
           (case mode
             [(#t) write]
             [(#f) display]
             [else (lambda (p port) (print p port mode))])]
        [keys (segment-keys seg)]
        [vals (map (curry segment-ref seg) keys)]
        [kvpairs
          (map (lambda (k v) (~a k "->" v)) keys vals)])
    (print-func kvpairs port)))



(struct segment
  (access-function keys)
  #:mutable
  #:transparent
  #:methods gen:custom-write
  [(define write-proc segment-print)])

(define (segment-ref seg i) 
    ((segment-access-function seg) i))


(define (segment-set! seg i v)
  (let ([af (segment-access-function seg)]
        [old-keys (segment-keys seg)])
    ;; TODO can remove this for symexec
    ;(when (not (member i old-keys))
      (set-segment-keys!
        seg
        (cons i old-keys));)
    (set-segment-access-function!
      seg
      (lambda (j)
        (if (equal? i j) v (af j))))))

; helper function
(define (make-default-segment) (segment (lambda (off) #f) empty))

; MemoryDict method, internal core method, refer to a memory slot (not segment)
(define (data-ref p addr)
    ;(tokamak:log "data-ref on addr ~a" addr)
    (tokamak:typed p vector?) ; p is the memory data
    (tokamak:typed addr rv? integer?) ; for integer, treat it as seg=0
    ;(set! addr (rewrite-ite addr))
    (define seg0 (if (integer? addr) 0 (rv-seg addr)))
    (define off0 (if (integer? addr) addr (rv-off addr)))
   
    (define l0 (vector-ref p seg0))
    ; (fixme) for l0, it's NOT ok to return null, since you need to initialize a segment first
    (when (null? l0) (tokamak:error "l0 is null, given addr: ~a." addr))
    ;(tokamak:log "Segment ref on segment with union size: ~a"
    ;             (if (union? l0) (length (union-contents l0)) 1))
    ;(tokamak:log "Segment ref on segment with number of NUMBER OF keys: ~a"
    ;             (if (union? (segment-keys l0)) (length (union-contents (segment-keys l0))) 1))
    ;(when (union? (segment-keys l0))
    ;  (tokamak:log "The segment: ~a" l0))
    ;(tokamak:log "Segment ref on segment with number of keys: ~a" (length (segment-keys l0)))
    (cond
        [(equal? seg0 1) (vector-ref ap-seg-vec off0)]
        [else 
            (define l1 (segment-ref l0 off0))
            l1
        ]
    )
    ; (note) for l1 it's ok to return null, e.g., compute_operands allows pre-fetch of null
    ; (when (null? l1) (tokamak:error "l1 is null, given addr: ~a." addr))
    ; return
    ;(tokamak:log "returning l1: ~a" l1)

    ;(for/all ([my-seg l0])
    ;  (begin
    ;    (when (union? (segment-keys l0))
    ;      (tokamak:log "Segment function in for/all ~a" (segment-access-function my-seg)))
    ;    (tokamak:log "Segment ref on segment with union size: ~a"
    ;                 (if (union? my-seg) (length (union-contents my-seg)) 1))
    ;    (tokamak:log "Segment ref on segment with number of NUMBER OF keys: ~a"
    ;                 (if (union? (segment-keys my-seg)) (length (union-contents (segment-keys my-seg))) 1))
    ;    (segment-ref my-seg off0)))
)

; MemoryDict method, internal core method, set a memory slot (not segment)
(define (data-set! p key val)
    (tokamak:typed p vector?) ; p is the memory data
    (tokamak:typed key rv? integer?) ; for integer, treat it as seg=0
    (tokamak:typed val rv? integer?) ; can only be a slot value, not segment
    ;(set! key (rewrite-ite key))
    ;(set! val (rewrite-ite val))
    (define seg0 (if (integer? key) 0 (rv-seg key)))
    (define off0 (if (integer? key) key (rv-off key)))
    (define l0 (let ([t0 (vector-ref p seg0)])
        (cond
            [(null? t0)
                (vector-set! p seg0 (make-default-segment)) ; create a segment
                (vector-ref p seg0) ; point to that newly created segment
            ]
            [else t0]
        )
    ))
    (cond
        [(equal? seg0 1) (tokamak:log "SETTING AP VAL: ~a ~a" off0 val) (vector-set! ap-seg-vec off0 val)]
    )
    (segment-set! l0 off0 val)
)

(define (seg-add! p ind)
    (tokamak:typed p vector?) ; p is the memory data
    (tokamak:typed ind integer?)
    (when (! (null? (vector-ref p ind)))
        (tokamak:error "segment already exists at ind: ~a." ind))
    (vector-set! p ind (make-default-segment))
)

; MemoryDict method
(define (check-element num name)
    (tokamak:typed num rv? integer?)
    (tokamak:typed name string?)
    ;(when (integer? num)
    ;    (when (< num 0)
    ;        (tokamak:error "~a must be nonnegative, got: ~a." name num)
    ;))
)

; MemoryDict method
(define (verify-same-value addr current value)
    (tokamak:typed addr rv? integer?)
    (tokamak:typed current rv? integer?)
    (tokamak:typed value rv? integer?)
    ; (fixme) this may cause exception if current or value is rv
    ;         since equal? is not overloaded here
    (when (! (equal? current value))
        (tokamak:error "inconsistency memory error, addr: ~a, current: ~a, value: ~a." addr current value))
)

; MemoryDict method
(define (memory-ref p addr)
    (tokamak:typed p memory?)
    (check-element addr "memory address")
    
    ; return
    (define value (data-ref (memory-data p) addr))
    (relocate-value value))
    ;(for/all ([value (data-ref (memory-data p) addr) #:exhaustive])
    ;        (relocate-value value)
    ;)) 

; MemoryDict method
(define (memory-set! p addr value)
    (tokamak:log "memory set addr=~a, value=~a" addr value)
    (tokamak:typed p memory?)
    ;(when (memory-frozen p)
    ;    (tokamak:error "memory is frozen and cannot be changed."))
    (when (israngechk addr)
      (tokamak:log "adding range check constraints")
      ;(tokamak:log "fromrv val: ~a" (fromrv value)))
      (assume (>= (fromrv value) 0))
      (assume (< (fromrv value) 340282366920938463463374607431768211456)))
    (check-element addr "memory address")
    (check-element value "memory value")
    (when (&& (rv? addr) (< (rv-off addr) 0))
        (tokamak:error "offset of rv must be nonnegative, got: ~a." (rv-off addr)))
    (when (or (not (memory-frozen p)) (not (data-ref (memory-data p) addr)))
      (data-set! (memory-data p) addr value))
    (define current (data-ref (memory-data p) addr))
    (verify-same-value addr current value)
)

; MemoryDict method
(define (relocate-value value)
    (tokamak:typed value rv? integer? null?)
    ;(tokamak:log "rv? val ~a" (rv? value))
    (cond
        [(! (rv? value)) value]
        [(>= (rv-seg value) 0) value]
        [else ; skipped a few
            value
        ]
    )
)

; MemoryDict method
(define (validate-existing-memory p)
    (tokamak:typed p memory?)
    ; (fixme) add implementation
)

; MemorySegmentManager method
(define (add-segment p #:size [size null])
    (tokamak:typed p memory?)
    (tokamak:typed size integer? null?)
    (let ([segment-index (memory-nsegs p)][data (memory-data p)])
        (set-memory-nsegs! p (+ 1 segment-index))
        (when (! (null? size))
            ; (fixme) finalize
            (void)
        )
        ;; NOTE: this is fine b/c nsegs still counts the segment index
        (when (not (memory-frozen p))
          (seg-add! data segment-index))
        ; (tokamak:log "added memory segment, now memory data is: ~a" data)
        ; return
        (rv segment-index 0)
    )
)

; MemorySegmentManager method
(define (load-data p ptr data)
    (tokamak:typed p memory?)
    (tokamak:typed ptr rv? integer?)
    (tokamak:typed data list?)
    (let ([n (length data)])
        (for ([i (range n)])
            (memory-set! p (rvadd ptr i) (list-ref data i))
        )
    )
    ; return
    (rvadd ptr (length data))
)