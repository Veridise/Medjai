#lang rosette
; a simplified version of cairo_run.py
(require racket/cmdline
    (prefix-in tokamak: "./medjai/tokamak.rkt")
    (prefix-in program: "./medjai/program.rkt")
    (prefix-in memory: "./medjai/memory.rkt")
    (prefix-in runner: "./medjai/runner.rkt")
    rosette/lib/angelic
)

; parse command line arguments
(define arg-cname null)
(define arg-starknet #f)
(command-line
  #:program "cairo-run.rkt"
  #:once-any
  [("--cname") p-cname "path to a compiled Cairo program (.json)"
   (set! arg-cname p-cname)]
  #:once-any
  [("--starknet") "Used to indicate compiled file is %lang starknet"
   (set! arg-starknet #t)])
(when (null? arg-cname) (tokamak:error "cname should not be null."))

(define args (make-hash (list
    (cons 'program arg-cname)
    (cons 'starknet arg-starknet)
    (cons 'program-input null)
)))

(define program
  (program:load-program
    (hash-ref args 'program)
    (hash-ref args 'starknet)))
(define initial-memory (memory:make-memory
    #:prime (program:program-prime program)))
(tokamak:log "initial memory data is: ~a" (memory:memory-data initial-memory))

(define runner (runner:make-runner
    #:prog program #:mem initial-memory))
(runner:initialize-segments runner)
(define end (runner:initialize-main-entrypoint runner))
(tokamak:log "end is: ~a" end)
(define program-input (let ([pi (hash-ref args 'program-input)])
    (if (null? pi) (make-hash) pi)
))
(runner:initialize-vm
    runner
    (make-hash (list (cons 'program-input program-input)))
)

(let ([mdl (verify (begin (runner:run-until-pc runner end)
                          (displayln "Finished Symbolic Execution")
                          (flush-output)))])
  (if (unsat? mdl)
    (displayln "No bugs found!")
    (let* ([mdl-hash (model mdl)]
           [str-model
             (for/hash ([key (hash-keys (model mdl))])
               (values (~a key) (hash-ref mdl-hash key)))])
      ;; TODO: better print statement using source code variable names
      (displayln "Bugs found with following variable assignments")
      (if (empty? (hash-keys mdl-hash))
        (displayln "Any assignment causes a bug")
        (for ([key (hash-keys mdl-hash)])
          (displayln (~a key " = " (hash-ref mdl-hash key))))))))