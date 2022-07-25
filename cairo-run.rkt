#lang rosette
; a simplified version of cairo_run.py
(require racket/cmdline
    (prefix-in tokamak: "./medjai/tokamak.rkt")
    (prefix-in program: "./medjai/program.rkt")
    (prefix-in memory: "./medjai/memory.rkt")
    (prefix-in runner: "./medjai/runner.rkt")
    (prefix-in vm: "./medjai/vm.rkt")
    rosette/lib/angelic
)

; parse command line arguments
(define arg-cname null)
(define arg-starknet #f)
(define arg-exec-hints #t)
(command-line
  #:program "cairo-run.rkt"
  #:once-any
  [("--cname") p-cname "path to a compiled Cairo program (.json)"
   (set! arg-cname p-cname)]
  #:once-any
  [("--starknet") "Used to indicate compiled file is %lang starknet"
   (set! arg-starknet #t)]
  #:once-any
  [("--ambivalent") "Used to indicate that Medjai should ignore hints"
   (set! arg-exec-hints #f)])
(when (null? arg-cname) (tokamak:error "cname should not be null."))

(define my-args (make-hash (list
    (cons 'program arg-cname)
    (cons 'starknet arg-starknet)
    (cons 'exec-hints arg-exec-hints)
    (cons 'program-input null)
)))

(define (run-my-program args)
  (let* ([program (program:load-program (hash-ref args 'program) (hash-ref args 'starknet))]
         [initial-memory (memory:make-memory #:prime (program:program-prime program))]
         [runner (runner:make-runner #:prog program #:mem initial-memory)]
         [end (begin (runner:initialize-segments runner)
                     (runner:initialize-main-entrypoint runner))]
         [program-input (hash-ref args 'program-input)]
         [program-input (if (null? program-input) (make-hash) program-input)]
         [exec-hints (hash-ref args 'exec-hints)])
    (tokamak:log "initial memory data is: ~a" (memory:memory-data initial-memory))
    (tokamak:log "end is: ~a" end)
    (runner:initialize-vm runner (make-hash (list (cons 'program-input program-input))))
    (runner:run-until-pc runner end #:execute-hints exec-hints)))

(let ([mdl (verify (begin (run-my-program my-args)
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
          (displayln (~a key " = " (hash-ref mdl-hash key)))))
      (displayln "Running again")
      (vm:turn-on-pop-stack)
      (vm:set-pop-stack
        (map (lambda (k) (hash-ref mdl-hash k))
             (sort (hash-keys mdl-hash)
                   (lambda (s1 s2)
                     (< (string->number (substring (~a s1) 2 (string-length (~a s1))))
                        (string->number (substring (~a s2) 2 (string-length (~a s2)))))))))
      (run-my-program my-args))))