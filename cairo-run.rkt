#lang rosette
; a simplified version of cairo_run.py
(require racket/cmdline
    (prefix-in tokamak: "./medjai/tokamak.rkt")
    (prefix-in program: "./medjai/program.rkt")
    (prefix-in memory: "./medjai/memory.rkt")
    (prefix-in runner: "./medjai/runner.rkt")
    (prefix-in vm: "./medjai/vm.rkt")
    (prefix-in vm: "./medjai/vm-core.rkt")
    (prefix-in hint: "./medjai/hint.rkt")
    "./medjai/sym.rkt"
    rosette/lib/angelic
)

; parse command line arguments
(define arg-cname null)
(define arg-entry "main")
(define arg-starknet #f)
(define arg-track-revert #f)
(define arg-exec-hints #t)
(command-line
  #:program "cairo-run.rkt"
  #:once-any
  [("--cname") p-cname "path to a compiled Cairo program (.json)"
   (set! arg-cname p-cname)]
  #:once-any
  [("--entry") p-entry "name of function to run"
   (set! arg-entry p-entry)]
  #:once-any
  [("--starknet") "Used to indicate compiled file is %lang starknet"
   (set! arg-starknet #t)]
  #:once-any
  [("--track-revert") "Indicates testing neccessary conditions for revert"
   (set! arg-track-revert #t)]
  #:once-any
  [("--ambivalent") "Used to indicate that Medjai should ignore hints"
   (set! arg-exec-hints #f)])
(when (null? arg-cname) (tokamak:error "cname should not be null."))

(define my-args
  (make-hash
    (list (cons 'program arg-cname)
          (cons 'entry-ident (string->symbol (~a "__main__." arg-entry)))
          (cons 'starknet arg-starknet)
          (cons 'exec-hints arg-exec-hints)
          (cons 'track-revert arg-track-revert)
          (cons 'program-input null))))

(define (run-my-program args [initial-memory #f])
  (hint:refresh-popable-hints)
  (let* ([program (program:load-program
                    (hash-ref args 'program)
                    (hash-ref args 'starknet)
                    (hash-ref args 'entry-ident))]
         [initial-memory
           (if initial-memory
             initial-memory
             (memory:make-memory #:prime (program:program-prime program)
                                 #:storage-var-ids (program:program-storage-var-ids program)))]
         [runner (runner:make-runner #:prog program #:mem initial-memory)]
         [end (begin (runner:initialize-segments runner)
                     (runner:initialize-main-entrypoint runner))]
         [program-input (hash-ref args 'program-input)]
         [program-input (if (null? program-input) (make-hash) program-input)]
         [exec-hints (hash-ref args 'exec-hints)]
         [track-revert (hash-ref args 'track-revert)])
    ; (tokamak:log "initial memory data is: ~a" (memory:memory-data initial-memory))
    (tokamak:log "end is: ~a" end)
    (runner:initialize-vm runner (make-hash (list (cons 'program-input program-input))) #:track-revert arg-track-revert)
    (runner:run-until-pc runner end #:execute-hints exec-hints #:track-revert track-revert)))

(let* ([vc+mem (with-vc
                 (begin0 (run-my-program my-args)
                         (displayln "Finished Symbolic Execution")
                         (flush-output)))]
       [my-vc (result-state vc+mem)]
       [my-mem (result-value vc+mem)]
       [mdl (verify (resume-vc my-vc))]
       [mdl (?complete-solution mdl (get-all-syms))])
  
  (if (unsat? mdl)
    (displayln "No bugs found!")
    (let ([mdl-hash (model mdl)])
      ;; TODO: better print statement using source code variable names
      (displayln "Bugs found with following variable assignments")
      (if (empty? (hash-keys mdl-hash))
        (displayln "Any assignment causes a bug")
        (for ([key (hash-keys mdl-hash)])
          (displayln (~a key " = " (modulo (hash-ref mdl-hash key) program:prog-prime)))))
      (cond 
        [arg-track-revert (tokamak:log "terminating")]
        [else (displayln "Running again") (assert #f)]))))
              ;(clear-vc!)
              ;(if (empty? (hash-keys mdl-hash))
              ;  (run-my-program my-args)
              ;  (run-my-program my-args (memory:make-concrete-memory my-mem mdl)))]))))
;(void (run-my-program my-args))
;(displayln "Finished symbolic execution")
;(displayln "done")