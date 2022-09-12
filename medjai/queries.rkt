#lang rosette

(require (prefix-in tokamak: lib-symexec/tokamak)
         (prefix-in program: "./program.rkt")
         (prefix-in memory: lib-symexec/memory)
         (prefix-in runner: "./runner.rkt")
         (prefix-in hint: "./hint.rkt")
         lib-symexec/sym
         rosette/lib/angelic)

(provide run-my-program
         verify-my-program)

(define (run-my-program args [initial-memory #f])
  (clear-terms!)
  (memory:reset-revert-var!)
  (hint:refresh-popable-hints)
  (let* ([program (program:load-program (hash-ref args 'program)
                                        (hash-ref args 'starknet)
                                        (hash-ref args 'entry-ident))]
         [initial-memory (if initial-memory
                             initial-memory
                             (memory:make-memory
                              #:prime (program:program-prime program)
                              #:constraint-checks (list runner:range-check)
                              #:storage-var-ids (program:program-storage-var-ids program)))]
         [runner (runner:make-runner #:prog program #:mem initial-memory)]
         [end (begin
                (runner:initialize-segments runner)
                (runner:initialize-main-entrypoint runner))]
         [program-input (hash-ref args 'program-input)]
         [program-input (if (null? program-input) (make-hash) program-input)]
         [exec-hints (hash-ref args 'exec-hints)]
         [track-revert (hash-ref args 'track-revert)])
    (tokamak:log "end is: ~a" end)
    (runner:initialize-vm runner
                          (make-hash (list (cons 'program-input program-input)))
                          #:track-revert track-revert)
    (runner:run-until-pc runner end #:execute-hints exec-hints)
    (when track-revert
      (assert (not memory:revert-var)))))

(define (verify-my-program args [initial-memory #f])
  (let* ([vc+mem (with-vc (begin0 (run-my-program args)
                            (tokamak:log "Finished Symbolic Execution")
                            (flush-output)))]
         [my-vc (result-state vc+mem)]
         [my-mem (result-value vc+mem)]
         [mdl (verify (resume-vc my-vc))]
         [mdl (?complete-solution mdl (get-all-syms))])
    mdl))
