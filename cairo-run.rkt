#lang rosette
; a simplified version of cairo_run.py
(require racket/cmdline
         (prefix-in tokamak: lib-symexec/tokamak)
         (prefix-in program: "./medjai/program.rkt")
         "./medjai/queries.rkt")

; parse command line arguments
(define arg-cname null)
(define arg-entry "main")
(define arg-starknet #f)
(define arg-track-revert #f)
(define arg-exec-hints #t)
(command-line
 #:program "cairo-run.rkt"
 #:once-any
 [("--cname") p-cname "path to a compiled Cairo program (.json)" (set! arg-cname p-cname)]
 #:once-any [("--entry") p-entry "name of function to run" (set! arg-entry p-entry)]
 #:once-any
 [("--starknet") "Used to indicate compiled file is %lang starknet" (set! arg-starknet #t)]
 #:once-any [("--track-revert")
             "Indicates testing neccessary conditions for revert"
             (set! arg-track-revert #t)]
 #:once-any [("--verbose") "Print all output" (tokamak:set-logmode 'verbose)]
 #:once-any [("--silent") "Print no output" (tokamak:set-logmode 'none)]
 #:once-any
 [("--ambivalent") "Used to indicate that Medjai should ignore hints" (set! arg-exec-hints #f)])
(when (null? arg-cname)
  (tokamak:error "cname should not be null."))

(define my-args
  (make-hash (list (cons 'program arg-cname)
                   (cons 'entry-ident (string->symbol (~a "__main__." arg-entry)))
                   (cons 'starknet arg-starknet)
                   (cons 'exec-hints arg-exec-hints)
                   (cons 'track-revert arg-track-revert)
                   (cons 'program-input null))))

(define mdl (verify-my-program my-args))

(if (unsat? mdl)
    (tokamak:log #:mode 'some "No bugs found!")
    (let ([mdl-hash (model mdl)])
      ;; TODO: better print statement using source code variable names
      (tokamak:log #:mode 'some "Bugs found with following variable assignments")
      (if (empty? (hash-keys mdl-hash))
          (tokamak:log #:mode 'some "Any assignment causes a bug")
          (for ([key (hash-keys mdl-hash)])
            (tokamak:log #:mode 'some
                         (~a key " = " (modulo (hash-ref mdl-hash key) program:prog-prime)))))
      (cond
        [arg-track-revert (tokamak:log "terminating")]
        [else
         (tokamak:log #:mode 'some "Running again")
         (assert #f)])))
