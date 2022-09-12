#lang rosette
; a simplified version of cairo_run.py
(require racket/cmdline
         (prefix-in parser: "./medjai/v-parser.rkt"))

; parse command line arguments
(define arg-vheader #f)
(define arg-output #f)
(define arg-spec-name "my_spec")
(define arg-vname
  (command-line
   #:program "parser-run.rkt"
   #:once-any
   [("--header") p-vheader "path to a header for Cairo specifications (.cairo)" (set! arg-vheader p-vheader)]
   #:once-any
   [("--spec-name") p-spec-name "name of specification function" (set! arg-spec-name p-spec-name)]
   #:once-any
   [("--output") p-output "path to the output file (.cairo)" (set! arg-output p-output)]
   #:args (vname)
   vname))

(when (and (not (null? arg-vname)) (not (null? arg-vheader)))
  (let ([out (if arg-output (open-output-file arg-output #:exists 'replace) (current-output-port))])
    (parser:convert-spec arg-vname arg-spec-name #:header arg-vheader #:print-func (curryr displayln out))
    (close-output-port out)))