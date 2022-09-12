#lang racket

(require json
         racket/hash)

(provide (all-defined-out))

(define num-fresh-vars-made 0)

(define (fresh-var-name [func? #f])
  (let ([num num-fresh-vars-made])
    (set! num-fresh-vars-made (add1 num))
    (~a "v_" (if func? "spec" "imd") "_" num)))

;; right now, we only support types felt or uint256 (because we can make symbolic versions of those)
(struct var (name type) #:transparent)

(define (get-type type-name)
  (cond
    [(equal? type-name "uint256") 'uint256]
    [(or (equal? type-name "felt") (equal? type-name "address")) 'felt]
    ;[else (error (~a "Invalid variable type: " type-name))]))
    [else #f]))

(define (get-variables jsobj)
  (let* ([vdecs (hash-ref (hash-ref jsobj 'var_decls) 'var_decls)])
    (filter-map (lambda (var-jsobj)
                  (let ([name (hash-ref (hash-ref var-jsobj 'var) 'name)]
                        [type (get-type (hash-ref (hash-ref var-jsobj 'type) 'name))])
                    (if type (var name type) #f)))
                vdecs)))

(define (get-v-pre jsobj)
  (hash-ref (hash-ref jsobj 'spec) 'pre #f))

(define (get-v-cond jsobj)
  (hash-ref (hash-ref jsobj 'spec) 'con))

(define (get-func-call jsobj)
  (hash-ref (hash-ref jsobj 'spec) 'fun))

(define (emit-one-var-decl my-var print-func)
  (print-func (~a "    let (local "
                  (var-name my-var)
                  (if (equal? (var-type my-var) 'uint256) " : Uint256)" ")")
                  " = medjai_make_symbolic_"
                  (if (equal? (var-type my-var) 'uint256) "uint256" "felt")
                  "()")))

(define (emit-var-decls vars print-func)
  (print-func "    alloc_locals")
  (map (lambda (my-var) (print-func (~a "    local " (var-name my-var))))
       (filter (lambda (my-var) (equal? 'felt (var-type my-var))) vars))
  (print-func "")
  (map (curryr emit-one-var-decl print-func) vars))

(define (comma-sep items)
  (cond
    [(empty? items) ""]
    [(empty? (rest items)) (first items)]
    [else (~a (first items) ", " (comma-sep (rest items)))]))

(define (emit-func-call func-call print-func)
  (let ([func-name (hash-ref (hash-ref func-call 'func) 'name)]
        [var-names (map (lambda (var-obj) (hash-ref (hash-ref var-obj 'var) 'name))
                        (hash-ref (hash-ref func-call 'args) 'args))])
    (print-func (~a "    " func-name "(" (comma-sep var-names) ")"))))

;; TODO merge this with the above?
;; TODO currently assuming felt, instead find return type of function in compiled code
(define (emit-func-call-expr jsexpr print-func)
  (let ([base-name (hash-ref (hash-ref (hash-ref jsexpr 'base) 'var) 'name)]
        [func-name (hash-ref (hash-ref jsexpr 'func) 'name)]
        [arg-names (map (lambda (var-obj) (hash-ref (hash-ref var-obj 'var) 'name))
                        (hash-ref (hash-ref jsexpr 'args) 'args))]
        [var-name (fresh-var-name)])
    (print-func
     (~a "    let (local " var-name ") = " base-name "." func-name "(" (comma-sep arg-names) ")"))
    (var var-name 'felt)))

;; NOTE: only allow felt here for now
(define (emit-const-expr jsexpr print-func)
  (let ([var-name (fresh-var-name)] [val (hash-ref jsexpr 'val)])
    (print-func (~a "    let " var-name " = " val))
    (var var-name 'felt)))

;; TODO support arithmetic
(define (emit-bin jsexpr obj->var print-func)
  (let ([op-str (hash-ref (hash-ref jsexpr 'op) 'op)])
    (cond
      [(equal? op-str "-") (error "- unsupported")]
      [(equal? op-str "+") (error "- unsupported")]
      [else (emit-cond jsexpr obj->var print-func)])))

(define (emit-expr jsexpr print-func #:obj->var [obj->var (hash)])
  (let ([expr-type (hash-ref jsexpr 'ntype)])
    (cond
      [(hash-has-key? obj->var jsexpr) (hash-ref obj->var jsexpr)]
      [(equal? expr-type "VFuncCallExpr") (emit-func-call-expr jsexpr print-func)]
      [(equal? expr-type "VConstExpr") (emit-const-expr jsexpr print-func)]
      [(equal? expr-type "VBinExpr") (emit-bin jsexpr print-func)]
      ;[(equal? expr-type "VVarExpr") (emit-var-expr jsexpr print-func)]
      [else
       (displayln jsexpr)
       (displayln "-----")
       (map displayln (hash-keys obj->var))
       (error (~a "Expression type " expr-type " not supported"))])))

;; Return a map of json objects to variables
(define (emit-olds jsexprs obj->var print-func)
  (for/hash ([jsexpr jsexprs])
    (let* ([inner-expr (first (hash-ref (hash-ref jsexpr 'args) 'args))]
           [my-var (emit-expr inner-expr print-func #:obj->var obj->var)])
      (values jsexpr my-var))))

(define (emit-binop-and jsexpr obj->var print-func)
  (let* ([lhs-expr (hash-ref jsexpr 'lhs)] [rhs-expr (hash-ref jsexpr 'rhs)])
    (emit-cond lhs-expr obj->var print-func)
    (emit-cond rhs-expr obj->var print-func)))

(define (emit-cond v-cond obj->var print-func)
  (let ([op-str (hash-ref (hash-ref v-cond 'op) 'op)])
    (cond
      [(equal? op-str "&&") (emit-binop-and v-cond obj->var print-func)]
      ; TODO
      [(equal? op-str "||") (error "|| unsupported")]
      [else (emit-compare v-cond obj->var print-func)])))

;; For now, assume _x.read() <op> old(_y.read())
(define (emit-compare v-cond obj->var print-func)
  (let* ([op-str (hash-ref (hash-ref v-cond 'op) 'op)]
         [lhs-var (emit-expr (hash-ref v-cond 'lhs) #:obj->var obj->var print-func)]
         [rhs-var (emit-expr (hash-ref v-cond 'rhs) #:obj->var obj->var print-func)]
         [op-text-str (cond
                        [(equal? op-str "<") "lt"]
                        [(equal? op-str ">") "gt"]
                        [(equal? op-str "<=") "le"]
                        [(equal? op-str ">=") "geq"]
                        [(equal? op-str "=") "eq"]
                        [(equal? op-str "!=") "neq"]
                        [else (error (~a "Unsupported binary op: " op-str))])]
         [op-type-str (if (equal? (var-type lhs-var) 'felt) "felt" "uint256")])
    (unless (equal? (var-type lhs-var) (var-type rhs-var))
      (error "LHS and RHS types are different in compare"))
    (print-func (~a "    medjai_assert_"
                    op-text-str
                    "_"
                    op-type-str
                    "("
                    (var-name lhs-var)
                    ", "
                    (var-name rhs-var)
                    ")"))))

(define (emit-spec-header spec-name print-func)
  (print-func "@external")
  (print-func (~a "func " spec-name " {"))
  ;; TODO include bitwise ptr as well
  (print-func
   "    syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr, bitwise_ptr : BitwiseBuiltin*")
  (print-func "}():"))

(define (get-old-exprs jsexpr)
  (cond
    [(list? jsexpr) (apply append (map get-old-exprs jsexpr))]
    [(not (hash? jsexpr)) empty]
    [(and (equal? (hash-ref jsexpr 'ntype #f) "VFuncCallExpr")
          (equal? (hash-ref (hash-ref jsexpr 'func) 'name) "old"))
     ;; NOTE: This should be singleton list; old takes only one argument
     (list jsexpr)]
    [else (apply append (map get-old-exprs (hash-values jsexpr)))]))

(define (var->obj my-var)
  (hasheq 'ntype "VVarExpr" 'var (hasheq 'name (var-name my-var) 'ntype "VID")))

;; TODO just take in json string?
;; TODO take port instead of file
(define (convert-spec jspath
                      spec-name
                      #:header [header-path #f]
                      #:print-func [print-func displayln])
  (let* ([jsobj (string->jsexpr (last (string-split (file->string jspath) "\n")))]
         [vars (get-variables jsobj)]
         [var-objs->vars (for/hash ([my-var vars])
                           (values (var->obj my-var) my-var))]
         [func-call (get-func-call jsobj)]
         [func-args (hash-ref (hash-ref func-call 'args) 'args)]
         ;; TODO for now, require arguments to all be felt
         [func-args->vars (for/hash ([my-obj func-args])
                            (values my-obj (var (hash-ref (hash-ref my-obj 'var) 'name) 'felt)))]
         [var-objs->vars (hash-union var-objs->vars func-args->vars)]
         [old-exprs (get-old-exprs jsobj)]
         [v-pre (get-v-pre jsobj)]
         [v-cond (get-v-cond jsobj)])
    (when header-path
      (print-func (file->string header-path)))
    (emit-spec-header spec-name print-func)
    (emit-var-decls (append vars (hash-values func-args->vars)) print-func)
    (define obj->var
      (hash-union (emit-olds old-exprs var-objs->vars print-func)
                  var-objs->vars
                  #:combine (lambda (a b) a)))
    (when (hash? v-pre) (emit-cond v-pre obj->var print-func))
    (emit-func-call func-call print-func)
    (emit-cond v-cond obj->var print-func)
    (print-func "    return ()")
    (print-func "end")))

;(convert-spec "vspecs/v-cage-spec.json" "vspecs/vatspec-header.cairo" "cage_spec")
;(convert-spec "vspecs/v-cage-spec-bad.json" "vspecs/vatspec-header.cairo" "cage_spec_bad")
;(convert-spec "vspecs/vat-move.json" "my_spec")
