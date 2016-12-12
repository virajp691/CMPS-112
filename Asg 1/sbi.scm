#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    ;(printf "==================================================~n")
    ;(printf "~a: ~s~n" *run-file* filename)
    ;(printf "==================================================~n")
    ;(printf "(~n")
    ;(map (lambda (line) (printf "~s~n" line)) program)
    ;(printf ")~n")
    (map (lambda (line) (set-label-table line)) program)
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program)
               (interp 1 program)
)))
;;; ------------------------------------------------------------- ;;;
;; CUSTOM DEFINES
;;; ------------------------------------------------------------- ;;;
(define *label-table* (make-hash))
(define *var-table* (make-hash))

(define *function-table* (make-hash))
(define (symbol-get key)
        (hash-ref *function-table* key))
(define (symbol-put! key value)
        (hash-set! *function-table* key value))

(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(

        (/       ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        (%       ,(lambda (x y) (- x (* (/ x y) y))))
        (+       ,+)
        (^       ,expt)
        (-       ,-)
        (*       ,*)
        (abs    ,abs)
        (acos   ,acos)
        (asin   ,asin)
        (atan   ,atan)
        (ceil   ,ceiling)
        (cos    ,cos)
        (exp    ,exp)
        (floor  ,floor)
        (log    ,log)
        (log10  ,(lambda (x) (/ (log x) (log 10.0))))
        (round  ,round)
        (sin    ,sin)
        (sqrt   ,sqrt)
        (tan    ,tan)
        (trunc  ,truncate)
     ))
(hash-set! *var-table* "pi" 
     3.141592653589793238462643383279502884197169399)
(hash-set! *var-table* "e" 
     2.718281828459045235360287471352662497757247093)

;; DONE
(define (eval-expr expr)
    (if (symbol? expr)
        (if (hash-has-key? *var-table* expr)
            (hash-ref *var-table* expr)
            (printf "~s does not exist~n" expr)
        )
        (if (number? expr)
            expr
            (let (
                 (op (symbol-get (car expr)))
                 (tail (cdr expr)))
            (apply op (map eval-expr tail)))
        )
    )
)

;; DONE
(define (set-label-table line)
    (cond ((null? (cdr line))
              (void))
          ((pair? (cadr line))
              (void))
          (else 
             (hash-set! *label-table* (cadr line) line)))

)

(define (has-label list)
    (hash-has-key? *label-table* (cadr list))
)

(define (if-statement stmt linenum)
    (let ((op (caadr stmt)))
        (cond ((eqv? op '=)
              (if (eq? (eval-expr (cadadr stmt))
                  (eval-expr (car (cddadr stmt))))
                  (car (hash-ref *label-table* (caddr stmt)))
                  (+ linenum 1)
              ))
              ((eqv? op '<>)
          (if (not (eq? (eval-expr (cadadr stmt)) 
              (eval-expr (car (cddadr stmt)))))
          (car (hash-ref *label-table* (caddr stmt)))
          (+ linenum 1)
          ))
          ((eqv? op '<)
          (if (< (eval-expr (cadadr stmt))
                 (eval-expr (car (cddadr stmt))))
                  (car (hash-ref *label-table* (caddr stmt)))
                  (+ linenum 1)
              ))
          ((eqv? op '>)
          (if (> (eval-expr (cadadr stmt))
                 (eval-expr (car (cddadr stmt))))
                  (car (hash-ref *label-table* (caddr stmt)))
                  (+ linenum 1)
              ))
          ((eqv? op '>=)
          (if (>= (eval-expr (cadadr stmt))
                  (eval-expr (car (cddadr stmt))))
                  (car (hash-ref *label-table* (caddr stmt)))
                  (+ linenum 1)
              ))
          ((eqv? op '<=)
          (if (<= (eval-expr (cadadr stmt))
                  (eval-expr (car (cddadr stmt))))
                  (car (hash-ref *label-table* (caddr stmt)))
                  (+ linenum 1)
              ))
          (else
           (+ linenum 1)))
    )
)

(define (print-stmt stmt)
    (cond
       ((null? (cdr stmt)) (printf "~n"))
       ((string? (cadr stmt))
    (if (null? (cddr stmt))
        (printf "~s" (cadr stmt))
        (printf "~s~s" (cadr stmt) (eval-expr(caddr stmt)))
        )
    (printf "~n"))
       (else (printf "~s~n" (eval-expr (cadr stmt))))
    )
)

(define (eval-stmt statement)
    ; Array 'DIM' Function call
    (if (eqv? (car statement) 'dim)
        (hash-set! *var-table* (cadr statement)
        (make-vector (eval-expr (caaddr statement))))
        (void)
    )
    ; 'Print' function call
    (if (eqv? (car statement) 'print)
    (print-stmt statement)
    (void)
    )
    ; 'Let' Function call
    (if (eqv? (car statement) 'let)
    ;(printf "cdadr:~s caddr:~s ~n" (cdadr statement) (caddr statement))
    (if (pair? (cadr statement))
        (if (vector? (hash-ref *var-table* (caadr statement)))
        (vector-set! (hash-ref *var-table* (caadr statement))
        (cadadr statement) (eval-expr (caddr statement)))
        (printf "Array does not exist~n")
        )
        (hash-set! *var-table* (cadr statement)
                  (eval-expr (caddr statement)))
    )
    (void)
    )
    (if (eqv? (car statement) 'input)
    (let ((in (read))) (hash-set! *var-table* (cadr statement) in))
    (void)
    )
    ;(printf "~s~n" (hash-values *var-table*))
    ;(printf "~s~n" (eqv? (car statement) 'if))
)

;; Interp determines what the function will do. Calls eval-expr 
(define (interp linenum program)
    (if (> linenum (length program))
    (exit 1)
    (void))
    (let ((line (list-ref program (- linenum 1))))
        (if (null? (cdr line))
        (interp (+ linenum 1) program)
        (void)
    )
        (if (has-label line)
       ; (let ((stmt (caddr line))) (eval-stmt stmt))  ;HAS LABEL
       ; (let ((stmt (cadr line))) (eval-stmt stmt))   ;NO LABEL
        (let ((stmt (caddr line)))
            ;(printf "~s~n" stmt)
            (if (eqv? (car stmt) 'goto)
            (interp (car (hash-ref *label-table* (cadr stmt))) program)
            (void)
        )
        (if (eqv? (car stmt) 'if)
            (interp(if-statement stmt linenum) program)
            (void)
        )
        (eval-stmt stmt)
        (interp (+ linenum 1) program)
        )
        (let ((stmt (cadr line)))
            ;(printf "~s~n" stmt)
            (if (eqv? (car stmt) 'goto)
            (interp (car (hash-ref *label-table* (cadr stmt))) program)
            (void)
        )
        (if (eqv? (car stmt) 'if)
            (interp (if-statement stmt linenum) program)
            (void)
        )
        (eval-stmt stmt)
        (interp (+ linenum 1) program)
        )
        )
    )
)

(main (vector->list (current-command-line-arguments)))
;(eval-expr '(+ (* 3 4)))
;(hash-count *var-table*)
;(printf "~s~n" (hash-values *var-table*))
