(load "io.scm")
(load "compiler-necessary.scm")
(load "runtime-support.scm")

; ######################### GENERATORS ###########################

(define ^^label
	(lambda (name)
		(let ((n 0))
			(lambda ()
				(set! n (+ n 1))
				; (string-append name
				(string-append name
				(number->string n))))))
				
(define ^^comment
	(lambda text
		(string-append
			"/*######################"
			(if (null? text) ""
				(string-append " " 
					(apply string-append
						(map
							(lambda (e)
								(string-append e " "))
							text))))
			"######################*/"
		)))
				
(define nl "\n")				

; ################## WRITE TO CISC FILE #########################
(define label-program-exit "Lprogram_exit")

(define print-stack-macro
	(string-append
		"#define PT5(MSG) 			\\" nl
		"	printf(MSG);			\\" nl
		"	printf(\"\\n\");			\\" nl
		"   SHOW(\"STACK[TOP]\",STARG(-1));			\\" nl
		"    SHOW(\"STACK[TOP-1]\",STARG(0));	\\" nl
		"    SHOW(\"STACK[TOP-2]\",STARG(1));				\\" nl
		"	SHOW(\"STACK[TOP-2]\",STARG(2));				\\" nl
		"	SHOW(\"STACK[TOP-3]\",STARG(3));				\\" nl
		"	SHOW(\"STACK[TOP-4]\",STARG(4));				\\" nl
		"	SHOW(\"STACK[TOP-5]\",STARG(5));	\\" nl
		"    SHOW(\"STACK[TOP-6]\",STARG(6));				\\" nl
		"	SHOW(\"STACK[TOP-7]\",STARG(7));				\\" nl
		"	SHOW(\"STACK[TOP-8]\",STARG(8));				\\" nl
		"	SHOW(\"STACK[TOP-9]\",STARG(9));				\\" nl
		"	SHOW(\"STACK[TOP-10]\",STARG(10)); " nl
	)
)


(define trim-.c
    (lambda (text)
      (list->string
				(reverse
					(cddr
						(reverse
							(string->list text)))))))

(define write-makefile
	(lambda (cisc-filename)
		(let ((mf-file "makefile"))
			(begin 
				(if (file-exists? mf-file)
						(delete-file mf-file)
						(void))
				(write-to-file mf-file
					(string-append
						"# All Targets" nl
						"CC=gcc" nl nl

						(format "CISC_SRC=~a" cisc-filename) nl
						(format "CISC_OUT=~a" (trim-.c cisc-filename)) nl nl

						"all: clean Run" nl nl

						"Run: Cisc" nl nl

						"Cisc: " nl
						"	$(CC) -o $(CISC_OUT) $(CISC_SRC)" nl
						"clean: " nl
						"	rm -f $(CISC_OUT)" nl
					)
				)
			)
		)
	)
)


(define write-to-cisc-file
	(lambda (text out)
		(let ((file-name out))
			(begin 
			(if (file-exists? file-name)
					(delete-file file-name)
					(void))
				(write-to-file file-name (string-append
											"#define DO_SHOW 2" nl
											"#include \"arch/cisc.h\"" nl
											"#include <stdio.h>" nl
											"#include <stdlib.h>" nl
											"#define MAGIC_SPOT { PUSH(IMM(777777)); }" nl
											"#define SCARG(x) FPARG(2+x)" nl
											print-stack-macro
											
											"int main()" nl
											"{" nl
											  "START_MACHINE;" nl nl

											  "JUMP(CONTINUE);" nl nl
											  
											  
											"#include \"arch/char.lib\"" nl
											"#include \"arch/io.lib\"" nl
											"#include \"arch/math.lib\"" nl
											"#include \"arch/string.lib\"" nl
											"#include \"arch/system.lib\"" nl
											"#include \"arch/scheme.lib\"" nl

											 "CONTINUE:\n"
																				 
											;########
											   text
											;########
											"Lprogram_exit:" nl
											  "\nSTOP_MACHINE;" nl nl
											  "return 0;" nl
											"}" nl
				))))))
				
				
		
; ###############################################################		


; ########### GENERAL  ###########################

(define make-functions
	(lambda ()
		(string-append
			"JUMP(FUNCTIONS_END);" nl
			"// MAKE_SOB_LIST(n, An, An-1, ..., A1)" nl
			"MAKE_SOB_LIST:" nl
				"PUSH(FP);" nl
				"MOV(FP,SP);" nl nl
				
				"MOV(R0,SOB_NIL);" nl
				"MOV(R7,FPARG(0));" nl
				"MAKE_PAIRS:" nl
				"CMP(R7,0);" nl
				"JUMP_LE(MAKE_SOB_LIST_EXIT);" nl
					"PUSH(R0);" nl
					"PUSH(FPARG(R7));" nl
					"CALL(MAKE_SOB_PAIR);" nl
					"DROP(2);" nl
					"DECR(R7);" nl
					"JUMP(MAKE_PAIRS);" nl nl
				
			"MAKE_SOB_LIST_EXIT:" nl
			"POP(FP);" nl
			"RETURN;" nl
			"FUNCTIONS_END:"nl nl
		)
	)
)

(define ^label-print-exit (^^label "L_print_exit"))
(define void?
	(lambda (e)
		(eq? (void) e)
	)
)

(define construct-sexprs-code
	(lambda (compiled-sexprs)
		(apply string-append
				(map 
					(lambda (e)
						(let ((label-print-exit (^label-print-exit)))
						(string-append
							(^^comment "SEXPR") nl nl
							
							(code-gen e 0 0)
							
							(^^comment "END-OF-SEXPR") nl nl
							(^^comment "PRINT R0") nl
							"CMP(R0, SOB_VOID);" nl
							(format "JUMP_EQ(~a);" label-print-exit) nl
							
							"PUSH (R0);" nl
							"CALL (WRITE_SOB);" nl
							"DROP(1);" nl
							"PUSH (IMM(10));" nl
							"CALL (PUTCHAR);" nl
							"DROP(1);" nl nl
							
							label-print-exit ":" nl
						)))
				compiled-sexprs)
		)
	)
)

; Compile --> i-gen-code --> Code-gen
(define go 
	(lambda (filename)
		(let* ((sexprs (file->sexpres filename))
			(compiled-sexprs 
					(map (lambda (e) (compile e)) sexprs))
			(data-structures (i-gen-code))
			(sexprs-code (construct-sexprs-code compiled-sexprs))
			)
		
			(string-append			
				nl
				"JUMP(LRuntime_support_exit);" nl nl
				(make-runtime-support)
				"LRuntime_support_exit:" nl nl
				data-structures
				sexprs-code
				(make-functions)
			)
		)
	)
)
					
					
(define compile-scheme-file
	(lambda (in out)
		(write-to-cisc-file (go in) out)
		(write-makefile out)
	)
)
		
(define compile
	(lambda (e)
		(annotate-tc  
		(pe->lex-pe (parse e))
		)
	))
		
(define goshow
	(lambda (e)
		(display (go e))))
		
(define gowrite
	(lambda (e out)
		(write-to-cisc-file (go e) out)))

		
; ###################################### TOP-LEVEL (CONSTS, FVARS, STRING-TABLE ######################################		
		
(define exp-topo-sort
  (lambda (e)
    (cond 
      ( (or (null? e) (boolean? e) (void? e) (number? e) (char? e) (string? e)) `(,e))
      ( (symbol? e) `(,@(exp-topo-sort (symbol->string e)) ,e))
      ( (pair? e) `(,@(exp-topo-sort (car e)) ,@(exp-topo-sort (cdr e)) ,e))
      ( (vector? e) `(,@(apply append (map exp-topo-sort (vector->list e))) ,e))
      ( else (error " exp-topo-sort error.")))))


(define address! 1)
(define string-address! 100002)
(define free-list! '())
(define exprs! '())
(define strings! '())
(define free-address! 200002)



(define str-tbl-lookup 
  (lambda (s) 
		(let ((ans (assoc s strings!)))
		(if ans
			(cdr ans)
			(error 'str-tbl-lookup (format "string ~a was not found" s))))
  )
)

(define exprs-tbl-lookup 
  (lambda (s) 
		(let ((ans (assoc s exprs!)))
		(if ans
			(cdr ans)
			(error 'exprs-tbl-lookup (format "expr ~a was not found" s))))
  )
)

(define make-null
  (lambda ()
    (let ((obj (assoc '() exprs!)))   
       (if obj
           (string-append "MOV(IND(" (number->string (cdr obj)) "),IMM(T_NIL));\n")
           (begin 
             (set! exprs! (cons (cons '() address!) exprs!)) 
             (let ((ans
             (string-append "MOV(IND(" (number->string address!) "),IMM(T_NIL));\n")))
               (set! address! (+ 1 address!))  
               ans)
        )
       )
    )
  )
)

(define make-void
  (lambda ()
    (let ((obj (assoc (void) exprs!)))    
       (if obj
           (string-append "MOV(IND(" (number->string (cdr obj)) "),IMM(T_VOID));\n")
           (begin 
             (set! exprs! (cons (cons (void) address!) exprs!)) 
             (let ((ans
             (string-append "MOV(IND(" (number->string address!) "),IMM(T_VOID));\n")))
               (set! address! (+ 1 address!))  
               ans)
        ))
    )
  )
)

(define make-bool
  (lambda (e)
    (let 
        ((obj (assoc e exprs!))
         (torf (if e 1 0)
          )       
      )
       (if obj
           (string-append
            "MOV(IND(" (number->string (cdr obj)) "),IMM(T_BOOL)); \n" 
            "MOV(IND(" (number->string (+ 1 (cdr obj))) "),IMM(" (number->string torf)"));\n"
           )
           (begin 
             (set! exprs! (cons (cons e address!) exprs!)) 
             (let ((ans (string-append "MOV(IND(" (number->string address!) "),IMM(T_BOOL)); \n" 
                            "MOV(IND(" (number->string (+ 1 address!)) "),IMM(" (number->string torf)"));\n" 
                            )))
               (set! address! (+ 2 address!))  
               ans)
        ))
    )
  )
)

(define make-number
  (lambda (e)
    (let 
        ((obj (assoc e exprs!)))       
       (if obj
           (string-append 
            "MOV(IND(" (number->string (cdr obj)) "),IMM(T_INTEGER));\n"
            "MOV(IND(" (number->string (+ 1 (cdr obj))) "),IMM(" (number->string (car obj))"));\n"
)
           (begin 
             (set! exprs! (cons (cons e address!) exprs!)) 
             (let ((ans (string-append "MOV(IND(" (number->string address!) "),IMM(T_INTEGER));\n" 
                            "MOV(IND(" (number->string (+ 1 address!)) "),IMM(" (number->string e)"));\n" 
                            )))
               (set! address! (+ 2 address!))  
               ans)
        ))
    )
  )
)

(define make-char
  (lambda (e)
    (let 
        ((obj (assoc e exprs!)))       
       (if obj
           (string-append 
             "MOV(IND(" (number->string (cdr obj)) "),IMM(T_CHAR));\n" 
             "MOV(IND(" (number->string (+ 1 (cdr obj))) "),IMM(" (number->string (char->integer (car obj)))"));\n"
            )
           (begin 
             (set! exprs! (cons (cons e address!) exprs!)) 
             (let ((ans (string-append "MOV(IND(" (number->string address!) "),IMM(T_CHAR));\n" 
                            "MOV(IND(" (number->string (+ 1 address!)) "),IMM(" (number->string (char->integer e))"));\n" 
                            )))
               (set! address! (+ 2 address!))  
               ans)
        ))
    )
  )
)

(define make-string-helper
  (lambda (charlist address)
   (letrec ((orig-addr address!) (foo (lambda (charlist address) 
     
      (if (null? charlist)
          "" 
          (string-append
            "MOV(IND(" (number->string address) "),IMM(" (number->string (char->integer (car charlist))) "));\n"
            (make-string-helper (cdr charlist) (+ address 1)))
          )
      
    )))
     (foo charlist address))
  )
)

(define t-make-string
  (lambda (e)
    (let 
        ((obj (assoc e strings!)))
      (if obj
          (string-append 
             "MOV(IND(" (number->string (cdr obj)) "),IMM(T_STRING));\n" 
             "MOV(IND(" (number->string (+ 1 (cdr obj))) "),IMM(" (number->string (string-length e)) "));\n"
             (make-string-helper (string->list (car obj)) (+ (cdr obj) 2))
           )
          (begin 
            (set! strings! (cons (cons e string-address!) strings!))
            (let ((ans (string-append  "MOV(IND(" (number->string string-address!) "),IMM(T_STRING));\n"
                                       "MOV(IND(" (number->string (+ string-address! 1)) "),IMM(" (number->string (string-length e)) "));\n"
                   (make-string-helper (string->list e) (+ string-address! 2))
                 )))
              (set! string-address! (+ (string-length e) 2 string-address!))
              ans
            )
         )
      )
     )
  )
)

(define make-pair
  (lambda (e)
    (let 
        ((obj (assoc e exprs!)))
      (if obj
         (string-append 
             "MOV(IND(" (number->string (cdr obj)) "),IMM(T_PAIR));\n" 
             "MOV(IND(" (number->string (+ 1 (cdr obj))) "),IMM(" (number->string (cdr (assoc (car e) exprs!)))"));\n"
             "MOV(IND(" (number->string (+ 2 (cdr obj))) "),IMM(" (number->string (cdr (assoc (cdr e) exprs!)))"));\n"
            )
          (begin 
            (set! exprs! (cons (cons e address!) exprs!))
            (let ((ans (string-append  "MOV(IND(" (number->string address!) "),IMM(T_PAIR));\n"
                                       "MOV(IND(" (number->string (+ address! 1)) "),IMM(" (number->string (cdr (assoc (car e) exprs!))) "));\n"
                                       "MOV(IND(" (number->string (+ address! 2)) "),IMM(" (number->string (cdr (assoc (cdr e) exprs!))) "));\n" 
                 )))
              (set! address! (+ 3 address!))
              ans
            )
         )
      )
     )
  )
)

(define make-vector-helper
  (lambda (exprlist address)
   (letrec ((orig-addr address!) (foo (lambda (exprlist address)    
      (if (null? exprlist)
          "" 
          (string-append
            "MOV(IND(" (number->string address) "),IMM(" (number->string (cdr (assoc (car exprlist) exprs!))) "));\n"
            (make-vector-helper (cdr exprlist) (+ address 1)))
          )
    )))
     (foo exprlist address))
  )
)

(define t-make-vector
  (lambda (e)
    (let 
        ((obj (assoc e exprs!)))
      (if obj
          (string-append 
             "MOV(IND(" (number->string (cdr obj)) "),IMM(T_VECTOR));\n" 
             "MOV(IND(" (number->string (+ 1 (cdr obj))) "),IMM(" (number->string (vector-length e)) "));\n"
             (make-vector-helper (vector->list (car obj)) (+ (cdr obj) 2))
           )
          (begin 
            (set! exprs! (cons (cons e address!) exprs!))
            (let ((ans (string-append  "MOV(IND(" (number->string address!) "),IMM(T_VECTOR));\n"
                                       "MOV(IND(" (number->string (+ address! 1)) "),IMM(" (number->string (vector-length e)) "));\n"
                   (make-vector-helper (vector->list e) (+ address! 2))
                 )))
              (set! address! (+ (vector-length e) 2 address!))
              ans
            )
         )
      )
     )
  )
)

(define make-symbol
  (lambda (e)
    (let 
        ((obj (assoc e exprs!)))
      (if obj
          (string-append 
             "MOV(IND(" (number->string (cdr obj)) "),IMM(T_SYMBOL));\n" 
             "MOV(IND(" (number->string (+ 1 (cdr obj))) "),IMM(" (number->string (str-tbl-lookup (symbol->string e))) "));\n"
           )
          (begin 
            (set! exprs! (cons (cons e address!) exprs!))
            (let ((ans (string-append  "MOV(IND(" (number->string address!) "),IMM(T_SYMBOL));\n"
                                       "MOV(IND(" (number->string (+ address! 1)) "),IMM(" (number->string (str-tbl-lookup (symbol->string e))) "));\n"                         
                 )))
              (set! address! (+ 2 address!))
              ans
            )
         )
      )
     )
  )
)



(define make-const 
  (lambda (e) 
      (cond 
        ( (null? e) `(,@(make-null)) )
        ( (void? e) `(,@(make-void)) )
        ( (boolean? e) `(,@(make-bool e)) )
        ( (char? e) `(,@(make-char e)) )
        ( (number? e) `(,@(make-number e)) )
        ( (string? e) `(,@(t-make-string e)) )
        ( (symbol? e) `(,@(make-symbol e)) )
        ( (pair? e) `(,@(make-pair e) ))
        ( (vector? e) `(,@(t-make-vector e)) )
      )
  )
) 

(define make-free
  (lambda (fv)
    (let ((ans
    (string-append "MOV (IND(IMM(" (number->string free-address!) ")),IMM(" (number->string (str-tbl-lookup (symbol->string fv))) "));\n"
                   "MOV (IND(IMM(" (number->string (+ free-address! 1)) ")),IMM(0));\n"
                   )))
      (set! free-address! (+ free-address! 2))
      ans
    )
  )
)

(define make-free-helper 
  (lambda (l)
    (if (null? l) 
        ""
        (string-append (make-free (car l)) (make-free-helper (cdr l)))
    )
  )
)

(define const-list! '())

;this will take const-list! after all consts are inserted and return the code gen for the constants)
(define i-make-frees
  (lambda ()
    (set! free-address! 200002)
    (make-free-helper (list->set free-list!))
  )
)

;gets a topo list of sub exprs and generates the code that initializes the consts.
(define make-consts-helper 
  (lambda (l)
    (if (null? l) 
        ""
        (string-append (make-const (car l)) (make-consts-helper (cdr l)))
    )
  )
)	

;this will take free-list! after all free vars are inserted and return the)
(define i-make-consts
  (lambda ()
	(let ((c-set (list->set const-list!)))
		(make-consts-helper c-set)
	))
)

;add free vars while traversing the code.
(define i-add-to-free-table
    (lambda (l)
         (begin
           (set! free-list! (cons l free-list!))
           (set! const-list! (append const-list! (exp-topo-sort l)))
         )
    )
)

;add consts while traversing the code.
(define i-add-to-const-list
    (lambda (l)
         (set! const-list! (append const-list! (exp-topo-sort l)))
    )
)
  
(define list->set-helper 
  (lambda (l s)
    (if (null? l)
        s
        (if (member (car l) s)
            (list->set-helper (cdr l) s)
            (list->set-helper (cdr l) (cons (car l) s)))
    )
  )
)

(define list->set
  (lambda (l)
    (reverse (list->set-helper l '()))
  )
)

(define i-make-super-consts
  (lambda ()
    (string-append 
      "MOV (IND(100000),IMM(100002));\n"
      "MOV (IND(100001),IMM(" 
      (number->string string-address!)
      "));\n"
      
      "MOV (IND(200000),IMM(200002));\n"
      "MOV (IND(200001),IMM(" 
      (number->string free-address!)
      "));\n"   
      
      "#define SOB_UNDEF 7109179\n"
      ; "MOV(IND(0),SOB_UNDEF);\n"
      "#define SOB_VOID 1\n"
      "#define SOB_NIL 2\n"
      "#define SOB_TRUE 3\n"
      "#define SOB_FALSE 5\n"
    )
  )
)
      
(define library-funcs '(apply < =  >  +  /  *  - boolean? car cdr char->integer char? cons eq? integer?
integer->char make-string make-vector null? number? pair? procedure?
remainder set-car! set-cdr! string-length string-ref string-set! string->symbol
string? symbol? symbol->string vector-length vector-ref vector-set!
vector? zero? map ym reverse))

(define i-make-library
  (lambda ()
      (letrec ((helper (lambda (library)  
        (if (not (null? library))
            (begin (i-add-to-free-table (car library)) (helper (cdr library))) 
            ""
        )
       )))
        (helper library-funcs)
        )
  )
)

(define i-make-library-symbols
  (lambda ()
      (letrec ((helper (lambda (library)  
        (if (not (null? library))
            (begin (i-add-to-const-list (car library)) (helper (cdr library))) 
            ""
        )
       )))
        (helper library-funcs)
        )
  )
)

(define func-address! 300000)


(define gen-vector?
  (lambda ()
    (let ((ans
           (string-append
            "JUMP(TvectorExit);\n"
"TvectorStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_VECTOR);\n"
"	JUMP_EQ (TvectorYes);\n"
"TvectorNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TvectorClean);\n"
"TvectorYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TvectorClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TvectorExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'vector? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TvectorStart));\n"
"\n"
)))
   (set! func-address! (+ 3 func-address!))
      ans)            
  )
)

(define gen-symbol?
  (lambda ()
    (let ((ans
           (string-append
            "JUMP(TsymbolExit);\n"
"TsymbolStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_SYMBOL);\n"
"	JUMP_EQ(TsymbolYes);\n"
"TsymbolNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP(TsymbolClean);\n"
"TsymbolYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TsymbolClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TsymbolExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'symbol? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TsymbolStart));\n"
)))
        (set! func-address! (+ 3 func-address!))
      ans)
  )
)

(define gen-string?
  (lambda ()
    (let ((ans
           (string-append
            "JUMP(TstringExit);\n"
"TstringStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_STRING);\n"
"	JUMP_EQ(TstringYes);\n"
"TstringNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TstringClean);\n"
"TstringYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TstringClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TstringExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'string? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TstringStart));\n"
)))
          (set! func-address! (+ 3 func-address!))
      ans)
  )
)


(define gen-procedure?
    (lambda ()
    (let ((ans
           (string-append
            "JUMP(TProcExit);\n"
            "TProcStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_CLOSURE);\n"
"	JUMP_EQ (TProcYes);\n"
"TProcNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TProcClean);\n"
"TProcYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TProcClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TProcExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'procedure? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TProcStart));\n"
"\n"
)))
       (set! func-address! (+ 3 func-address!))
      ans)
      
  )
)

(define gen-pair?
  (lambda ()
    (let ((ans
           (string-append
            "JUMP(TpairExit);\n"
            "TpairStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_PAIR);\n"
"	JUMP_EQ (TpairYes);\n"
"TpairNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TpairClean);\n"
"TpairYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TpairClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TpairExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'pair? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TpairStart));\n"
"\n"
)))
       (set! func-address! (+ 3 func-address!))
      ans)
      
  )
)

(define gen-number?
  (lambda ()
    (let ((ans
      (string-append         
      

    "JUMP(TnumberExit);	\n"
"TnumberStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_INTEGER);\n"
"	JUMP_EQ(TnumberYes);\n"
"TnumberNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TnumberClean);\n"
"TnumberYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TnumberClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TnumberExit:	\n"
"PUSH("  
          (number->string (cdr (assoc 'number? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TnumberStart));\n"
"\n"
) ))
      (set! func-address! (+ 3 func-address!))
      ans)
  )
)

(define gen-null?
  (lambda ()
    (let ((ans
           (string-append
            "JUMP(TnullExit);\n"
            "TnullStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_NIL);\n"
"	JUMP_EQ (TnullYes);\n"
"TnullNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TnullClean);\n"
"TnullYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TnullClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TnullExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'null? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TnullStart));\n"
"\n"
)))
      (set! func-address! (+ 3 func-address!))
      ans)
  )
)


(define gen-integer?
  (lambda ()
    (let ((ans 
           (string-append
            "JUMP(TintegerExit);\n"
            "TintegerStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_INTEGER);\n"
"	JUMP_EQ (TintegerYes);\n"
"TintegerNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TintegerClean);\n"
"TintegerYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TintegerClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"	\n"
"TintegerExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'integer? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TintegerStart));\n"
"\n"
)))
    (set! func-address! (+ 3 func-address!))
      ans)  
  )
)


(define gen-char?
  (lambda ()
    (let ((ans
           (string-append
            "JUMP(TcharExit);\n"
"TcharStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_CHAR);\n"
"	JUMP_EQ (TcharYes);\n"
"TcharNo:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP (TcharClean);\n"
"TcharYes:\n"
"	MOV(R0,SOB_TRUE);\n"
"TcharClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TcharExit:	\n"
"PUSH("  
          (number->string (cdr (assoc 'char? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TcharStart));\n"
) ))
      (set! func-address! (+ 3 func-address!))
      ans)
  )
)


(define gen-cdr
  (lambda ()
    (let ((ans
           (string-append
"\n"
"JUMP(TcdrExit);\n"
"TcdrStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R0,FPARG(2));\n"
"	MOV(R0,INDD(R0,2));\n"
"TcdrClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TcdrExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'cdr exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TcdrStart));\n"
"\n"
)))
   (set! func-address! (+ 3 func-address!))
      ans)
  )
)

(define gen-car
  (lambda ()
    (let ((ans
           (string-append
"\n"
"JUMP(TcarExit);\n"
"TcarStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R0,FPARG(2));\n"
"	MOV(R0,INDD(R0,1));\n"
"TcarClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TcarExit:\n"
"PUSH("  
          (number->string (cdr (assoc 'car exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TcarStart));\n"
"\n"
)))
   (set! func-address! (+ 3 func-address!))
      ans)

  )
)


(define gen-apply
(lambda ()
(let ((ans
(string-append
"JUMP(TApplyExit);\n"
"// F(2) - f, F(3) - s\n"
"TApplyStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	// proc\n"
"	CMP(IND(FPARG(2)),T_CLOSURE);\n"
" 	JUMP_NE(TApplyNotProc);\n"
"	/* back up important */" nl
"	PUSH(IMM(4));\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R3,R0);\n"
"	MOV(INDD(R3,0),FPARG(2)); // proc\n"
"	MOV(INDD(R3,1),FPARG(0)); // env\n"
"	MOV(INDD(R3,2),FPARG(-1)); // ret\n"
"	MOV(INDD(R3,3),FPARG(-2)); // FP\n"
"	PUSH(R3);" nl

"	MOV(R1,FPARG(3)); // list\n"
"	PUSH(R1);" nl
"	PUSH(IMM(1));" nl	
"	PUSH(IMM(5555));" nl
"	CALL(TreverseStart);" nl
"	DROP(3);" nl
"	POP(R3);" nl

"	MOV(R1,R0);" nl

"	// clean up stack (FP+ret+env+#args+f+s)\n"
"	DROP(6);\n"
"	MOV(R7,0);\n"
"	TApplyLoop:\n"
"	CMP(R1,SOB_NIL);\n"
"	JUMP_EQ(TApplyClean);\n"

"	CMP(IND(R1),T_PAIR);\n"
"	JUMP_NE(TApplyNotPair);\n"
"		PUSH(INDD(R1,1));\n"
"		INCR(R7);\n"
"		MOV(R1,INDD(R1,2));\n"
"		JUMP(TApplyLoop);\n"
"TApplyNotProc:\n"
"	SHOW(\"Error in Apply: Not A Procedure!\",FPARG(2));\n"
"	JUMP(Lprogram_exit);\n"
"TApplyNotPair:\n"
"	SHOW(\"Error in Apply: Not A Pair!\",INDD(R1,R7));\n"
"	JUMP(Lprogram_exit);\n"
"TApplyClean:\n"
"	PUSH(R7); // #args\n"
"	PUSH(INDD(R3,1)); // env\n"
"	PUSH(INDD(R3,2)); // previous ret address\n"
"	JUMPA(INDD(INDD(R3,0),2));\n"
"TApplyExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'apply exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TApplyStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))





(define gen-boolean?
  (lambda ()
    (let ((ans
      (string-append
          "JUMP (TbooleanExit);\n"
          "TbooleanStart:\n"
          "	PUSH(FP);\n"
          "	MOV(FP,SP);\n"
          "	MOV(R15,FPARG(2));\n"
          "	CMP(IND(R15),IMM(T_BOOL));\n"
          "	JUMP_EQ(TbooleanYes);\n"
          "TbooleanNo:\n"
          "	MOV(R0,IMM(SOB_FALSE));\n"
          "	JUMP (TbooleanClean);\n"
          "TbooleanYes:\n"
          "	MOV(R0,SOB_TRUE);\n"
          "TbooleanClean:\n"
          "	POP(FP);\n"
          "	RETURN;\n"
          "TbooleanExit:\n"
          "PUSH("  
          (number->string (cdr (assoc 'boolean? exprs!)))
          ");\n"
          "CALL(Tlookup);\n"
		"DROP(IMM(1));\n"
          "/* address of funcion in the free table is now in R0 */\n"
          "MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
          "MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
	        "MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
	        "MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TbooleanStart));\n"
      ) ))
      (set! func-address! (+ 3 func-address!))
      ans)
  )
)

; ######### FROM FUNCTIONS.SCM #########

(define gen-integer->char
(lambda ()
(let ((ans
(string-append
"JUMP(TintegerToCharExit);\n"
"TintegerToCharStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	PUSH(IMM(2));\n"
"	CALL(MALLOC);\n"
"	DROP(IMM(1));\n"
"	MOV(IND(R0),T_CHAR);\n"
"	MOV(INDD(R0,1),INDD(R15,1));\n"
"TintegerToCharClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TintegerToCharExit:\n"
"	\n"
"PUSH(IMM( " (number->string (cdr (assoc 'integer->char exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TintegerToCharStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-char->integer
(lambda ()
(let ((ans
(string-append
"JUMP(TcharToIntegerExit);\n"
"TcharToIntegerStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	PUSH(IMM(2));\n"
"	CALL(MALLOC);\n"
"	DROP(IMM(1));\n"
"	MOV(IND(R0),T_INTEGER);\n"
"	MOV(INDD(R0,1),INDD(R15,1));\n"
"TcharToIntegerClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TcharToIntegerExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'char->integer exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TcharToIntegerStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-cons
(lambda ()
(let ((ans
(string-append
"JUMP(TconsExit);\n"
"TconsStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	PUSH(IMM(3));\n"
"	CALL(MALLOC);\n"
"	DROP(IMM(1));\n"
"	MOV(IND(R0),T_PAIR);\n"
"	MOV(INDD(R0,1),FPARG(2));\n"
"	MOV(INDD(R0,2),FPARG(3));\n"
"TconsClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TconsExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'cons exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TconsStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-zero?
(lambda ()
(let ((ans
(string-append
"JUMP(TzeroExit);\n"
"TzeroStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	CMP(IND(R15),T_INTEGER);\n"
"	JUMP_NE(TzeroFalse);\n"
"	CMP(INDD(R15,1),IMM(0));\n"
"	JUMP_NE(TzeroFalse);\n"
"	MOV(R0,SOB_TRUE);\n"
"	JUMP(TzeroClean);\n"
"TzeroFalse:\n"
"	MOV(R0,SOB_FALSE);\n"
"TzeroClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TzeroExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'zero? exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TzeroStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-set-car!
(lambda ()
(let ((ans
(string-append
"JUMP(TsetcarExit);\n"
"TsetcarStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	MOV(INDD(R15,1),FPARG(3));\n"
"TsetcarClean:\n"
"	MOV(R0, SOB_VOID);" nl
"	POP(FP);\n"
"	RETURN;\n"
"TsetcarExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'set-car! exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TsetcarStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-set-cdr!
(lambda ()
(let ((ans
(string-append
"JUMP(TsetcdrExit);\n"
"TsetcdrStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	MOV(INDD(R15,2),FPARG(3));\n"
"TsetcdrClean:\n"
"	MOV(R0, SOB_VOID);" nl
"	POP(FP);\n"
"	RETURN;\n"
"TsetcdrExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'set-cdr! exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TsetcdrStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-string-length
(lambda ()
(let ((ans
(string-append
"JUMP(TstrlengthExit);\n"
"TstrlengthStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,INDD(FPARG(2),1) );\n"
"	PUSH(IMM(2));\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(IND(R0),T_INTEGER);\n"
"	MOV(INDD(R0,1),R15);\n"
"TstrlengthClean:	\n"
"	POP(FP);\n"
"	RETURN;\n"
"TstrlengthExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'string-length exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TstrlengthStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-vector-length
(lambda ()
(let ((ans
(string-append
"JUMP(TvectorlengthExit);\n"
"TvectorlengthStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,INDD(FPARG(2),1) );\n"
"	PUSH(IMM(2));\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(IND(R0),T_INTEGER);\n"
"	MOV(INDD(R0,1),R15);\n"
"TvectorlengthClean:	\n"
"	POP(FP);\n"
"	RETURN;\n"
"TvectorlengthExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'vector-length exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TvectorlengthStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-remainder
(lambda ()
(let ((ans
(string-append
"JUMP(TremainderExit);\n"
"TremainderStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	PUSH(IMM(2));\n"
"	CALL(MALLOC);\n"
"	DROP(IMM(1));\n"
"	MOV(R15,R0);\n"
"	MOV(IND(R15),T_INTEGER);\n"
"	MOV(R14,INDD(FPARG(2),1)); //dividened\n"
"	MOV(R13,INDD(FPARG(3),1)); //divisor\n"
"	MOV(R12,R14);\n"
"	DIV(R14,R13);\n"
"	MUL(R14,R13);\n"
"	SUB(R12,R14);\n"
"TremainderCleanup:\n"
"	MOV(INDD(R15,1),R12);\n"
"	MOV(R0,R15);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TremainderExit:	\n"
"PUSH(IMM( " (number->string (cdr (assoc 'remainder exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TremainderStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))




(define gen-reverse
(lambda ()
(let ((ans
(string-append
"JUMP(TreverseExit);\n"
"TreverseStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R12,SOB_NIL);\n"
"	MOV(R11,SOB_NIL);\n"
"	MOV(R15,FPARG(2));\n"
"	MOV(R14,0); //counter\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TreverseEmpty);\n"
"TreverseLoopPush:\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TreverseMalloc);\n"
"	PUSH(INDD(R15,1));\n"
"	INCR(R14);\n"
"	MOV(R15,INDD(R15,2));\n"
"	JUMP(TreverseLoopPush);\n"
"TreverseMalloc:\n"
"	MOV(R11,3);\n"
"	MUL(R11,3);\n"
"	PUSH(R11);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R11,R0);\n"
"	MOV(R12,R0);\n"
"TreverseLoopPop:\n"
"	CMP(R14,0);\n"
"	JUMP_EQ(TreverseFixLast);		\n"
"	POP(R13);\n"
"	MOV(INDD(R12,0),T_PAIR);\n"
"	MOV(INDD(R12,1),R13);\n"
"	MOV(INDD(R12,2),R12+3);		\n"
"	ADD(R12,3);\n"
"	DECR(R14);	\n"
"	JUMP(TreverseLoopPop);\n"
"TreverseEmpty:\n"
"	MOV(R11,SOB_NIL);\n"
"	JUMP(TreverseClean);\n"
"TreverseFixLast:\n"
"	MOV(INDD(R12,-1),2);\n"
"TreverseClean:\n"
"	MOV(R0,R11);  \n"
"	POP(FP);\n"
"	RETURN;\n"
"TreverseExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'reverse exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TreverseStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))




(define gen-+
(lambda ()
(let ((ans
(string-append
"JUMP(TplusExit);\n"
"TplusStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-var)
"	MOV(R15,FPARG(2)); \n"
"	PUSH(2);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R14,R0);\n"
"	MOV(INDD(R14,0),T_INTEGER);\n"
"	MOV(INDD(R14,1),IMM(0));	\n"
"TplusLoop:\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TplusClean);\n"
"	ADD(INDD(R14,1),INDD(INDD(R15,1),1));\n"
"	MOV(R15,INDD(R15,2));\n"
"	JUMP(TplusLoop);\n"
"TplusClean:\n"
"	MOV(R0,R14);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TplusExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc '+ exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TplusStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-*
(lambda ()
(let ((ans
(string-append
"JUMP(TtimesExit);\n"
"TtimesStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-var)
"	MOV(R15,FPARG(2)); \n"
"	PUSH(2);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R14,R0);\n"
"	MOV(INDD(R14,0),T_INTEGER);\n"
"	MOV(INDD(R14,1),IMM(1));	\n"
"TtimesLoop:\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TtimesClean);\n"
"	MUL(INDD(R14,1),INDD(INDD(R15,1),1));\n"
"	MOV(R15,INDD(R15,2));\n"
"	JUMP(TtimesLoop);\n"
"TtimesClean:\n"
"	MOV(R0,R14);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TtimesExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc '* exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TtimesStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen--
(lambda ()
(let ((ans
(string-append
"JUMP(TminusExit);\n"
"TminusStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-var)
"	MOV(R15,FPARG(2)); //list\n"
"	PUSH(2);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R14,R0); //output\n"
"	MOV(INDD(R14,0),T_INTEGER);\n"
"	MOV(INDD(R14,1),IMM(1));\n"
"	//list is empty	\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TminusErrorNull);\n"
"	//list's length is 1.\n"
"	MOV(R13,R15);\n"
"	MOV(R15,INDD(R15,2));\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TminusUnary);\n"
"	MOV(INDD(R14,1),INDD(INDD(R13,1),1));	\n"
"TminusLoop:\n"
"	SUB(INDD(R14,1),INDD(INDD(R15,1),1));\n"
"	MOV(R15,INDD(R15,2));\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TminusClean);\n"
"	JUMP(TminusLoop);\n"
"TminusErrorNull:\n"
"	//error\n"
"	SHOW(\"in function - : illegal number of arguments\",FPARG(2));\n"
"	POP(FP);\n"
"	RETURN;\n"
"TminusUnary:\n"
"	MOV(INDD(R14,1),INDD(INDD(R13,1),1));\n"
"	MUL(INDD(R14,1),IMM(-1));\n"
"TminusClean:\n"
"	MOV(R0,R14);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TminusExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc '- exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TminusStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-/
(lambda ()
(let ((ans
(string-append
"JUMP(TdivExit);\n"
"TdivStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-var)
"	MOV(R15,FPARG(2)); //list\n"
"	PUSH(2);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R14,R0); //output\n"
"	MOV(INDD(R14,0),T_INTEGER);\n"
"	MOV(INDD(R14,1),IMM(1));\n"
"	//list is empty	\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TdivErrorNull);\n"
"	//list's length is 1.\n"
"	MOV(R13,R15);\n"
"	MOV(R15,INDD(R15,2));\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TdivUnary);\n"
"	MOV(INDD(R14,1),INDD(INDD(R13,1),1));	\n"
"TdivLoop:\n"
"	DIV(INDD(R14,1),INDD(INDD(R15,1),1));\n"
"	MOV(R15,INDD(R15,2));\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TdivClean);\n"
"	JUMP(TdivLoop);\n"
"TdivErrorNull:\n"
"	//error\n"
"	SHOW(\"in function / : illegal number of arguments\",FPARG(2));\n"
"	POP(FP);\n"
"	RETURN;\n"
"TdivUnary:\n"
"	MOV(INDD(R14,1),IMM(1));\n"
"	DIV(INDD(R14,1),INDD(R13,1));\n"
"TdivClean:\n"
"	MOV(R0,R14);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TdivExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc '/ exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TdivStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-=
(lambda ()
(let ((ans
(string-append
"JUMP(TequalExit);\n"
"TequalStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-var)
"	MOV(R15,FPARG(2)); //list\n"
"	//list is empty	\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TequalErrorNull);\n"
"	//list's length is 1.\n"
"	MOV(R13,INDD(R15,1)); //first number R13\n"
"	MOV(R15,INDD(R15,2));\n"
"TequalLoop:\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TequalTrue);\n"
"	CMP(INDD(R13,1),INDD(INDD(R15,1),1));\n"
"	JUMP_NE(TequalFalse);\n"
"	MOV(R13,INDD(R15,1));\n"
"	MOV(R15,INDD(R15,2));\n"
"	JUMP(TequalLoop);\n"
"TequalErrorNull:\n"
"	//error\n"
"	SHOW(\"in function = : illegal number of arguments\",FPARG(2));\n"
"	JUMP(TequalClean);\n"
"TequalFalse:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP(TequalClean);\n"
"TequalTrue:\n"
"	MOV(R0,SOB_TRUE);\n"
"TequalClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TequalExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc '= exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TequalStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen->
(lambda ()
(let ((ans
(string-append
"JUMP(TbiggerExit);\n"
"TbiggerStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-var)
"	MOV(R15,FPARG(2)); //list\n"
"	//list is empty	\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TbiggerErrorNull);\n"
"	//list's length is 1.\n"
"	MOV(R13,INDD(R15,1)); //first number R13\n"
"	MOV(R15,INDD(R15,2));\n"
"TbiggerLoop:\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TbiggerTrue);\n"
"	CMP(INDD(R13,1),INDD(INDD(R15,1),1));\n"
"	JUMP_LE(TbiggerFalse);\n"
"	MOV(R13,INDD(R15,1));\n"
"	MOV(R15,INDD(R15,2));\n"
"	JUMP(TbiggerLoop);\n"
"TbiggerErrorNull:\n"
"	//error\n"
"	SHOW(\"in function > : illegal number of arguments\",FPARG(2));\n"
"	JUMP(TbiggerClean);\n"
"TbiggerFalse:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP(TbiggerClean);\n"
"TbiggerTrue:\n"
"	MOV(R0,SOB_TRUE);\n"
"TbiggerClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TbiggerExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc '> exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TbiggerStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-<
(lambda ()
(let ((ans
(string-append
"JUMP(TsmallerExit);\n"
"TsmallerStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-var)
"	MOV(R15,FPARG(2)); //list\n"
"	//list is empty	\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TsmallerErrorNull);\n"
"	//list's length is 1.\n"
"	MOV(R13,INDD(R15,1)); //first number R13\n"
"	MOV(R15,INDD(R15,2));\n"
"TsmallerLoop:\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TsmallerTrue);\n"
"	CMP(INDD(R13,1),INDD(INDD(R15,1),1));\n"
"	JUMP_GE(TsmallerFalse);\n"
"	MOV(R13,INDD(R15,1));\n"
"	MOV(R15,INDD(R15,2));\n"
"	JUMP(TsmallerLoop);\n"
"TsmallerErrorNull:\n"
"	//error\n"
"	SHOW(\"in function < : illegal number of arguments\",FPARG(2));\n"
"	JUMP(TsmallerClean);\n"
"TsmallerFalse:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP(TsmallerClean);\n"
"TsmallerTrue:\n"
"	MOV(R0,SOB_TRUE);\n"
"TsmallerClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TsmallerExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc '< exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TsmallerStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-symbol->string
(lambda ()
(let ((ans
(string-append
"JUMP(TsymbolToStringExit);\n"
"TsymbolToStringStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(2));\n"
"	MOV(R0,INDD(R15,1));\n"
"	POP(FP);\n"
"	RETURN;\n"
"TsymbolToStringExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'symbol->string exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TsymbolToStringStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-string->symbol
(lambda ()
(let ((ans
(string-append
"JUMP(TstringToSymbolExit);\n"
"TstringToSymbolStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	PUSH(FPARG(2));\n"
"	CALL(Tstrlookup);\n"
"	DROP(1);\n"
"	MOV(R15,R0);\n"
"	PUSH(IMM(2));\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(INDD(R0,0),T_SYMBOL);\n"
"	MOV(INDD(R0,1),R15);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TstringToSymbolExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'string->symbol exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TstringToSymbolStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-make-string
(lambda ()
(let ((ans
(string-append
"JUMP(TmakeStringExit);\n"
"TmakeStringStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-opt 2)
"	MOV(R15,INDD(FPARG(2),1));\n"
"	ADD(R15,2); //string length\n"
"	PUSH(R15);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R14,R0); //string ptr\n"
"	MOV(INDD(R14,0),T_STRING);\n"
"	SUB(R15,2);\n"
"	MOV(INDD(R14,1),R15);\n"
"	CMP(FPARG(3),SOB_NIL);\n"
"	JUMP_EQ(TmakeStringClean);\n"
"	PUSH(R15);\n"
"	PUSH(FPARG(3));\n"
"	PUSH(1);\n"
"	PUSH(0);\n"
"	CALL(TcharToIntegerStart);\n"
"	DROP(3);\n"
"	POP(R15);\n"
"	MOV(R12,INDD(R0,1)); // ascii value.\n"
"	MOV(R13,0); //iterator	\n"
"TmakeStringLoop:\n"
"	CMP(R13,R15);\n"
"	JUMP_GE(TmakeStringClean);\n"
"	ADD(R13,2);\n"
"	MOV(INDD(R14,R13),R12);\n"
"	SUB(R13,1);\n"
"	JUMP(TmakeStringLoop);\n"
"TmakeStringClean:\n"
"	MOV(R0,R14);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TmakeStringExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'make-string exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TmakeStringStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-make-vector
(lambda ()
(let ((ans
(string-append
"JUMP(TmakeVectorExit);\n"
"TmakeVectorStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
(stack-correction-code-lambda-opt 2)
"	MOV(R15,INDD(FPARG(2),1));\n"
"	ADD(R15,2); //vector length\n"
"	PUSH(R15);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R14,R0); //vector ptr\n"
"	MOV(INDD(R14,0),T_VECTOR);\n"
"	SUB(R15,2);\n"
"	MOV(INDD(R14,1),R15);\n"
"	CMP(FPARG(3),SOB_NIL);\n"
"	JUMP_EQ(TmakeVectorClean);\n"
"	MOV(R13,0); //iterator	\n"
"TmakeVectorLoop:\n"
"	CMP(R13,R15);\n"
"	JUMP_GE(TmakeVectorClean);\n"
"	ADD(R13,2);\n"
"	MOV(INDD(R14,R13),FPARG(3));\n"
"	SUB(R13,1);\n"
"	JUMP(TmakeVectorLoop);\n"
"TmakeVectorClean:\n"
"	MOV(R0,R14);\n"
"	POP(FP);\n"
"	RETURN;\n"
"TmakeVectorExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'make-vector exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TmakeVectorStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-string-ref
(lambda ()
(let ((ans
(string-append
"JUMP(TstringRefExit);\n"
"TstringRefStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,INDD(FPARG(3),1));\n"
"	ADD(R15,IMM(2));\n"
"	MOV(R14,INDD(FPARG(2),R15));\n"
"	//r14 contains the ascii num.\n"
"	PUSH(IMM(2));\n"
"	CALL(MALLOC);\n"
"	DROP(IMM(1));\n"
"	MOV(IND(R0),T_CHAR);\n"
"	MOV(INDD(R0,1),R14);\n"
"TstringRefClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TstringRefExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'string-ref exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TstringRefStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-vector-ref
(lambda ()
(let ((ans
(string-append
"JUMP(TvectorRefExit);\n"
"TvectorRefStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,INDD(FPARG(3),1));\n"
"	ADD(R15,IMM(2));\n"
"	MOV(R0,INDD(INDD(FPARG(2),R15),1));\n"
"	//r0 contains the ptr to the nth element.\n"
"TvectorRefClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TvectorRefExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'vector-ref exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TvectorRefStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-vector-set!
(lambda ()
(let ((ans
(string-append
"JUMP(TvectorSetExit);\n"
"TvectorSetStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,INDD(FPARG(3),1));\n"
"	ADD(R15,IMM(2));\n"
"	MOV(INDD(FPARG(2),R15),FPARG(4));\n"
"	MOV(R0,SOB_VOID);\n"
"TvectorSetClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TvectorSetExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'vector-set! exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TvectorSetStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-string-set!
(lambda ()
(let ((ans
(string-append
"JUMP(TstringSetExit);\n"
"TstringSetStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,INDD(FPARG(3),1));\n"
"	ADD(R15,IMM(2));\n"
"	MOV(INDD(FPARG(2),R15),INDD(FPARG(4),1));\n"
"	MOV(R0,SOB_VOID);\n"
"TstringSetClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TstringSetExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'string-set! exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TstringSetStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-eq?
(lambda ()
(let ((ans
(string-append
"JUMP(TeqExit);\n"
"TeqStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	CMP(IND(FPARG(2)),IND(FPARG(3)));\n"
"	JUMP_NE(TeqFalse);\n"
"	CMP(IND(FPARG(2)),T_SYMBOL);\n"
"	JUMP_EQ(TeqDeep);\n"
"	CMP(IND(FPARG(2)),T_CHAR);\n"
"	JUMP_EQ(TeqDeep);\n"
"	CMP(IND(FPARG(2)),T_INTEGER);\n"
"	JUMP_EQ(TeqDeep);\n"
"	CMP(IND(FPARG(2)),T_BOOL);\n"
"	JUMP_EQ(TeqDeep);\n"
"	CMP(FPARG(2),FPARG(3));\n"
"	JUMP_EQ(TeqTrue);	\n"
"	JUMP(TeqFalse);\n"
"TeqDeep:\n"
"	CMP(INDD(FPARG(2),1),INDD(FPARG(3),1));\n"
"	JUMP_EQ(TeqTrue);	\n"
"	JUMP_EQ(TeqFalse);\n"
"TeqFalse:\n"
"	MOV(R0,SOB_FALSE);\n"
"	JUMP(TeqClean);	\n"
"TeqTrue:\n"
"	MOV(R0,SOB_TRUE);\n"
"TeqClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TeqExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'eq? exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TeqStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))

(define gen-map
(lambda ()
(let ((ans
(string-append
"JUMP(TmapExit);\n"
"TmapStart:\n"
"	PUSH(FP);\n"
"	MOV(FP,SP);\n"
"	MOV(R15,FPARG(3)); //list\n"
"	//list is empty	\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_EQ(TmapNull);\n"
"	MOV(R14,0); //counter\n"
"	MOV(R13,R15);\n"
"TmapSize:\n"
"	INCR(R14);\n"
"	MOV(R13,INDD(R13,2));\n"
"	CMP(R13,SOB_NIL);\n"
"	JUMP_NE(TmapSize);\n"
"	MUL(R14,3);\n"
"	PUSH(R14);\n"
"	CALL(MALLOC);\n"
"	DROP(1);\n"
"	MOV(R14,R0);\n"
"	MOV(R13,R0);\n"
"TmapLoop:\n"
"	CMP(IND(R15),T_PAIR);\n"
"	JUMP_NE(TmapError);\n"
"	//backup\n"
"	PUSH(R13); PUSH(R14); PUSH(R15);\n"
"	MAGIC_SPOT;" nl
"	PUSH(INDD(R15,1));\n"
"	PUSH(IMM(1));\n"
"	PUSH(IMM(5555));\n"
"	CALLA(INDD(FPARG(2),2));\n"
"	DROP(3+STARG(0));\n"
"	//restore\n"
"	POP(R15); POP(R14); POP(R13); \n"
"	MOV(INDD(R14,0),T_PAIR);\n"
"	MOV(INDD(R14,1),R0);\n"
"	MOV(INDD(R14,2),R14+3);		\n"
"	ADD(R14,3);\n"
"	MOV(R15,INDD(R15,2));\n"
"	CMP(R15,SOB_NIL);\n"
"	JUMP_NE(TmapLoop);		\n"
"TmapFixLast:\n"
"	MOV(INDD(R14,-1),2);\n"
"	MOV(R0,R13);		\n"
"	JUMP(TmapClean);\n"
"TmapNull:\n"
"	MOV(R0,SOB_NIL);\n"
"	JUMP(TmapClean);\n"
"TmapError:\n"
"	SHOW(\"not a pair\",R15);\n"
"TmapClean:\n"
"	POP(FP);\n"
"	RETURN;\n"
"TmapExit:\n"
"PUSH(IMM( " (number->string (cdr (assoc 'map exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TmapStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))


(define gen-ym
(lambda ()
(let ((ans
(string-append
"JUMP(TymExit);" nl
"TymStart:" nl
"/* Lambda-body */" nl
"Lbody_code_Ym1:" nl
"PUSH(FP);" nl
"MOV(FP,SP);" nl
"/* STACK CORRECTION */" nl
"/* Make list from extra parameters*/" nl
"/* FPARG(1) - actual params */" nl
"MOV(R7, FPARG(1)+1);" nl
"Lmake_params_list_Ym10:" nl
"CMP(R7, 1);" nl
"JUMP_LE(Lmake_params_list_exit_Ym10);" nl
"PUSH(FPARG(R7));" nl
"DECR(R7);" nl
"JUMP(Lmake_params_list_Ym10);" nl
"Lmake_params_list_exit_Ym10:" nl
"PUSH(FPARG(1));" nl
"CALL(MAKE_SOB_LIST);" nl
"DROP(FPARG(1)+1);" nl
"MOV(FPARG(2),R0);" nl
"/* Move the stack upwards (also return address and FP) */" nl
"MOV(R8, FPARG(1)-1);" nl
"CMP(R8,0);" nl
"JUMP_LE(LFix_Number_Arguments_Ym10);" nl
"MOV(R7, FPARG(1)+1);" nl
"LCorrect_Stack_Upwards_Ym10:" nl
"CMP(R7, R8-3);" nl
"JUMP_LE(LCorrect_Stack_Upwards_exit_Ym10);" nl
"MOV(FPARG(R7),FPARG(R7-R8));" nl
"DECR(R7);" nl
"JUMP(LCorrect_Stack_Upwards_Ym10);" nl
"LCorrect_Stack_Upwards_exit_Ym10:" nl
"DROP(R8);" nl
"SUB(FP,R8);" nl
"LFix_Number_Arguments_Ym10:" nl
"/* Fix to true number of arguments */" nl
"CMP(FPARG(1),0);" nl
"JUMP_EQ(Lzero_arguments_Ym10);" nl
"MOV(FPARG(1),1);" nl
"Lzero_arguments_Ym10:" nl
"/*###################### LAMBDA-BODY ######################*/" nl
"/*###################### TC-APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"/*###################### APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(2+0));" nl
"PUSH(R0);" nl
"/*###################### EXPRESSION ######################*/" nl
"/*###################### LAMBDA-SIMPLE ######################*/" nl
"MOV(R2, FPARG(0)); // env" nl
"PUSH(IMM(1+1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R1,R0);" nl
"/* FILL OLD ENV */" nl
"MOV(R7,0);" nl
"Lfill_old_env_Ym3:" nl
"CMP(R7,1);" nl
"JUMP_GE(Lfill_old_env_exit_Ym3);" nl
"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_old_env_Ym3);" nl
"Lfill_old_env_exit_Ym3:" nl
"PUSH(IMM(1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R3,R0);" nl
"/* FILL OLD-LAMBDA's PARAMS IN FREE SPOT */" nl
"MOV(R7,0);" nl
"Lfill_params_Ym3:" nl
"CMP(R7,1);" nl
"JUMP_GE(Lfill_params_exit_Ym3);" nl
"MOV(INDD(R3,R7), FPARG(2+R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_params_Ym3);" nl
"Lfill_params_exit_Ym3:" nl
"MOV(IND(R1), R3);" nl
"PUSH(IMM(3));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(IND(R0), T_CLOSURE);" nl
"MOV(INDD(R0,1), R1);" nl
"MOV(INDD(R0,2), LABEL(Lbody_code_Ym3));" nl
"JUMP(Llambda_exit_Ym3);" nl
"/* Lambda-body */" nl
"Lbody_code_Ym3:" nl
"PUSH(FP);" nl
"MOV(FP,SP);" nl
"/*###################### LAMBDA-BODY ######################*/" nl
"/*###################### LAMBDA-VAR ######################*/" nl
"MOV(R2, FPARG(0)); // env" nl
"PUSH(IMM(2+1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R1,R0);" nl
"/* FILL OLD ENV */" nl
"MOV(R7,0);" nl
"Lfill_old_env_Ym4:" nl
"CMP(R7,2);" nl
"JUMP_GE(Lfill_old_env_exit_Ym4);" nl
"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_old_env_Ym4);" nl
"Lfill_old_env_exit_Ym4:" nl
"PUSH(IMM(1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R3,R0);" nl
"/* FILL PARAMS IN FREE SPOT */" nl
"MOV(R7,0);" nl
"Lfill_params_Ym4:" nl
"CMP(R7,1);" nl
"JUMP_GE(Lfill_params_exit_Ym4);" nl
"MOV(INDD(R3,R7), FPARG(2+R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_params_Ym4);" nl
"Lfill_params_exit_Ym4:" nl
"MOV(IND(R1), R3);" nl
"PUSH(IMM(3));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(IND(R0), T_CLOSURE);" nl
"MOV(INDD(R0,1), R1);" nl
"MOV(INDD(R0,2), LABEL(Lbody_code_Ym4));" nl
"JUMP(Llambda_exit_Ym4);" nl
"/* Lambda-body */" nl
"Lbody_code_Ym4:" nl
"PUSH(FP);" nl
"MOV(FP,SP);" nl
"/* STACK CORRECTION */" nl
"/* Make list from extra parameters*/" nl
"/* FPARG(1) - actual params */" nl
"MOV(R7, FPARG(1)+1);" nl
"Lmake_params_list_Ym9:" nl
"CMP(R7, 1);" nl
"JUMP_LE(Lmake_params_list_exit_Ym9);" nl
"PUSH(FPARG(R7));" nl
"DECR(R7);" nl
"JUMP(Lmake_params_list_Ym9);" nl
"Lmake_params_list_exit_Ym9:" nl
"PUSH(FPARG(1));" nl
"CALL(MAKE_SOB_LIST);" nl
"DROP(FPARG(1)+1);" nl
"MOV(FPARG(2),R0);" nl
"/* Move the stack upwards (also return address and FP) */" nl
"MOV(R8, FPARG(1)-1);" nl
"CMP(R8,0);" nl
"JUMP_LE(LFix_Number_Arguments_Ym9);" nl
"MOV(R7, FPARG(1)+1);" nl
"LCorrect_Stack_Upwards_Ym9:" nl
"CMP(R7, R8-3);" nl
"JUMP_LE(LCorrect_Stack_Upwards_exit_Ym9);" nl
"MOV(FPARG(R7),FPARG(R7-R8));" nl
"DECR(R7);" nl
"JUMP(LCorrect_Stack_Upwards_Ym9);" nl
"LCorrect_Stack_Upwards_exit_Ym9:" nl
"DROP(R8);" nl
"SUB(FP,R8);" nl
"LFix_Number_Arguments_Ym9:" nl
"/* Fix to true number of arguments */" nl
"CMP(FPARG(1),0);" nl
"JUMP_EQ(Lzero_arguments_Ym9);" nl
"MOV(FPARG(1),1);" nl
"Lzero_arguments_Ym9:" nl
"/*###################### LAMBDA-BODY ######################*/" nl
"/*###################### TC-APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"/*###################### APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(2+0));" nl
"PUSH(R0);" nl
"/*###################### EXPRESSION ######################*/" nl
"/*###################### LAMBDA-SIMPLE ######################*/" nl
"MOV(R2, FPARG(0)); // env" nl
"PUSH(IMM(3+1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R1,R0);" nl
"/* FILL OLD ENV */" nl
"MOV(R7,0);" nl
"Lfill_old_env_Ym5:" nl
"CMP(R7,3);" nl
"JUMP_GE(Lfill_old_env_exit_Ym5);" nl
"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_old_env_Ym5);" nl
"Lfill_old_env_exit_Ym5:" nl
"PUSH(IMM(1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R3,R0);" nl
"/* FILL OLD-LAMBDA's PARAMS IN FREE SPOT */" nl
"MOV(R7,0);" nl
"Lfill_params_Ym5:" nl
"CMP(R7,1);" nl
"JUMP_GE(Lfill_params_exit_Ym5);" nl
"MOV(INDD(R3,R7), FPARG(2+R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_params_Ym5);" nl
"Lfill_params_exit_Ym5:" nl
"MOV(IND(R1), R3);" nl
"PUSH(IMM(3));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(IND(R0), T_CLOSURE);" nl
"MOV(INDD(R0,1), R1);" nl
"MOV(INDD(R0,2), LABEL(Lbody_code_Ym5));" nl
"JUMP(Llambda_exit_Ym5);" nl
"/* Lambda-body */" nl
"Lbody_code_Ym5:" nl
"PUSH(FP);" nl
"MOV(FP,SP);" nl
"/*###################### LAMBDA-BODY ######################*/" nl
"/*###################### LAMBDA-VAR ######################*/" nl
"MOV(R2, FPARG(0)); // env" nl
"PUSH(IMM(4+1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R1,R0);" nl
"/* FILL OLD ENV */" nl
"MOV(R7,0);" nl
"Lfill_old_env_Ym6:" nl
"CMP(R7,4);" nl
"JUMP_GE(Lfill_old_env_exit_Ym6);" nl
"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_old_env_Ym6);" nl
"Lfill_old_env_exit_Ym6:" nl
"PUSH(IMM(1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R3,R0);" nl
"/* FILL PARAMS IN FREE SPOT */" nl
"MOV(R7,0);" nl
"Lfill_params_Ym6:" nl
"CMP(R7,1);" nl
"JUMP_GE(Lfill_params_exit_Ym6);" nl
"MOV(INDD(R3,R7), FPARG(2+R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_params_Ym6);" nl
"Lfill_params_exit_Ym6:" nl
"MOV(IND(R1), R3);" nl
"PUSH(IMM(3));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(IND(R0), T_CLOSURE);" nl
"MOV(INDD(R0,1), R1);" nl
"MOV(INDD(R0,2), LABEL(Lbody_code_Ym6));" nl
"JUMP(Llambda_exit_Ym6);" nl
"/* Lambda-body */" nl
"Lbody_code_Ym6:" nl
"PUSH(FP);" nl
"MOV(FP,SP);" nl
"/* STACK CORRECTION */" nl
"/* Make list from extra parameters*/" nl
"/* FPARG(1) - actual params */" nl
"MOV(R7, FPARG(1)+1);" nl
"Lmake_params_list_Ym8:" nl
"CMP(R7, 1);" nl
"JUMP_LE(Lmake_params_list_exit_Ym8);" nl
"PUSH(FPARG(R7));" nl
"DECR(R7);" nl
"JUMP(Lmake_params_list_Ym8);" nl
"Lmake_params_list_exit_Ym8:" nl
"PUSH(FPARG(1));" nl
"CALL(MAKE_SOB_LIST);" nl
"DROP(FPARG(1)+1);" nl
"MOV(FPARG(2),R0);" nl
"/* Move the stack upwards (also return address and FP) */" nl
"MOV(R8, FPARG(1)-1);" nl
"CMP(R8,0);" nl
"JUMP_LE(LFix_Number_Arguments_Ym8);" nl
"MOV(R7, FPARG(1)+1);" nl
"LCorrect_Stack_Upwards_Ym8:" nl
"CMP(R7, R8-3);" nl
"JUMP_LE(LCorrect_Stack_Upwards_exit_Ym8);" nl
"MOV(FPARG(R7),FPARG(R7-R8));" nl
"DECR(R7);" nl
"JUMP(LCorrect_Stack_Upwards_Ym8);" nl
"LCorrect_Stack_Upwards_exit_Ym8:" nl
"DROP(R8);" nl
"SUB(FP,R8);" nl
"LFix_Number_Arguments_Ym8:" nl
"/* Fix to true number of arguments */" nl
"CMP(FPARG(1),0);" nl
"JUMP_EQ(Lzero_arguments_Ym8);" nl
"MOV(FPARG(1),1);" nl
"Lzero_arguments_Ym8:" nl
"/*###################### LAMBDA-BODY ######################*/" nl
"/*###################### TC-APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(2+0));" nl
"PUSH(R0);" nl
"/*###################### EXPRESSION ######################*/" nl
"/*###################### APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(0));" nl
"MOV (R1, INDD(R0,1));" nl
"MOV (R0, INDD(R1, 0));" nl
"PUSH(R0);" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(0));" nl
"MOV (R1, INDD(R0,0));" nl
"MOV (R0, INDD(R1, 0));" nl
"PUSH(R0);" nl
"PUSH(IMM(2));" nl
"/*###################### PROC ######################*/" nl
"MOV(R0,7);" nl
"/* Tlookup returns key. fvar needs address of closure. */" nl
"PUSH(R0);" nl
"CALL(Tlookup);" nl
"DROP(1);" nl
"MOV(R0, INDD(R0,1));" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym8);" nl
"PUSH(INDD(R0,1));" nl
"CALLA(INDD(R0,2));" nl
"/* env, #Args, magic_spot, Args */" nl
"DROP(3+STARG(0));" nl
"JUMP(L_Applic_Exit_Ym4);" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym8:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"L_Applic_Exit_Ym4:" nl
"/*###################### APPLIC-EXIT ######################*/" nl
"PUSH(R0);" nl
"PUSH(IMM(2));" nl
"/*###################### PROC ######################*/" nl
"MOV(R0,7);" nl
"/* Tlookup returns key. fvar needs address of closure. */" nl
"PUSH(R0);" nl
"CALL(Tlookup);" nl
"DROP(1);" nl
"MOV(R0, INDD(R0,1));" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym7);" nl
"PUSH(INDD(R0,1));" nl
"/* Push the return address from current frame */" nl
"PUSH(FPARG(-1));" nl
"/* Restore old FP */" nl
"MOV(FP,FPARG(-2));" nl
"/* Overwrite frame */" nl
"/* params(1) + exprs(2) + (2XTrio + FP)(7)*/" nl
"MOV(R7,10);" nl
"MOV(R8,6);" nl
"L_Overwrite_Frame_Ym4:" nl
"CMP(R7,4);" nl
"JUMP_LE(L_Overwrite_Frame_Exit_Ym4);" nl
"MOV(STARG(R7), STARG(R7-R8));" nl
"DECR(R7);" nl
"JUMP(L_Overwrite_Frame_Ym4);" nl
"L_Overwrite_Frame_Exit_Ym4:" nl
"DROP(R8);" nl
"JUMPA(INDD(R0,2));" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym7:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"/*###################### TC-APPLIC-EXIT ######################*/" nl
"/*###################### LAMBDA-BODY-END ######################*/" nl
"POP(FP);" nl
"RETURN;" nl
"Llambda_exit_Ym6:" nl
"/*###################### LAMBDA-VAR-END ######################*/" nl
"/*###################### LAMBDA-BODY-END ######################*/" nl
"POP(FP);" nl
"RETURN;" nl
"Llambda_exit_Ym5:" nl
"/*###################### LAMBDA-SIMPLE-END ######################*/" nl
"PUSH(R0);" nl
"PUSH(IMM(2));" nl
"/*###################### PROC ######################*/" nl
"MOV(R0,83);" nl
"/* Tlookup returns key. fvar needs address of closure. */" nl
"PUSH(R0);" nl
"CALL(Tlookup);" nl
"DROP(1);" nl
"MOV(R0, INDD(R0,1));" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym6);" nl
"PUSH(INDD(R0,1));" nl
"CALLA(INDD(R0,2));" nl
"/* env, #Args, magic_spot, Args */" nl
"DROP(3+STARG(0));" nl
"JUMP(L_Applic_Exit_Ym3);" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym6:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"L_Applic_Exit_Ym3:" nl
"/*###################### APPLIC-EXIT ######################*/" nl
"PUSH(R0);" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(0));" nl
"MOV (R1, INDD(R0,0));" nl
"MOV (R0, INDD(R1, 0));" nl
"PUSH(R0);" nl
"PUSH(IMM(2));" nl
"/*###################### PROC ######################*/" nl
"MOV(R0,7);" nl
"/* Tlookup returns key. fvar needs address of closure. */" nl
"PUSH(R0);" nl
"CALL(Tlookup);" nl
"DROP(1);" nl
"MOV(R0, INDD(R0,1));" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym5);" nl
"PUSH(INDD(R0,1));" nl
"/* Push the return address from current frame */" nl
"PUSH(FPARG(-1));" nl
"/* Restore old FP */" nl
"MOV(FP,FPARG(-2));" nl
"/* Overwrite frame */" nl
"/* params(1) + exprs(2) + (2XTrio + FP)(7)*/" nl
"MOV(R7,10);" nl
"MOV(R8,6);" nl
"L_Overwrite_Frame_Ym3:" nl
"CMP(R7,4);" nl
"JUMP_LE(L_Overwrite_Frame_Exit_Ym3);" nl
"MOV(STARG(R7), STARG(R7-R8));" nl
"DECR(R7);" nl
"JUMP(L_Overwrite_Frame_Ym3);" nl
"L_Overwrite_Frame_Exit_Ym3:" nl
"DROP(R8);" nl
"JUMPA(INDD(R0,2));" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym5:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"/*###################### TC-APPLIC-EXIT ######################*/" nl
"/*###################### LAMBDA-BODY-END ######################*/" nl
"POP(FP);" nl
"RETURN;" nl
"Llambda_exit_Ym4:" nl
"/*###################### LAMBDA-VAR-END ######################*/" nl
"/*###################### LAMBDA-BODY-END ######################*/" nl
"POP(FP);" nl
"RETURN;" nl
"Llambda_exit_Ym3:" nl
"/*###################### LAMBDA-SIMPLE-END ######################*/" nl
"PUSH(R0);" nl
"PUSH(IMM(2));" nl
"/*###################### PROC ######################*/" nl
"MOV(R0,83);" nl
"/* Tlookup returns key. fvar needs address of closure. */" nl
"PUSH(R0);" nl
"CALL(Tlookup);" nl
"DROP(1);" nl
"MOV(R0, INDD(R0,1));" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym4);" nl
"PUSH(INDD(R0,1));" nl
"CALLA(INDD(R0,2));" nl
"/* env, #Args, magic_spot, Args */" nl
"DROP(3+STARG(0));" nl
"JUMP(L_Applic_Exit_Ym2);" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym4:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"L_Applic_Exit_Ym2:" nl
"/*###################### APPLIC-EXIT ######################*/" nl
"PUSH(R0);" nl
"PUSH(IMM(1));" nl
"/*###################### PROC ######################*/" nl
"/*###################### LAMBDA-SIMPLE ######################*/" nl
"MOV(R2, FPARG(0)); // env" nl
"PUSH(IMM(1+1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R1,R0);" nl
"/* FILL OLD ENV */" nl
"MOV(R7,0);" nl
"Lfill_old_env_Ym2:" nl
"CMP(R7,1);" nl
"JUMP_GE(Lfill_old_env_exit_Ym2);" nl
"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_old_env_Ym2);" nl
"Lfill_old_env_exit_Ym2:" nl
"PUSH(IMM(1));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(R3,R0);" nl
"/* FILL OLD-LAMBDA's PARAMS IN FREE SPOT */" nl
"MOV(R7,0);" nl
"Lfill_params_Ym2:" nl
"CMP(R7,1);" nl
"JUMP_GE(Lfill_params_exit_Ym2);" nl
"MOV(INDD(R3,R7), FPARG(2+R7));" nl
"INCR(R7);" nl
"JUMP(Lfill_params_Ym2);" nl
"Lfill_params_exit_Ym2:" nl
"MOV(IND(R1), R3);" nl
"PUSH(IMM(3));" nl
"CALL(MALLOC);" nl
"DROP(1);" nl
"MOV(IND(R0), T_CLOSURE);" nl
"MOV(INDD(R0,1), R1);" nl
"MOV(INDD(R0,2), LABEL(Lbody_code_Ym2));" nl
"JUMP(Llambda_exit_Ym2);" nl
"/* Lambda-body */" nl
"Lbody_code_Ym2:" nl
"PUSH(FP);" nl
"MOV(FP,SP);" nl
"/*###################### LAMBDA-BODY ######################*/" nl
"/*###################### TC-APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(2+0));" nl
"PUSH(R0);" nl
"/*###################### EXPRESSION ######################*/" nl
"/*###################### APPLIC ######################*/" nl
"/* Magic Spot */" nl
"MAGIC_SPOT;" nl
"/*###################### EXPRESSION ######################*/" nl
"MOV (R0, FPARG(2+0));" nl
"PUSH(R0);" nl
"PUSH(IMM(1));" nl
"/*###################### PROC ######################*/" nl
"MOV(R0,25);" nl
"/* Tlookup returns key. fvar needs address of closure. */" nl
"PUSH(R0);" nl
"CALL(Tlookup);" nl
"DROP(1);" nl
"MOV(R0, INDD(R0,1));" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym3);" nl
"PUSH(INDD(R0,1));" nl
"CALLA(INDD(R0,2));" nl
"/* env, #Args, magic_spot, Args */" nl
"DROP(3+STARG(0));" nl
"JUMP(L_Applic_Exit_Ym1);" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym3:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"L_Applic_Exit_Ym1:" nl
"/*###################### APPLIC-EXIT ######################*/" nl
"PUSH(R0);" nl
"PUSH(IMM(2));" nl
"/*###################### PROC ######################*/" nl
"MOV(R0,7);" nl
"/* Tlookup returns key. fvar needs address of closure. */" nl
"PUSH(R0);" nl
"CALL(Tlookup);" nl
"DROP(1);" nl
"MOV(R0, INDD(R0,1));" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym2);" nl
"PUSH(INDD(R0,1));" nl
"/* Push the return address from current frame */" nl
"PUSH(FPARG(-1));" nl
"/* Restore old FP */" nl
"MOV(FP,FPARG(-2));" nl
"/* Overwrite frame */" nl
"/* params(1) + exprs(2) + (2XTrio + FP)(7)*/" nl
"MOV(R7,10);" nl
"MOV(R8,6);" nl
"L_Overwrite_Frame_Ym2:" nl
"CMP(R7,4);" nl
"JUMP_LE(L_Overwrite_Frame_Exit_Ym2);" nl
"MOV(STARG(R7), STARG(R7-R8));" nl
"DECR(R7);" nl
"JUMP(L_Overwrite_Frame_Ym2);" nl
"L_Overwrite_Frame_Exit_Ym2:" nl
"DROP(R8);" nl
"JUMPA(INDD(R0,2));" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym2:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"/*###################### TC-APPLIC-EXIT ######################*/" nl
"/*###################### LAMBDA-BODY-END ######################*/" nl
"POP(FP);" nl
"RETURN;" nl
"Llambda_exit_Ym2:" nl
"/*###################### LAMBDA-SIMPLE-END ######################*/" nl
"CMP(IND(R0),T_CLOSURE);" nl
"JUMP_NE(L_Not_Proc_Ym1);" nl
"PUSH(INDD(R0,1));" nl
"/* Push the return address from current frame */" nl
"PUSH(FPARG(-1));" nl
"/* Restore old FP */" nl
"MOV(FP,FPARG(-2));" nl
"/* Overwrite frame */" nl
"/* params(1) + exprs(1) + (2XTrio + FP)(7)*/" nl
"MOV(R7,9);" nl
"MOV(R8,6);" nl
"L_Overwrite_Frame_Ym1:" nl
"CMP(R7,4);" nl
"JUMP_LE(L_Overwrite_Frame_Exit_Ym1);" nl
"MOV(STARG(R7), STARG(R7-R8));" nl
"DECR(R7);" nl
"JUMP(L_Overwrite_Frame_Ym1);" nl
"L_Overwrite_Frame_Exit_Ym1:" nl
"DROP(R8);" nl
"JUMPA(INDD(R0,2));" nl
"/* NOT-PROC */" nl
"L_Not_Proc_Ym1:" nl
"SHOW(\"Error in Ym: Not a procedure!\",R0);" nl
"JUMP(Lprogram_exit);" nl
"/*###################### TC-APPLIC-EXIT ######################*/" nl
"/*###################### LAMBDA-BODY-END ######################*/" nl
"POP(FP);" nl
"RETURN;" nl
"Llambda_exit_Ym1:" nl
"TymExit:" nl
"PUSH(IMM( " (number->string (cdr (assoc 'ym exprs!)))"));\n"
"CALL(Tlookup);\n"
"DROP(IMM(1));\n"
"MOV(INDD(R0,1),IMM(" (number->string func-address!) "));\n"
"	MOV (IND(" (number->string func-address!) "),IMM(T_CLOSURE));\n"
"	MOV (IND(" (number->string (+ 1 func-address!)) "),IMM(0));\n"
"	MOV (IND(" (number->string (+ 2 func-address!)) "),LABEL(TymStart));\n")))


(set! func-address! (+ 3 func-address!))
ans)))



; ######### FROM FUNCTIONS.SCM - END #######





(define i-make-func-defs
  (lambda ()
    (set! func-address! 300000)
    (string-append (gen-reverse) (gen-map) (gen-ym) (gen-boolean?) (gen-apply) (gen-<) (gen-=) (gen->) (gen-+) (gen-*) (gen-/) (gen--) (gen-car) (gen-cdr) (gen-char->integer)
(gen-char?) (gen-cons) (gen-eq?) (gen-integer?) (gen-integer->char) (gen-make-string) (gen-make-vector)
(gen-null?) (gen-number?) (gen-pair?) (gen-procedure?) (gen-remainder) (gen-set-car!) (gen-set-cdr!)
(gen-string-length) (gen-string-ref) (gen-string-set!) (gen-string->symbol) (gen-string?) (gen-symbol?)
(gen-symbol->string) (gen-vector-length) (gen-vector-ref) (gen-vector-set!) (gen-vector?) (gen-zero?))))

(define i-gen-malloc
  (lambda ()
    (string-append
     "PUSH(IMM(400000));\n"
     "CALL(MALLOC);\n"
     "DROP(IMM(1));\n"
    )
  )
)

(define i-gen-code
  (lambda ()
    (let*
        (
         (consts (i-make-consts))
         (freevars (i-make-frees))
         (misc (i-make-super-consts))
         (function-defs (i-make-func-defs))
         (malloc (i-gen-malloc))
         )
      (string-append malloc consts freevars misc function-defs)
    )
  )
)

(define code-gen-const 
  (lambda (name)
		(let ((address
					(cond ((string? name) (str-tbl-lookup name))
						  (else (exprs-tbl-lookup name)))))		  
		  (format "MOV(R0,~a);\n" address)
    )
  )
)

(define code-gen-fvar
  (lambda (name) 
    (string-append
	(code-gen-const name)
	"/* Tlookup returns key. fvar needs address of closure. */\n"
     "PUSH(R0);\n"
     "CALL(Tlookup);\n"
     "DROP(1);\n"
	 "MOV(R0, INDD(R0,1));\n"
    )
  )
)

(define code-gen-define 
  (lambda (key value env-size num-params)
    (string-append 
		(^^comment "DEFINE-START") nl
		(^^comment "KEY") nl
		(code-gen-const key)
		"/* Tlookup returns key. fvar needs address of closure. */\n"
		 "PUSH(R0);\n"
		 "CALL(Tlookup);\n"
		 "DROP(1);\n"
		"MOV(R1,R0);" nl nl
		" /* In order to save the key's address */ " nl
		"PUSH(R1);" nl
		
		(^^comment "VALUE") nl
		(code-gen value env-size num-params) nl
		
		"/* Assign Value to Key */" nl
		"POP(R1);" nl
		"MOV(INDD(R1,1), R0);" nl nl
		"MOV(R0,SOB_VOID);" nl
		(^^comment "DEFINE-END") nl
    )
  )
)


((lambda () (begin (set! const-list! '())
					(i-add-to-const-list (void))
                   (i-add-to-const-list '())
                   (i-add-to-const-list #t)
                   (i-add-to-const-list #f)
                   (i-make-library-symbols)
				   (i-make-library)
                   (void)
                   )))
	

	
	
; ###################################### TOP-LEVEL-END ################################

		
; (define make-consts
	; (lambda (sexprs)
		; (string-append
			; (^^comment "MAKE-CONSTS") nl
			; "ADD (IND(0), IMM(1000));" nl
			; "MOV (IND(1), IMM(T_VOID));" nl
			; "#define SOB_VOID 1" nl
			; "MOV (IND(2), IMM(T_NIL));" nl
			; "#define SOB_NIL 2" nl
			; "MOV (IND(3), IMM(T_BOOL));" nl
			; "MOV (IND(4), IMM(0));" nl
			; "#define SOB_FALSE 3" nl
			; "MOV (IND(5), IMM(T_BOOL));" nl
			; "MOV (IND(6), IMM(1));" nl
			; "#define SOB_TRUE 5" nl nl
			; (^^comment) nl nl
		; )
	; )
; )


; ##############################################		


				
; ################### IF3 #######################

(define ^label-if3else (^^label "Lif3else"))
(define ^label-if3exit (^^label "Lif3exit"))

(define code-gen-if3
	(lambda (test dit dif env-size num-params)
          (let ((code-test (code-gen test env-size num-params))
					(code-dit (code-gen dit env-size num-params))
					(code-dif (code-gen dif env-size num-params))
					(label-else (^label-if3else))
					(label-exit (^label-if3exit)))
					(string-append
						; test
						(^^comment "IF") nl nl
						(^^comment "TEST") nl
						code-test nl
						
						; check
						(^^comment) nl
						"CMP(R0, SOB_FALSE);" nl
						"JUMP_EQ(" label-else ");" nl
						
						;dit
						(^^comment "DIT") nl
						code-dit nl
						"JUMP(" label-exit ");" nl
						
						;dif
						label-else ":" nl
						(^^comment "DIF") nl
						code-dif nl
						label-exit ":" nl
						(^^comment "END-OF-IF") nl
				))))
						
; ################# OR #########################

(define ^label-or-exit (^^label "Lor_exit"))

(define code-gen-or
	(lambda (exprs env-size num-params)
          (let* ((label-exit (^label-or-exit))
				(code-exprs (map 
								(lambda (e)
									(string-append
										(^^comment "Element") nl
										(code-gen e env-size num-params)
										"CMP(R0, SOB_FALSE);" nl
										"JUMP_NE(" label-exit ");" nl
										(^^comment) nl nl
									))
								exprs))
					)
					
					(string-append
						(^^comment "OR") nl nl
						(apply string-append code-exprs)
						label-exit ":" nl
						(^^comment "END-OF-OR") nl nl
			))))
						
						
; ##################### SEQ ######################################						

(define code-gen-seq
	(lambda (exprs env-size num-params)
          (let ((code-exprs 
				  (map 
						(lambda (e)
							(string-append
								(^^comment "Element") nl
								(code-gen e env-size num-params) nl
							))
						exprs)))
			(string-append
				(^^comment "SEQ") nl nl nl
				(apply string-append code-exprs) nl
				(^^comment "END-OF-SEQ") nl nl
			))))
						
; ##################### VARS #####################################					

(define code-gen-pvar
	(lambda (name minor)
		(string-append
			(format "MOV (R0, FPARG(2+~a));"  minor) nl 
		)
	))

(define code-gen-bvar
	(lambda (name major minor)
		(string-append
			"MOV (R0, FPARG(0));" nl ; this is the env
			(format "MOV (R1, INDD(R0,~a));" major) nl
			(format "MOV (R0, INDD(R1, ~a));" minor) nl
		)
	)
)
	
; ##################### CONSTS ###################################	

; ; ################################################################
						

; ######################## LAMBDAS ##################################						
; ########### LAMBDA-SIMPLE ############
(define ^label-fill-env (^^label "Lfill_old_env"))
(define ^label-fill-env-exit (^^label "Lfill_old_env_exit"))
(define ^label-fill-params (^^label "Lfill_params"))
(define ^label-fill-params-exit (^^label "Lfill_params_exit"))
(define ^label-body-code (^^label "Lbody_code"))
(define ^label-zero-arguments (^^label "Lzero_arguments"))
(define ^label-lambda-exit (^^label "Llambda_exit"))


(define stack-correction-code-lambda-var
	(lambda ()
		(let (
			(label-make-params-list (^label-make-params-list))
			(label-make-params-list-exit (^label-make-params-list-exit))
			(label-fix-number-arguments (^label-fix-number-arguments))
			(label-zero-arguments (^label-zero-arguments))
			(label-correct-stack-updwards (^label-correct-stack-updwards))
			(label-correct-stack-updwards-exit (^label-correct-stack-updwards-exit))
			)
			(string-append
			
				"/* STACK CORRECTION */" nl
				"/* Make list from extra parameters*/" nl
				"/* FPARG(1) - actual params */" nl
				"MOV(R7, FPARG(1)+1);" nl nl
				label-make-params-list ":" nl
				"CMP(R7, 1);" nl

				(format "JUMP_LE(~a);" label-make-params-list-exit) nl
				"PUSH(FPARG(R7));" nl
				"DECR(R7);" nl
				(format "JUMP(~a);" label-make-params-list) nl
				label-make-params-list-exit ":" nl nl
				
				"PUSH(FPARG(1));" nl
				"CALL(MAKE_SOB_LIST);" nl
				
				"DROP(FPARG(1)+1);" nl
				"MOV(FPARG(2),R0);"  nl nl

				"/* Move the stack upwards (also return address and FP) */" nl
				"MOV(R8, FPARG(1)-1);" nl
				"CMP(R8,0);" nl
				(format "JUMP_LE(~a);" label-fix-number-arguments) nl
				"MOV(R7, FPARG(1)+1);" nl
				label-correct-stack-updwards ":" nl
				
				"CMP(R7, R8-3);" nl
				(format "JUMP_LE(~a);" label-correct-stack-updwards-exit) nl
				"MOV(FPARG(R7),FPARG(R7-R8));" nl
				"DECR(R7);" nl
				(format "JUMP(~a);" label-correct-stack-updwards) nl nl
				
				label-correct-stack-updwards-exit ":" nl
				"DROP(R8);" nl
				"SUB(FP,R8);" nl nl
				
				label-fix-number-arguments ":" nl
				"/* Fix to true number of arguments */" nl
				"CMP(FPARG(1),0);" nl
				(format "JUMP_EQ(~a);" label-zero-arguments) nl
				"MOV(FPARG(1),1);" nl
				label-zero-arguments ":" nl
				
			)
		)
	)
)			


(define code-gen-lambda-simple
	(lambda (current-params body env-size num-params)
		(let (
				(label-fill-env (^label-fill-env))
				(label-fill-env-exit (^label-fill-env-exit))
				(label-fill-params (^label-fill-params))
				(label-fill-params-exit (^label-fill-params-exit))
				; (|params| (number->string (length params)))
				; (|env| (number->string env-size))
				(label-body-code (^label-body-code))
				(label-lambda-exit (^label-lambda-exit))
			)
			
			(string-append
				(^^comment "LAMBDA-SIMPLE") nl

					"MOV(R2, FPARG(0)); // env" nl
					(format "PUSH(IMM(~a+1));" env-size) nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(R1,R0);" nl nl
					"/* FILL OLD ENV */" nl
					"MOV(R7,0);" nl
					label-fill-env ":" nl
					(format "CMP(R7,~a);" env-size) nl
					(format "JUMP_GE(~a);" label-fill-env-exit) nl nl
					
					"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
					
					"INCR(R7);" nl
					(format "JUMP(~a);" label-fill-env) nl nl
					
					label-fill-env-exit ":" nl nl
					
					(format "PUSH(IMM(~a));" num-params) nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(R3,R0);" nl
					
					"/* FILL OLD-LAMBDA's PARAMS IN FREE SPOT */" nl
					
					"MOV(R7,0);" nl
					label-fill-params ":" nl
					(format "CMP(R7,~a);" num-params) nl
					(format "JUMP_GE(~a);" label-fill-params-exit) nl nl
					
						"MOV(INDD(R3,R7), FPARG(2+R7));" nl
					
					"INCR(R7);" nl
					(format "JUMP(~a);" label-fill-params) nl nl
					
					label-fill-params-exit ":" nl nl
					
					"MOV(IND(R1), R3);" nl
					"PUSH(IMM(3));" nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(IND(R0), T_CLOSURE);" nl
					"MOV(INDD(R0,1), R1);" nl
					(format "MOV(INDD(R0,2), LABEL(~a));" label-body-code) nl
					(format "JUMP(~a);" label-lambda-exit) nl nl
					
					"/* Lambda-body */" nl
					label-body-code ":" nl
					"PUSH(FP);" nl
					"MOV(FP,SP);" nl
					
					; ##########
					(^^comment "LAMBDA-BODY") nl
					; ###
					(code-gen body (add1 env-size) current-params)
					; ###
					(^^comment "LAMBDA-BODY-END") nl
					;##########
					
					"POP(FP);" nl
					"RETURN;" nl nl
					
					label-lambda-exit ":" nl nl
					
				(^^comment "LAMBDA-SIMPLE-END") nl
			)
		)
	)
)

; ########### LAMBDA-OPT ############

(define ^label-make-params-list (^^label "Lmake_params_list"))
(define ^label-make-params-list-exit (^^label "Lmake_params_list_exit"))
(define ^label-correct-stack-updwards (^^label "LCorrect_Stack_Upwards"))
(define ^label-correct-stack-updwards-exit (^^label "LCorrect_Stack_Upwards_exit"))
(define ^label-fix-number-arguments (^^label "LFix_Number_Arguments"))

(define stack-correction-code-lambda-opt
	(lambda (current-params)
		(let (
			(label-make-params-list (^label-make-params-list))
			(label-make-params-list-exit (^label-make-params-list-exit))
			(label-correct-stack-updwards (^label-correct-stack-updwards))
			(label-correct-stack-updwards-exit (^label-correct-stack-updwards-exit))
			(label-fix-number-arguments (^label-fix-number-arguments))
			(label-zero-arguments (^label-zero-arguments))
			)
			(string-append
			; Stack-Correction
						"/* STACK CORRECTION */" nl
						"/* Make list from extra parameters*/" nl
						"/* FPARG(1) - actual params */" nl
						"MOV(R7, FPARG(1)+1);" nl nl
						label-make-params-list ":" nl
						(format "CMP(R7, ~a);" current-params) nl
						(format "JUMP_LE(~a);" label-make-params-list-exit) nl
						"PUSH(FPARG(R7));" nl
						"DECR(R7);" nl
						(format "JUMP(~a);" label-make-params-list) nl
						label-make-params-list-exit ":" nl nl
						
						(format "PUSH(FPARG(1)-~a);" (sub1 current-params)) nl
						"CALL(MAKE_SOB_LIST);" nl
						
						(format "DROP(FPARG(1)-~a);" (- current-params 2)) nl
						(format "MOV(FPARG(~a),R0);" (add1 current-params)) nl nl
						
						"/* Move the stack upwards (also return address and FP) */" nl
						(format "MOV(R8, FPARG(1)-~a);" current-params) nl
						"CMP(R8,0);" nl
						(format "JUMP_LE(~a);" label-fix-number-arguments) nl
						"MOV(R7, FPARG(1)+1);" nl
						label-correct-stack-updwards ":" nl
						
						"CMP(R7, R8-3);" nl
						(format "JUMP_LE(~a);" label-correct-stack-updwards-exit) nl
						"MOV(FPARG(R7),FPARG(R7-R8));" nl
						"DECR(R7);" nl
						(format "JUMP(~a);" label-correct-stack-updwards) nl nl
						
						label-correct-stack-updwards-exit ":" nl
						"DROP(R8);" nl
						"SUB(FP,R8);" nl nl
						
						label-fix-number-arguments ":" nl
						"/* Fix to true number of arguments in case needed*/" nl
						(format "CMP(~a,FPARG(1));" (sub1 current-params)) nl
						(format "JUMP_EQ(~a);" label-zero-arguments) nl
						(format "MOV(FPARG(1),~a);" current-params) nl
						label-zero-arguments ":" nl
			)
		)
	)
)


(define code-gen-lambda-opt
	(lambda (current-params body env-size num-params)
		(let (
				(label-fill-env (^label-fill-env))
				(label-fill-env-exit (^label-fill-env-exit))
				(label-fill-params (^label-fill-params))
				(label-fill-params-exit (^label-fill-params-exit))
				; (|env| (number->string env-size))
				(label-body-code (^label-body-code))
				(label-lambda-exit (^label-lambda-exit))
			)
			
			(string-append
				(^^comment "LAMBDA-OPT") nl

					"MOV(R2, FPARG(0)); // env" nl
					(format "PUSH(IMM(~a+1));" env-size) nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(R1,R0);" nl nl
					"/* FILL OLD ENV */" nl
					"MOV(R7,0);" nl
					label-fill-env ":" nl
					(format "CMP(R7,~a);" env-size) nl
					(format "JUMP_GE(~a);" label-fill-env-exit) nl nl
					
					"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
					
					"INCR(R7);" nl
					(format "JUMP(~a);" label-fill-env) nl nl
					
					label-fill-env-exit ":" nl nl
					
					(format "PUSH(IMM(~a));" num-params) nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(R3,R0);" nl
					
					"/* FILL PARAMS IN FREE SPOT */" nl
					
					"MOV(R7,0);" nl
					label-fill-params ":" nl
					(format "CMP(R7,~a);" num-params) nl
					(format "JUMP_GE(~a);" label-fill-params-exit) nl nl
					
						"MOV(INDD(R3,R7), FPARG(2+R7));" nl
					
					"INCR(R7);" nl
					(format "JUMP(~a);" label-fill-params) nl nl
					
					label-fill-params-exit ":" nl nl
					
					"MOV(IND(R1), R3);" nl
					"PUSH(IMM(3));" nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(IND(R0), T_CLOSURE);" nl
					"MOV(INDD(R0,1), R1);" nl
					(format "MOV(INDD(R0,2), LABEL(~a));" label-body-code) nl
					(format "JUMP(~a);" label-lambda-exit) nl nl
					
					"/* Lambda-body */" nl
					label-body-code ":" nl
					"PUSH(FP);" nl
					"MOV(FP,SP);" nl

					(stack-correction-code-lambda-opt current-params)
					
					; ##########
					(^^comment "LAMBDA-BODY") nl
					; ###
					(code-gen body (add1 env-size) current-params)
					; ###
					(^^comment "LAMBDA-BODY-END") nl
					;##########
					
					"POP(FP);" nl
					"RETURN;" nl nl
					
					label-lambda-exit ":" nl nl
					
				(^^comment "LAMBDA-OPT-END") nl
			)
		)
	)
)


; ########### LAMBDA-VAR ############
; basically the same as lambda var
(define code-gen-lambda-var
	(lambda (body env-size num-params)
		(let (
				(label-fill-env (^label-fill-env))
				(label-fill-env-exit (^label-fill-env-exit))
				(label-fill-params (^label-fill-params))
				(label-fill-params-exit (^label-fill-params-exit))
				; (|env| (number->string env-size))
				(label-body-code (^label-body-code))
				(label-lambda-exit (^label-lambda-exit))
			)
			
			(string-append
				(^^comment "LAMBDA-VAR") nl

					"MOV(R2, FPARG(0)); // env" nl
					(format "PUSH(IMM(~a+1));" env-size) nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(R1,R0);" nl nl
					"/* FILL OLD ENV */" nl
					"MOV(R7,0);" nl
					label-fill-env ":" nl
					(format "CMP(R7,~a);" env-size) nl
					(format "JUMP_GE(~a);" label-fill-env-exit) nl nl
					
					"MOV(INDD(R1,R7+1), INDD(R2,R7));" nl
					
					"INCR(R7);" nl
					(format "JUMP(~a);" label-fill-env) nl nl
					
					label-fill-env-exit ":" nl nl
					
					"PUSH(IMM(1));" nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(R3,R0);" nl
					
					"/* FILL PARAMS IN FREE SPOT */" nl
					
					"MOV(R7,0);" nl
					label-fill-params ":" nl
					(format "CMP(R7,~a);" num-params) nl
					(format "JUMP_GE(~a);" label-fill-params-exit) nl nl
					
						"MOV(INDD(R3,R7), FPARG(2+R7));" nl
					
					"INCR(R7);" nl
					(format "JUMP(~a);" label-fill-params) nl nl
					
					label-fill-params-exit ":" nl nl
					
					"MOV(IND(R1), R3);" nl
					"PUSH(IMM(3));" nl
					"CALL(MALLOC);" nl
					"DROP(1);" nl
					"MOV(IND(R0), T_CLOSURE);" nl
					"MOV(INDD(R0,1), R1);" nl
					(format "MOV(INDD(R0,2), LABEL(~a));" label-body-code) nl
					(format "JUMP(~a);" label-lambda-exit) nl nl
					
					"/* Lambda-body */" nl
					label-body-code ":" nl
					"PUSH(FP);" nl
					"MOV(FP,SP);" nl
					
					;stack correction code
					(stack-correction-code-lambda-var)
					
					; ##########
					(^^comment "LAMBDA-BODY") nl
					; ###
					(code-gen body (add1 env-size) 1)
					; ###
					(^^comment "LAMBDA-BODY-END") nl
					;##########
					
					"POP(FP);" nl
					"RETURN;" nl nl
					
					label-lambda-exit ":" nl nl
					
				(^^comment "LAMBDA-VAR-END") nl
			)
		)
	)
)
; ######################### APPLICS ###################################

; ############ APPLIC ############
			
(define ^label-not-proc (^^label "L_Not_Proc"))
(define ^label-applic-exit (^^label "L_Applic_Exit"))

(define code-gen-applic
	(lambda (proc exprs env-size num-params)
		(let ((label-not-proc (^label-not-proc))
			  (label-applic-exit (^label-applic-exit))
			  (num-exprs (length exprs))
			  )
			(string-append
				(^^comment "APPLIC") nl
				"/* Magic Spot */" nl
				"MAGIC_SPOT;" nl nl
				(apply string-append
					(map
						(lambda (e)
							(string-append
								(^^comment "EXPRESSION") nl
								(code-gen e env-size num-params) nl
								"PUSH(R0);" nl
							)
						)
						(reverse exprs)))
				(format "PUSH(IMM(~a));" num-exprs) nl
				nl (^^comment "PROC") nl
				; proc
				(code-gen proc env-size	num-params) nl
				"CMP(IND(R0),T_CLOSURE);" nl
				(format "JUMP_NE(~a);" label-not-proc) nl
				"PUSH(INDD(R0,1));" nl
				
				"CALLA(INDD(R0,2));" nl
				"/* env, #Args, magic_spot, Args */" nl
				"DROP(3+STARG(0));" nl
				
				(format "JUMP(~a);" label-applic-exit) nl nl
				"/* NOT-PROC */" nl
				label-not-proc ":" nl
				"SHOW(\"Not a procedure!\",R0);" nl nl
				(format "JUMP(~a);" label-program-exit) nl
				label-applic-exit ":" nl
				(^^comment "APPLIC-EXIT") nl
			)
		)
	)
)

;  ############ TC-APPLIC ############
			
(define ^label-tc-applic-exit (^^label "L_TcApplic_Exit"))
(define ^label-overwrite-frame (^^label "L_Overwrite_Frame"))
(define ^label-overwrite-frame-exit (^^label "L_Overwrite_Frame_Exit"))

(define code-gen-tc-applic
	(lambda (proc exprs env-size num-params)
		(let ((label-not-proc (^label-not-proc))
			  ; (label-tc-applic-exit (^label-tc-applic-exit))
			  (label-overwrite-frame (^label-overwrite-frame))
			  (label-overwrite-frame-exit (^label-overwrite-frame-exit))
			  (num-exprs (length exprs))
			  )
			  
			(string-append
				(^^comment "TC-APPLIC") nl
				"/* Magic Spot */" nl
				"MAGIC_SPOT;" nl nl
				(apply string-append
					(map
						(lambda (e)
							(string-append
								(^^comment "EXPRESSION") nl
								(code-gen e env-size num-params) nl
								"PUSH(R0);" nl
							)
						)
						(reverse exprs)))
				(format "PUSH(IMM(~a));" num-exprs) nl
				nl (^^comment "PROC") nl
				; proc
				(code-gen proc env-size	num-params) nl
				"CMP(IND(R0),T_CLOSURE);" nl
				(format "JUMP_NE(~a);" label-not-proc) nl
				
				"PUSH(INDD(R0,1));" nl
				"/* Push the return address from current frame */" nl
				"PUSH(FPARG(-1));" nl
				"/* Restore old FP */" nl
				"MOV(FP,FPARG(-2));" nl
				"/* Overwrite frame */" nl
				(format "/* params(~a) + exprs(~a) + (2XTrio + FP)(7)*/" num-params num-exprs) nl
				(format "MOV(R7,~a);" (+ num-exprs num-params 7)) nl
				(format "MOV(R8,~a);" (+ num-params 5)) nl
				label-overwrite-frame ":" nl
				(format "CMP(R7,~a);" (+ num-params 3)) nl
				(format "JUMP_LE(~a);" label-overwrite-frame-exit) nl
				"MOV(STARG(R7), STARG(R7-R8));" nl
				"DECR(R7);" nl
				(format "JUMP(~a);" label-overwrite-frame) nl nl
				
				label-overwrite-frame-exit ":" nl
				"DROP(R8);" nl

				"JUMPA(INDD(R0,2));" nl
				
				"/* NOT-PROC */" nl
				label-not-proc ":" nl
				"SHOW(\"Not a procedure!\",R0);" nl nl
				(format "JUMP(~a);" label-program-exit) nl
				(^^comment "TC-APPLIC-EXIT") nl
			)
		)
	)
)

; ######################### CODE-GEN #################################
						
(define code-gen
  (lambda (e env-size num-params)
    (let ((run
           (compose-patterns
        
       ; ### CONST ###
       (pattern-rule 
         `(const ,(? 'expr))
			(lambda (c) 
				(code-gen-const c)
			)
       )
       
       ; ### VARS ###
			; ### fvar ###
		   (pattern-rule
		   `(fvar ,(? 'name))
				(lambda (name) 
					(code-gen-fvar name)
				)	
			)
		   
			;### pvar ###
			(pattern-rule
				`(pvar ,(? 'name) ,(? 'minor number?))
					(lambda (name minor) 
						(code-gen-pvar name minor)
				)
			)
			
		   ;### bvar ###
			(pattern-rule
				`(bvar ,(? 'name) ,(? 'major number?)  ,(? 'minor number?))
					(lambda (name major minor) 
						(code-gen-bvar name major minor)
				)
			)
      
       ; ### IF3 ###
       (pattern-rule 
         `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
         (lambda (test dit dif) 
           (code-gen-if3 test dit dif env-size num-params)
         )
       )
       
       ;DEFINE
      (pattern-rule 
        `(define (fvar ,(? 'var)) ,(? 'val))
        (lambda (var val) 
          (code-gen-define var val env-size num-params)
        )
      )
       
       ; ### LAMBDAS ###
		   ; ### lambda-simple ###
		   (pattern-rule 
			 `(lambda-simple ,(? 'params) ,(? 'body) )
			 (lambda (params body) 
				 (code-gen-lambda-simple (length params) body env-size num-params)
			 )
		   )
			   
		   ; lambda-opt
		   (pattern-rule 
			 `(lambda-opt ,(? 'args) ,(? 'rest) ,(? 'body))
			 (lambda (params rest body) 
				 (code-gen-lambda-opt (add1 (length params)) body env-size num-params)
			 )
		   )

		   ;lambda-variadic
		   (pattern-rule 
			 `(lambda-variadic ,(? 'params) ,(? 'body) )
			 (lambda (param body) 
				(code-gen-lambda-var body env-size num-params)
			 )
		   )
       
       ;### APPLICS ###
			; ### applic ###
		   (pattern-rule 
			 `(applic ,(? 'proc) ,(? 'exprs))
			 (lambda (proc exprs) 
			   (code-gen-applic proc exprs env-size num-params)
			 )
		   )
		   
		   ;tc-APPLIC
		   (pattern-rule 
			 `(tc-applic ,(? 'proc) ,(? 'exprs))
			 (lambda (proc exprs) 
			   (code-gen-tc-applic proc exprs env-size num-params)
			 )
		   )
         
       ;### SEQ ###
      (pattern-rule 
        `(seq ,(? 'exprs))
        (lambda (exprs) 
          (code-gen-seq exprs env-size num-params)
        )
      )
       
       
       ;### OR ###
      (pattern-rule 
        `(or ,(? 'exprs))
        (lambda (exprs) 
            (code-gen-or exprs env-size num-params)
        )
      )
       
    ))) 
	
      (run e
           (lambda ()
             (error 'code-gen (format "I can't recognize this: ~s" e))
           )
      )    
   )
  )
)		


(define gen 
  (lambda (e)
    (display (code-gen (compile e) 0 0))))

(define empty
	(vector))
	
; old = vec, new = list	
(define extend-vec-env
	(lambda (old new-list)
		(let ((new (list->vector new-list))
				(ans (make-vector (add1 (vector-length old))))
				(n (vector-length old)))
			(letrec
				((fill (lambda (_ans _old i)
					(if (= i n)
						(vector-set! _ans n new) 
						(begin
							(vector-set! _ans i (vector-ref _old i))
							(fill _ans old (add1 i)))))))
						
				(begin 
					(fill ans old 0)
					ans)
			))
	))
	
	
	
	
