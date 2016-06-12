
; ################ Addition for graders ####################

(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

(revert-interaction-semantics) ; allow builtins to be redefined

;;; fix bug in optimizer
(#%$sputprop 'append '*flags* 122)
(#%$sputprop 'append! '*flags* 34)
(#%$sputprop 'list* '*flags* 1250)
(#%$sputprop 'cons* '*flags* 1250)

; ##########################################################

; ############### pattern matcher ###############

(define match
  (letrec ((match
	    (lambda (pat e ret-vals ret-fail)
	      (cond ((and (pair? pat) (pair? e))
		     (match (car pat) (car e)
			    (lambda (vals-car)
			      (match (cdr pat) (cdr e)
				     (lambda (vals-cdr)
				       (ret-vals
					(append vals-car vals-cdr)))
				     ret-fail))
			    ret-fail))
		    ((and (vector? pat) (vector? e)
			  (= (vector-length pat) (vector-length e))
			  (match (vector->list pat) (vector->list e)
				 ret-vals ret-fail)))
		    ((procedure? pat)
		     (let ((v (pat e)))
		       (if v (ret-vals v) (ret-fail))))
		    ((equal? pat e) (ret-vals '()))
		    (else (ret-fail))))))
    (lambda (pat e ret-with ret-fail)
      (match pat e
	     (lambda (vals) (apply ret-with vals))
	     ret-fail))))

(define ?
  (lambda (name . guards)
    (let ((guard?
	   (lambda (e)
	     (andmap 
	      (lambda (g?) (g? e))
	      guards))))
      (lambda (value)
	(if (guard? value)
	    (list value)
	    #f)))))

(define _ (? 'dont-care))


(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

(define compose-patterns
  (letrec ((match-nothing
	    (lambda (e failure)
	      (failure)))
	   (loop
	    (lambda (s)
	      (if (null? s)
		  match-nothing
		  (let ((match-rest
			 (loop (cdr s)))
			(match-first (car s)))
		    (lambda (e failure)
		      (match-first e
		       (lambda ()
			 (match-rest e failure)))))))))
    (lambda patterns
      (loop patterns))))

; ################################################

(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
	  ((unquote-splicing? e)
	   (error 'expand-qq "unquote-splicing here makes no sense!"))
	  ((pair? e)
	   (let ((a (car e))
		 (b (cdr e)))
	     (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
		   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
		   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
	  ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
	  ((or (null? e) (symbol? e)) `',e)
	  (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))


(define *reserved-words*
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote 
    unquote-splicing quote set!))

(define *void-object* 
  (if #f #f (void)))

(define proc?
	(lambda (e)
      (and
          (not (simple-const? e))
          (if (not (symbol? e)) 
              #t 
              (not (member e *reserved-words*))
           )
      )
))

(define return-last
  (lambda (l)
    (if (pair? l)
    (if (not (pair? (cdr l)))
        (cdr l)
        (return-last (cdr l)))
    '())))

(define trim-last-helper
  (lambda (l agg)
    (if (pair? l)
        (if (not (pair? (cdr l)))
          (cons (car l) agg)
          (trim-last-helper (cdr l) (cons (car l) agg)))
        (void)
        )))

(define trim-last
  (lambda (l)
    (reverse (trim-last-helper l '()))))
    

(define simple-const?
  (lambda (c)
    (or (boolean? c) (char? c) (number? c) (string? c))
  )
)

(define ccat 
  (lambda (l e)
    (if (null? l)
        (cons e '())
        (cons (car l) (ccat (cdr l) e))
    )
  )
)

(define flatten-dotted-list
  (lambda (dotted-list)
    (if (null? (return-last dotted-list))
      dotted-list  
      (ccat (trim-last dotted-list) (return-last dotted-list))
    )
  )
)

(define var?
  (lambda (c) 
    (if (and (not (member c *reserved-words*)) (symbol? c)) c #f)
  )
)


(define check-vars
  (lambda (vars)
	(and (list? vars)
    (andmap (lambda (v)
              (not (eq? ((? 'v var?) v) #f)))
              vars
            ))
  )
)

(define application?
	(lambda (e)
		(and (list? e)
			(not (null? e)))))


(define check-exprs
  (lambda (exprs)
    (map  
       (lambda (expr) 
         (parse expr))
       exprs
    )
  )
)

(define check-vars-opt
  (lambda (vars)
     (if (and 
			(not (list? vars))
			(pair? vars)
      )
      `(,@(trim-last vars) ,(return-last vars))
      #f
    )
  )
)

(define seq
  (lambda (expr exprs)
    (if (and (list? exprs) (> (length exprs) 0))
        `(seq (,(parse expr) ,@(map parse exprs)))
        (parse expr)
    )
  )
)

(define get-key-list
  (lambda (alst)
    (if (list? alst)
        (map car alst)
        '()
    )
  )
)

(define get-val-list
  (lambda (alst)
    (if (list? alst)
        (map cadr alst)
        '()
    )
  )
)

(define symbol>?
  (lambda (a b)
    (string<? (symbol->string a) (symbol->string b))))

(define check-distinct-values-sequential
  (lambda (lst)
    (if (and (list? lst) (< 1 (length lst)))
        (or (equal? (car lst) (cadr lst))
         (check-distinct-values-sequential (cdr lst)))
        #f
    )
  )
)

(define check-distinct-values 
  (lambda (lst)
    (not (check-distinct-values-sequential (sort symbol>? lst))
    )
  )
)

(define let-list?
  (lambda (lst)
    (let ((key-list (get-key-list lst)))
    (and 
     (list? lst)
     (check-vars key-list)
     (check-distinct-values key-list)
    )
   )
  )
)

(define with (lambda (s f) (apply f s)))

(define Ycurry
  (lambda (f)
    ((lambda (x) (f (lambda s (apply (x x) s))))
     (lambda (x) (f (lambda s (apply (x x) s)))))))

(define Ym
  (lambda fs
    (let ((ms (map
		(lambda (fi)
		  (lambda ms
		    (apply fi (map (lambda (mi)
				     (lambda args
				       (apply (apply mi ms) args))) ms))))
		fs)))
      (apply (car ms) ms))))

(define expand-letrec
  (lambda (letrec-expr)
    (with letrec-expr
      (lambda (_letrec ribs . exprs)
	(let* ((fs (map car ribs))
	       (lambda-exprs (map cdr ribs))
	       (nu (gensym))
	       (nu+fs `(,nu ,@fs))
	       (body-f `(lambda ,nu+fs ,@exprs))
	       (hofs
		(map (lambda (lambda-expr) `(lambda ,nu+fs ,@lambda-expr))
		  lambda-exprs)))
	  `(Ym ,body-f ,@hofs))))))


;(expand-and)
(define expand-and
  (lambda (boolist)
    (if (null? (cdr boolist))
        (car boolist)
    `(if ,(car boolist) ,(expand-and (cdr boolist) ) #f )
    )
  )
)


;expand cond
(define expand-cond 
  (lambda (condlist)
    (if (null? (cdr condlist))
        (if (equal? (caar condlist) 'else)
            `(,@(car (cdr (car condlist))))
            `(if ,(caar condlist) ,@(cdr (car condlist)))
        )
             `(if ,(caar condlist) ,@(cdr (car condlist)) ,(expand-cond (cdr condlist)) )
        ;)
          
    )
  )
)

;expand MIT-STYLE-BIATCH!
(define expand-mit
  (lambda (name args expr exprs)
    `(define ,name (lambda (,@args) ,expr ,@exprs))
  )
)

(define not-list?
  (lambda (l)
    (not (list? l))))


(define mit-vars
  (lambda (l)
    (or (check-vars l) (check-vars-opt l))
  )
)

(define let*?
  (lambda (e) 
    (and
     (equal? (car e) 'l)
     (equal? (cadr e) 'e)
    )
  )
)

(define beginify
  (lambda (exp exps)
    `(begin ,exp ,@exps)
  )
)

(define expand-or
  (lambda (exprs)
    (cond 
      ((null? exprs) (parse #f))
      ((= 1 (length exprs)) (parse (car exprs)))
      (else `(or ,(map parse exprs)))
    )  
  )
)

(define fact
  (Ycurry
   (lambda (!)
     (lambda (n)
       (if (zero? n) 1
	   (* n (! (- n 1))))))))
	   
	   
; ################ annotate-tc procedures #####################	   

(define annotate-if
  (lambda (test dit dif tp?)
    `(if3 ,(ant test #f) ,(ant dit tp?) ,(ant dif tp?))
  )
)

(define annotate-define
  (lambda (var val tp?)
    `(define ,(ant var #f) ,(ant val #f))
  )
)

(define annotate-lambda-s
  (lambda (params body)
    `(lambda-simple ,params ,(ant body #t))
  )
)

(define annotate-lambda-o
  (lambda (args rest body)
    `(lambda-opt ,args ,rest ,(ant body #t))
  )
)
  
(define annotate-lambda-v
  (lambda (args body)
    `(lambda-variadic ,args ,(ant body #t))
  )
)


(define annotate-applic
  (lambda (expr exprs tp?)
    (if 
       tp?
       `(tc-applic ,((ant-false) expr) ,(map (ant-false) exprs) )
       `(applic ,((ant-false) expr) ,(map (ant-false) exprs) )
    )
  )
)

(define annotate-applic-lists
  (lambda (exprs)
    (if (or (null? exprs) (null? (car exprs)))
        (void)
        `(,@(map (ant-false) exprs))
    )
  )
)

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))
		
		
		
		

; (define annotate-seq
  ; (lambda (exprs tp?)
    ; (cond
        ; ((= (length exprs) 1) `(seq ,(ant (car exprs) tp?)))
        ; (else `(seq ,((lambda () (ant (car exprs) #f))) ,(annotate-seq (cdr exprs) tp?)))
    ; )
  ; )
; )

(define seperate-last
	(lambda (lst)
		(let* ((reversed (reverse lst))
				(last (car reversed))
				(trimmed (reverse (cdr reversed))))
		(cons last trimmed))
		))
		
(define annotate-seq
  (lambda (exprs tp?)
		(if (= (length exprs) 1)
			`(seq ,(ant (car exprs) tp?))
			(let* ((last-trimmed (seperate-last exprs))
					(last (car last-trimmed))
					(trimmed (cdr last-trimmed)))
					
			`(seq (,@(map (lambda (e) (ant e #f)) trimmed) ,(ant last tp?)))
		))))		
		

(define annotate-or
  (lambda (exprs tp?)
		(if (= (length exprs) 1)
			`(or ,(ant (car exprs) tp?))
			(let* ((last-trimmed (seperate-last exprs))
					(last (car last-trimmed))
					(trimmed (cdr last-trimmed)))
					
			`(or (,@(map (lambda (e) (ant e #f)) trimmed) ,(ant last tp?)))
		))))
 

(define ant-true
    (lambda ()
      (lambda (e)
        (ant e #t)
      )
    )
)

(define ant-false
    (lambda ()
      (lambda (e)
        (ant e #f)
      )
    )
)          

(define ant
  (lambda (e tp?)
    (let ((run
           (compose-patterns
        
       ;### CONST ###
       (pattern-rule 
         `(const ,(? 'expr))
         (lambda (c) 
			(i-add-to-const-list c)
			`(const ,c))
       )
	   
	   ;fvar
	   (pattern-rule
	   `(fvar ,(? 'name))
	   (lambda (name) (i-add-to-free-table name) `(fvar ,name)))
	   
	  ;pvar 
		(pattern-rule
	   `(pvar ,(? 'name) ,(? 'minor number?))
	   (lambda (name minor) `(pvar ,name ,minor)))
	   ;bvar
		(pattern-rule
	   `(bvar ,(? 'name) ,(? 'major number?)  ,(? 'minor number?))
	   (lambda (name major minor) `(bvar ,name ,major ,minor)))
       
       ;IF
       (pattern-rule 
         `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
         (lambda (test dit dif) 
           (annotate-if test dit dif tp?)
         )
       )
       
       ;DEFINE
       (pattern-rule 
         `(define ,(? 'var) ,(? 'val))
         (lambda (var val) 
           (annotate-define var val tp?)
         )
       )
       
       ;LAMBDAs
       ;lambda-simple
       ;no multiple bodies. begin.
       (pattern-rule 
         `(lambda-simple ,(? 'params) ,(? 'body) )
         (lambda (params body) 
             (annotate-lambda-s params body)
         )
       )
           
       ;lambda-opt
       (pattern-rule 
         `(lambda-opt ,(? 'args) ,(? 'rest) ,(? 'body))
         (lambda (args rest body) 
             (annotate-lambda-o args rest body)
         )
       )
       ;UNIMPLEMENTED
       ;lambda-variadic
       (pattern-rule 
         `(lambda-variadic ,(? 'params) ,(? 'body) )
         (lambda (params body) 
             (annotate-lambda-v params body)
         )
       )
       
       ;APPLIC
       (pattern-rule 
         `(applic ,(? 'proc) ,(? 'exprs))
         (lambda (proc exprs) 
           (annotate-applic proc exprs tp?)
         )
       )
          
       ;SEQ
       (pattern-rule 
         `(seq ,(? 'exprs))
         (lambda (exprs) 
           (annotate-seq exprs tp?)
         )
       )
       
       
       ;OR
       (pattern-rule 
         `(or ,(? 'exprs))
         (lambda (exprs) 
             (annotate-or exprs tp?)
         )
       )
       
       (pattern-rule 
         `(,(? 'exprs))
         (lambda (exprs) 
             (if tp?
                 `(,@(map (ant-true) exprs))
                 `(,@(map (ant-false) exprs))
             )
         )
       )
       
    ))) 
      (run e
           (lambda ()
             (error 'annotate-tc (format "I can't recognize this: ~s" e))
           )
      )    
   )
  )
)

(define annotate-tc 
	(lambda (expr)
		(ant expr #f)
	)
)

; ################ Parse #######################

(define parse
  (let ((run
	 (compose-patterns
	  ;const
     (pattern-rule
	   (? 'c simple-const?)
	   (lambda (c) `(const ,c)))
     ;quote
	  (pattern-rule
	   `(quote ,(? 'c))
	   (lambda (c) `(const ,c)))
     ;var
	  (pattern-rule
	   (? 'v var?)
	   (lambda (v) `(var ,v)))
     ;if void
	  (pattern-rule
	   `(if ,(? 'test) ,(? 'dit))
	   (lambda (test dit)
	     `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
     ;if 3
	  (pattern-rule
	   `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
	   (lambda (test dit dif)
	     `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
     ;lambda-simple
     (pattern-rule
	   `(lambda ,(? 'vars check-vars) ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (vars expr exprs)
	     `(lambda-simple ,vars ,(seq expr exprs))))
     ;lambda-opt
     (pattern-rule
	   `(lambda ,(? 'vars-list check-vars-opt) ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (vars-list expr exprs)
	     `(lambda-opt ,(trim-last vars-list) ,(return-last vars-list) ,(seq expr exprs))))
     ;lambda-variadic
     (pattern-rule
	   `(lambda ,(? 'vars) ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (vars expr exprs)
	     `(lambda-variadic ,vars ,(seq expr exprs))))
     ;begin
     (pattern-rule
	   `(begin ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (expr exprs)
	     (seq expr exprs)))
     
     
	  ;; let*
	  (pattern-rule
	   `(let* () ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (expr exprs)
	     (parse 
             (beginify expr exprs)
             ))
        )
	  (pattern-rule
	   `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
	   (lambda (var val rest exprs)
	     (parse 
              `(let ((,var ,val))
		      (let* ,rest . ,exprs)) 
                 
        )
             ))
	  ;let
     (pattern-rule
	   `(let ,(? 'let-list let-list?) ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (let-list expr exprs)         
	       (parse `((lambda ,(get-key-list let-list) ,expr ,@exprs) ,@(get-val-list let-list)))))     
          
     ;letrec
     ; (pattern-rule
	   ; `(letrec ,(? 'let-list let-list?) ,(? 'expr) . ,(? 'exprs list?))
           ; (lambda (let-list expr exprs)
             ; (parse (expand-letrec `(,let-list ,expr ,exprs)))
           ; )
     ; )
	 
	 (pattern-rule
	   `(letrec ,(? 'ribs let-list?) . ,(? 'exprs list?))
           (lambda (ribs exprs)
             (parse (expand-letrec `(letrec ,ribs ,exprs)))
           )
     )
     
     ;or
     (pattern-rule
      `(or . ,(? 'exprs list?))
      (lambda (exprs)
         
         `(,@(expand-or exprs))
         
      ))
     
     ;and
     (pattern-rule
      `(and . ,(? 'boolist list?))
      (lambda (boolist)
        (parse `(,@(expand-and boolist)))
      ))
     
     ;cond
     (pattern-rule
      `(cond . ,(? 'conds list?))
      (lambda (conds)
         (parse `(,@
        (expand-cond conds)
        ))
      ))
       
     ; define   
              
     ;MIT-style
     (pattern-rule
      `(define ,(? 'args pair?) ,(? 'expr) . ,(? 'exprs list?))
        (lambda (args expr exprs)
        (parse (expand-mit (car args) (cdr args) expr exprs)) 
      ))
     
     ;regular-define
	  (pattern-rule
	   `(define ,(? 'var not-list?) ,(? 'val)) 
		(lambda (var val)
				`(define ,(parse var) ,(parse val))))
     
     ;quasiquote
     (pattern-rule 
      `(,'quasiquote ,(? 'arg)) 
      (lambda (arg) 
        (parse (expand-qq arg))))
     
     ; application
     (pattern-rule
	   `(,(? 'proc proc?) . ,(? 'args list?))
           (lambda (proc args)
                 `(applic ,(parse proc) ,(map parse args))
                 ))     		
	  )))
    (lambda (e)
      (run e
	   (lambda ()
	     (error 'parse
		    (format "I can't recognize this: ~s" e)))))))

			


; ######################## lexical-addressing ########################			
			
(define extend-scope
	(lambda (old new)
			(cons (number-list new) old)))
		
(define number-list
	(lambda (_lst)
		(letrec ((_number-list 
			(lambda (lst n)
				(if (null? lst) '()
					(cons (list (car lst) n)
							(_number-list (cdr lst) (add1 n)))))
			))
		(_number-list _lst 0))))
		
(define get-lexical
	(lambda (var _scope)
		(letrec ((_get-lexical
			(lambda (scope var n)
				(if (null? scope)
					#f
					(let ((ans (assoc var (car scope))))
						(if (equal? ans #f)
							(_get-lexical (cdr scope) var (add1 n))
							(list n (cadr ans))))))
							))
		(_get-lexical _scope var -1))))
		
		
(define get-lexical-name
		(lambda (var scope)
			(let ((ans (get-lexical var scope)))
				(if (equal? ans #f)
					`(fvar ,var)
					(if (= (car ans) -1)
						`(pvar ,var ,(cadr ans))
						`(bvar ,var ,@ans)
						)))))
						
									
(define parse-in-lambda
	(lambda (scope)
		(let ((run
		 (compose-patterns
		 
		 ; ### lambdas ###
			 ; lambda simple
			 (pattern-rule
			   `(lambda-simple ,(? 'args) ,(? 'body))
			   (lambda (args body)
				`(lambda-simple ,args ,((parse-in-lambda (extend-scope scope args)) body))
				))
				
			; lambda optional
			 (pattern-rule
			   `(lambda-opt ,(? 'args) ,(? 'rest) ,(? 'body))
					(lambda (args rest body)
						`(lambda-opt ,args ,rest ,((parse-in-lambda (extend-scope scope `(,@args ,rest))) body))
						
					))
			
			; lambda variadic
			(pattern-rule
			   `(lambda-variadic ,(? 'args) ,(? 'body))
					(lambda (args body)
						`(lambda-variadic ,args ,((parse-in-lambda (extend-scope scope `(,args))) body))
					))
			
		; ### seq ###
		(pattern-rule
			`(seq ,(? 'exps))
				(lambda (exps)
					`(seq ,(map (parse-in-lambda scope) exps))))
					
		; ### applic ###			
		(pattern-rule
			`(applic ,(? 'proc) ,(? 'exps))
				(lambda (proc exps)
					`(applic ,((parse-in-lambda scope) proc),(map (parse-in-lambda scope) exps))))					
					
		; ### tc-applic ###			
		(pattern-rule
			`(tc-applic ,(? 'proc) ,(? 'exps))
				(lambda (proc exps)
					`(tc-applic ,((parse-in-lambda scope) proc),(map (parse-in-lambda scope) exps))))
					
		; ### if3 ###
		 (pattern-rule
		   `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
		   (lambda (test dit dif)
			 `(if3 ,((parse-in-lambda scope) test) ,((parse-in-lambda scope) dit) ,((parse-in-lambda scope) dif))))
		
		; ### define ###
	    (pattern-rule
		   `(define ,(? 'var) ,(? 'val)) 
				(lambda (var val)
					`(define ,((parse-in-lambda scope) var) ,((parse-in-lambda scope) val))))			

		; ### or ###
		(pattern-rule
		   `(or ,(? 'exps)) 
				(lambda (exps)
					`(or ,(map (parse-in-lambda scope) exps))
					))
					
		; ### var ###						
		(pattern-rule
		   `(var ,(? 'var))
		   (lambda (var)
			(get-lexical-name var scope)))	

		; ### const ###						
		(pattern-rule
		   `(const ,(? 'const))
		   (lambda (const)
			`(const ,const)))			
		 
		 )))
		 (lambda (e)
		  (run e
		   (lambda ()
			 (error 'pe->lex-pe
				(format "I can't recognize this: ~s" e))
				e
				))))))
	 
	 
(define pe->lex-pe
	(lambda (e)
		((parse-in-lambda '()) e)))	 			