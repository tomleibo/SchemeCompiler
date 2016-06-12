
(define write-to-file 
  (lambda (filename output)
    (let ((port (open-output-file filename)))
      (display output port)
      (close-output-port port)
    )
  )
)
           
(define read-from-file
  (lambda (filename)
    (let* ((port (open-input-file filename))
           (output (read port))) 
       (close-input-port port)
      output
    )
  )
)

(define file->sexpres
	(lambda (filename)
         (let ((input (open-input-file filename)))
			(letrec ((run
						(lambda ()
							(let ((e (read input)))
								(if (eof-object? e)
									(begin (close-input-port input)
											'())
									(cons e (run)))))))
			(run)
			))))

(define fs file->sexpres)