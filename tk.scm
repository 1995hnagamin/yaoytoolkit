(use tk)
(load "./yaoy.scm")

(define (yo-button-symbol friend)
  (string->symbol (string-append ".yoto" friend)))

(define-syntax tk-after
  (syntax-rules ()
    [(_ ms command args ...)
     (tk-call 'after ms (format command args ...))]))

(define (pack-buttons button-paths)
  (dolist (path button-paths)
    (tk-pack path :fill 'x)))

(define (make-yo-buttons friends)
  (map (lambda (friend)
         (let1 path (yo-button-symbol friend)
           (tk-button
             path
             :text friend
             :command (lambda ()
                        (let1 result (send-yo friend)
                          (tk-call path 'configure :text result)
                          (tk-after 2000 "~A configure -text ~A"
                                    path friend))))
           path))
       friends))

(define (main args)
  (tk-init '())
  (pack-buttons (make-yo-buttons (get-friends)))
  (tk-mainloop))
