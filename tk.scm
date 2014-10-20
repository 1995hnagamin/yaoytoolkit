(use tk)
(load "./yaoy.scm")

(define (yo-button-symbol friend)
  (string->symbol (string-append ".yoto" friend)))

(define-syntax tk-after
  (syntax-rules ()
    [(_ ms command args ...)
     (tk-call 'after ms (format command args ...))]))

(define (pack-yo-buttons)
  (let1 friends (get-friends)
    (dolist (friend friends)
      (let1 path (yo-button-symbol friend)
        (tk-button
          path 
          :text friend
          :command (lambda () 
                     (let1 result (send-yo friend)
                       (tk-call path 'configure :text result)
                       (tk-after 800 "~A configure -text ~A"
                                 path friend))))
        (tk-pack path :fill 'x)))))

(define (main args)
  (tk-init '())
  (pack-yo-buttons)
  (tk-mainloop))
