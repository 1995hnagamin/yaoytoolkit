(use tk)
(load "./yaoy.scm")

(tk-init '())
(let1 friends (get-friends)
  (dolist (friend friends)
    (let1 path (string->symbol (string-append ".yoto" friend))
      (tk-button
        path 
        :text friend
        :command (lambda () 
                   (let1 result (send-yo friend)
                     (tk-call path 'configure :text result)
                     (tk-call 'after 1000
                              (format "~A configure -text ~A"
                                      path friend)))))
      (tk-pack path :fill 'x))))
(tk-mainloop)
