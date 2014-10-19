(use tk)
(load "./yaoy.scm")

(tk-init '())
(let1 friends (get-friends)
  (dolist (friend friends)
    (let1 path (string->symbol (string-append ".yoto" friend))
      (tk-button path 
                 :text friend
                 :command (lambda () 
                            (send-yo friend)
                             (tk-call path 'configure
                                      :text "Send Yo!")
                             (tk-call 
                               'after 800
                               (format "~A configure -text ~A"
                                       path friend))))
      (tk-pack path :fill 'x))))
(tk-mainloop)
