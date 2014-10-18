(use tk)
(load "./yaoy.scm")

(tk-init '())
(let1 friends (get-friends)
  (dolist (friend friends)
    (let1 path (string->symbol (string-append ".yo" friend))
      (tk-button path 
                 :text friend
                 :command (lambda () 
                            (send-yo friend)
                             (tk-call path 'configure
                                      :text "Send Yo!")
                             (sys-sleep 1)
                             (tk-call path 'configure
                                      :text friend)))
      (tk-pack path :fill 'x))))
(tk-mainloop)
