(use tk)
(load "./yaoy.scm")

(tk-init '())
(let1 friends (get-friends)
  (dolist (friend friends)
    (let1 path (string->symbol (string-append ".yo" friend))
      (tk-button path 
                 :text friend
                 :command (^[] (send-yo friend)))
      (tk-pack path :fill 'x))))
(tk-mainloop)
