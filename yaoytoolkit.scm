(define (status-code http-response) (car http-response))

(define (success-status? response)
  (equal? (status-code response) "200"))

(define (http-response-jsonbody response)
  (parse-json-string (list-ref response 2)))

(define (get-openyo endpoint asset query-list)
  (values->list (http-get endpoint (cons asset query-list)
                          :secure #t)))

(define (post-openyo endpoint asset query-list)
  (values->list (http-post endpoint (cons asset query-list)
                           ""
                           :secure #t)))

(define (datestring->date str)
  (string->date str "~Y-~m-~d ~H:~M:~S ~z"))

(define (openyo-history endpoint api-ver api-token :key (count #f))
  (let1 response
        (get-openyo endpoint
                    "/history/"
                    (append `((api_ver ,api-ver)
                              (api_token ,api-token))
                            (if count (list count) '())))
    (if (success-status? response)
      (map (lambda (elem)
             (cons (cdar elem) (datestring->date (cdadr elem))))
        (vector->list
          (cdr (assoc "result" (http-response-jsonbody response)))))
      '())))

(define (openyo-sendyo endpoint api-ver api-token username)
  (post-openyo endpoint
               "/yo/"
               `((api_ver ,api-ver)
                 (api_token ,api-token)
                 (username ,username))))

(define (openyo-yoall endpoint api-ver api-token)
  (post-openyo endpoint
               "/yoall/"
               `((api_ver ,api-ver)
                 (api_token ,api-token))))

(define (openyo-friends-count endpoint api-ver api-token)
  (get-openyo endpoint
              "/friends_count/"
              `((api_ver ,api-ver)
                (api_token ,api-token))))

(define (openyo-get-list-friends endpoint api-ver api-token)
  (get-openyo endpoint
              "/list_friends/"
              `((api_ver ,api-ver)
                (api_token ,api-token))))

(define (openyo-list-friends endpoint api-ver api-token)
  (let1 response (openyo-get-list-friends endpoint api-ver api-token)
    (if (success-status? response)
      (vector->list
        (cdr (assoc "result" (http-response-jsonbody response))))
      #f)))

(define (openyo-add-imkayac endpoint api-ver
                            username password kayac-id 
                            :key (kayac-pass #f) 
                                 (kayac-sec #f))
  (post-openyo endpoint
               "/add_imkayac/"
               (append `((api_ver ,api-ver)
                         (username ,username)
                         (password ,password)
                         (kayac_id ,kayac-id))
                       (if kayac-pass (list kayac-pass) '())
                       (if kayac-id (list kayac-id) '()))))

(define (openyo-new-api-token endpoint api-ver username password)
  (get-openyo endpoint
              "/config/new_api_eoken/"
              `((api_ver ,api-ver)
                (username ,username)
                (password ,password))))

(define (openyo-create-user endpoint api_ver username password)
  (let1 response (post-openyo endpoint
                              "/config/create_user/"
                              `((api_ver ,api-ver)
                                (username ,username)
                                (password ,password)))
     (if (success-status? response)
       (let* ((body (http-response-jsonbody response))
              (code (cdr (assoc "code" body)))
              (result (cdr (assoc "result" body))))
         (if (equal? code "200")
           result
           (list code result)))
       (list response))))
