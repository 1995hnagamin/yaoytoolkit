#!/usr/bin/env gosh
(use rfc.http)
(use rfc.json)
(use srfi-19)
(use file.util)
(use gauche.parseopt)

(load "./yaoytoolkit.scm")

(define *yaoy-config* (expand-path "~/.yaoy"))

(define (get-yaoy-settings)
  (call-with-input-file *yaoy-config*
    (lambda (iport)
      (if iport
        (parse-json iport)
        '()))
    :if-does-not-exist #f))

(define (get-user-info key)
  (let1 result (assoc key (get-yaoy-settings))
    (if result
      (cdr result) #f)))

(define (set-user-info! key value)
  (let1 settings (get-yaoy-settings)
    (call-with-output-file *yaoy-config*
      (lambda (oport)
        (construct-json (assoc-set! settings key value) oport)))))

(define (sendyo)
  (apply openyo-sendyo
         (map get-user-info
              '("endpoint" "api_ver" "api_token" "username"))))

(define (yoall)
  (apply openyo-yoall
         (map get-user-info
              '("endpoint" "api_ver" "api_token"))))

(define (show-history . n)
  (let1 result (if (null? n)
                 (apply openyo-history
                        (map get-user-info
                             '("endpoint" "api_ver" "api_token")))
                 (openyo-history
                   (get-user-info "endpoint")
                   (get-user-info "api_ver")
                   (get-user-info "api_token")
                   :count (car n)))
     (for-each
       (lambda (e)
         (print (format "~A\t~A" (car e) (cdr e))))
       result)))

(define (create-user username password)
  (let1 result (openyo-create-user (get-user-info "endpoint")
                                   (get-user-info "api_ver")
                                   username
                                   password)
    (cond
      ((string? result) 
       (set-user-info! "api_token" result)
       (print (format "User created: ~A" username)))
      ((null? (cdr result))
       (print "Couldn't post request to ~A." 
              (get-user-info "endpoint"))
       (print "error log:")
       (print (car result)))
      (else
        (print "Couldn't create user ~A: ~A"
               username
               (cadr result))))))

(define (initialize endpoint api_ver)
  (set-user-info! "endpoint" endpoint)
  (set-user-info! "api_ver" api_ver))
