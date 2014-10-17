#!/usr/bin/env gosh
(use rfc.http)
(use rfc.json)
(use srfi-19)
(use file.util)
(use gauche.parseopt)

(load "./yaoytoolkit.scm")

(define *subcommands* '())

(define-macro (aif test consequent alternative)
  `(let ((it ,test))
     (if it ,consequent ,alternative)))

(define (get-subcommand command)
  (aif (assoc command *subcommands*)
    (cdr it)
    #f))

(define (set-subcommand! command procedure)
  (set! *subcommands* (assoc-set! *subcommands* command procedure)))

(define *yaoy-config* (expand-path "~/.yaoy/config"))

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


(define (send-yo username)
  (apply openyo-sendyo
         (append
           (map get-user-info
                '("endpoint" "api_ver" "api_token"))
           `(,username))))

(define (send-yo-all)
  (apply openyo-yoall
         (map get-user-info
              '("endpoint" "api_ver" "api_token"))))

(define (date->datestring date)
  (date->string date "~Y/~m/~d ~H:~M:~S"))

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
         (print (format "~A\t~A" (date->datestring (cdr e)) (car e))))
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

(define (initialize-yaoy-config endpoint api-ver)
  (set-user-info! "endpoint" endpoint)
  (set-user-info! "api_ver" api-ver))

(define (print-all . exprs)
  (for-each print exprs))

(define (help)
  (print-all "usage: yaoy <command> [<args>]"
             ""
             "  yo \t Send yo to your friend"
             "  yoall \t Send yo to all of your friends"
             "  history \t Show history of when and who yoed you"
             "  init \t Initialize yaoy configuration file"
             "  register \t Create new user"))

(define (yo-help)
  (print-all "usage: yaoy yo <username>"
             "  send yo to <username>"))

(define (history-help)
  (print-all "usage: yaoy history [n]"
             " Show history of when and who yoed yo (at most n)"))

(define (init-help)
  (print-all "usage: yaoy init [endpoint] [api_ver]"
             (format "  Initialize yaoy confiruration file (~A)" *yaoy-config*)))


(define (yo args)
  (if (null? args)
    (yo-help)
    (send-yo (car args))))
(set-subcommand! "yo" yo)

(define (yoall args)
  (send-yo-all))
(set-subcommand! "yoall" yoall)

(define (history args)
  (cond
    ((null? args) (show-history))
    ((string->number (car args)) (show-history (car args)))
    (else (print (format "error: ~A is not a number" (car args)))
          (history-help))))
(set-subcommand! "history" history)

(define (get-prompt prompt)
  (display prompt)
  (flush)
  (read-line))

(define get-pass get-prompt)

(define (length>? x k)
  (cond
    ((null? x) #f)
    ((zero? k) x)
    (else (length>? (cdr x) (- k 1)))))

(define-macro (let-with-list lst binds . body)
  `(let ,(map (lambda (bind n)
                `(,(car bind) (aif (length>? ,lst ,n) 
                                   (car it)
                                   ,(cadr bind))))
              binds (liota +inf.0))
     ,@body))

(define (init args)
  (let-with-list args ((endpoint (get-prompt "input endpoint> "))
                       (api-ver (get-prompt "input api_ver> ")))
    (if (string->number api-ver)
      (initialize-yaoy-config endpoint api-ver)
      (begin
        (print (format "error: ~A is not a number" api-ver))
        (init-help)))))
(set-subcommand! "init" init)

(define (register args)
  (let ((username (if (null? args)
                    (get-prompt "input username> ")
                    (car args)))
        (password (if (null? (cdr args))
                    (get-pass "input password> ")
                    (cadr args))))
    (create-user username password)))
(set-subcommand! "register" register)

(define (main args)
  (let1 args (cdr args)
    (if (null? args)
      (help)
      (aif (get-subcommand (car args))
        (it (cdr args))
        (print (class-of (car args)))))))
