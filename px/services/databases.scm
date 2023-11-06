;;; Databases service definitions for PantherX
;;; Reza Alizadeh Majd (r.majd@pantherx.org)

(define-module (px services databases)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (px packages databases)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (mongodb-configuration 
            mongodb-configuration?
            mongodb-configuration-mongodb
            mongodb-configuration-config-file
            mongodb-configuration-data-directory
            mongodb-service-type))

;;;
;;; MongoDB
;;;
;;; @subsubheading MongoDB
;;; @defvr {Scheme Variable} mongodb-service-type
;;; This is the service type for @uref{https://www.mongodb.com/, MongoDB}.
;;; The value for the service type is a @code{mongodb-configuration} object.
;;; @end defvr
;;; @lisp
;;; (service mongodb-service-type)
;;; @end lisp
;;; @deftp {Data Type} mongodb-configuration
;;; Data type representing the configuration of mongodb.
;;; @table @asis
;;; @item @code{mongodb} (default: @code{mongodb})
;;; The MongoDB package to use.
;;; @item @code{config-file} (default: @code{%default-mongodb-configuration-file})
;;; The configuration file for MongoDB.
;;; @item @code{data-directory} (default: @code{"/var/lib/mongodb"})
;;; This value is used to create the directory, so that it exists and is
;;; owned by the mongodb user.  It should match the data-directory which
;;; MongoDB is configured to use through the configuration file.
;;; @end table
;;; @end deftp

(define %default-mongodb-configuration-file
  (plain-file "mongodb.yaml" "# GNU Guix: MongoDB default configuration file
processManagement:
  pidFilePath: /var/run/mongodb/pid
storage:
  dbPath: /var/lib/mongodb
"))

(define-record-type* <mongodb-configuration> mongodb-configuration
                     make-mongodb-configuration
  mongodb-configuration?
  (mongodb mongodb-configuration-mongodb
           (default mongodb))
  (config-file mongodb-configuration-config-file
               (default %default-mongodb-configuration-file))
  (data-directory mongodb-configuration-data-directory
                  (default "/var/lib/mongodb")))

(define %mongodb-accounts
  (list (user-group
          (name "mongodb")
          (system? #t))
        (user-account
          (name "mongodb")
          (group "mongodb")
          (system? #t)
          (comment "Mongodb server user")
          (home-directory "/var/lib/mongodb")
          (shell (file-append shadow "/sbin/nologin")))))

(define mongodb-activation
  (match-lambda
    (($ <mongodb-configuration> mongodb config-file data-directory)
     #~(begin
         (use-modules (guix build utils))
         (let ((user (getpwnam "mongodb")))
           (for-each (lambda (directory)
                       (mkdir-p directory)
                       (chown directory
                              (passwd:uid user)
                              (passwd:gid user)))
                     '("/var/run/mongodb" #$data-directory)))))))

(define mongodb-shepherd-service
  (match-lambda
    (($ <mongodb-configuration> mongodb config-file data-directory)
     (shepherd-service (provision '(mongodb))
                       (documentation "Run the Mongodb daemon.")
                       (requirement '(user-processes loopback))
                       (start #~(make-forkexec-constructor `(,(string-append #$mongodb
                                                               "/bin/mongod")
                                                             "--config"
                                                             ,#$config-file)
                                 #:user "mongodb"
                                 #:group "mongodb"
                                 #:pid-file "/var/run/mongodb/pid"
                                 #:log-file "/var/log/mongodb.log"))
                       (stop #~(make-kill-destructor))))))

(define mongodb-service-type
  (service-type (name 'mongodb)
                (description "Run the MongoDB document database server.")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   (compose list mongodb-shepherd-service))
                                  (service-extension activation-service-type
                                                     mongodb-activation)
                                  (service-extension account-service-type
                                                     (const %mongodb-accounts))))
                (default-value (mongodb-configuration))))

(use-modules (gnu system)
             (gnu bootloader)
             (gnu bootloader grub)
             (gnu system file-systems)
             (px system config))

