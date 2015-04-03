;; #############################################################################
;; SQL Connections
;;
;; Preconfigured database connections
;; #############################################################################

;; -----------------------------------------------------------------------------
;; NOTE:
;; This is a demo file.
;; I store my connections info in ~/config/. If you don't want to put
;; it your file in that location, make sure you edit the SQL section of the
;; mode-specific-config.el file.
;; -----------------------------------------------------------------------------

(setq sql-connection-alist
  (quote (
          (
           "Connection Name"
           (sql-product (quote postgres))
           (sql-server "192.168.1.2")
           (sql-user "UserName")
           (sql-database "DatabaseName")
          )

          (
           "Connection Name"
           (sql-product (quote sybase))
           (sql-server "192.168.1.2")
           (sql-user "UserName")
           (sql-database "DatabaseName")
          )

         )))


;; -----------------------------------------------------------------------------
;; Database Config Notes:
;;
;; Postgres:
;; - Edit ~/.pgpass to enable passwordless logins.
;;
;; SQL Server:
;; - If you are using sqsh to connect to SQL Server, tell Emacs to connect to
;;   a Sybase DB because sqsh uses Sybase style command-line parameters.
;; -----------------------------------------------------------------------------
