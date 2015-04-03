;; #############################################################################
;; -- init-first.el file --
;;
;; Used to bootstrap the rest of my config.
;;
;; #############################################################################

(setq debug-on-error t)                 ;; Enables debugging

;; Package Archives ------------------------------------------------------------
(require 'package)
(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")
       ))
