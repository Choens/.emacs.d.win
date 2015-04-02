;; #############################################################################
;; -- Andy's init.el file --
;;
;; Sections:
;; - External Files
;; - Proxy Settings
;; - Package Repos
;; - Editor Settings
;;   - Theme
;;   - Tabs
;;   - Editing
;;   - Backup
;;   - Proxy
;; - Mode / Language Specific Config
;; - Custom Lisp
;; - Aliases
;; - Keybindings
;; - Session
;; #############################################################################

;; -------------------------------------
;; Uncomment to enable debugging
;; -------------------------------------
;(setq debug-on-error t)



;; =============================================================================
;; -- External Files --
;; =============================================================================

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))



;; =============================================================================
;; -- Proxy --
;; =============================================================================

(setq url-proxy-services
      '(("http"  . "proxy.health.state.ny.us:8080")
        ("https" . "proxy.health.state.ny.us:8080")))

(setq url-http-proxy-basic-auth-storage
      (list (list "proxy.health.state.ny.us:8080"
                  (cons "AXC38"
                        (base64-encode-string "january2015")))))



;; =============================================================================
;; -- Package Archives --
;; This is where we get new packages.
;; =============================================================================

(require 'package)
(setq package-archives'(
                        ("gnu" . "http://elpa.gnu.org/packages/")
                        ;; I tend to use melpa preferentially over marmalade.
                        ;;("marmalade" . "http://marmalade-repo.org/packages/")
                        ("melpa" . "http://melpa.milkbox.net/packages/")
                        ("org" . "http://orgmode.org/elpa/")
                       ))



;; =============================================================================
;; -- Editor Settings --
;; =============================================================================

;; -----------------------------------------------------------------------------
;; -- Theme --
;; If you want, you can have one theme on the console and a different theme in
;; the gui. Either way, uncomment preferred theme(s)
;; -----------------------------------------------------------------------------
(require 'solarized-theme)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                ;(load-theme 'ir-black t)
                ;(load-theme 'solarized-dark t)
                (load-theme 'solarized-light t)
                ;(load-theme 'spacegray t)
                ;(load-theme 'tango-dark t)
                )
              )
  ;(load-theme 'ir-black t)
  ;(load-theme 'solarized-dark t)
  (load-theme 'solarized-light t)
  ;(load-theme 'spacegray t)
  ;(load-theme 'tango-dark t)

)

;; -----------------------------------------------------------------------------
;; -- Frames --
;; -----------------------------------------------------------------------------
;; Menu-bar / Toolbar-----------------------------------------------------------
;; Although I disable the menu bar, it is always accessible if you hit F10.
(menu-bar-mode 1) ; 1 = On, 0 = Off
(tool-bar-mode 1) ; 1 = On, 0 = Off

;; Pop-Up / Special buffers must create new frame ------------------------------
;;(set 'pop-up-frames t)
;; (setq special-display-buffer-names
;;       '("*R*" "*SQL*" "*grep*" ) )

;; -----------------------------------------------------------------------------
;; -- Tabs --
;; -----------------------------------------------------------------------------
(setq-default tab-width 4)              ; Specifies default offset.
(setq-default c-basic-offset 4)         ; Specifies offset for C
(setq-default indent-tabs-mode nil)     ; Use spaces, not tabs!

;; -----------------------------------------------------------------------------
;; -- Editing --
;; -----------------------------------------------------------------------------
(setq column-number-mode t)             ; Col # in mode bar
(set-scroll-bar-mode nil)               ; Scrollbar
(show-paren-mode t)                     ; Show Parenthesis
(set-default 'cursor-type 'bar)         ; Use Bar Cursor

; EVAL ------------------------
;(setq blink-cursor-mode nil)
;(setq x-stretch-cursor 1)
; EVAL ------------------------

(setq global-hl-line-mode t)
(setq visible-bell t)                   ; Enable visual bell
(setq fill-column 80)                   ; Set max # columns = 80
(setq truncate-lines t)                 ; Truncate lines by default
(setq display-time-day-and-date t)      ; Display time & date in mode-line
(setq cua-mode t)                       ; Enable CUA Mode (Enables rectangular select) by default
(cua-selection-mode t) 
(delete-selection-mode 1)				; Delete seleted text when typing over selection
(put 'narrow-to-region 'disabled nil)	; Enable Narrow to region.
(iimage-mode)							; Enable Inline Images

;; Splash Screen Options -------------------------------------------------------
; I prefer my own silly commentary.
(setq initial-scratch-message
      ;;"To err is human... to really foul up requires the root password.\n\n")
      "Nullius in verba.\n\n")
; I don't need a splash screen from a text editor.
(setq inhibit-splash-screen t)

;; -----------------------------------------------------------------------------
;; -- Backup --
;; -----------------------------------------------------------------------------
(setq  backup-by-copying t )            ; Enables Emacs backups
(setq   backup-directory-alist          ; Sets the backup directory
    '(("." . "~/.emacs.d/backups")) )     
(setq delete-old-versions t )           ; Delete old versions
(setq kept-new-versions 5 )             ; Keep no more than 5 new versions
(setq version-control t )               ; Use 'version' control for files



;; =============================================================================
;; -- Load Custom Lisp --
;; =============================================================================
(load "key-bindings.el" )
(load "misc.el" )
(load "mode-specific.el" )



;; =============================================================================
;; Session Start
;; =============================================================================
;; I go back and forth on using this.
;;(server start)
