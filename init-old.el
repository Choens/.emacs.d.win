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
;; - Mode / Language Specific Config
;; - Custom Lisp
;; - Aliases
;; - Keybindings
;; - Session
;; #############################################################################

;; -------------------------------------
;; Uncomment to enable debugging
;; -------------------------------------
(setq debug-on-error t)



;; =============================================================================
;; -- External Files --
;; =============================================================================

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))



;; =============================================================================
;; Proxy
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
;; =============================================================================

(require 'package)
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ))



;; =============================================================================
;; -- Editor Settings --
;; =============================================================================

;; -----------------------------------------------------------------------------
;; -- Theme --
;; Uncomment preferred theme
;; -----------------------------------------------------------------------------
(require 'solarized-theme)
(if (daemonp)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame
                                 'background-mode
                                 (if (display-graphic-p frame) 'light 'dark))
            (load-theme 'solarized-dark t)))
    (load-theme 'solarized-dark t)
    ;(load-theme 'solarized-light t)
    ;(load-theme 'tango-dark t)
    ;(load-theme 'ir-black t)
    ;(load-theme 'spacegray t)
)
; If the above code works in Windows, remove the following lines.


;; -----------------------------------------------------------------------------
;; -- Frames --
;; -----------------------------------------------------------------------------

;; Menu-bar / Toolbar ----------------------------------------------------------
;; 1 = On, 0 = Off
(menu-bar-mode 1)
(tool-bar-mode 0)

;; Pop-Up / Special buffers must create new frame  -----------------------------
;;(set 'pop-up-frames t)
;; (setq special-display-buffer-names
;;       '("*R*" "*SQL*" "*grep*" ) )

;; -----------------------------------------------------------------------------
;; -- Tabs --
;; -----------------------------------------------------------------------------
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil) ;; Use spaces, not tabs!

;; -----------------------------------------------------------------------------
;; -- Editing --
;;
;; - Col # in mode bar
;; - Scrollbar
;; - Show Parenthesis
;; - Use Bar Cursor
;; - Enable visual bell
;; - Set max # columns = 80
;; - Truncate lines by default
;; - Display the time and date in the mode-line
;; - Display time
;; - Set default message in the *scratch* buffer
;; - Enable CUA Mode (Enables rectangular select) by default
;; - Delete seleted text when typing over selection
;; - Enable Narrow to region.
;; - Enable Inline Images
;; -----------------------------------------------------------------------------
(setq column-number-mode t)
(set-scroll-bar-mode nil)
(show-paren-mode t)
(set-default 'cursor-type 'bar)

; EVAL ------------------------
;(setq blink-cursor-mode nil)
;(setq x-stretch-cursor 1)
; EVAL ------------------------

(setq global-hl-line-mode t)
(setq visible-bell t)
(setq fill-column 80)
(setq truncate-lines t)
(setq display-time-day-and-date t)
(display-time)
(setq cua-mode t)
(cua-selection-mode t)
(delete-selection-mode 1)
(put 'narrow-to-region 'disabled nil)
(iimage-mode)

;; Splash Screen Options -------------------------------------------------------
(setq initial-scratch-message
    "To err is human... to really foul up requires the root password.\n\n")
(setq inhibit-splash-screen t)

;; -----------------------------------------------------------------------------
;; -- Backup --
;;
;; - Enables Emacs backups
;; - Sets the backup directory (~/emacs.d/backups)
;; - Delete old versions
;; - Keep no more than 5 new versions
;; - Keep no more than 2 old versions
;; - Use 'version' control for files
;; -----------------------------------------------------------------------------
(setq  backup-by-copying t )
(setq   backup-directory-alist
    '(("." . "~/.emacs.d/backups")) )
(setq delete-old-versions t )
(setq kept-new-versions 5 )
(setq kept-old-versions 2 )
(setq version-control t )



;; =============================================================================
;; -- Load Custom Lisp --
;; =============================================================================
(load "key-bindings.el" )
(load "misc.el" )
(load "mode-specific.el" )



;; =============================================================================
;; Session Start
;; =============================================================================
;(server-start)
;(org-agenda-list 1)
;(delete-other-windows)

