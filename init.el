;; #############################################################################
;; -- Andy's init.el file --
;;
;; Sections:
;; - External Files
;; - Package Repos
;; - Editor Settings
;;   - Tabs
;;   - Editing
;;   - Backup
;; - Mode / Language Specific Config
;; - Custom Lisp
;; - Aliases
;; - Keybindings
;; - Server
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
      '(
        ("http" . "localhost:8888")
        ("https" . "localhost:8888")
        )
      )

        ;("http" . "axc38@websense2.health.state.ny.us:8080")
        ;("https" . "axc38@websense2.health.state.ny.us:8080")



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
; TODO
(require 'solarized-theme)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                ;(load-theme 'solarized-light-theme t)
                (load-theme 'solarized-dark t)
                ;(load-theme 'spacegray t)
                ;(load-theme 'ir-black t)
                )
              )
    (load-theme 'solarized-dark t)
    ;(load-theme 'spacegray t)
)
; If the above code works in Windows, remove the following lines.
;(load-theme 'solarized-light t)
;(load-theme 'solarized-dark t)
;(load-theme 'tango-dark t)
;(load-theme 'ir-black t)

;; -----------------------------------------------------------------------------
;; -- Frames --
;;
;; - Disables menu-bar
;; - Enables tool-bar
;; - Pop-Up / Special buffers must create new frame
;; -----------------------------------------------------------------------------
;;(menu-bar-mode 1)
;;(tool-bar-mode 1)

;; - Pop-Up / Special buffers must create new frame  ---------------------------
;;(set 'pop-up-frames t)
;; (setq special-display-buffer-names
;;       '("*R*" "*SQL*" "*grep*" ) )

;; -----------------------------------------------------------------------------
;; -- Tabs --
;;
;; - Sets default indentation
;; - Use spaces not tabs
;; -----------------------------------------------------------------------------
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

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
(setq initial-scratch-message
    "To err is human... to really foul up requires the root password.\n\n")
(setq cua-mode t)
(cua-selection-mode t)
(delete-selection-mode 1)
(put 'narrow-to-region 'disabled nil)
(iimage-mode)

;;(setq default-directory "C:/Users/AXC38/")

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
;; Server
;; =============================================================================
;;(server-start)
