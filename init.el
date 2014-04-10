;; #############################################################################
;; -- Andy's init.el file --
;;
;; Sections:
;; - External Files
;; - Package Repos
;; - Editor Settings
;;   - Tabs
;;   - Editor Settings
;;   - Backup Settings
;; - Mode / Language Specific Config
;; - Custom Lisp
;; - Aliases
;; - Keybindings
;; - Start Session
;; #############################################################################

;; -------------------------------------
;; Enable debugging
;; -------------------------------------
;;(setq debug-on-error t)

;; =============================================================================
;; -- External Files --
;; =============================================================================
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/elpa")
;(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-theme")


;; =============================================================================
;; -- Package Archives --
;; =============================================================================
(load "package" )  ;; TODO - I can't seem to get autoload to work. Why?
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/") ) )


;; =============================================================================
;; -- Editor Settings --
;; =============================================================================

;; -----------------------------------------------------------------------------
;; -- Theme --
;; Uncomment preferred theme
;; -----------------------------------------------------------------------------
; TODO (load-theme 'solarized-light t)
;;(load-theme 'solarized-dark t)

;; -----------------------------------------------------------------------------
;; -- Frames --
;;
;; - Disables menu-bar
;; - Enables tool-bar
;; - Pop-Up / Special buffers must create new frame
;; -----------------------------------------------------------------------------
(menu-bar-mode t)
(tool-bar-mode t)

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
;; -- Editor Settings --
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
; TODO (bar-cursor-mode 1)
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

;; -----------------------------------------------------------------------------
;; -- Backup Settings --
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
;; -- Mode Specific Settings --
;; =============================================================================
;TODO (load "mode-specific-config.el" )

;; =============================================================================
;; -- Miscellaneous Lisp --
;; =============================================================================
(load "misc.el" )

;; =============================================================================
;; -- Aliases --
;; =============================================================================
(defalias 'ttl 'toggle-truncate-lines)

;; =============================================================================
;; -- Key Bindings --
;; =============================================================================
;; This is last b/c it references functions defined in the preceding menagerie.
; TODO (load "key-bindings.el" )


;; =============================================================================
;; -- Start New Session --
;;
;; - Sets EDE mode to global
;; - Inhibits Emacs startup message
;; - Opens useful files by default
;; - Initializes the Emacs server
;; =============================================================================
(global-ede-mode t)
(setq inhibit-startup-message t)
;(find-file "~/Documents/Andy.org")
;(find-file "~/Documents/Work.org")
;;(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )