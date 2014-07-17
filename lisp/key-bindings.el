;; #############################################################################
;; -- Keybindings --
;;
;; Note - Shortcuts to built in functions tend to start with a C-x.
;;        Shortcuts to Emacs extensions tend to start with a C-c.
;;
;; #############################################################################

;; =============================================================================
;; -- Emacs Key Bindings --
;; =============================================================================

;; -----------------------------------------------------------------------------
;; -- Emacs Interface --
;; -----------------------------------------------------------------------------
;; Go to a specific line.
(global-set-key (kbd "C-x g") 'goto-line)


;; -----------------------------------------------------------------------------
;; -- Window Management (Default) --
;; -----------------------------------------------------------------------------
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; -----------------------------------------------------------------------------
;; -- Tmux Style Window Management --
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; -----------------------------------------------------------------------------
;; -- Toggles --
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
(global-set-key [(f9)] 'speedbar)
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key [f12] 'menu-bar-mode)


;; =============================================================================
;; -- Mode Specific Key Bindings --
;; =============================================================================

;; -- Ess --
(setq ess-S-assign-key (kbd "C-x <"))

;; -- Org-Mode --
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)
