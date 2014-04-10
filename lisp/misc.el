;; #############################################################################
;; misc.el
;;
;; Collection of miscellaneous lisp I have collected over the years.
;;
;; Note:
;; Where possible, I document the original source of the function, but the
;; function here may or may not be the same as the original source function(s).
;;
;; - Open Functions
;; - Highlight Functions
;; - toggle-fullscreen
;; - Text Manipulation
;; #############################################################################


;; =============================================================================
;; -- Open Functions --
;; =============================================================================

;; -- User-Specific Files --
;(defun open-todo ()
;  (interactive)
;  (find-file "~/Documents/ToDo.org"))

;(defun open-goals ()
;  (interactive)
;  (find-file "~/Documents/Goals.org"))

;(defun open-passwords ()
;  (interactive)
;  (find-file "~/Documents/Passwords.org.gpg"))

;(defun open-andy ()
;  (interactive)
;  (find-file "~/Documents/Andy.org"))

;(defun open-shopping ()
;  (interactive)
;  (find-file "~/Documents/Shopping.org"))

;; -- Emacs-Specific Files --
;(defun open-init ()
;  (interactive)
;  (find-file "~/.emacs.d/init.el"))

;(defun open-sql-conn ()
;  (interactive)
;  (find-file "~/Ubuntu One/config/sql-connections.el"))


;; =============================================================================
;; -- Highlight Functions --
;; =============================================================================

(defun hl-todo ()
	"Highlight all instances of the phrase 'TODO - '"
	(interactive)
	(highlight-phrase "TODO - "))

(defun hl-fixme ()
	"Highlight all instanaces of the phrase 'FIXME - '"
	(interactive)
	(highlight-phrase "FIXME - "))

(defun hl-note ()
	"Highlight all instanaces of the phrase 'NOTE - '"
	(interactive)
	(highlight-phrase "NOTE - "))


;; =============================================================================
;; -- toggle-fullscreen --
;; Source: http://www.emacswiki.org/emacs/FullScreen#toc17
;; =============================================================================
;(defun toggle-fullscreen ()
;  "Toggles full screen on X11"
;  (interactive)
;  (when (eq window-system 'x)
;    (set-frame-parameter
;     nil 'fullscreen
;     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))


;; =============================================================================
;; -- Text Manipulation --
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Clears the current buffer.
;; -----------------------------------------------------------------------------
(defun clear-buffer ()
  (interactive)
  (mark-whole-buffer)
  (delete-region (region-beginning) (region-end)))

;; -----------------------------------------------------------------------------
;; Use M-w and C-w on entire line when a
;; region is not selected.
;; Source: http://www.emacswiki.org/emacs/WholeLineOrRegion
;; -----------------------------------------------------------------------------
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
