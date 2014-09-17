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
(defun open-buy ()
  (interactive)
  (find-file "~/Notes/buy.org"))

(defun open-git ()
  (interactive)
  (find-file "~/Git/"))

(defun open-notes ()
  (interactive)
  (find-file "~/Notes/"))

(defun open-passwords ()
  (interactive)
  (find-file "~/Git/Notes/passwords.org.gpg"))

(defun open-templates-file ()
  (interactive)
  (find-file "~/Git/analytic_templates/file_templates"))
 
(defun open-templates-proj ()
  (interactive)
  (find-file "~/Git/analytic_templates/project_templates"))
 
(defun open-templates-snip ()
  (interactive)
  (find-file "~/Git/analytic_templates/snippets"))

;; -- Emacs-Specific Files --
(defun open-init ()
  (interactive)
  (find-file "~/.emacs.d/"))

(defun open-lisp ()
  (interactive)
  (find-file "~/.emacs.d/lisp/"))

(defun open-sql-conn ()
  (interactive)
  (find-file "~/config/sql-connections.el"))


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
