;; #############################################################################
;; -- Mode Specific Config --
;;
;;   - Dired
;;   - EPA Mode
;;   - ESS Mode
;;   - Ido Mode
;;   - Magit
;;   - Markdown
;;   - Org Mode
;;   - Polymode
;;   - Python
;;   - SQL Mode
;;
;;
;; #############################################################################



;; =============================================================================
;; -- Dired --
;;
;; - Makes sizes human-readable, sorts version numbers
;; - Orders dotfiles and capital-letters first.
;; - Suggests dired targets
;; =============================================================================
(setq-default dired-listing-switches "-alhv")
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)



;; =============================================================================
;; -- EPA Mode --
;;
;; - Enables Easy PG (GNU PG interface for Emacs)
;; =============================================================================
(autoload 'epa-file "epa-file.elc")



;; =============================================================================
;; -- ESS Mode --
;;
;; ESS is the layer connecting Emacs to R, SAS, etc.
;; -
;; =============================================================================
;;(autoload 'ess-R-data-view "ess-R-data-view.el")
(setq-default ess-indent-offset 4)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(setq ess-use-auto-complete t)
(setq ess-help-own-frame nil)

; Simple fix for the ESS underscore thing --------------------------------------
(setq ess-toggle-underscore nil)
(setq ess-toggle-S-assign-key t)



;; =============================================================================
;; -- Ido Mode --
;;
;; - Enables IDO Mode
;; - Enables flexible matching
;; =============================================================================
(ido-mode t)
(setq ido-enable-flex-matching t)



;; =============================================================================
;; -- Magit Mode --
;; =============================================================================
(require 'magit)



;; =============================================================================
;; -- Markdown Mode --
;; =============================================================================
(autoload 'markdown-mode "markdown-mode.el" t)

(setq auto-mode-alist
      (append '(
                ("\\.text" . markdown-mode)
                ("\\.md" . markdown-mode)
                ("\\.Rmd" . markdown-mode)
                ("\\.Rpres" . markdown-mode)
                )
              auto-mode-alist))



;; =============================================================================
;; -- Org Mode --
;; =============================================================================

;; ORG PIM Config --------------------------------------------------------------
;;(setq org-agenda-files (list "~/Git/Notes/agenda.org"))
(setq org-default-notes-file "~/Git/Notes/notes.org")

(setq org-agenda-files (quote (
                               "~/Git/Notes/andy.org"
                               "~/Git/Notes/doh-admin.org"
                               "~/Git/Notes/doh-health-homes.org"
                               "~/Git/Notes/doh-health-plans.org"
                               "~/Git/Notes/doh-hrob.org"
                               "~/Git/Notes/habits.org"
                               "~/Git/Notes/uag-website.org"
                               "~/Git/Notes/uag-1st-friday-website.org"
                              )))
(setq org-todo-keywords
           '((sequence "TODO(t@)" "IN PROGRESS(i@)" "|" "DONE(d@/!)")
             (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")
             (sequence "|" "CANCELED(c@/!)")
             (sequence "|" "WAITING(w@/!)")
             ))

;; ORG Programming Config ------------------------------------------------------
(setq org-src-fontify-natively t)
(add-hook 'text-mode-hook '(lambda () (auto-fill-mode 1)))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; -- Babelfish --
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa      . t)
   (emacs-lisp . t)
   (latex      . t)
   (python     . t)
   (R          . t)
   (sh         . t)
   (sql        . t)
   (sqlite     . t)
   ))

;; Disables org-mode from asking for permission to run stuff -------------------
;(setq org-confirm-babel-evaluate nil)


;; =============================================================================
;; -- Polymode --
;; =============================================================================

(require 'polymode-common)
(require 'polymode-classes)
(require 'polymode-methods)
(require 'polymode-export)
(require 'polymode-weave)
(require 'poly-R)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.mdw" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rpres" . poly-noweb+r-mode))



;; =============================================================================
;; -- Python Mode --
;; =============================================================================

;; Not relevant. Python not installed on my DOH Windows computer.


;; =============================================================================
;; -- SQL Mode --
;; =============================================================================

;; SQL Editing -----------------------------------------------------------------
(setq-default sql-indent-offset 4)

(setq auto-mode-alist
      (append '(("\\.sql$" . sql-mode)
                ("\\.tbl$" . sql-mode)
                ("\\.sp$"  . sql-mode))
              auto-mode-alist))

;; Runs SQL commands asynchronously, improves usability for big stuff.
(set 'sql-preferred-evaluation-method "background")

;; Save SQL History in product-specific files ----------------------------------
;; Source: http://www.emacswiki.org/emacs/SqlMode
(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       (symbol-name (symbol-value rval))
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       (format "SQL history will not be saved because %s is nil"
               (symbol-name rval))))))

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)


;; Make SQL Returns look nicer (lines things up correctly) ---------------------
;; Source: http://www.emacswiki.org/emacs/SqlMode
(defvar sql-last-prompt-pos 1
  "position of last prompt when added recording started")
(make-variable-buffer-local 'sql-last-prompt-pos)
(put 'sql-last-prompt-pos 'permanent-local t)

(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'
    This fixes up the display of queries sent to the inferior buffer
    programatically."
  (let ((begin-of-prompt
         (or (and comint-last-prompt-overlay
                  ;; sometimes this overlay is not on prompt
                  (save-excursion
                    (goto-char (overlay-start comint-last-prompt-overlay))
                    (looking-at-p comint-prompt-regexp)
                    (point)))
             1)))
    (if (> begin-of-prompt sql-last-prompt-pos)
        (progn
          (setq sql-last-prompt-pos begin-of-prompt)
          (concat "\n" output))
      output)))

(defun sqli-add-hooks ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions
            'sql-add-newline-first))

(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
