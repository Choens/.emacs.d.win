;; #############################################################################
;; -- Mode Specific Config --
;;
;;   - Dired
;;   - Egg
;;   - EPA Mode
;;   - ESS Mode
;;   - Ido Mode
;;   - Markdown
;;   - Org Mode
;;   - Polymode
;;   - Python
;;   - SQL Mode
;;   - TRAMP Mode
;;   - Web Development
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
;; -- Egg --
;; =============================================================================
;;(require 'egg)

;; =============================================================================
;; -- EPA Mode --
;;
;; - Enables Easy PG (GNU PG interface for Emacs)
;; =============================================================================
;(autoload 'epa-file "epa-file.elc")


;; =============================================================================
;; -- ESS Mode --
;;
;; ESS is the layer connecting Emacs to R, SAS, etc.
;; -
;; =============================================================================
(autoload 'ess-R-data-view "ess-R-data-view.el")
(setq-default ess-indent-offset 4)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(setq ess-use-auto-complete t)
(setq ess-help-own-frame nil)

;(ess-toggle-underscore nil)
;(setq ess-S-assign-key (kbd "C-="))
;(ess-toggle-S-assign-key t)



;; =============================================================================
;; -- Ido Mode --
;;
;; - Enables IDO Mode
;; - Enables flexible matching
;; =============================================================================
(ido-mode t)
(setq ido-enable-flex-matching t)


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
;; -- Magit Mode --
;; =============================================================================
;;(add-to-list 'load-path "/path/to/git-modes")
;;(add-to-list 'load-path "/path/to/magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "/path/to/magit/")))
(require 'magit)


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
                              )))
(setq org-todo-keywords
           '((sequence "TODO(t)" "|" "DONE(d!)")
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

(setq-default py-indent-offset 4)
;; Rather than use python.el
;(autoload 'python-mode "python-mode.el" t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; -- IPython --
;(autoload 'ipython "ipython.el" t)
;(setq ipython-command "/usr/bin/ipython")
;(setq-default py-python-command-args '("--pylab"))
;;(require 'ein)



;; =============================================================================
;; -- SQL Mode --
;; =============================================================================
(setq-default sql-indent-offset 4)
;(load-file "~/Ubuntu One/config/sql-connections.el" )


(setq auto-mode-alist
      (append '(("\\.sql$" . sql-mode)
                ("\\.tbl$" . sql-mode)
                ("\\.sp$"  . sql-mode))
              auto-mode-alist))

;(set 'sql-sqlite-program "sqlite3")
;(set 'sql-sybase-program "sqsh")

;; Runs SQL commands asynchronously, improves usability for big stuff.
(set 'sql-preferred-evaluation-method "background")

;; Save SQL History in product-specific files.
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


;; Make SQL Returns look nicer (lines things up correctly).
;; Source: http://www.emacswiki.org/emacs/SqlMode
;; (defvar sql-last-prompt-pos 1
;;   "position of last prompt when added recording started")
;; (make-variable-buffer-local 'sql-last-prompt-pos)
;; (put 'sql-last-prompt-pos 'permanent-local t)

;; (defun sql-add-newline-first (output)
;;   "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'
;;     This fixes up the display of queries sent to the inferior buffer
;;     programatically."
;;   (let ((begin-of-prompt
;;          (or (and comint-last-prompt-overlay
;;                   ;; sometimes this overlay is not on prompt
;;                   (save-excursion
;;                     (goto-char (overlay-start comint-last-prompt-overlay))
;;                     (looking-at-p comint-prompt-regexp)
;;                     (point)))
;;              1)))
;;     (if (> begin-of-prompt sql-last-prompt-pos)
;;         (progn
;;           (setq sql-last-prompt-pos begin-of-prompt)
;;           (concat "\n" output))
;;       output)))

;; (defun sqli-add-hooks ()
;;   "Add hooks to `sql-interactive-mode-hook'."
;;   (add-hook 'comint-preoutput-filter-functions
;;             'sql-add-newline-first))

;; (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)


;; =============================================================================
;; -- TRAMP Mode --
;; =============================================================================
;(setq tramp-default-method "ssh")


;; =============================================================================
;; -- Web Development --
;; =============================================================================
;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist
;      (append '(
;                ("\\.html$" . html-helper-mode)
;                ("\\.asp$" . html-helper-mode)
;                ("\\.phtml$" . html-helper-mode)
;                ("\\.php$" . php-mode)
;                ("\\.PHP$" . php-mode)
;                )
;              auto-mode-alist))



;; -- PHP --
;; (autoload 'php-mode "php-mode.el" t)

;; (setq auto-mode-alist
;;       (append '(("\\.php$" . php-mode)
;;                 ("\\.PHP$" . php-mode))
;;               auto-mode-alist))

;; To use abbrev-mode, add lines like this:
;;   (add-hook 'php-mode-hook
;;     '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;; To make php-mode compatible with html-mode, see http://php-mode.sf.net

;; Many options available under Help:Customize
;; Options specific to php-mode are in
;;  Programming/Languages/Php
;; Since it inherits much functionality from c-mode, look there too
;;  Programming/Languages/C
