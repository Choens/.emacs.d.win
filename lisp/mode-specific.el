;; #############################################################################
;; -- Mode Specific Config --
;;
;; - Dired
;; - EPA
;; - ESHELL
;; - ESS
;; - Ido
;; - flx
;; - Magit
;; - Markdown
;; - Org Mode
;; - Perspective
;; - Polymode
;; - Projectile
;; - Python
;; - SQL
;; - YaSnippet
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
;; -- EPA --
;;
;; - Enables Easy PG (GNU PG interface for Emacs)
;; =============================================================================
(autoload 'epa-file "epa-file.elc")



;; =============================================================================
;; -- ESS --
;;
;; - ESS is the layer connecting Emacs to R, SAS, etc.
;; =============================================================================
;;(autoload 'ess-R-data-view "ess-R-data-view.el")
;;(setq-default ess-directory-containing-R "C:/Program Files/R/R-3.1.2/bin/x64")
(setq inferior-R-program-name "C:/Program Files/R/R-3.1.2/bin/x64/Rterm.exe")
(setq-default ess-indent-offset 4)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(setq ess-use-auto-complete t)
(setq ess-help-own-frame nil)

; Simple fix for the ESS underscore thing --------------------------------------
;(ess-toggle-underscore nil)



;; =============================================================================
;; -- Ido --
;;
;; - Enables IDO Mode
;; - Enables flexible matching
;; =============================================================================
(ido-mode t)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
;; -----------------------------------------------------------------------------
;; -- flx / flx-ido --
;;
;; https://github.com/lewang/flx
;; -----------------------------------------------------------------------------
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)


;; =============================================================================
;; -- Magit --
;; =============================================================================
(require 'magit)



;; =============================================================================
;; -- Markdown --
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
(setq org-agenda-files (list "~/Git/Notes/agenda.org"))
(setq org-default-notes-file "~/Notes/notes.org")

(setq org-agenda-files (quote (
                               "~/Notes/andy-tasks.org"
                               "~/Notes/doh-meetings.org"
                               "~/Notes/doh-tasks.org"
                               "~/Notes/habits.org"
                               "~/Notes/uag-tasks.org"
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
(setq org-confirm-babel-evaluate nil)

(setq org-html-doctype "html5")

;; =============================================================================
;; -- Perspective --
;;
;; https://github.com/nex3/perspective-el
;;
;; =============================================================================
(require 'perspective)
(persp-mode)
(require 'persp-projectile)



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
;; -- Projectile --
;; =============================================================================
(require 'projectile)
(projectile-global-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)


;; =============================================================================
;; -- Python Mode --
;; =============================================================================

;; Not relevant. Python not installed on my DOH Windows computer.


;; =============================================================================
;; -- SQL --
;; =============================================================================

;; SQL Editing -----------------------------------------------------------------
(setq-default sql-indent-offset 4)

(setq auto-mode-alist
      (append '(("\\.sql$" . sql-mode)
                ("\\.tbl$" . sql-mode)
                ("\\.sp$"  . sql-mode))
              auto-mode-alist))

(add-hook 'sql-mode-hook 'my-sql-mode-hook)
    (defun my-sql-mode-hook ()
      (define-key sql-mode-map (kbd "RET") 'newline-and-indent)

      ;; Make # start a new line comment in SQL. This is MySQL-specific
      ;; syntax.

      (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
      (set-syntax-table sql-mode-syntax-table))

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




;; =============================================================================
;; -- SQL --
;; =============================================================================

(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ;;"/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        ;;"/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        ;;"/path/to/yasnippet/snippets"         ;; the default collection
        ))

(yas-global-mode 1)
