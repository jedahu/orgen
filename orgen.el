(defvar orgen--orig-message
  (symbol-function 'message))

(defun orgen--info (format &rest args)
  (apply orgen--orig-message (concat "\nORGEN: " format) args))

(defun orgen--read (proj-root)
  (with-temp-buffer
    (insert-file-contents-literally
     (concat proj-root ".orgen.el"))
    (goto-char (point-min))
    (read (current-buffer))))

(defun orgen--noninteractive-init (proj-root)
  (when noninteractive
    (orgen--info "init")
    (require 'package)
    (require 'cl)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (setq package-user-dir (concat proj-root ".elisp"))
    (package-initialize)
    (orgen--info "install packages")
    (let* ((proj (orgen--read proj-root))
           (required-pkgs (plist-get (cdr proj) :require))
           (pkgs (remove-if #'package-installed-p
                           (cons 'org-plus-contrib required-pkgs))))
      (when pkgs
        (package-refresh-contents)
        (dolist (pkg pkgs)
          (package-install pkg)))
      proj)))

(defun orgen-noninteractive-publish ()
  (let* ((proj-root default-directory)
         (proj (orgen--noninteractive-init proj-root)))
    (require 'org)
    (orgen-publish proj-root proj)))

(with-eval-after-load 'org
  (require 'ox-publish)
  (require 'ox-html)
  (require 'ob-shell)
  (require 'ansi-color)

  (defun orgen--fontify-ansi-colors (limit)
    (ansi-color-apply-on-region (point) limit))

  (define-derived-mode ansi-mode fundamental-mode "ansi"
    "Fundamental mode that understands ANSI colors."
    (require 'ansi-color)
    (font-lock-add-keywords nil '((orgen--fontify-ansi-colors))))

  (defun orgen--store-message (format &rest args)
    (with-current-buffer (get-buffer-create "*orgen-log*")
      (insert (apply 'format format args) "\n")))

  (defmacro orgen--with-store-messages (&rest body)
    `(progn
       (fset 'message (symbol-function 'orgen--store-message))
       (unwind-protect
           (let ((standard-output (get-buffer-create "*orgen-log*")))
             ,@body)
         (fset 'message orgen--orig-message))))

  (defun orgen--next-heading-or-rule ()
    (let ((heading (save-excursion
                     (ignore-errors (outline-next-heading))
                     (point)))
          (rule (save-excursion
                  (search-forward-regexp "^-----+$")
                  (forward-line 0)
                  (point))))
      (goto-char (min heading rule))
      (if (< heading rule) 'heading 'rule)))

  (defun orgen--org-skip-drawer ()
    (search-forward-regexp "^:END:$")
    (forward-line))

  (defun orgen--org-remove-contents (backend)
    (when (eq 'orgen-html backend)
      (show-all)
      (ignore-errors
        (org-map-entries
         (lambda ()
           (forward-line 0)
           (forward-line)
           (when (member "nav" org-scanner-tags)
             (while (org-at-drawer-p)
               (orgen--org-skip-drawer))
             (let ((beg (point)))
               (backward-char)
               (outline-next-heading)
               (backward-char)
               (when (< beg (point))
                 (delete-region beg (point))))))
         t nil))))

  (defun orgen--translate-org-link-html (link contents info)
    (let ((props (plist-get link 'link)))
      (if (string= "proj" (plist-get props :type))
          (progn (plist-put props :type "file")
                 (plist-put props :path (concat "/" (plist-get props :path)))
                 (replace-regexp-in-string
                  "file:///\\|file://[a-zA-Z]:/" "/"
                  (org-export-with-backend 'html link contents info)))
        (org-export-with-backend 'html link contents info))))

  (defun orgen--org-expand-navigation (backend)
    (when (eq 'orgen-html backend)
      (while (re-search-forward "^[ \t]*#\\+\\(HTML_NAV\\):" nil t)
        (replace-match "INCLUDE" t t nil 1))))

  (defun orgen-publish (proj-root &optional proj)
    (orgen--info "begin publishing")
    (orgen--with-store-messages
     (prefer-coding-system 'utf-8)
     (let* ((proj (or proj (orgen--read proj-root)))
            (plist (cdr proj))
            (buffer-file-coding-system 'utf-8)
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8)
            (org-babel-default-header-args (plist-get plist :babel-header-args))
            (org-babel-default-inline-header-args (plist-get plist :babel-inline-header-args))
            (org-html-htmlize-output-type 'css)
            (org-publish-use-timestamps-flag (plist-get plist :use-timestamps))
            (org-link-abbrev-alist (plist-get plist :link-abbrevs))
            (org-export-babel-evaluate t)
            (org-confirm-babel-evaluate nil)
            (default-directory proj-root))
       (org-add-link-type "proj"
                          (lambda (path)
                            (find-file (concat doc-root path))))
       (org-publish-projects
        (plist-get plist :org-projects)))))

  (defmacro orgen--with-message-on-error (&rest body)
    `(condition-case e
         (progn
           ,@body)
       (error
        (funcall orgen--orig-message "\nORGEN: ERROR: %s" e)
        (funcall orgen--orig-message "\nORGEN: LOG: %s"
                 (with-current-buffer (get-buffer-create "*orgen-log*")
                   (buffer-string)))
        (when noninteractive
          (kill-emacs 1)))))

  (defun orgen-org-babel-tangle-publish-inplace (_ filename _)
    (orgen--info "tangle %s" filename)
    (orgen--with-message-on-error
     (org-babel-tangle-file filename)))

  (defun orgen-org-html-publish-to-html (plist filename pub-dir)
    (orgen--info "publish %s" filename)
    (orgen--with-message-on-error
     (org-publish-org-to 'orgen-html filename ".html" plist pub-dir)))

  (defun org-export-collect-headlines (info &optional n scope)
    (let* ((scope (cond ((not scope) (plist-get info :parse-tree))
                        ((eq (org-element-type scope) 'headline) scope)
                        ((org-export-get-parent-headline scope))
                        (t (plist-get info :parse-tree))))
           (limit (plist-get info :headline-levels))
           (n (if (not (wholenump n)) limit
                (min (if (eq (org-element-type scope) 'org-data) n
                       (+ (org-export-get-relative-level scope info) n))
                     limit))))
      (org-element-map (org-element-contents scope) 'headline
        (lambda (headline)
          (unless (or
                   (org-element-property :footnote-section-p headline)
                   (member "notoc" (org-export-get-tags headline info nil t)))
            (let ((level (org-export-get-relative-level headline info)))
              (and (<= level n) headline))))
        info)))

  (defun org-html-inline-src-block (inline-src-block contents info)
    "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
    (let ((lang (org-element-property :language inline-src-block))
          (code (org-html-format-code inline-src-block info))
          (label
           (let ((lbl (and (org-element-property :name inline-src-block)
                           (org-export-get-reference inline-src-block info))))
             (if (not lbl) "" (format " id=\"%s\"" lbl)))))
      (format "<code class=\"src src-%s\"%s>%s</code>" lang label
              (org-trim code))))

  (add-hook 'org-export-before-processing-hook #'orgen--org-expand-navigation)
  (add-hook 'org-export-before-parsing-hook #'orgen--org-remove-contents)

  (org-export-define-derived-backend
   'orgen-html 'html
   :translate-alist '((link . orgen--translate-org-link-html))))

(provide 'orgen)
