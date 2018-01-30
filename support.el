(setq org-latex-default-packages-alist
      '(;; ("T1" "fontenc" t)
        ("" "fontspec" nil)
        ("" "xunicode" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "amssymb" t)
        ("" "hyperref" nil)
        "\\tolerance=1000"))

(require 'rx)
(require 'ox-latex)
(require 'ox-beamer)
(require 'ob-diagrams)
(require 'haskell-mode)
(require 'haskell-customize)

(setq org-plantuml-jar-path "/run/current-system/sw/lib/plantuml.jar")
(setq org-ditaa-jar-path "/run/current-system/sw/lib/ditaa.jar")
(setq org-diagrams-executable "diagrams-builder-svg")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python     . t)
   (emacs-lisp . t)
   (haskell    . t)
   (calc       . t)
   (coq        . t)
   (ledger     . t)
   (ditaa      . t)
   (plantuml   . t)
   (diagrams   . t)
   (sh         . t)
   (sql        . t)
   (dot        . t)
   (restclient . t)))

(setq org-beamer-frame-default-options "fragile")

(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate t)

(setq org-latex-listings 'minted)

(setq org-latex-minted-options
      '(("fontfamily" "courier")
        ("fontsize" "\\footnotesize")
        ("linenos" "true")
        ("xleftmargin" "2em")))

(setq org-export-latex-minted-options
      '(("fontsize" "\\small")
        ("linenos" "true")))

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-export-latex-classes
      '(("beamer" "\\documentclass{beamer}" org-beamer-sectioning)))

(defun extract-code (name)
  "Where name has the form foo.bar.baz"
  (with-temp-buffer
    (insert-file-contents-literally "Main.hs")

    (let ((parts (split-string name "\\.")))
      (while parts
        (re-search-forward
         (rx-to-string `(: word-start ,(if (cdr parts) "describe" "it")
                           space ?\" ,(car parts) ?\")))
        (forward-line)
        (setq parts (cdr parts))))

    (let ((beg (point)))
      (forward-paragraph)
      (let ((str (buffer-substring-no-properties beg (point))))
        (with-temp-buffer
          (insert str)
          (goto-char (point-min))
          (let ((width (skip-chars-forward " ")))
            (goto-char (point-min))
            (while (not (eobp))
              (delete-char width)
              (forward-line)))
          (buffer-string))))))

(defun extract-code-blocks ()
  (goto-char (point-min))
  (while (re-search-forward "^### \\(.+\\)$" nil t)
    (let ((name (match-string 1))
          (line (line-number-at-pos (match-end 0))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert "#+begin_src haskell" ?\n)
      (condition-case err
          (insert (extract-code name))
        (error
         (error "Failed to locate test for %s: %s" name err)))
      (insert "#+end_src"))))

(defun perform-extraction ()
  (find-file (car command-line-args-left))
  (extract-code-blocks)
  (org-beamer-export-to-latex))

(provide 'support)
