;; -*- lexical-binding: t -*-
;; Copyright (C) 2019-2020  Christian Barthel <bch@online.de>

;; Author: Christian Barthel
;; Keywords: TLA+

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package defines TLA+ Major Mode to manipulate TLA+ specification
;; files.  Use the `describe-mode' to see additional commentary on
;; its features, use and configuration.


;;; Code:

(require 'seq)                          ; reduce (fold) function
(require 'widget)                       ; TLC Configuration dialogue
(eval-when-compile
  (require 'wid-edit))


;; -------------------------------------------------------------------
;; Customization variables:

(defcustom tla+-mode-hook '()
  "Normal hook run when entering TLA+ mode."
  :type 'hook
  :options '()
  :group 'tla+)

(defcustom tla+-java-path "java"
  "Path to the `java' binary"
  :type 'file
  :group 'tla+)

(defcustom tla+-tlatools-path
  nil
  "Path to the TLA+ `tlatools.jar' toolbox java archive"
  :type 'file
  :group 'tla+)

(defcustom tla+-dvipdf-path
  "dvipdf"
  "Path to the `dvipdf' program"
  :type 'file
  :group 'tla+)

(defcustom tla+-dvips-path
  "dvips"
  "Path to the `dvips' program"
  :type 'file
  :group 'tla+)


(defcustom tla+-tlatex-arguments
  " -shade -number "
  "Arguments which will be used when running `TLaTeX'"
  :type 'string
  :group 'tla+)

(defcustom tla+-tlc-deadlock
  " -deadlock "
  "Tell `TLC' not to check for deadlocks"
  :type 'string
  :group 'tla+)

(defcustom tla+-tlc-simulate
  "  "
  "Tell `TLC' to do simulation (-simulate)"
  :type 'string
  :group 'tla+)

(defcustom tla+-tlc-depth
  " -depth 1000 "
  "Tell `TLC' to do max. X steps (default: 100)"
  :type 'string
  :group 'tla+)

(defcustom tla+-tlc-coverage
  "  "
  "Tell `TLC' to print coverage every X minutes"
  :type 'string
  :group 'tla+)

(defcustom tla+-tlc-workers
  " -workers 2 "
  "Tell `TLC' how many threads to generate."
  :type 'string
  :group 'tla+)


(defcustom tla+-option-list
  '()
  "Assoc list for TLC Options"
  :type 'string)

(defcustom tla+-dot-convert
  "out.png"
  "If non-nil, convert states.dot to the filename given by the string"
  :type 'string)

(defcustom tla+-dot-binary
  nil
  "path to dot binary"
  :type 'string)


;; -------------------------------------------------------------------
(defvar tla+-mode-map
  (let
      ((map (make-sparse-keymap))
       (menu-map (make-sparse-keymap "TLA+"))

       (operator-map (make-sparse-keymap))

       (action-operator-map (make-sparse-keymap))
       (temporal-operator-map (make-sparse-keymap))
       (export-map (make-sparse-keymap))

       (modul-operators (make-sparse-keymap))
       (modul-sequences-operators (make-sparse-keymap))
       (modul-naturals-operators (make-sparse-keymap))
       )
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\t" 'ispell-complete-word)
    (define-key map (kbd "C-c C-t e") 'tla+-run-tlatex-dvi)
    (define-key map (kbd "C-c C-t o") 'tla+-open-dvi)
    (define-key map (kbd "C-c C-t c") 'tla+-run-sany)
    (define-key map (kbd "C-c C-t p") 'tla+-run-pluscal)
    (define-key map (kbd "C-c C-t m") 'tla+-run-model)

    ;; Main TLA+ Menu:
    (bindings--define-key map [menu-bar tla+]
      (cons "TLA+" menu-map))

    ;; Modul Operators
    (bindings--define-key menu-map [modul-operators]
      (cons "Insert Modul Operator.." modul-operators))

    ;; Naturals Operators
    (bindings--define-key modul-operators
        [modul-naturals-operator]
      (cons "Naturals" modul-naturals-operators))

    (bindings--define-key
        modul-naturals-operators [add-nat]
      '(menu-item "+"
                  (lambda () (interactive)
                    (insert "+"))
                  :help "Addition"))

    (bindings--define-key
        modul-naturals-operators [sub-nat]
      '(menu-item "-"
                  (lambda () (interactive)
                    (insert "-")
                    (backward-char 2))
                  :help "Subtraction"))

    (bindings--define-key
        modul-naturals-operators [mult-nat]
      '(menu-item "*"
                  (lambda () (interactive)
                    (insert "*"))
                  :help "Multiplication"))

    (bindings--define-key
        modul-naturals-operators [div-nat]
      '(menu-item "/"
                  (lambda () (interactive)
                    (insert "/"))
                  :help "Division"))

    (bindings--define-key
        modul-naturals-operators [exp-nat]
      '(menu-item "^"
                  (lambda () (interactive)
                    (insert "^"))
                  :help "Exponentiation"))

    (bindings--define-key
        modul-naturals-operators [ft-nat]
      '(menu-item ".."
                  (lambda () (interactive)
                    (insert ".."))
                  :help "From..To"))

    (bindings--define-key
        modul-naturals-operators [div2-nat]
      '(menu-item "\\div"
                  (lambda () (interactive)
                    (insert "\div"))
                  :help "Integer Division"))


    (bindings--define-key
        modul-naturals-operators [mod-nat]
      '(menu-item "%"
                  (lambda () (interactive)
                    (insert "%"))
                  :help "Modulo"))


    (bindings--define-key
        modul-naturals-operators [leq-nat]
      '(menu-item "\\leq"
                  (lambda () (interactive)
                    (insert "\\leq"))
                  :help "Lower Equal"))

    (bindings--define-key
        modul-naturals-operators [geq-nat]
      '(menu-item "\\geq"
                  (lambda () (interactive)
                    (insert "\\geq"))
                  :help "Greater Equal"))

    (bindings--define-key
        modul-naturals-operators [lt-nat]
      '(menu-item "<"
                  (lambda () (interactive)
                    (insert "<"))
                  :help "Lower than"))

    (bindings--define-key
        modul-naturals-operators [gt-nat]
      '(menu-item ">"
                  (lambda () (interactive)
                    (insert ">"))
                  :help "Greater than"))


    ;; Sequence Operators
    (bindings--define-key modul-operators
        [modul-sequences-operator]
      (cons "Sequences" modul-sequences-operators))

    (bindings--define-key
        modul-sequences-operators [seq-s]
      '(menu-item "Seq(S)"
                  (lambda () (interactive)
                    (insert "Seq(S)")
                    (backward-char 2))
                  :help
                  "Set of all Sequences of Elements of Set `S'"))

    (bindings--define-key
        modul-sequences-operators [head-s]
      '(menu-item "Head(s)"
                  (lambda () (interactive)
                    (insert "Head(s)")
                    (backward-char 2))
                  :help "Head element of sequence `s'"))

    (bindings--define-key
        modul-sequences-operators [tail-s]
      '(menu-item "Tail(s)"
                  (lambda () (interactive)
                    (insert "Tail(s)")
                    (backward-char 2))
                  :help "Tail sequence of sequence `s'"))

    (bindings--define-key
        modul-sequences-operators [append-s]
      '(menu-item "Append(s,e)"
                  (lambda () (interactive)
                    (insert "Tail(S)")
                    (backward-char 2))
                  :help "Tail sequence of sequence `S'"))

    (bindings--define-key
        modul-sequences-operators [circ-s]
      '(menu-item "s \\circ t"
                  (lambda () (interactive)
                    (insert "s \\circ t")
                    (backward-char 2))
                  :help "concatenation of sequence s and t"))

    (bindings--define-key
        modul-sequences-operators [len-s]
      '(menu-item "Len(s)"
                  (lambda () (interactive)
                    (insert "Len(s)")
                    (backward-char 2))
                  :help "length of sequence s"))


    ;; Operators
    (bindings--define-key menu-map [operator]
      (cons "Insert Operator.." operator-map))
    (bindings--define-key map [menu-bar tla+ sep] menu-bar-separator)
    ;; Action Operators
    (bindings--define-key operator-map [action-operator]
      (cons "Action Operator" action-operator-map))

    (bindings--define-key
        action-operator-map [primed-operator]
      '(menu-item "e'" tla+-operator-primed
                  :help "Value of e in final state of a step"))

    (bindings--define-key
        action-operator-map [stutter-operator]
      '(menu-item "[A]_e" tla+-operator-stutter
                  :help "A \\/ (e' = e)"))

    (bindings--define-key
        action-operator-map [nonstutter-operator]
      '(menu-item "<<A>>_e" tla+-operator-nonstutter
                  :help "A /\\ (e' \\\\ = e)"))

    (bindings--define-key
        action-operator-map [enabled-operator]
      '(menu-item "ENABLED A" tla+-operator-enabled
                  :help "An A step is possible"))

    (bindings--define-key
        action-operator-map [unchanged-operator]
      '(menu-item "UNCHANGED e" tla+-operator-unchanged
                  :help "e = e'"))

    (bindings--define-key
        action-operator-map [composition-operator]
      '(menu-item "A \\cdot B" tla+-operator-composition
                  :help "Composition of actions"))

    ;; Temporal operators:
    (bindings--define-key operator-map [temporal-operator]
      (cons "Temporal Operator" temporal-operator-map))

    (bindings--define-key
        temporal-operator-map [always-temporal-operator]
      '(menu-item "[]F" tla+-operator-alwaystrue
                  :help "F is always true"))

    (bindings--define-key
        temporal-operator-map [eventually-temporal-operator]
      '(menu-item "<>F" tla+-operator-eventually
                  :help "F is eventually true"))

    (bindings--define-key
        temporal-operator-map [wf-temporal-operator]
      '(menu-item "WF_e(A)" tla+-operator-weakfairness
                  :help "Weak fairness for action A"))

    (bindings--define-key
        temporal-operator-map [sf-temporal-operator]
      '(menu-item "SF_e(A)" tla+-operator-strongfairness
                  :help "Strong fairness for action A"))

    (bindings--define-key
        temporal-operator-map [leadsto-temporal-operator]
      '(menu-item "F ~> G" tla+-operator-leadsto
                  :help "F leads to G"))

    ;; Export Menu:
    (bindings--define-key menu-map [export]
      (cons "Export.." export-map))

    (bindings--define-key export-map [export-pdf]
      '(menu-item "Run TLatex and create PDF" tla+-run-tlatex-pdf
                  :help "Typeset TLaTex and create PDF Document"))

    (bindings--define-key export-map [export-dvi]
      '(menu-item "Run TLatex and create DVI" tla+-run-tlatex-dvi
                  :help "Typeset TLaTex and create DVI Document"))

    (bindings--define-key export-map [export-ps]
      '(menu-item "Run TLatex and create PS" tla+-run-tlatex-ps
                  :help "Typeset TLaTex and create PS Document"))

    (bindings--define-key export-map [export-open]
      '(menu-item "Open DVI Viewer" tla+-open-dvi
                  :help "Open DVI Viewer"))

    ;; Main Menu
    (bindings--define-key map [menu-bar tla+ sep] menu-bar-separator)

    (bindings--define-key map [menu-bar tla+ tla+-run-shell]
      '(menu-item "Run TLA+ REPL Shell" tla+-run-shell
                  :help
                  "Run tlc2.REPL"
                  "(Execute TLA+ Read Eval Print Loop)"))

    (bindings--define-key map [menu-bar tla+ tla+-run-model]
      '(menu-item "Run TLC Model Checker" tla+-run-model
                  :help
                  "Run tlc2.TLC"
                  "(Execute the TLC Model Checker)"))

    (bindings--define-key map [menu-bar tla+ tla+-conf-model]
      '(menu-item "Create TLC Model" tlc-widget-start
                  :help
                  "Create a TLC Model"
                  "(Open the TLC Configuration dialogue)"))


    (bindings--define-key map [menu-bar tla+ tla+-run-pluscal]
      '(menu-item "Run PlusCal Translator" tla+-run-pluscal
                  :help
                  "Run pcal.trans"
                  "(Translator (PlusCal algorithm language -> TLA+)"))

    (bindings--define-key map [menu-bar tla+ tla+-run-sany]
      '(menu-item "Run Syntax Checker" tla+-run-sany
                  :help
                  "Run SANY2 "
                  "(syntax checker for TLA+ specifications) "))

    (bindings--define-key map [menu-bar tla+ tla+-update-note]
      '(menu-item "Add Update Comment" tla+-add-update-note
                  :help "Add Update Note"))

    (bindings--define-key map [menu-bar tla+ tla+-create-module]
      '(menu-item "Create new Module" tla+-create-module
                  :help "Create a new TLA+ Module"))
                                        ;(bindings--define-key map [menu-bar tla+ sep] menu-bar-separator)
    map)
  "Keymap for `tla+-mode'.")


(defconst tla+-font-lock-keywords
  (list
   '("\\\\\\*.*$" . font-lock-comment-face)
   '("(\\*.*\\*)" . font-lock-comment-face)
   '("\\<\\(MODULE\\|CONSTANTS?\\|UNCHANGED\\|VARIABLES?\\|THEOREM\\|LOCAL\\|ASSUME\\|LET\\|EXTENDS?\\)\\>"
     . font-lock-builtin-face)
   '("\\<\\(TRUE\\|FALSE\\|OTHER\\|BOOLEAN\\|DOMAIN\\)\\>" . font-lock-type-face)
   '("\\<\\(CONSTANT\\|SUBSET\\|ENABLED\\|UNCHANGED\\|INSTANCE\\|WITH\\|EXCEPT\\|UNION\\|IN\\|DOMAIN\\|EXCEPT\\|IF\\|THEN\\|ELSE\\|CHOOSE\\)\\>" . font-lock-keyword-face)

   '("\\('\\w*'\\)" . font-lock-variable-name-face)
   "Highlighting for TLA+ Operators, Keywords etc."))


;;;###autoload (add-to-list 'auto-mode-alist '("\\.tla\\'" . tla+-mode))
;;;###autoload
(define-derived-mode tla+-mode fundamental-mode "TLA+"
  "Major mode for editing TLA+ specification files.

This package defines TLA+ Major Mode to manipulate TLA+
specifications as invented by Leslie Lamport [1].

In this mode, syntax highlighting is activated for TLA+
specification files.  Various operators can be inserted
by `tla+-operator-NAME'.

Features:
   - Syntax Highlighting for TLA+ Specification Files
   - Running `SANY Syntactic Analyzer' on TLA+ files,
     default keybinding: `C-c C-t c'.
   - Exporting TLA+ Specification files with TLATeX to
     DVI, PS or PDF files (Export it with `C-c C-t e'
     and open the viewer `C-c C-t o')
   - Inserting TLA+ Operators with an easy to use menu
     or typical Emacs keystrokes.
   - Use templates to generate Modules
   - An Emacs widget to create TLC configuration files
   - Options to run the TLC model checker on TLA+
     specifications (`C-c C-t m').
   - Translate PlusCal code to TLA+ specification
     with `C-c C-t p'.

Installation:
   To use tla+-mode, you have to load the tla+-mode.el file
   with
       (load </path/to/tla+-mode.el>)
   and use it with
       (require tla+-mode)
   The TLA+ Toolbox can be downloaded at [1].

Configuration:
   You must at least set the variable to the TLA2 Toolbox.  This
   can be done by setting the variable in the emacs configuration
   file (i.e. ~/.emacs or ~/.emacs.d/init.el)
      (setq tla+-tlatools-path </path/to/tla2tools.jar>)
   or with:
      M-x customize-group <RET> tla+

   You may also set the following paths:
      tla+-java-path
      tla+-dvipdf-path
      tla+-dvips-path
      tla+-tla+-tlatex-arguments
   TLC options can be set globally or in the TLC configuration
   GUI dialogue:
      tla+-tlc-deadlock
      tla+-tlc-simulate
      tla+-tlc-depth
      tla+-tlc-coverage
      tla+-tlc-workers
   To get help on one of the variables:
      C-h v <variablename>
      M-x describe-variable <variablename>

Getting, and Using the tlatoolbox:
[1]  <https://tla.msr-inria.inria.fr/tlatoolbox/dist/>
[2]  <https://lamport.azurewebsites.net/tla/tools.html>
[3]  <https://lamport.azurewebsites.net/tla/standalone-tools.html>
\\{tla+-mode-map}
Turning on Text mode runs the normal hook `text-mode-hook'."
  (setq-local font-lock-defaults
              '(tla+-font-lock-keywords))
  (setq-local text-mode-variant t)
  (setq-local comment-start "\\* ")
  (setq-local require-final-newline mode-require-final-newline))


(defun tla+-create-module ()
  "Insert the base template for a new Module.
A new module will be created with the typical structure of a TLA+
specification file."
  (interactive)
  (let ((modulename
         (concat (file-name-base (buffer-file-name)))))
    (insert
     (concat
      "------------------- MODULE "
      modulename
      " -----------------------\n"
      "EXTENDS Naturals, ...\n"
      "VARIABLE ..\n"
      "------------------------------------------------------------\n"
      "============================================================\n"
      "\\* Modification History\n"
      "\\* Created "
      (current-time-string) " by " user-full-name "\n"))))


(defun tla+-add-update-note ()
  "Add an Update note"
  (interactive)
  (let ((text "...describe changes here..."))
    (goto-char (point-max))
    (insert (concat "\\* Updated " (current-time-string) " by "
                    user-full-name "\n"))
    (insert (concat "\\*    " text))
    (goto-char (- (point-max)  (length text)))))


(defun tla+-run-sany ()
  "This function runs tla2sany.SANY (TLA syntax checker)
The function executes the TLA+ Syntax checker and reports errors
in a new frame.  Errors are clickable buttons and the user can
directly go to the error line.

It is meant to be run from a TLA+ Specification buffer.

Operation:
1. execute the tla2sany.SANY program
2. the output of the shell-command is inserted into a newly
   created buffer
3. errors are converted into clickable buttons by using the
   `tla+/find-error-marks' function."
  (interactive)
  (if (not tla+-tlatools-path)
      (error "need the TLA+ toolbox java archive: tla+-tlatools-path"))
  (let* ((filename (buffer-file-name))
         (sanybuffer
          (get-buffer-create
           (format "*tla2sany.SANY* [%s]" (buffer-name))))
         (output   (shell-command-to-string (concat
                                             tla+-java-path
                                             " -cp "
                                             tla+-tlatools-path
                                             " tla2sany.SANY "
                                             filename))))
    (save-excursion
      (split-window-below)
      (other-window 1)
      (switch-to-buffer sanybuffer)
      (erase-buffer)
      (insert output)
      (tla+/find-error-marks)
      (other-window 1))))


(defun tla+-run-dot ()
  (if tla+-dot-binary
      (let ((cmd (format "%s -Tpng states.dot > %s"
                         tla+-dot-binary
                         tla+-dot-convert)))
        (shell-command-to-string cmd))))


(defun tla+-run-model (cfgfile)
  "Run the tlc2.TLC Model checker
Run the tlc2.TLC model checker on a TLA+ buffer with a given
moden CFGFILE.  The function constructs an absolute path to
a model file spec.cfg and uses the current buffer as TLA+
specification.  It then executes the tlc2.TLC model checker
with the given model.

Note: The TLA+ specification file is a relative path.
"
  (interactive
   (list (read-file-name
          "Filename (or enter to use current buffer): "
          (file-name-directory (buffer-file-name))
          nil nil
          (concat
           (file-name-base (buffer-file-name))
           ".cfg" ))))
  (if (not tla+-tlatools-path)
      (error "need the TLA+ toolbox java archive: tla+-tlatools-path"))
  (let* ((filename (buffer-file-name))
         (tlcbufname (format "*tlc2.TLC* [%s]" (buffer-name)))
                                        ;(tlcbuffer (get-buffer-create
                                        ;            (format "*tlc2.TLC* [%s]" (buffer-name))))
         (tlcbuffer (get-buffer tlcbufname))
         (cubuffer  (current-buffer))
         ;; XXX: tlc2.TLC -config /full/path/to/TLA.cfg TLA.tla
         ;;  not sure why the tla file itself should not be a
         ;;  full path?
         (finame (concat (file-name-base (buffer-file-name)) ".tla"))
         (tlcconf (concat finame "tlcopt"))
         (loadconf (tla+/read-options tlcconf))
         (opt-deadlock (tla+/coalesce
                        (tla+/get-option 'tla+-tlc-deadlock)
                        tla+-tlc-deadlock))
         (opt-simulate (tla+/coalesce
                        (tla+/get-option 'tla+-tlc-simulate)
                        tla+-tlc-simulate))
         (opt-depth (tla+/coalesce
                     (tla+/get-option 'tla+-tlc-depth)
                     tla+-tlc-depth))
         (opt-workers (tla+/coalesce
                       (tla+/get-option 'tla+-tlc-workers)
                       tla+-tlc-workers))
         (cmd (concat
               tla+-java-path " -cp " tla+-tlatools-path
               " tlc2.TLC "
               " -dump dot states.dot "
               opt-deadlock " "
               opt-simulate " "
               opt-depth " "
               tla+-tlc-coverage " "
               opt-workers " "
               " -config " cfgfile " "
               finame))
         (output (shell-command-to-string cmd))
         (dot    (tla+-run-dot)))
    (save-excursion
      (message (concat "Ran cmd: " cmd))
      (if tlcbuffer
          (progn
            (switch-to-buffer tlcbuffer)
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert output)
            (tla+/find-error-marks)
            (setq buffer-read-only 't)
            (switch-to-buffer cubuffer))
        (progn
          (let ((newbuf (get-buffer-create tlcbufname)))
            (split-window-below)
            (other-window 1)
            (switch-to-buffer newbuf)
            (insert output)
            (tla+/find-error-marks)
            (setq buffer-read-only 't)
            (other-window 1)))))))


(defun tla+-run-pluscal ()
  "This function runs pcal.trans (PlusCal Translator to TLA+)
This function executes the PlusCal Translator on the current
buffer (Assumption: the buffer is a TLA+ buffer that contains
PlusCal code).
On success, the buffer will be reloaded and a message will be
printed.  On failure (i.e. the exit code of the shell command
was not 0), an error message will be printed.
"
  (interactive)
  (let* ((filename (buffer-file-name))
         (oldfile (replace-regexp-in-string
                   ".tla$" ".old" filename))
         (pcalbuf (get-buffer-create
                   (format "*pcal.trans* [%s]" (buffer-name))))
         (cmd (concat
               tla+-java-path " -cp " tla+-tlatools-path
               " pcal.trans "filename ))
         (output (shell-command cmd)))
    (if (not tla+-tlatools-path)
        (error
         "need the TLA+ toolbox java archive: tla+-tlatools-path"))
    (if (= output 0)
        (progn
          (revert-buffer nil t)
          (message (concat
                    "Successfully Transalted"
                    " - You can visit the old file at "
                    oldfile)))
      (progn
        (split-window-below)
        (other-window 1)
        (switch-to-buffer pcalbuf)
        (erase-buffer)
        (insert
         (shell-command-to-string cmd))
        (message "Translation Failed")))))


(defun tla+-open-dvi (buffername)
  "Function to open a DVI file.
This function starts the `xdvi' utility and opens the TLA+
specification file as DVI.  It must be created beforehand
with the `tla+-run-tlatex-dvi'.

The argument BUFFERNAME is a TLA+ Buffer that will be opend
as DVI file.
"
  (interactive "b")
  (save-excursion
    (switch-to-buffer buffername)
    (let* ((filename (buffer-file-name))
           (dviname (replace-regexp-in-string
                     ".tla$" ".dvi" filename)))
      (shell-command (concat "xdvi " dviname " &")))))

(defun tla+-operator-alwaystrue ()
  (interactive) (insert " []F"))

(defun tla+-operator-eventually ()
  (interactive) (insert " <>F"))

(defun tla+-operator-weakfairness ()
  (interactive) (insert " WF_e(A)"))

(defun tla+-operator-strongfairness ()
  (interactive) (insert " SF_e(A)"))

(defun tla+-operator-leadsto ()
  (interactive) (insert " F~>G "))

(defun tla+-operator-stutter ()
  (interactive) (insert " [A]_e"))

(defun tla+-operator-primed ()
  (interactive) (insert " e'"))

(defun tla+-operator-nonstutter ()
  (interactive) (insert " <<A>>_e"))

(defun tla+-operator-enabled ()
  (interactive) (insert " ENABLED A"))

(defun tla+-operator-unchanged ()
  "insert UNCHANGED operator.
Variables declared as unchanged will have the same value when a
step is taken to a new state, i.e. e' = e. "
  (interactive) (insert " UNCHANGED e"))

(defun tla+-operator-cdot ()
  "insert operator A \\cdot B
A \\cdot B describes the composition of actions A, B."
  (interactive) (insert " A \\cdot B"))

(defun tla+-run-shell ()
  "Execute TLA REPL"
  (interactive)
  (shell (get-buffer-create "*TLA+ Repl*"))
  ;; java -cp ../tla2tools.jar tlc2.REPL
  (insert (format "%s -cp %s tlc2.REPL"
                  tla+-java-path tla+-tlatools-path)))

(defun tla+-run-tlatex (type)
  "Run TLaTeX and generate DVI, PDF or PS file.
This function runs `pdflatex(1)' and geneartes a DVI file which can
be viewed with `xdvi'.  If TYPE is 'pdf or 'ps, a PDF or PostScript
file will be generated as well.
The function executes one or two shell commands synchronously."
  (interactive
   (list
    (completing-read
     "Format: "
     '(("pdf" 1) ("dvi" 2) ("ps" 3)) nil t "")))
  (let* ((filename (buffer-file-name))
         (output   (shell-command (concat
                                   tla+-java-path
                                   " -cp "
                                   tla+-tlatools-path
                                   " tla2tex.TLA "
                                   tla+-tlatex-arguments
                                   " "
                                   filename)))
         (dvipath (replace-regexp-in-string
                   ".tla$" ".dvi" filename))
         (convert2pdf (concat
                       tla+-dvipdf-path " " dvipath))
         (convert2ps (concat
                      tla+-dvips-path " " dvipath)))
    (progn
      (if (not tla+-tlatools-path)
          (error
           "need the TLA+ toolbox java archive: tla+-tlatools-path"))
      (cond
       ((eq type 'pdf)
        (shell-command convert2pdf))
       ((eq type 'ps)
        (shell-command convert2ps)))
      type)))


(defun tla+-run-tlatex-pdf ()
  "Create PDF file from TLA+ Specification.
This function creates a PDF file of a TLA+ Specification by
`tla+-run-tlatex'"
  (interactive) (tla+-run-tlatex 'pdf))
(defun tla+-run-tlatex-dvi ()
  "Create DVI file from TLA+ Specification.
This function creates a DVI file of a TLA+ Specification by
`tla+-run-tlatex'"
  (interactive) (tla+-run-tlatex 'dvi))
(defun tla+-run-tlatex-ps ()
  "Create PS file from TLA+ Specification.
This function creates a PS file of a TLA+ Specification by
`tla+-run-tlatex'"
  (interactive) (tla+-run-tlatex 'ps))


;; internal functions:

(define-button-type
  'tla+/sany-button-pressed
  'action 'tla+/sany-button-pressed
  'follow-link t
  'help-echo "Goto Source"
  'help-args "test")

;; example: (make-button 1 12 :type 'custom-button)

(defun tla+/sany-button-pressed (button)
  "Find the position and goto a sany error position.
This function will be executed when a user clicks on a button inside
the SANY error report buffer.  This is done by selecting the other
window and going to the line mentioned in the error message.
The parameter BUTTON is currently unused.

Steps done by this function:
1. get the button at the current position of the `point'
2. get the text label of the button which is usually a line like
       line XX, column YY
3. Get line number and column count of the error message
4. select the other-window (it is assumed that this is the *.tla
   specification where SANY was executed.
5. goto line and column.
"
  (let* ((pos (point))
         (buffername
          (replace-regexp-in-string
           "\\*.*\\* \\[\\(.*\\)\\]"
           "\\1"
           (buffer-name)))
         (the-button (button-at pos))
         (text (button-label the-button))
         (s (string-match
             (concat
              "line \\([0-9][0-9]*\\), " ; line XYZ
              "\\(column\\|col\\) "     ; "column" or "col"
              "\\([0-9][0-9]*\\)")      ; XYZ
             text))
         (line (string-to-number (match-string 1 text)))
         (col (string-to-number (match-string 3 text))))
                                        ;(message (format "Button pressed: %s" text))
    (progn
      (switch-to-buffer-other-window buffername)
      (goto-char (point-min))
      (goto-line line)
      (forward-char (- col 1))
      )))

(defun tla+/make-link-button (begin end)
  "Generate button from BEGIN until END
This function generates a new button starting at BEGIN until END.
The button will execute the `tla+/sany-button-pressed' when a user
clicks on it.
"
  (make-button begin end :type 'tla+/sany-button-pressed))

(defun tla+/find-error-marks ()
  "find error marks reported by SANY.
This function tries to find errors reported by SANY and converts them
into clickable buttons.  By clicking on an error, the point will be
set to the other-buffer on that particular line and column.  Currently
supported:
    line XX, column YY
    line XX, col YY
XX,YY are natural numbers.

The procedure works by:
1. finding a line that may look like an error message as described
   above.
2. find the first offset and the last one
3. execute the `tla+/make-link-button' function to create a new button
"
  (progn
    (goto-char (point-min))
    (while (re-search-forward
            (concat
             "line \\([0-9][0-9]*\\), "
             "\\(column\\|col\\) "
             "\\([0-9][0-9]*\\)")
            (point-max) t)
      (let* ((rstart (match-beginning 0))
             (linenum (match-beginning 1))
             (colnum (match-beginning 3))
             (rend (+ colnum (- (length (int-to-string colnum)) 0)))
             )
        (message "%s %s" linenum colnum)
                                        ;(message "%d %d" rstart rend)
        (tla+/make-link-button rstart rend)
        ))))


(defun tla+/read-options (filename)
  (if (file-exists-p filename)
      (let ((data (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-string))))
        (setq tla+-option-list (read data)))
    (setq tla+-option-list '())))

(defun tla+/write-options (filename)
  (write-region
   (prin1-to-string tla+-option-list)
   nil filename))

(defun tla+/add-option (filename key value)
  (let ((newopt (list (cons key value))))
    (tla+/read-options filename)
    (setq tla+-option-list
          (assq-delete-all key tla+-option-list))
    (setq tla+-option-list
          (append tla+-option-list newopt))
    (tla+/write-options filename)))

(defun tla+/get-option (key)
  (cdr (assoc key tla+-option-list)))

(defun tla+/coalesce (var1 var2)
  (if var1 var1 var2))

;; GUI
(defvar widget-tlc-confname)
(defvar widget-tlc-specname)
(defvar widget-tlc-init)
(defvar widget-tlc-next)
(defvar widget-tlc-props)
(defvar widget-tlc-inv)
(defvar widget-tlc-constant)
(defvar widget-tlc-constraint)

(defun tlc-widget-start ()
  "Start TLC Config GUI"
  (interactive)
  (progn
    (tlc-widget-example (buffer-name))
    (message "%s" (buffer-name))))

(defun tlc-widget-example (filename)
  "Create the widgets from the Widget manual."
  (interactive)
  (let ((cfgname (replace-regexp-in-string
                  ".tla$" ".cfg" filename)))
    (switch-to-buffer (format "*TLC Configuration* [%s]"
                              cfgname))
    (kill-all-local-variables)
    (make-local-variable 'widget-tlc-specname)
    (make-local-variable 'widget-tlc-init)
    (make-local-variable 'widget-tlc-next)
    (make-local-variable 'widget-tlc-props)
    (make-local-variable 'widget-tlc-inv)
    (make-local-variable 'widget-tlc-constant)
    (make-local-variable 'widget-tlc-constraint)
    (setq widget-tlc-props '())
    (setq widget-tlc-inv '())
    (setq widget-tlc-constant '())
    (setq widget-tlc-constraint '())
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (widget-insert "TLC Configuration Dialogue\n\n")
    (setq widget-tlc-confname
          (widget-create 'editable-field
                         :size 18
                         :format "Config Name.......: %v "
                         (replace-regexp-in-string
                          ".tla$" ".cfg" filename)
                         filename))
    (widget-insert "\n")

    (setq widget-tlc-specname
          (widget-create 'editable-field
                         :size 18
                         :format "Specification Name: %v " ""))
    (widget-insert "\n")

    (setq widget-tlc-init
          (widget-create 'editable-field
                         :size 18
                         :format "Init..............: %v " "Init"))
    (widget-insert "\n")

    (setq widget-tlc-next
          (widget-create 'editable-field
                         :size 18
                         :format "Next..............: %v " "Next"))
    (widget-insert "\n")

    (widget-insert "\nList of properties (PROPERTY): \n")
    (setq widget-tlc-props
          (widget-create 'editable-list
                         :entry-format "%i %d %v"
                         :notify
                         (lambda (widget &rest ign)
                           (setq widget-tlc-props widget))
                         :value '()
                         '(editable-field :value "")))

    (widget-insert "\nList of invariants (INVARIANT): \n")
    (setq widget-tlc-inv
          (widget-create 'editable-list
                         :entry-format "%i %d %v"
                         :notify
                         (lambda (widget &rest ign)
                           (setq widget-tlc-inv widget))
                         :value '()
                         '(editable-field :value "Spec => TypeInv")))

    (widget-insert
     "\nList of Constants (CONSTANTS): \n")
    (setq widget-tlc-constant
          (widget-create 'editable-list
                         :entry-format "%i %d %v"
                         :notify
                         (lambda (widget &rest ign)
                           (setq widget-tlc-constant widget))
                         :value '()
                         '(editable-field :value "")))

    (widget-insert
     "\nList of constraints (CONSTRAINT): \n")
    (setq widget-tlc-constraint
          (widget-create 'editable-list
                         :entry-format "%i %d %v"
                         :notify
                         (lambda (widget &rest ign)
                           (setq widget-tlc-constraint widget))
                         :value '()
                         '(editable-field :value "")))
    (widget-insert "--------------------------------------")
    (widget-insert "\nTLC Options\n\n")
    (widget-insert "No Deadlocks.......: " )
    (widget-create 'checkbox
                   :notify
                   (lambda (&rest ignore)
                     (let ((cfgtlc
                            (replace-regexp-in-string
                             ".*\\[\\(.*\\)\\].*"
                             "\\1tlcopt"
                             (buffer-name))))
                       (tla+/read-options cfgtlc)
                       (let ((value
                              (cdr
                               (assoc 'tla+-tlc-deadlock
                                      tla+-option-list))))
                         (if (or (equal nil value)
                                 (string= value " "))
                             (tla+/add-option
                              cfgtlc
                              'tla+-tlc-deadlock
                              " -deadlock ")
                           (tla+/add-option
                            cfgtlc
                            'tla+-tlc-deadlock
                            " ")
                           ))))
                   nil
                   )

    (widget-insert "\nSimulation Mode....: ")
    (widget-create 'checkbox
                   :notify
                   (lambda (&rest ignore)
                     (let ((cfgtlc
                            (replace-regexp-in-string
                             ".*\\[\\(.*\\)\\].*"
                             "\\1"
                             (buffer-name))))
                       (tla+/read-options cfgtlc)
                       (let ((value
                              (cdr
                               (assoc 'tla+-tlc-simulate
                                      tla+-option-list))))
                         (if (or (equal value nil)
                                 (string= value " "))
                             (tla+/add-option
                              cfgtlc
                              'tla+-tlc-simulate
                              " -simulate ")
                           (tla+/add-option
                            cfgtlc
                            'tla+-tlc-simulate
                            " ")
                           ))))
                   nil)


    (widget-insert "\nDepth (# behaviors): ")
    (widget-create 'menu-choice
                   :tag "Choose"
                   :value "100"
                   :help-echo "Choose -depth"
                   :notify
                   (lambda (widget &rest ignore)
                     (let ((cfgtlc
                            (replace-regexp-in-string
                             ".*\\[\\(.*\\)\\].*"
                             "\\1"
                             (buffer-name))))
                       (tla+/add-option cfgtlc
                                        'tla+-tlc-depth
                                        (format " -depth %s "
                                                (widget-value widget)))))
                   '(item :tag "100" :value "100")
                   '(choice-item "250" )
                   '(choice-item "300" )
                   '(choice-item "500" )
                   '(choice-item "1000" )
                   '(editable-field :menu-tag "No option" "100"))

    (widget-insert "Threads............: ")
    (widget-create 'menu-choice
                   :tag "Choose"
                   :value "1"
                   :help-echo "Choose -workers"
                   :notify
                   (lambda (widget &rest ignore)
                     (let ((cfgtlc
                            (replace-regexp-in-string
                             ".*\\[\\(.*\\)\\].*"
                             "\\1"
                             (buffer-name))))
                       (tla+/add-option cfgtlc
                                        'tla+-tlc-workers
                                        (format " -workers %s "
                                                (widget-value widget)))))
                   '(item :tag "1" :value "1")
                   '(choice-item "2" )
                   '(choice-item "4" )
                   '(choice-item "8" )
                   '(choice-item "16" )
                   '(editable-field :menu-tag "No option" "100"))

    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest ignore)
                     (let*
                         ((confname
                           (replace-regexp-in-string
                            ".tla$" ".cfg"
                            (replace-regexp-in-string
                             "\\*TLC Configuration\\* \\[\\(.*\\)\\]"
                             "\\1"
                             (buffer-name))
                            ))
                          (config-buffer (get-buffer-create confname))
                          (config-buffer-file-name (buffer-file-name config-buffer))
                          (str-confname (widget-value widget-tlc-confname))
                          (str-specname (widget-value widget-tlc-specname))
                          (str-init (widget-value widget-tlc-init))
                          (str-next (widget-value widget-tlc-next))
                          (lst-props (widget-value widget-tlc-props))
                          (lst-inv  (widget-value widget-tlc-inv))
                          (lst-const (widget-value widget-tlc-constant))
                          (lst-constraint (widget-value widget-tlc-constraint)))
                       (progn
                         (if (and confname
                                  (file-exists-p confname))
                             (rename-file confname
                                          (concat confname ".old."
                                                  (number-to-string
                                                   (nth 1 (current-time)))
                                                  )))
                         (switch-to-buffer config-buffer)
                         (erase-buffer)
                         (tla+-mode)
                         (insert
                          (format
                           (concat "\\* -*- mode: tla+; -*-\n"
                                   "\\* TLA+ Config %s\n"
                                   "\\* Created %s\n"
                                   "%s\n"
                                   "\\* properties\n"
                                   "%s\n"
                                   "\\* invariants\n"
                                   "%s\n"
                                   "\\* constants\n"
                                   "%s\n"
                                   "\\* constraints\n"
                                   "%s\n"
                                   "\n")
                           str-confname
                           (current-time-string)
                           (if (not (string= str-specname "")) ""
                             (concat "INIT " str-init "\n"
                                     "NEXT " str-next "\n"))
                           (if (not (equal lst-props '()))
                               (seq-reduce
                                'concat
                                (mapcar
                                 (lambda (s) (concat "PROPERTY " s))
                                 (mapcar (lambda (s) (concat s "\n"))
                                         lst-props)) "") "")
                           (if (not (equal lst-inv '()))
                               (seq-reduce
                                'concat
                                (mapcar
                                 (lambda (s) (concat "INVARIANT " s))
                                 (mapcar (lambda (s) (concat s "\n"))
                                         lst-inv)) "") "")
                           (if (not (equal lst-const '()))
                               (seq-reduce
                                'concat
                                (mapcar
                                 (lambda (s) (concat "CONSTANT " s))
                                 (mapcar (lambda (s) (concat s "\n"))
                                         lst-const)) "") "")
                           (if (not (equal lst-constraint '()))
                               (seq-reduce
                                'concat
                                (mapcar
                                 (lambda (s) (concat "CONSTRAINT " s))
                                 (mapcar (lambda (s) (concat s "\n"))
                                         lst-constraint)) "") ""))))))
                   "Get Configuration")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest ignore)
                     (tlc-widget-example
                      (replace-regexp-in-string
                       "\\*TLC Configuration\\* \\[\\(.*\\)\\]"
                       "\\1"
                       (buffer-name)))
                     )
                   "Reset Form")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)))


(defun tla+/load-symbols ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("<"   . ?<)
          (">"   . ?>)
          ("<>"  . ?◇)
          ("<="  . ?≤)
          ("\\leq"  . ?≤)
          ("\\geq"  . ?≥)
          (">="  . ?≥)
          ("~>" . ?⇝)
          ("\\E" . ?∃)
          ("\\A" . ?∀)
          ("\\cup" . ?∪)
          ("\\union" . ?∪)
          ("\\cap" . ?∩)
          ("\\intersect" . ?∩)
          ("\\in"    . ?∈)
          ("\\notin" . ?∉)
          ("#" . ?≠)
          ("/=" . ?≠)
          ("<<" . ?⟨ )
           (">>" . ?⟩ )
          ("[]" . ?□)
          ("\\equiv" . ?≡)
          ("<=>"    . ?≡)
          ("/\\"    . ?∧)
          ("\\/"     . ?∨)
          ("=>"     . ?⇒)    ; ⇒
          ("=="     . ?≜)
          ("\\neg"   . ?¬)
          ("\\lnot"  . ?¬)
          ("\\neg"   . ?¬)
          )))


(add-hook 'tla+-mode-hook 'tla+/load-symbols)


(provide 'tla+-mode)
