;;; objed.el --- Navigate and edit text objects. -*- lexical-binding: t -*-
;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Clemens Radermacher <clemera@posteo.net>
;; Package-Requires: ((emacs "25") (cl-lib "1.0") (avy "0.5"))
;; Version: 0.8.6
;; Keywords: convenience
;; URL: https://github.com/clemera/objed

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A global minor-mode to navigate and edit text objects. Objed enables modal
;; editing and composition of commands, too. It combines ideas of other
;; Editors like Vim or Kakoune and tries to align them with regular Emacs
;; conventions.
;;
;; For more information also see:
;;
;; - My Blog: https://www.with-emacs.com/categories/objed/
;; - Project Readme: https://github.com/clemera/objed/blob/master/README.asc
;; - Project News: https://github.com/clemera/objed/blob/master/News.asc.
;;
;; Text objects are textual patterns like a line, a top level definition, a
;; word, a sentence or any other unit of text. When `objed-mode' is enabled,
;; certain editing commands (configurable) will activate `objed' and enable
;; its modal editing features. When active, keys which would usually insert a
;; character are mapped to objed commands. Other keys and commands will
;; continue to work as they normally would and exit this editing state again.
;;
;; By default important self inserting keys like Space or Return are not bound
;; to modal commands and will exit `objed' on insertion. This makes it
;; convenient to move around and continue adding new text.
;;
;; With activation `objed' shows the current object type in the mode-line. The
;; textual content of the object is highlighted visually in the buffer and the
;; cursor color is changed, too. The user can now navigate by units of this
;; object, change the object state or switch to other object types.
;;
;; The object state is either "inner" or "whole" and is indicated in the
;; modeline by (i) or (w) after the object type. With inner state, anything
;; that would be considered delimiters or padding around an object is
;; excluded.
;;
;; The user can apply operations to objects. By marking objects before
;; applying an operation, s?he can even operate on multiple objects at once.
;; This works similar to the way you interact with files in `dired'. When
;; marking an object the point moves on to the next object of this type.
;;
;; The object type used for initialization is determined by the mapping of the
;; entry command (see `objed-cmd-alist'). For example using
;; `beginning-of-defun' will activate `objed' using the `defun' object as
;; initial object type. With command `next-line', `objed' would initialize
;; with the `line' object.
;;
;; Objeds modal state provides basic movement commands which move by line,
;; word or character. Those switch automatically to the corresponding object
;; type, otherwise they work the same as the regular Emacs movement commands.
;; Other commands only activate the part between the initial position and the
;; new position moved to. By repeating commands you can often expand/proceed
;; to other objects. This way you can compose movement and editing operations
;; very efficiently.
;;
;; The expansion commands distinguish between block objects (objects built out
;; of lines of text) and context objects (programming constructs like strings,
;; brackets or textual components like sentences). This way you can quickly
;; expand to the desired objects.
;;
;; For example to move to the end of the paragraph, the user would first move
;; to the end of the line with "e". This would activate the text between the
;; starting position and the end of the line. The user can now continue to the
;; end of the paragraph by by pressing "e" again. Now s?he is able to proceed
;; even further by pressing "e" again OR to continue by adding new text to the
;; end of the paragraph OR to continue by acting on the text moved over, for
;; example killing it by pressing "k".
;;
;; As often with text editing, the explanation sounds more complicated than
;; using it. To get a better impression of the editing workflow with `objed'
;; have look at https://github.com/clemera/objed where you can find some
;; animated demos.
;;
;; To learn more about available features and commands have a look at the
;; descriptions below or the Docstrings and bindings defined in `objed-map'.
;; To define your own operations and text objects see `objed-define-op' and
;; `objed-define-object'.
;;
;; Although some features are still experimental the basic user interface will
;; stay the same.
;;
;;
;; CONTRIBUTE:
;;
;; I'm happy to receive pull requests or ideas to improve this package. Some
;; parts suffer from the bottom up approach of developing it, but this also
;; allowed me to experiment a lot and try ideas while working on them,
;; something that Emacs is especially good at. Most of the features are tested
;; using `emacs-lisp-mode' but hopefully there aren't to many problems using
;; modes for other languages, I tried my best to write text objects in a
;; language agnostic way. Testing this and writing more tests in general would
;; be an important next step.
;;
;; This package would never been possible without the helpful community around
;; Emacs. Thank you all and see you in parendise...Share the software!
;;

;;
;;; Code:


;;;; Requirements:

(require 'cl-lib)
(require 'nadvice)
(require 'face-remap)
(require 'subword)
(require 'hl-line)
(require 'avy)
(require 'objed-objects)

;;;; External variables

(defvar which-key-idle-delay)
(defvar which-key--using-top-level)
(defvar which-key-replacement-alist)
(defvar ivy-sort-function-alist)
(defvar edit-indirect--overlay)
(defvar eval-sexp-fu-flash-mode)
(defvar comment-combine-change-calls)

;;;; External functions

(declare-function objed--exit-objed "objed" nil t)
(declare-function electric-pair-post-self-insert-function "ext:electric")
(declare-function which-key-description-order "ext:which-key")
(declare-function which-key--create-buffer-and-show "ext:which-key")
(declare-function which-key--hide-popup "ext:which-key")
(declare-function edit-indirect-region "ext:edit-indirect")
(declare-function edit-indirect-commit "ext:edit-indirect")
(declare-function electric-pair-syntax-info "ext:elec-pair")
(declare-function hl-line-unhighlight "ext:hl-line")
(declare-function hl-line-highlight "ext:hl-line")
(declare-function multiple-cursors-mode "ext:multiple-cursors")
(declare-function mc/create-fake-cursor-at-point "ext:multiple-cursors")
(declare-function mc/num-cursors "ext:multiple-cursors")

;;;; Support for other libs

(with-eval-after-load 'multiple-cursors-core
  (when (bound-and-true-p mc/cursor-specific-vars)
    (push 'objed--current-obj mc/cursor-specific-vars)
    (push 'objed--obj-state mc/cursor-specific-vars)
    (push 'objed--object mc/cursor-specific-vars)
    (push 'objed--look-around mc/cursor-specific-vars)
    (push 'objed--marked-ovs mc/cursor-specific-vars)
    (push 'objed--last-states mc/cursor-specific-vars)))


;;;; Options:

(defgroup objed nil
  "Navigate and edit text objects."
  :group 'convenience
  :prefix "objed-")

(defcustom objed-auto-init t
  "Whether to enable automatic activation in `objed-mode'.

This option controls whether commands mapped in `objed-cmd-alist'
will activate objed.

This value need to be set before `objed-mode' is activated
otherwise you have to restart `objed-mode' so it can take
effect."
  :type 'boolean)

(defcustom objed-auto-init-on-buffer-change nil
  "Whether to enable automatic activation on buffer change in `objed-mode'.

This option controls auto activation after the user interactively
switches to other buffers.

This value need to be set before `objed-mode' is activated
otherwise you have to restart `objed-mode' so it can take
effect."
  :type 'boolean)

(defcustom objed-disabled-modes '()
    "List of modes for which objed should stay disabled.

If the current `major-mode' is in the list or derives from a
member of it `objed' will not activate.

See also `objed-disabled-p'"
    :type '(repeat symbol))

(defcustom objed-init-p-function #'objed-init-p
  "Function which tests if objed is allowed to initialize.

The function should return nil if objed should not initialize."
  :type 'function)


(defcustom objed-init-hook '(objed-init-mode-line objed-init-which-key)
  "Hook that runs after objed initialized."
  :type 'hook)

(defcustom objed-exit-hook '()
  "Hook that runs when objed exits."
  :type 'hook)

(defcustom objed-cmd-alist
  '((left-char . char)
    (right-char . char)
    (forward-word . word)
    (capitalize-word . word)
    (backward-word . word)
    (move-beginning-of-line . line)
    (move-end-of-line . line)
    (previous-line . line)
    (next-line . line)
    (beginning-of-buffer . buffer)
    (end-of-buffer . buffer)
    (scroll-up-command . char)
    (scroll-down-command . char)
    (View-scroll-half-page-forward . char)
    (View-scroll-half-page-backward . char)
    (move-to-window-line-top-bottom . line)
    (imenu . line)
    (backward-paragraph . paragraph)
    (forward-paragraph . paragraph)
    (fill-paragraph . textblock)
    (down-list . sexp)
    (backward-up-list . sexp)
    (up-list . sexp)
    (forward-sexp . sexp)
    (backward-sexp . sexp)
    (indent-pp-sexp . bracket)
    (back-to-indentation . line)
    (org-beginning-of-line . line)
    (org-end-of-line . line)
    (recenter-top-bottom . line)
    (forward-sentence . sentence)
    (org-forward-sentence . sentence)
    (backward-sentence . sentence)
    (org-backward-sentence . sentence)
    (org-backward-element . block)
    (beginning-of-defun . defun)
    (end-of-defun . defun)
    (outline-previous-visible-heading . section)
    (outline-next-visible-heading . section)
    (org-previous-visible-heading . section)
    (comint-previous-prompt . output)
    (comint-next-prompt . output)
    (forward-button . face)
    (backward-button . face)
    (sgml-skip-tag-backward . tag)
    (Info-next-reference . face)
    (Info-prev-reference . face)
    (objed-next-identifier . identifier)
    (objed-prev-identifier . identifier)
    (objed-first-identifier . identifier)
    (objed-last-identifier . identifier)
    ;; editing entry commands
    (yank . region)
    (yank-pop . region)
    ;; misc
    (kill-buffer . char)
    (kill-this-buffer . char))
  "Entry commands and associated objects."
  :type '(alist :key-type sexp
                :value-type (choice sexp
                                    (repeat sexp))))

(defcustom objed-switch-alist  '()
  "Alist mapping objects to region functions.

When switching to an object interactively using its object
command, any mapped function in this alist gets called. The
function receives the beginning and end position of object as
arguments.

This can be used to execute any additional actions when switching
to an object like for example indenting the object. The mapping
for t acts as the default to use when no other mapping for
object exists."
  :type '(alist :key-type sexp
                :value-type function))

(defcustom objed-switch-functions '(objed-switch-goto-beg)
  "Hook that runs after switching to an object.

Functions in this hook get the object name, start and end
position as arguments. This hook runs after any mappings in
`objed-switch-alist'."
  :type 'hook)


(defcustom objed-states-max 20
  "Maximal number of states to remember.

This option holds the number of times `objed-last' can
be used to restore previous states."
  :type 'integer)


(defcustom objed-keeper-commands
  '(save-buffer
    read-only-mode
    undo
    undo-only
    delete-other-windows
    reposition-window
    eval-defun
    eval-last-sexp
    kmacro-start-macrop
    kmacro-start-macro-or-insert-counter
    kmacro-end-or-call-macro
    kmacro-call-macro)
  "Regular Emacs commands which should not exit modal edit state.

When regular commands are executed `objed' will exit its editing
state. Commands added to this list wont do that."
  :type '(repeat function))

(defcustom objed-cursor-color "#e52b50"
  "Cursor color to use when `objed' is active."
  :type 'color)

(defcustom objed-cursor-type 'bar
  "Cursor shape to use when `objed' is active."
  :type '(choice
          (const :tag "Frame default" t)
          (const :tag "Filled box" box)
          (cons :tag "Box with specified size"
                (const box)
                integer)
          (const :tag "Hollow cursor" hollow)
          (const :tag "Vertical bar" bar)
          (cons :tag "Vertical bar with specified height"
                (const bar)
                integer)
          (const :tag "Horizontal bar" hbar)
          (cons :tag "Horizontal bar with specified width"
                (const hbar)
                integer)
          (const :tag "None " nil)))

(defcustom objed-which-key-order #'which-key-description-order
  "Key sort order to use for which key help popups."
  :type 'function)

(define-obsolete-variable-alias 'objed-modeline-hint-p
  'objed-modeline-hint "ea0be40dd"
  "Whether to show hint for current object in mode line.")

(defcustom objed-modeline-hint t
  "Whether to show hint for current object in mode line."
  :type 'boolean)

(defcustom objed-mode-line-format
  '(:eval (propertize
           (format " %s(%s) "
                   (symbol-name objed--object)
                   (char-to-string (aref (symbol-name objed--obj-state) 0)))
           'face 'objed-mode-line))
  "Format used to display hint in mode-line.

Only relevant when `objed-modeline-hint' is non-nil."
  :type 'sexp)

(defcustom objed-modeline-setup-func #'objed--setup-mode-line
  "Function to setup the mode line.

This function recieves `objed-mode-line-format' as an argument to
add/remove the mode line hint.

It also recieves a second optional argument which indicates if
the hint should be remove or added. If non-nil the hint should be
removed."
  :type 'symbol)

(defcustom objed-initial-object 'region
  "Object to use as fallback for `objed-activate'."
  :type 'symbol)


;; optional dep options

(define-obsolete-variable-alias 'objed-use-which-key-if-available-p
  'objed-use-which-key-if-available "ea0be40dd"
  "Whether to allow loading and use of `which-key'.

To avoid loading `which-key' set this var before activating `objed-mode.'")

(defcustom objed-use-which-key-if-available t
  "Whether to allow loading and use of `which-key'.

To avoid loading `which-key' set this var before activating `objed-mode.'"
  :type 'boolean)

(define-obsolete-variable-alias 'objed-auto-wk-top-level-p
  'objed-auto-wk-top-level "ea0be40dd"
  "Whether to show top level help automatically when activating.

Respects `which-key-idle-delay'.
The top level help is also available via `objed-show-top-level'.")

(defcustom objed-auto-wk-top-level nil
  "Whether to show top level help automatically when activating.

Respects `which-key-idle-delay'.
The top level help is also available via `objed-show-top-level'."
  :type 'boolean)

(define-obsolete-variable-alias 'objed-use-avy-if-available-p
  'objed-use-avy-if-available "ea0be40dd"
  "Whether to allow loading and use of `avy'.

To avoid loading `avy' set this var before activating `objed-mode.'")

(defcustom objed-use-avy-if-available t
  "Whether to allow loading and use of `avy'.

To avoid loading `avy' set this var before activating `objed-mode.'"
  :type 'boolean)

(define-obsolete-variable-alias 'objed-use-hl-p
  'objed-use-hl "ea0be40dd"
  "Whether allow to use `hl-line' to highlight the current object.")

(defcustom objed-use-hl t
  "Whether allow to use `hl-line' to highlight the current object."
  :type 'boolean)

;;;; Faces:

(defgroup objed-faces nil
  "Faces for `objed'."
  :group 'objed
  :group 'faces)

(defface objed-hl
  '((t (:inherit highlight)))
  "Face used for highlighting textual content of current object."
  :group 'objed-faces)

(defface objed-mark
  '((t (:inherit region)))
  "Face used for marked objects."
  :group 'objed-faces)

(defface objed-mode-line
  '((t (:inherit mode-line-inactive)))
  "Face used for the mode line hint."
  :group 'objed-faces)

;;;; Keymaps:

(cl-defmacro objed-make-keymap (name
                                docstring
                                &rest definitions
                                &key prefix
                                &key with-defaults
                                &allow-other-keys)
  "Make a sparse keymap with DEFINITIONS and set it as NAME.

DOCSTRING is the documentation string for the variable NAME.

If PREFIX is non-nil, the variable's function definition is set
to the sparse keymap. The variable NAME can then be used as a
prefix binding.

If WITH-DEFAULTS is non-nil, the keymap is initialized with basic
bindings for numeric arguments and help.  Other single character
keys default to `objed-undefined'.

DEFINITIONS are cons cells with a key sequence string as the car
and the function symbol as the cdr."
  (declare (indent 1) (doc-string 2))
  (dolist (key '(:prefix :with-defaults))
    (cl-remf definitions key))
  `(eval-and-compile
     (let ((map (make-sparse-keymap)))
       ,@(if with-defaults
             '((dolist (c (number-sequence ?a ?z))
                 (define-key map (char-to-string c) 'objed-undefined))
               (dolist (d (number-sequence ?0 ?9))
                 (define-key map (char-to-string d) 'digit-argument))
               (dolist (k '("-" "C--" "M--"))
                 (define-key map (kbd k) 'negative-argument))
               (define-key map (kbd "C-h ?") 'objed-describe-prefix-bindings)))
       (mapc
        (lambda (k)
          (define-key map (kbd (car k)) (cdr k)))
        ',definitions)
       (defvar ,name map ,docstring)
       ,@(if prefix `((fset ',name map))))))

(objed-make-keymap objed-mode-map
  "Keymap for /function`objed-mode'."
  ("M-#" . objed-activate-object)
  ("M-[" . objed-beg-of-object-at-point)
  ("M-]" . objed-end-of-object-at-point)
  ("C-," . objed-prev-identifier)
  ("C-." . objed-next-identifier)
  ("C-<" . objed-first-identifier)
  ("C->" . objed-last-identifier))

(objed-make-keymap objed-map
  "Keymap used for additional text-objects by `objed'.

To define new objects see `objed-define-object'."
  :with-defaults t
  ;; Common Emacs keys
  ("C-g" . objed-toggle)
  ("?" . objed-show-top-level)
  ("q" . objed-quit-window-or-reformat)
  ("g" . objed-quit)
  ("C-u" . universal-argument)
  ("0" . universal-argument)
  
  ;; Help
  ("C-h k" . objed-describe-key)
  ("C-h n" . which-key-show-next-page-cycle)
  ("C-h p" . which-key-show-previous-page-cycle)
  
  ;; Append mode
  ("C-M-w" . objed-append-mode)
  ("W" . objed-append-mode)
  
  ;; Undo/redo
  ("/" . objed-undo)
  ("u" . objed-undo)
  ("U" . objed-undo-redo)
  ("~" . objed-undo-in-object)
  ("l" . objed-last)
  
  ;; General movement
  ("f" . objed-right-char)
  ("b" . objed-left-char)
  ("F" . objed-forward-symbol)
  ("B" . objed-objed-backward-symbol)
  ("s" . objed-forward-word)
  ("r" . objed-backward-word)
  ("S" . objed-objed--forward-sexp)
  ("R" . objed-objed--backward-sexp)
  ("p" . objed-previous-line)
  ("n" . objed-next-line)
  ("E" . objed-forward-defun)
  ("A" . objed-beginning-of-defun)
  ("N" . objed-forward-paragraph)
  ("P" . objed-backward-paragraph)
  ("`" . objed-backward-until-context)
  ("'" . objed-forward-until-context)
  ("[" . objed-previous)
  ("]" . objed-next)
  
   ;; Identifiers
  ("." . objed-goto-next-identifier)
  ("," . objed-goto-prev-identifier)
  
  ;; Top and bottom
  ("<home>" . objed-top-object)
  ("<" . objed-top-object)
  ("<end>" . objed-bottom-object)
  (">" . objed-bottom-object)
  
  ;; Block expansion
  ("_" . objed-include)
  ("(" . objed-include-backward)
  (")" . objed-include-forward)
  ("a" . objed-beg-of-block)
  ("e" . objed-end-of-block)
  ("h" . objed-expand-block)
  ("@" . objed-extend)
  ("O" . objed-expand-context)
  
  ;; Object state
  ("t" . objed-toggle-state)
  ("j" . objed-toggle-side)
  
  ;; Edit objects
  ("i" . objed-quit)
  ("c" . objed-del-insert)
  ("k" . objed-kill)
  ("K" . objed-kill)
  ("w" . objed-copy)
  ("d" . objed-delete)
  ("D" . objed-delete)
  ("y" . objed-yank)
  
  ;; Move objects
  ("{" . objed-move-object-backward)
  ("}" . objed-move-object-forward)
  ("\\" . objed-objed-indent)
  ("C-<left>" . objed-indent-left)
  ("C-<right>" . objed-indent-right)
  ("M-<right>" . objed-indent-to-right-tab-stop)
  ("M-<left>" . objed-indent-to-left-tab-stop)
  ("S-<left>" . objed-move-object-backward)
  ("S-<right>" . objed-move-object-forward)
  ("S-<up>" . objed-move-object-backward)
  ("S-<down>" . objed-move-object-forward)
  
  ;; Marking
  ("m" . objed-mark)
  ("M" . objed-toggle-mark-backward)
  ("*" . objed-mark-more)
  ("C-<SPC>" . set-mark-command)
  ("C-x C-x" . objed-exchange-point-and-mark)
  
  ;; Miscellaneous
  (";" . objed-objed-comment-or-uncomment-region)
  ("$" . objed-flyspell-region)
  ("\"" . objed-objed-electric-pair)
  ("^" . objed-raise)
  
  ;; Prefix keys
  ("o" . objed-object-map)
  ("x" . objed-op-map)
  ("v" . objed-user-map)
  ("z" . objed-other-user-map)
  
  ;; Special commands
  ("G" . objed-ace)
  ("%" . objed-replace)
  (":" . objed-eval-expression)
  ("&" . objed-objed-pipe-region)
  ("|" . objed-objed-ipipe)
  ("!" . objed-execute)
  ("C-<return>" . objed-objed-run-or-eval)
  ("S-<return>" . objed-objed-comment-duplicate)
  ("M-<return>" . objed-objed-duplicate-down)
  ("C-M-<return>" . objed-insert-new-object)
  
  ;; Resize windows
  ("s-<left>" . objed-move-window-line-left)
  ("s-<right>" . objed-move-window-line-right)
  ("s-<up>" . objed-move-window-line-up)
  ("s-<down>" . objed-move-window-line-down)
  
  ;; Slurp and barf
  ("C-M-<left>" . objed-forward-barf-sexp)
  ("C-M-<right>" . objed-forward-slurp-sexp)
  ("C-S-<left>" . objed-forward-barf-sexp)
  ("C-S-<right>" . objed-forward-slurp-sexp)
  ("C-M-<down>" . objed-backward-barf-sexp)
  ("C-M-<up>" . objed-backward-slurp-sexp)
  ("C-S-<down>" . objed-backward-barf-sexp)
  ("C-S-<up>" . objed-backward-slurp-sexp))

(objed-make-keymap objed-object-map
  "Keymap used for additional text-objects by `objed'.
To define new objects see `objed-define-object'."
  :with-defaults t
  :prefix t
  ("%" . objed-contents-object)
  ("*" . objed-section-object)
  ("." . objed-sentence-object)
  (";" . objed-comment-object)
  ("=" . objed-face-object)
  ("SPC" . objed-region-object)
  ;; choose via completion
  ("TAB" . objed-object-x)
  ("[" . objed-page-object)
  ("a" . objed-block-object)
  ("b" . objed-bracket-object)
  ("c" . objed-char-object)
  ("d" . objed-defun-object)
  ("e" . objed-error-object)
  ("f" . objed-file-object)
  ("h" . objed-buffer-object)
  ("i" . objed-indent-object)
  ("l" . objed-line-object)
  ("m" . objed-email-object)
  ("n" . objed-output-object)
  ("o" . objed-expand-context)
  ("p" . objed-paragraph-object)
  ("q" . objed-textblock-object)
  ("s" . objed-string-object)
  ("t" . objed-tag-object)
  ("u" . objed-url-object)
  ("w" . objed-word-object)
  ("x" . objed-sexp-object)
  ("y" . objed-symbol-object)
  ("z" . objed-ace-object))

(objed-make-keymap objed-op-map
  "Map for additional operations called via a prefix from `objed-map'.

To define new operations see `objed-define-op'."
  :prefix t
  :with-defaults t
  ("TAB" . objed-op-x)
  ("c" . objed-objed-case-op)
  ("x" . objed-eval-defun)
  ("e" . objed-eval-exp)
  ("y" . objed-insert)
  ("i" . insert-file)
  ("q" . read-only-mode)
  ("r" . ctl-x-r-map)
  ("n" . objed-narrow)
  ("u" . objed-undo)
  ("d" . dired-jump)
  ("s" . save-buffer)
  ("f" . find-file)
  ("w" . write-file)
  ("v" . find-alternate-file)
  ("b" . switch-to-buffer)
  ("o" . objed-other-window)
  ("k" . objed-kill-buffer)
  ("j" . imenu)
  ("0" . delete-window)
  ("1" . delete-other-window)
  ("2" . split-window-vertically)
  ("3" . split-window-horizontally))

(objed-make-keymap objed-user-map
  "Keymap for custom user bindings."
  :with-defaults t
  :prefix t)

(objed-make-keymap objed-other-user-map
  "Keymap for custom user bindings."
  :with-defaults t
  :prefix t)

(defvar objed-dispatch-map
  (let ((map (make-composed-keymap nil objed-object-map)))
    (define-key map (kbd "C-h") 'objed-describe-dispatch-bindings)
    map)
  "Keymap for dispatching commands to objects.

Use `objed-define-dispatch' to define a dispatch command.")

;;;; Variables:

(defvar objed--exit-alist nil
  "Maps operations to exit functions.

If this list contains an entry for an operation, the
exit function is called after execution of the operation.")

(defvar objed--after-init-alist
  '((move-beginning-of-line . objed--object-until-bol)
    (org-beginning-of-line . objed--object-until-bol)
    (move-end-of-line . objed--object-trailing-line)
    (org-end-of-line . objed--object-trailing-line)
    (back-to-indentation . objed--until-start)
    (beginning-of-buffer . objed--until-start)
    (end-of-buffer . objed--until-end)
    (backward-sentence . objed--goto-start))
  "Maps commands which need special initialization to init functions.

The init functions are called with the position point was before
the command was executed.")

(defvar objed--opoint nil
  "Position before movement command activated `objed'.")

(defvar objed--with-allow-input nil)

(defvar objed--dispatch-key-alist nil
  "Store keys for dispatch commands.")

(defvar objed-mode nil)
(defvar objed--buffer nil
  "Buffer where objed got initialized.")

(defvar objed--dispatch-alist nil
  "Maps prefix commands to functions.

Don't modify this list manually, use `objed-define-dispatch'.")

(defvar objed--look-around nil
  "Object data used by toggle ends commands.")

(defvar objed--saved-vars nil
  "Variables to save and restore.")

(defvar objed--hl-line-keep-p nil
  "Whether to keep command `hl-line-mode' active on reset.")

(defvar objed--saved-cursor nil
  "Cursor color before objed got initialized.")

(defvar objed--saved-cursor-type nil
  "Cursor type before objed got initialized.")

(defvar objed--hl-cookie nil
  "The remapping cookie for `hl-line' face.")

(defvar objed--wk-replacement-alist
  (list
   ;; commands to ignore
   `((nil . ,(regexp-opt
              (list "objed-describe-prefix-bindings"
                    "objed-describe-dispatch-bindings"
                    "objed-show-top-level"
                    "digit-argument"
                    "universal-argument"
                    "undefined"
                    "negative-argument")))
     . t)
   ;; show only object name for objects
   '((nil . "\\`objed-\\(.*?\\)-.*?object\\'")
     . (nil . "\\1"))
   ;; remove package prefix all others
   '((nil . "\\`\\(objed-\\)+\\(.*\\)")
     . (nil . "\\2")))
  "Rules to add to `which-key-replacement-alist'.

Only relevant when `which-key' is used.")

(defvar objed--ignored-keys nil
  "Keys recently ignored by `which-key' popup.

Useful for keeping the same popup when pressing undefined keys.")

(defvar-local objed-disabled-p nil
  "If non-nil objed will not activate.")

(defvar-local objed-active-p nil
  "Whether objed is activated or not.")

(defvar objed--avy-err-msg
  "Package `avy' is not available.
Add `avy' to your load path and restart `objed-mode' with a
non-nil value of `objed-use-avy-if-available'."
  "Error message to use if avy commands are not ready to run.")

(defvar objed--block-objects '(buffer paragraph block indent textblock)
  "List of objects which are `line based'.

Objects which are built by lines of text.")

(defvar objed--context-objects '(string bracket tag comment)
  "List of objects to be choosen by context.")

(defvar objed--cmd-cache nil
  "Caching results for `objed--read-cmd'.")

(defvar-local objed--last-states nil
  "Stack of last states.

See `objed--get-current-state' for details.

Each state is a list of the form:

    (`current-buffer', `point', variable/`objed--object',
      `objed--current-obj', position data of the marked objects
      if any")

(defvar objed--electric-event nil
  "Saves the event used for `objed-electric-event'.")

(defvar objed-ipipe-hist '()
  "History for `objed-ipipe'.")

(defvar objed-ipipe-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "<tab>") #'objed-ipipe-complete)
    (define-key map (kbd "<return>") #'objed-ipipe-commit)
    map)
  "Keymap for `objed-ipipe'.")

;; ** Display update

(defvar objed--ipipe-schedule-time 0.15
  "Time frame to update the display.")

(defvar objed--ipipe-timer nil
  "Timer used for `objed-ipipe'.")

(defvar objed--ipipe-overlay nil
  "Overlay showing the results in current buffer.")

(defvar objed--ipipe-last ""
  "Cache last used command.")

(defvar objed--ipipe-region-string ""
  "Cache last region.")

(defvar objed--eir-alist
  '((emacs-lisp-mode . ielm)
    (lisp-interaction-mode . ielm)
    (clojure-mode . cider)
    (lisp-mode . slime)
    (racket-mode . racket)
    (scheme-mode . scheme)
    (hy-mode . hy)
    (python-mode . python)
    (ruby-mode . ruby)
    (sml-mode . sml)
    (scala-mode . scala)
    (lua-mode . lua)
    (erlang-mode . erlang)
    (elixir-mode . iex)
    (ocaml-mode . ocaml)
    (prolog-mode . prolog)
    (js3-mode . javascript)
    (js2-mode . javascript)
    (js-mode . javascript)
    (sh-mode . shell))
  "Map major mode symbols to `eval-in-repl' REPL names.")

(defvar objed-use-ielm-for-eval-p nil
  "Whether to use ielm for `objed-run-or-eval' for Elisp.

If nil `eval-region' is used instead.")

;;;; Minor modes


;;;###autoload
(define-minor-mode objed-mode
  "Enable objeds modal editing features after certain commands.

With a prefix argument ARG, enable Objed mode if ARG is positive,
and disable it otherwise. If called from Lisp, enable the mode if
ARG is omitted or nil.

Objed mode is a global minor mode. When enabled, any command
configured in `objed-cmd-alist' will activate modal navigation
and editing features on text objects. Available commands,
operations and objects can be found in `objed-map',
`objed-op-map' and `objed-object-map'.

To define your own text objects and editing operations see
`objed-define-object' and `objed-define-op'."
  :global t
  :keymap objed-mode-map
  :require 'objed
  (if objed-mode
      (progn
        (add-hook 'minibuffer-setup-hook 'objed--reset)
        (when objed-auto-init
          ;; interactive cmds
          (objed--install-advices objed-cmd-alist t))
        (when objed-auto-init-on-buffer-change
          (dolist (f '(quit-window
                       create-file-buffer
                       rename-buffer
                       switch-to-buffer
                       display-buffer
                       pop-to-buffer))
            ;; auto entry cmds
            (advice-add f :after #'objed--init-later)))
        (advice-add 'keyboard-quit :before #'objed-toggle))
    (remove-hook 'minibuffer-setup-hook 'objed--reset)
    (objed--remove-advices objed-cmd-alist)
    (advice-remove 'keyboard-quit #'objed-toggle)
    (dolist (f '(quit-window
                 create-file-buffer
                 rename-buffer
                 switch-to-buffer
                 display-buffer
                 pop-to-buffer))
      ;; auto entry cmds
      (advice-remove f #'objed--init-later))))

;;;###autoload
(define-minor-mode objed-local-mode
  "Enable `objed-mode' in current buffer."
  :variable (objed-mode . (lambda (s) (setq-local objed-mode s)))
  ;; Same mechanism as in electric-{indent,layout,quote}-mode
  (cond
   ((eq objed-mode (default-value 'objed-mode))
    ;; If the local value is set to the default value, unmark
    ;; `objed-mode' as local
    (kill-local-variable 'objed-mode))
   ((not (default-value 'objed-mode))
    ;; If `objed-mode' isn't enabled by default, enable it globally to
    ;; invoke the setup routines, and then reset the default value
    (objed-mode 1)
    (setq-default objed-mode nil))))

(define-minor-mode objed-append-mode
  "Append kills on `objed-copy'.

When `objed-append-mode' is active `objed-copy' and `objed-kill'
will append kills to the `kill-ring'."
  :init-value nil
  :global t
  :require 'objed
  :lighter " >>")

;;;; Macros:

(defmacro objed-with-temp-object (object &rest body)
  "Switch to objed OBJECT temporarily and evaluate BODY."
  (declare (indent 1))
  `(let ((cur-object objed--object)
         (cur-object-bounds objed--current-obj))
     (prog1
         (save-excursion
           (setq objed--object ,object)
           (setq objed--current-obj (objed--get))
           ,@body)
       (setq objed--object cur-object)
       (setq objed--current-obj cur-object-bounds))))

(defmacro objed--with-allow-input (&rest body)
  "Allow input in minibuffer while `objed' is active.

The code executed in BODY allows minibuffer input without
interferring with `objed'."
  `(let ((overriding-terminal-local-map nil)
         (minibuffer-setup-hook (remq 'objed--reset minibuffer-setup-hook))
         (objed--with-allow-input t))
     (setq-local cursor-type objed--saved-cursor-type)
     (set-cursor-color objed--saved-cursor)
     (unwind-protect (progn ,@body)
       ;; body might exit objed...
       (when objed--buffer
         (setq-local cursor-type objed-cursor-type)
         (set-cursor-color objed-cursor-color)))))

(defmacro objed--save-state (&rest body)
 "Preserve state during execution of BODY."
  `(let ((state (objed--get-current-state)))
     (unwind-protect (progn ,@body )
       (prog1 nil (objed--restore-state state)))))

(defmacro objed--call-and-switch (name cmd obj &optional before after)
  "Create objed command NAME which will invoke CMD and switch to OBJ.

BEFORE and AFTER are forms to execute before/after calling the command."
  (declare (indent 1))
  `(defun ,name ()
     ,(format "Call `%s' and switch to object %s"
              (symbol-name cmd) (symbol-name obj))
     (interactive)
     ,before
     (let* ((prb (and (region-active-p) (= (point) (region-beginning))))
            (pre (and (region-active-p) (= (point) (region-end))))
            (pos (point)))
       (setq this-command ',cmd)
       (call-interactively ',cmd)
       ,after
       (objed--switch-to ',obj
                         (if (eq objed--object ',obj)
                             objed--obj-state
                           'whole))
       (when (or prb pre)
         (cond ((and prb
                     (= (point) (region-end)))
                (set-mark pos))
               ((and pre
                     (= (point) (region-beginning)))
                (set-mark pos)))))))

(defmacro objed-define-dispatch (key def)
  "Define a dispatch binding.

Defines a prefix key KEY. After pressing this prefix key the user
is expected to invoke an object command bound in
variable/`objed-object-map' to choose an object.

KEY is a string to be interpreted as spelled-out keystrokes,
using same format as for `kbd'.

DEF is the symbol for the function to be called with the choosen
object as an argument."
  (declare (indent 2))
  (let ((dp (intern (format "objed-%s-dispatch" (symbol-name def)))))
    `(prog1 ',def
       (push (cons ',dp ',def) objed--dispatch-alist)
       (setf (symbol-function (define-prefix-command ',dp))
             objed-dispatch-map)
       (define-key objed-map (kbd ,key) ',dp)
       ;; save for possible reinit of objed-map
       (push (cons ,key ',dp) objed--dispatch-key-alist))))

(defmacro objed-define-op (key cmd &optional exit)
  "Macro to create operations for `objed'.

KEY is a string to be interpreted as spelled-out keystrokes,
using same format as for `kbd'. If KEY is non-nil it is bound to
and operation that will use CMD in `objed-map'.

CMD should be a region command or a non-interactive function.

In the latter case the function recieves two buffer positions as
arguments. If the function is able to recieve three arguments the
third argument will be the prefix argument passed to the
operation.

If EXIT is given it should be a symbol or function which is
called after executing the operation. The function recieves one
argument which is the string of the textual content the operation
acted on. See `objed-exit-op'.

This macro returns a command that can be used as an `objed'
operation."
  (let ((name (intern (format "objed-%s" (symbol-name cmd))))
        (res (list 'progn)))
    (when key
      (push `(define-key objed-map (kbd ,key) ',name) res))
    (when exit
      (push `(setq objed--exit-alist
                   (cons (cons ',name ',exit) objed--exit-alist))
            res))
    (push `(defun ,name (arg)
             "Objed operation."
             (interactive "P")
             (let ((cmd (objed--create-op ',cmd arg)))
               (objed--do cmd ',name)))
          res)
    (nreverse res)))

;;;; Commands

(defun objed-undefined ()
  "Provide feedback if an undefined key is pressed."
  (interactive)
  (let ((msg (format "%s is undefined"
                     (key-description (this-single-command-keys)))))
    (message msg)))

;; Help commands

(defun objed-describe-prefix-bindings ()
  "Show current prefix bindings and exit."
  (interactive)
  (call-interactively 'describe-prefix-bindings)
  (objed--exit-objed))

(defun objed-show-top-level (&optional interactive)
  "Show bindings of `objed-map'.

When `which-key' isn't use only show bindings if INTERACTIVE is
non-nil which is the case when called interactively."
  (interactive "p")
  (unless (objed--maybe-which-key
           objed-map "Op keys"
           interactive)
    (when interactive
      (objed--describe-bindings objed-map))))

(defun objed-describe-dispatch-bindings ()
  "Describe `objed-dispatch-map' bindings."
  (interactive)
  (unless (objed--maybe-which-key
           objed-dispatch-map "Select" t)
    (setq mark-active nil)
    (objed--describe-bindings
     objed-dispatch-map (vector (aref (this-command-keys-vector) 0)))))

;; Entering/exiting objed

(defun objed-activate-object ()
  "Query for object and activate with it."
  (interactive)
  (objed--maybe-which-key objed-object-map "Object:")
  (let ((real-this-command (lookup-key objed-object-map (vector (read-key)))))
    (when (and (bound-and-true-p which-key-mode)
               (fboundp #'which-key--hide-popup))
      (which-key--hide-popup))
    (when real-this-command
      (call-interactively real-this-command))))

(defun objed-beg-of-object-at-point ()
  "Activate and move to beginning of object at point.

On repeat or at boundary move to previous."
  (interactive)
  (objed--init 'char)
  (objed-current-or-previous-context))

(defun objed-end-of-object-at-point ()
  "Activate and move to end of object at point.

On repeat or at boundary move to next."
  (interactive)
  (objed--init 'char)
  (objed-current-or-next-context))

(defun objed-prev-identifier ()
  "Activate object with identifier at point."
  (interactive)
  (objed--prev-identifier))

(defun objed-next-identifier ()
  "Activate object with identifier at point."
  (interactive)
  (if (and objed--buffer
           (eq objed--object 'identifier))
      (objed--next-identifier)
    (unless (thing-at-point 'symbol)
      (re-search-forward  "\\_<" nil t))
    (when (objed--init 'identifier)
      (goto-char (objed--beg)))))

(defun objed-first-identifier ()
  "Move to first instance of identifier at point."
  (interactive)
  (let ((ib (if (looking-at "\\_<")
                (point)
              (save-excursion
                (re-search-backward "\\_<")
                (point))))
        (format (objed--get-ident-format)))
    (when format
      (goto-char (point-min))
      (when (re-search-forward format nil t)
        (goto-char (match-beginning 0))
        (when (= (point) ib)
          (message "No previous indentifier"))))))

(defun objed-last-identifier ()
  "Move to last instance of identifier at point."
  (interactive)
  (let ((ib (if (looking-at "\\_<") (point)
              (save-excursion
                (re-search-backward "\\_<")
                (point))))
        (format (objed--get-ident-format)))
    (when format
      (goto-char (point-max))
      (re-search-backward format nil t)
      (when (= (point) ib)
        (message "No next indentifier")))))

(defun objed-toggle ()
  "Toggle activation of objed."
  (interactive)
  (if objed-active-p
      (objed--exit-objed)
    (if objed-mode
        (objed-activate))))

(defun objed-quit-window-or-reformat ()
  "Quit window for objed."
  (interactive)
  (let ((nc (let ((overriding-terminal-local-map nil))
              (key-binding "q" nil t))))
    (if (and (string-match "insert" (symbol-name nc))
             (not buffer-read-only))
        (progn
          (when (eq last-command this-command)
            (or (objed--goto-next)
                (objed--switch-to 'defun)))
          (cond ((and (not (eq objed--object 'defun))
                      (or (eq major-mode 'fundamental-mode)
                          (derived-mode-p 'text-mode)
                          (objed--at-comment-p)
                          (objed--in-string-or-comment-p)))
                 (call-interactively 'fill-paragraph)
                 (objed--switch-to 'textblock)
                 (message "Filled paragraph."))
                ((objed--switch-to 'defun)
                 (indent-region (objed--beg) (objed--end))
                 (objed--update-current-object)
                 (message "Indented defun."))))
      (objed--reset)
      (call-interactively nc))))

(defun objed-quit ()
  "Quit and deactivate.

If region is active deactivate it first."
  (interactive)
  (cond (mark-active
         (setq mark-active nil)
         (objed--init objed--object))
        (t
         (objed--exit-objed))))

;; Undo/redo

(objed--call-and-switch objed-undo
  undo char)
(objed--call-and-switch objed-undo-redo
  undo-redo char)

(defun objed-undo-in-object ()
  "Undo in current object range."
  (interactive)
  (unless (eq last-command 'undo)
    (push-mark (objed--end) t)
    (objed--update-current-object
     (objed-make-object :beg (objed--beg)
                        :end (set-marker (make-marker)
                                         (objed--end)))))
  ;; move back to start on each turn
  (goto-char (objed--beg))
  (undo '(4)))

(defun objed-last ()
  "Resume to last state.

This is for quickly resuming to states on navigation. Note that
the state is only restored correctly if the buffer was not
modified."
  (interactive)
  (objed--restore-state))

;; General movement

(objed--call-and-switch objed-right-char
  right-char char)

(objed--call-and-switch objed-left-char
  left-char char)

(objed--call-and-switch objed-forward-symbol
  forward-symbol symbol)

(objed--call-and-switch objed-objed-backward-symbol
  objed-backward-symbol symbol)

(defun objed-forward-word ()
  "Call `forward-word' and switch to object word."
  (interactive)
  (if (objed--inner-p)
      (let* ((subword-mode t)
             (superword-mode nil)
             (find-word-boundary-function-table
              subword-find-word-boundary-function-table))
        (setq this-command 'forward-word)
        (call-interactively 'forward-word))
    (setq this-command 'forward-word)
    (call-interactively 'forward-word))
  (objed--switch-to 'word
                    (if (eq objed--object 'word)
                        objed--obj-state
                      'whole)))

(defun objed-backward-word ()
  "Call `backward-word' and switch to object word."
  (interactive)
  (if (objed--inner-p)
      (let* ((subword-mode t)
             (superword-mode nil)
             (find-word-boundary-function-table
              subword-find-word-boundary-function-table))
        (setq this-command 'backward-word)
        (call-interactively 'backward-word))
    (setq this-command 'backward-word)
    (call-interactively 'backward-word))
  (objed--switch-to 'word
                    (if (eq objed--object 'word)
                        objed--obj-state
                      'whole)))

(objed--call-and-switch objed-objed--forward-sexp
  objed--forward-sexp sexp)

(objed--call-and-switch objed-objed--backward-sexp
  objed--backward-sexp sexp)

(objed--call-and-switch objed-previous-line
  previous-line line
  nil
  (when (objed--point-in-periphery)
    (back-to-indentation)))

(objed--call-and-switch objed-next-line
  next-line line
  nil
  (when (objed--point-in-periphery)
    (back-to-indentation)))

(defun objed-forward-defun ()
  "Switch to defun object and move forward one defun."
  (interactive)
  (objed--switch-to 'defun)
  (goto-char (objed--end)))


(objed--call-and-switch objed-beginning-of-defun
  beginning-of-defun defun)

(defun objed-forward-paragraph ()
  "Switch to paragraph object and move forward one paragraph."
  (interactive)
  (call-interactively 'forward-paragraph)
  (objed--skip-ws t)
  (objed--switch-to 'paragraph)
  (goto-char (objed--end)))

(objed--call-and-switch objed-backward-paragraph
  backward-paragraph paragraph)

(defun objed-backward-until-context ()
  "Goto object inner beginning and activate part moved over.

At bracket or string self insert ARG times."
  (interactive)
  (if (and (objed--inner-p)
           (not (objed--basic-p)))
      (progn (objed--toggle-state)
             (goto-char (objed--beg)))
    (when (save-excursion
            (prog1 (objed-context-object)
              (unless (eq last-command this-command)
                (objed--toggle-state))))
      (unless (eq last-command this-command)
        (objed--change-to :iend (point) :end (point)))
      (when (eq (point) (objed--beg))
        (objed-context-object))
      (goto-char (objed--beg)))))


(defun objed-forward-until-context ()
  "Goto object inner end and activate part moved over.

At bracket or string self insert ARG times."
  (interactive)
  (if (and (objed--inner-p)
           (not (objed--basic-p)))
      (progn (objed--toggle-state)
             (goto-char (objed--end)))
    (when (save-excursion (prog1 (objed-context-object)
                            (unless (eq last-command this-command)
                              (objed--toggle-state))))
      (if (eq last-command this-command)
          (goto-char (objed--end))
        (objed--change-to :ibeg (point) :beg (point)))
      (when (eq (point) (objed--end))
        (objed-context-object))
      (goto-char (objed--end)))))

(defun objed-previous (&optional arg)
  "Move to ARG previous object of current type."
  (interactive "p")
  (let ((pos (point)))
    (objed--goto-previous (or arg 1))
    (when (eq pos (point))
      (error "No previous %s" objed--object))))


(defun objed-next (&optional arg)
  "Move to ARG next object of current type."
  (interactive "p")
  ;; on init skip current
  (when (and (region-active-p)
             (eq last-command 'objed-extend))
    (exchange-point-and-mark))
  (let ((pos (point)))
    (objed--goto-next (or arg 1))
    (when (eq pos (point))
      (error "No next %s" objed--object))))

;; Identifiers

(defun objed-goto-prev-identifier ()
  "Switch to previous identifier."
  (interactive)
  (objed--prev-identifier)
  (when (objed--switch-to 'identifier)
    (goto-char (objed--beg))))

(defun objed-goto-next-identifier ()
  "Switch to next identifier."
  (interactive)
  (if (eq objed--object 'identifier)
      (progn (objed--next-identifier)
             (objed--update-current-object))
    (unless (thing-at-point 'symbol)
      (re-search-forward  "\\_<" nil t))
    (objed--switch-to 'identifier)
    (goto-char (objed--beg))))

;; Top and bottom

(defun objed-top-object ()
  "Go to first instance of current object type."
  (interactive)
  ;; TODO: creat macro keyword so delegation
  ;; can happen automatically, when specified
  (cond ((eq objed--object 'identifier)
         (objed-first-identifier)
         (objed--update-current-object))
        (t
         (let ((top (save-excursion
                      (goto-char (point-min))
                      (objed--get-next (point)))))
           (if (equal top objed--current-obj)
               (message "No previous %s" objed--object)
             (objed--update-current-object top)
             (objed--goto-char (objed--beg)))))))


(defun objed-bottom-object ()
  "Go to first instance of current object type."
  (interactive)
  (cond ((eq objed--object 'identifier)
         (objed-last-identifier)
         (objed--update-current-object))
        (t
         (let ((bot (save-excursion
                      (goto-char (point-max))
                      (objed--get-prev (point)))))
           (if (equal bot objed--current-obj)
               (message "No next %s" objed--object)
             (objed--update-current-object bot)
             (objed--goto-char (objed--beg)))))))

;; Block expansion


(defun objed-include-forward ()
  "Include trailing punctuation or whitespace of object."
  (interactive)
  (let ((end
         (save-excursion
           (goto-char (objed--end))
           (when (= 0 (skip-syntax-forward "_."))
             (objed--skip-ws))
           (point))))
    (objed--change-to :end end)))

(defun objed-include-backward ()
  "Include leading punctuation or whitespace of object."
  (interactive)
  (let ((beg
         (save-excursion
           (goto-char (objed--beg))
           (when (= 0 (skip-syntax-backward "_.'"))
             (objed--skip-ws 'back))
           (point))))
    (objed--change-to :beg beg)))

(defun objed-include ()
  "Include leading and trailing whitespace of punctuation."
  (interactive)
  (objed-include-forward)
  (objed-include-backward))

(let ((blocks nil))
  (defun objed-beg-of-block ()
    "Jump to beginning of line based objects.

Moves to beginning of line/indentation and activates the text
moved over. On repeat proceed to beginning of the indentation
block, paragraph and other `line based' objects.

See also `objed--block-objects'."
    (interactive)
    (when (not (eq last-command this-command))
      ;; get all which make sense from starting point
      (setq blocks
            (cl-delete-if
             (lambda (a)
               (let ((as (objed--beg (car (nthcdr 3 a)))))
                 (>= as (line-beginning-position))))
             (cl-delete-duplicates
              (objed--get-blocks
               (if (eq last-command 'move-beginning-of-line)
                   nil 'line)
               #'objed--beg)
              :test (lambda (a b)
                      (let ((as (objed--beg (car (nthcdr 3 a))))
                            (bs (objed--beg (car (nthcdr 3 b)))))
                        (eq  as bs)))))))
    (cond ((or (eq last-command this-command)
               (eq last-command 'move-beginning-of-line))
           (if (and (eq objed--object 'line)
                    (objed--inner-p)
                    (not (bolp))
                    (objed--point-in-periphery))
               (progn (objed--toggle-state)
                      (goto-char (objed--beg)))
             (when blocks
               (let ((pos (point))
                     (end (objed--end)))
                 (objed--restore-state (pop blocks))
                 (while (and (eq pos (objed--beg))
                             blocks)
                   (objed--restore-state (pop blocks)))
                 (objed--change-to :end end :iend end)
                 (goto-char (objed--beg))))))
          (t
           (objed--switch-to 'line
                             (unless (<= (point)
                                         (objed--indentation-position))
                               'inner))
           (objed--change-to :end (point) :iend (point))
           (goto-char (objed--beg))))))


(let ((blocks nil))
  ;; line end of block
  (defun objed-end-of-block ()
    "Jump to end of line based objects.

Moves to end of line and activates the text moved over. On repeat
proceed to end of the indentation block, paragraph and other
`line based' objects.

See also `objed--block-objects'."
    (interactive)
    (when (not (eq last-command this-command))
      (setq blocks
            (cl-delete-if
             (lambda (a)
               (let ((as (objed--end (car (nthcdr 3 a)))))
                 (<= as (line-end-position))))
             (cl-delete-duplicates
              (nreverse
               (objed--get-blocks
                'line
                #'objed--end
                ;; better for most cases
                'inner))
              :test (lambda (a b)
                      (let ((as (objed--end (car (nthcdr 3 a))))
                            (bs (objed--end (car (nthcdr 3 b)))))
                        (eq as bs)))))))
    (cond ((or (eq last-command this-command)
               (eq last-command 'move-end-of-line))
           (when blocks
             (let ((pos (point))
                   (beg (objed--beg)))
               (objed--restore-state (pop blocks))
               (while (and (eq pos (objed--end))
                           blocks)
                 (objed--restore-state (pop blocks)))
               (objed--change-to :beg beg :ibeg beg)
               (goto-char (objed--end)))))
          (t
           (objed--switch-to 'line 'inner)
           (objed--change-to :beg (point) :ibeg (point))
           (goto-char (objed--end))))))

(let ((blocks nil))
  (defun objed-expand-block ()
  "Jump to beginning of line based objects.

Activates (line based) object at point (and move to its start).
On repeat proceed to beginning of the indentation block,
paragraph and other `line based' objects.

See also `objed--block-objects'."
  (interactive)
  (let ((init (not (memq last-command
                         (list this-command
                               'objed-toggle-side)))))
    (when init
      (setq blocks
            (cl-remove-duplicates
             (objed--get-blocks
              ;; ignore current or allow toggle between
              ;; inner/whole here as well?
              objed--object
              #'objed--beg)
             :test (lambda (a b)
                     (let ((as (car (nthcdr 3 a)))
                           (bs (car (nthcdr 3 b))))
                       (equal as bs))))))
    (if (and init
             (not (eq objed--object 'line)))
        (progn (objed--switch-to 'line)
               (goto-char (objed--beg)))
      (when blocks
        (let ((sdiff (abs (- (point) (objed--beg))))
              (ediff (abs (- (point) (objed--end)))))
          (objed--restore-state (pop blocks))
          (goto-char (cond ((or (> ediff sdiff)
                                init)
                            (objed--beg))
                           (t
                            (objed--end))))))))))

(defun objed-extend ()
  "Extend current object.

This activates the region for current object and allows
extending/shrinking the region by moving around using regular
objed movement commands.

The active region will be used as the current object when an
objed operation is used."
  (interactive)
  (when objed--marked-ovs
    (user-error "Currently not supported with marked objects"))
  (if (region-active-p)
      (if (fboundp 'er/expand-region)
          (call-interactively 'er/expand-region)
        ;; reinit on repeat
        (setq mark-active nil)
        (objed--init objed--object)
        (message "Install expand-region to expand on repeat."))
    (when (< (objed--beg) (point) (objed--end))
      (goto-char (objed--beg)))
    (push-mark (if (or (>= (point) (objed--end))
                       (eq objed--object 'char))
                   (objed--beg)
                 (objed--end))
               t t)))

(defun objed-expand-context ()
  "Expand to objects based on context.

Starts at the inner part of the object point is in or with the
object point is at. Any whitespace following point is skipped.

On expand move to start of object."
  (interactive)
  (unless objed--buffer
    (objed--init 'char))
  (if (objed--basic-p)
      (let ((pos (point)))
        (save-excursion
          (objed-context-object)
          (when (< (objed--skip-forward (objed--beg) 'ws)
                   pos (objed--end))
            (objed--toggle-state)))
        (when (or (< (objed--beg) (point) (objed--end))
                  (< (point) (objed--beg)))
          (goto-char (objed--beg))))
    (if (objed--inner-p)
        (let ((curr (objed--current)))
          (objed--toggle-state)
          (goto-char (objed--beg))
          (when (equal curr (objed--current))
            (objed-context-object)
            (goto-char (objed--beg))))
      (objed-context-object)
      (goto-char (objed--beg))))
  (let ((map (make-sparse-keymap)))
    (define-key map "o" 'objed-expand-context)
    (set-transient-map map t)))

;; Object state

(defun objed-toggle-state ()
  "Toggle object state.

Switches between inner and whole object state."
  (interactive)
  (unless (objed--inner-p)
    (objed--maybe-switch-to-sexp-fallback))
  (let ((boo (eq (point) (objed--beg)))
        (eoo (eq (point) (objed--end))))
    (objed--toggle-state)
    ;; for words force update because
    ;; a word can contain multiple inner
    ;; objects (CamelCaseSubWords)
    (when (eq objed--object 'word)
      (objed--update-current-object))
    (cond (boo
           (goto-char (objed--beg)))
          ((and eoo
                (not (eq objed--object 'line)))
           (goto-char (objed--end)))
          ((< (point) (objed--beg))
           (goto-char (objed--beg)))
          ((and
            (> (point) (objed--beg))
            (> (point) (objed--end)))
           (goto-char (objed--end))))))

(defun objed-toggle-side ()
  "Move to other side of object.

Default to sexp at point."
  (interactive)
  (if (use-region-p)
      (exchange-point-and-mark)
    (let ((sdiff (abs (- (point) (objed--beg))))
          (ediff (abs (- (point) (objed--end)))))
      (cond ((> ediff sdiff)
             (goto-char (objed--end)))
            (t
             (goto-char (objed--beg)))))))

;; Edit objects

(defun objed-del-insert ()
  "Delete current object and exit to insert state."
  (interactive)
  (objed--do #'delete-region 'mc)
  (objed--exit-objed))

(defun objed-kill (&optional times)
  "Kill object(s).

Kill marked objects or TIMES instances of current
object (defaults to 1)."
  (interactive "p")
  (when objed-append-mode
    (setq last-command 'kill-region))
  (if objed--marked-ovs
      (objed--do #'kill-region)
    (let ((times (or times 1)))
      (dotimes (_ times)
        (objed--do #'kill-region)
        (setq last-command #'kill-region)))
    (message (if objed-append-mode
                 "Appended to `kill-ring'"
               "Added to `kill-ring.'"))))


(defun objed-copy (&optional reg)
  "Copy objects.

On repeat add text to objed register.
With prefix arg REG non nil ask for register."
  (interactive "P")
  (when objed-append-mode
    ;; append on repeat
    (setq last-command 'kill-region))
  (objed--do #'copy-region-as-kill 'keep)
  (cond ((eq real-last-command real-this-command)
         (set-register :objed-register
                       (objed--object-string))
         (message "Copied to objed register, insert with x-y."))
        (reg
         (set-register (register-read-with-preview "Save to register: ")
                       (objed--object-string))
         (message "Copied to register"))

        (t
         (message (if objed-append-mode
                      "Appended to `kill-ring'"
                    "Copied to `kill-ring.'")))))

(defun objed-delete (&optional times)
  "Delete object(s).

Delete marked objects or TIMES instances of current
object (defaults to 1)."
  (interactive)
  (if objed--marked-ovs
      (objed--do #'delete-region)
    (let ((times (or times 1)))
      (dotimes (_ times)
        (objed--do #'delete-region)))))

(defun objed-yank (arg)
  "Yank and indent.

ARG is passed to `yank'. On repreat `yank-pop'."
  (interactive "*P")
  (when objed-append-mode
    (objed-append-mode -1))
  (let ((start (point))
        (inhibit-message t))
    (if (eq last-command 'yank)
        (yank-pop arg)
      (yank arg)
      (objed--switch-to 'region))
    (indent-region start (point))
    (indent-according-to-mode)
    (objed--update-current-object)))

;; Move objects

(defun objed-move-object-forward ()
  "Move object forward.

Swaps the current object with the next one."
  (interactive)
  (let* ((current (buffer-substring (objed--beg)
                                    (objed--end)))

         (nexto (objed--get-next))
         (next (and nexto (apply #'buffer-substring
                                 (objed--current nexto))))
         (nend (objed--end nexto)))
    (apply #'delete-region (objed--current nexto))
    (goto-char (objed--beg nexto))
    (insert current)

    (apply #'delete-region (objed--current))
    (goto-char (objed--beg))
    (insert next)

    (goto-char (- nend (length current)))
    (objed--update-current-object)))


(defun objed-move-object-backward ()
  "Move object backward.

Swaps the current object with the previous one."
  (interactive)
  (let* ((current (buffer-substring (objed--beg)
                                    (objed--end)))

         (prevo (objed--get-prev))
         (prev (and prevo (apply #'buffer-substring
                                 (objed--current prevo))))
         (pbeg (objed--beg prevo)))

    (apply #'delete-region (objed--current))
    (goto-char (objed--beg))
    (insert prev)

    (apply #'delete-region (objed--current prevo))
    (goto-char (objed--beg prevo))
    (insert current)
    (goto-char pbeg)
    (objed--update-current-object)))

(objed-define-op nil objed-indent ignore)

(defun objed-indent-left (arg)
  "Indent all lines in object leftward by ARG space."
  (interactive "p")
  (objed--do (lambda (beg end)
               (indent-rigidly beg end (- arg)))
             'current))

(defun objed-indent-right (arg)
  "Indent all lines in object rightward by ARG space."
  (interactive "p")
  (objed--do (lambda (beg end)
               (indent-rigidly beg end arg))
             'current))

(defun objed-indent-to-left-tab-stop ()
  "Indent all lines in object lefttward to a tab stop."
  (interactive)
  (objed--do  #'indent-rigidly-left-to-tab-stop
              'current))

(defun objed-indent-to-right-tab-stop ()
  "Indent all lines in object rightward to a tab stop."
  (interactive)
  (objed--do #'indent-rigidly-right-to-tab-stop
             'current))

;; Marking

(defun objed-mark (&optional arg)
  "Mark/unmark objects.

Marks the object at point or unmarks it if it is marked already.
If the object contains marked objects those are unmarked.

Afterwards move to the next object. With positive numeric prefix
argument ARG mark that many objects."
  (interactive "p")
  (let ((arg (or arg 1)))
    (dotimes (_ arg)
      (objed--toggle-mark)
      (objed--goto-next))))


(defun objed-toggle-mark-backward (&optional arg)
  "Move to previous object and toggle mark it.

With positive numeric prefix argument ARG unmark that many
previous objects."
  (interactive "p")
  (let ((arg (or arg 1)))
    (dotimes (_ arg)
      ;; enf. top down order
      (objed--toggle-mark nil 'append)
      (objed--goto-previous))))


(defun objed-mark-more ()
  "Mark more instances of current object.

First try to mark more in current defun after that mark more in
current buffer."
  (interactive)
  (let* ((n nil)
         (msg (cond ((and (eq last-command this-command)
                          (setq n (objed--mark-all-inside 'buffer)))
                     (format "Marked %s %ss in %s." n objed--object 'buffer))
                    ((setq n (objed--mark-all-inside 'defun))
                     (format "Marked %s %ss in %s." n objed--object 'defun))
                    ((setq n (objed--mark-all-inside 'buffer))
                     (format "Marked %s %ss in %s." n objed--object 'buffer))
                    (t
                     "No other objects to mark."))))
    ;; wait for redisplay
    (run-at-time 0.1 nil
                 #'message msg)))

(defun objed-exchange-point-and-mark ()
  "Exchange point and mark.

Update to object at current side."
  (interactive)
  (when (region-active-p)
    (when (= (point) (region-end))
      (goto-char (objed--end)))
    (exchange-point-and-mark)
    (if (= (point) (region-end))
        (objed--skip-ws t)
      (objed--skip-ws))
    (objed--update-current-object)))

;; Miscellaneous

(objed-define-op nil objed-comment-or-uncomment-region)

(objed-define-op nil flyspell-region)

(objed-define-op nil objed-electric-pair)

(defun objed-raise ()
  "Replace object with inner part."
  (interactive)
  (let* ((ibeg (objed--ibeg))
         (iend (objed--iend))
         (istring (buffer-substring ibeg iend))
         (obeg (objed--obeg))
         (oend (objed--oend)))
    (objed--reset)
    (delete-region obeg oend)
    (objed--init
     (objed-make-object :beg (point)
                        :end (save-excursion (insert istring)
                                             (point))))))

;; Special commands

(defun objed-ace (&optional obj beg end)
  "Jump to an object with `avy'.

OBJ defaults to current object. BEG and END limit the region
which should be searched for candidates and default to
`window-start' and `window-end.'"
  (interactive)
  (when objed-use-avy-if-available
    (unless (require 'avy nil t)
      (user-error objed--avy-err-msg))
    (if (eq objed--object 'char)
        (progn (call-interactively #'avy-goto-char)
               (objed--update-current-object))
      (let* ((avy-action #'goto-char)
             (avy-style 'at-full)
             (avy-all-windows t)
             (posns (let* ((objed--object (or obj objed--object))
                           (beg (or beg (window-start)))
                           (end (or end (window-end))))
                      (objed--collect-object-positions
                       beg end
                       (when obj
                         (point))))))
        (cond (posns
               (if (> (length posns) 1)
                   (avy-process
                    posns (avy--style-fn avy-style))
                 (goto-char (caar posns)))
               (if obj
                   (objed--switch-to obj)
                 (objed--update-current-object)))
              (t
               (message "No objects found.")))))))

(defun objed-replace ()
  "Query replace narrowed to region BEG, END."
  (interactive)
  (if (memq objed--object '(word symbol identifier))
      (objed--replace-object)
    (save-excursion
      (save-restriction
        (let ((beg (objed--beg))
              (end (objed--end)))
          (narrow-to-region beg end)
          (goto-char (point-min))
          (hl-line-unhighlight)
          (deactivate-mark)
          (if (fboundp 'anzu-query-replace-regexp)
              (call-interactively 'anzu-query-replace-regexp)
            (call-interactively 'query-replace-regexp)))))))

(defun objed-eval-expression (&optional ins)
  "Eval expression.

Prefix arg INS:

  With \\[universal-argument] insert result into current buffer.

  With \\[universal-argument] \\[universal-argument] insert
  functioncall at point into minibuffer.

  With \\[universal-argument] \\[universal-argument]
  \\[universal-argument] insert function call and insert result."
  (interactive "P")
  (let* ((funs (car (elisp--fnsym-in-current-sexp)))
         (func (and (or (functionp funs)
                        (subrp funs)
                        (macrop funs)
                        (special-form-p funs))
                    (format "(%s )" (symbol-name funs)))))
    (if (and func (and ins (member ins '((64) (16)))))
        (minibuffer-with-setup-hook (lambda ()
                                      (insert func)
                                      (forward-char -1))
          (let ((current-prefix-arg (equal '(64) ins)))
            (call-interactively 'eval-expression)))
      (let ((current-prefix-arg ins))
        (call-interactively 'eval-expression)))))

(objed-define-op nil objed-pipe-region)

(objed-define-op nil objed-ipipe)

(defun objed-execute ()
  "Execute object contents as shell commands."
  (interactive)
  (let* ((beg (objed--beg))
         (end (objed--end))
         (str (concat (buffer-substring beg end) "\n"))
         (lines (split-string str "[\r\n]" t)))
    (objed--reset)
    (dolist (line lines)
      (when (y-or-n-p (concat (format "$ %s" line)
                              "?"))
        (shell-command line)))
    (objed--init objed--object)))

(objed-define-op nil objed-run-or-eval ignore)

(objed-define-op nil objed-comment-duplicate)

(objed-define-op nil objed-duplicate-down)

(defun objed-insert-new-object ()
  "Insert boundaries of object after current one."
  (interactive)
  (let* ((opos (objed--oend))
         (left (objed--get-left-boundary))
         (right (objed--get-right-boundary))
         (oc (buffer-substring (objed--obeg) (objed--oend)))
         (ic (buffer-substring (objed--ibeg) (objed--iend)))
         (linep (objed--line-p oc))
         (lispy (derived-mode-p 'lisp-mode
                                  'emacs-lisp-mode
                                  'clojure-mode
                                  'racket-mode
                                  'lisp-interaction-mode))
         (nb nil))
    (goto-char opos)
    (if (string= ic oc)
        (insert " " oc)
      (insert (format "%s%s"
                      (if (and linep
                               (not (string= "" ic))
                               (not (string-match "\\`\n\\|\n\\'" right))
                               (not (string-match "\\`\n\\|\n\\'" left)))
                         "\n"
                        (if (string-match "\n\\'" oc) ""
                          (if (looking-at "\n") "\n" " ")))
                      left))
      ;; stay in the middle
      (save-excursion
        (insert (format "%s" right))))
    ;; dumb search for probably best pos
    (when (re-search-backward "\\_>" opos t)
      (re-search-backward "\\_<" opos t))
    (if (and (not lispy)
             (setq nb (bounds-of-thing-at-point 'list)))
        (goto-char (car nb))
      (when lispy
        (indent-according-to-mode)))
    ;; goto ref if there is one...
    (objed--exit-objed)))


;; Resize windows

(defun objed-move-window-line-up ()
  "Move window line up."
  (interactive)
  (cond ((and (window-in-direction 'above)
              (window-in-direction 'below))
         (shrink-window 1))
        ((window-in-direction 'above)
         (enlarge-window 1))
        ((window-in-direction 'below)
         (shrink-window 1))))

(defun objed-move-window-line-down ()
  "Move window line down."
  (interactive)
  (cond ((and (window-in-direction 'above)
              (window-in-direction 'below))
         (enlarge-window 1))
        ((window-in-direction 'above)
         (shrink-window 1))
        ((window-in-direction 'below)
         (enlarge-window 1))))


(defun objed-move-window-line-left ()
  "Move window line to the left."
  (interactive)
  (cond ((and (window-in-direction 'left)
              (window-in-direction 'right))
         (shrink-window-horizontally 1))
        ((window-in-direction 'left)
         (enlarge-window-horizontally 1))
        ((window-in-direction 'right)
         (shrink-window-horizontally 1))))

(defun objed-move-window-line-right ()
  "Move window line to the right."
  (interactive)
  (cond ((and (window-in-direction 'left)
              (window-in-direction 'right))
         (enlarge-window-horizontally 1))
        ((window-in-direction 'left)
         (shrink-window-horizontally 1))
        ((window-in-direction 'right)
         (enlarge-window-horizontally 1))))


;; Slurp and barf


(defun objed-forward-slurp-sexp (&optional arg)
  "Slurp following sexp into current bracket or string object.

Automatically reindent the newly slurped sexp, unless it is a
string.

With numerical prefix ARG, slurp that many sexps into
current object.

With \\[universal-argument], slurp all proceeding sexps into the
current object."
  (interactive "P")
  (save-excursion
    (cond
     ((objed--in-comment-p)
      (error "Invalid context for forward slurping sexps"))
     ((numberp arg)
      (if (< arg 0)
          (objed-forward-barf-sexp (- arg))
        (while (< 0 arg)
          (objed-forward-slurp-sexp)
          (setq arg (1- arg)))))
     ((objed--in-string-p)
      ;; If there is anything to slurp into the string, take that.
      ;; Otherwise, try to slurp into enclosing list.
      (if (objed-with-temp-object 'string
            (scan-sexps (objed--oend) 1))
          (objed-with-temp-object 'string
            (objed--forward-slurp-object arg))
        (objed-with-temp-object 'bracket
          (objed--forward-slurp-object arg))))
     (t
      (objed-with-temp-object 'bracket
        (objed--forward-slurp-object arg))))))

(defun objed-forward-barf-sexp (&optional arg)
  "Remove the last sexp in the current list or string object.

Automatically reindent the newly barfed sexp, unless it is a
string.

With a numerical prefix ARG, barf that many sexps out of
current object.

With \\[universal-argument], barf all sexps out of current
object."
  (interactive "P")
  (save-excursion
    (cond
     ((objed--in-comment-p)
      (error "Invalid context for forward barfing sexps"))
     ((and (numberp arg) (< arg 0))
      (objed-forward-slurp-sexp (- arg)))
     ((objed--in-string-p)
      ;; If there is anything in the string, take that.
      ;; Otherwise, try to barf string out of enclosing list.
      (if (save-excursion
            (objed-with-temp-object 'string
              (let ((start (objed--ibeg))
                    (end (objed--iend)))
                (goto-char end)
                (backward-sexp (and (numberp arg) arg))
                (>= (point) start))))
          (objed-with-temp-object 'string
            (objed--forward-barf-object arg))
        (objed-with-temp-object 'bracket
          (objed--forward-barf-object arg))))
     (t
      (objed-with-temp-object 'bracket
        (objed--forward-barf-object arg))))))

(defun objed-backward-slurp-sexp (&optional arg)
  "Add in the preceding sexp to the current bracket or string.

If it is a list, automatically reindent.

With numerical prefix ARG, slurp that many sexps into the
current object.

With \\[universal-argument], slurp all preceding sexps into
current object."
  (interactive "P")
  (save-excursion
    (cond ((objed--in-comment-p)
           (error "Invalid context for slurping sexps"))
          ((numberp arg)
           (if (< arg 0)
               (objed-backward-barf-sexp (- arg))
             (while (< 0 arg)
               (objed-backward-slurp-sexp)
               (setq arg (1- arg)))))
          ((objed--in-string-p)
           ;; If there is anything to slurp into the string, take that.
           ;; Otherwise, try to slurp into the enclosing list.
           (if (objed-with-temp-object 'string
                 (scan-sexps (objed--obeg) -1))
               (objed-with-temp-object 'string
                 (objed--backward-slurp-object arg))
             (objed-with-temp-object 'bracket
               (objed--backward-slurp-object arg))))
          (t
           (objed-with-temp-object 'bracket
             (objed--backward-slurp-object arg))))))

(defun objed-backward-barf-sexp (&optional arg)
  "Remove the first sexp in the current list or string object.

Automatically reindent the newly barfed sexp, unless it is a
string.

With a numerical prefix ARG, barf that many sexps out of
current object.

With \\[universal-argument], barf all sexps out of current
object."
  (interactive "P")
  (save-excursion
    (cond
     ((objed--in-comment-p)
      (error "Invalid context for backward barfing sexps"))
     ((and (numberp arg) (< arg 0))
      (objed-backward-slurp-sexp (- arg)))
     ((objed--in-string-p)
      ;; If there is anything in the string, take that.
      ;; Otherwise, try to barf string out of enclosing list.
      (if (save-excursion
            (objed-with-temp-object 'string
              (let ((start (objed--ibeg))
                    (end (objed--iend)))
                (goto-char start)
                (forward-sexp (and (numberp arg) arg))
                (<= (point) end))))
          (objed-with-temp-object 'string
            (objed--backward-barf-object arg))
        (objed-with-temp-object 'bracket
          (objed--backward-barf-object arg))))
     (t
      (objed-with-temp-object 'bracket
        (objed--backward-barf-object arg))))))

;; Operations

(defun objed-op-x (&optional arg)
  "Choose and apply an operation from region commands with completion.

With numeric prerfix ARG of 0 invalidate the used cache. Any
other value of ARG gets passed as prefix argument to the choosen
region command."
  (interactive "P")
  (let* ((rcmd (objed--read-cmd (= 0 (prefix-numeric-value arg))))
         (cmd (objed--create-op
               rcmd
               (and (not (= 0 (prefix-numeric-value arg)))
                    arg))))
    (objed--do cmd rcmd)))

(objed-define-op nil objed-case-op)

(defun objed-eval-defun (&optional replace)
  "Eval defun or objects.

If REPLACE is non-nil replace evaluated code with result."
  (interactive "P")
  (if objed--marked-ovs
      (save-excursion
        (dolist (ov (nreverse (copy-sequence objed--marked-ovs)))
          (let ((beg (overlay-start ov))
                (end (overlay-end ov)))
            (delete-overlay ov)
            (when (and beg end)
              (goto-char beg)
              (funcall 'objed--eval-func beg end replace)))))
    (let* ((odata (objed--get-object 'defun))
           (reg (objed--current odata))
           (res nil))
      (when (and reg
                 (setq res (apply 'objed--eval-func
                                  (append reg (list replace)))))
        (prog1 res
          (objed--switch-to 'defun nil odata))))))

(defun objed-eval-exp (&optional replace)
  "Eval expression at point, fallback to defun.

If REPLACE is non-nil replace evaluated code with result."
  (interactive "P")
  (let* ((obj (cond ((objed--at-object-p 'bracket)
                       'bracket)
                    ((or (objed--at-object-p 'identifier)
                         (objed--inside-object-p 'identifier))
                     'identifier)
                    (t 'defun)))
         (odata (objed--get-object obj))
         (res (and odata
                   (apply 'objed--eval-func
                          (append (objed--current odata) (list replace))))))
    (if replace
        (objed--switch-to 'char)
      (when res
        (prog1 res
          (objed--switch-to obj nil odata))))))

(defun objed-narrow (&optional arg)
  "Narrow to object.

With prefix argument ARG call `edit-indirect-region' if
`edit-indirect' is available."
  (interactive "P")
  (if objed--marked-ovs
      (message "Narrowing not possible with multiple objects.")
    (cond ((bound-and-true-p edit-indirect--overlay)
           (edit-indirect-commit))
          ((buffer-narrowed-p)
           (widen))
          (t
           (if (and (require 'edit-indirect nil t)
                    arg)
               (switch-to-buffer
                (apply #'edit-indirect-region (objed--current)))
             (apply 'narrow-to-region (objed--current)))))))

(defun objed-other-window ()
  "Like `other-window' for objed."
  (interactive)
  (objed--reset--objed-buffer)
  (other-window 1)
  (objed--init (or objed--object 'char)))


(defun objed-kill-buffer ()
  "Like `kill-this-buffer' for objed."
  (interactive)
  (objed--reset--objed-buffer)
  (kill-buffer (current-buffer))
  (objed--init (or objed--object 'char)))

;; Dispatch commands


(objed-define-dispatch "-" objed--backward-until)
(objed-define-dispatch "+" objed--forward-until)
(objed-define-dispatch "=" objed--ace-switch-in-current)
(objed-define-dispatch "#" objed--ace-switch-object)


;;;; Utility expressions

(defun objed-backward-symbol (arg)
  "Wrapper around `forward-symbol'.
Move ARG times backward."
  (forward-symbol (- arg)))

(defun objed-repeat ()
  "Repeat last command for objed."
  (call-interactively 'repeat)
  (setq real-this-command 'repeat)
  (objed--switch-to 'char))

(defun objed--forward-sexp ()
  "Forward a sexp."
  (interactive)
  (let ((stringp nil))
    (while (and (not (eobp))
                (or  (and (not (bobp))
                          (save-excursion
                            (objed--skip-ws)
                            (eq (char-syntax (char-after)) ?\"))
                          (setq stringp (objed--in-string-p nil t)))
                     (not (ignore-errors
                            (let ((real-this-command 'forward-sexp))
                              (call-interactively 'forward-sexp))
                            t))))
      (if stringp
          (progn (goto-char stringp)
                 (forward-sexp 1))
        (forward-char 1))
      (setq stringp nil))))


(defun objed--backward-sexp ()
  "Backward a sexp."
  (interactive)
  (let ((stringp nil))
    (while (and (not (bobp))
                (or  (and (not (eobp))
                          (save-excursion
                            (objed--skip-ws t)
                            (eq (char-syntax (char-before)) ?\"))
                          (setq stringp (objed--in-string-p nil t)))
                     (not (ignore-errors
                            (let ((real-this-command 'forward-sexp))
                              (call-interactively 'backward-sexp))
                            t))))
      (if stringp
          (goto-char stringp)
        (forward-char -1))
      (setq stringp nil))))


(defun objed-describe-key ()
  "Like `describe-key' but also exit objed."
  (interactive)
  (call-interactively 'describe-key)
  (objed--exit-objed))


(defun objed-current-or-previous-context (&optional arg)
  "Move to end of object at point and activate it.

On repeat move to the previous instance of this object type. With
postitive prefix argument ARG move to the nth previous object."
  (interactive "p")
  (if (objed--basic-p)
      (let ((pos (point)))
        (objed-context-object)
        (if (<= (objed--beg) pos (objed--end))
            (goto-char (objed--beg))
          (objed--goto-previous (or arg 1))))
    (objed-previous arg)))


(defun objed-current-or-next-context (&optional arg)
  "Move to beginning of object at point and activate it.

On repeat move to the next instance of this object type. With
postitive prefix argument ARG move to the nth next object."
  (interactive "p")
  (if (objed--basic-p)
      (let ((pos (point)))
        (objed-context-object)
        (if (<= (objed--beg) pos (objed--end))
            (goto-char (objed--end))
          (objed--goto-next (or arg 1))))
    (objed-next arg)))


(defun objed-upto-context ()
  "Toggle between inner sides of object at point.

Moves to end of (inner) object at point and activates the text
moved over. On repeat toggle between beginning/end inside the
object.

Object is choosen based on context."
  (interactive)
  (objed--toggle-ends
   (lambda ()
     (objed-context-object)
     (objed--toggle-state))))


;; * General object commands

(defun objed-object-x ()
  "Choose and switch to object with completion."
  (interactive)
  (let (objects obj)
    (map-keymap (lambda (_ev cmd)
                  (let ((name (symbol-name cmd)))
                    (when (string-match
                           "\\`objed-\\(.*?\\)-object\\'"
                           name)
                      (push (match-string 1 name)
                            objects))))
                objed-object-map)
    (when (setq obj (objed--with-allow-input
                     (completing-read "Objects: " objects)))
      (when (objed--switch-to obj)
        (goto-char (objed--beg))))))

(defun objed-activate (&optional obj)
  "Activate objed.

When called non interactively activate with object OBJ which
defaults to char object. Otherwise uses associated
`objed-cmd-alist' for `last-command' as initial object. Falls
back to `objed-initial-object' if no match found."
  (interactive)
  (if (called-interactively-p 'any)
      (objed--init
       (if (assq last-command objed-cmd-alist)
           last-command
         objed-initial-object))
    (objed-init obj)))

(defun objed-until-beg-of-object-at-point ()
  "Move to beginning of object at point and active text moved over."
  (interactive)
  (let ((pos (point)))
    (objed--init 'char)
    (when (objed-context-object)
      (objed--reverse)
      (goto-char (objed--ibeg))
      (objed--change-to :end pos :iend pos))))

(defun objed-until-end-of-object-at-point ()
  "Move to end of object at point and active text moved over."
  (interactive)
  (let ((pos (point)))
    (objed--init 'char)
    (when (objed-context-object)
      (objed--reverse)
      (goto-char (objed--iend))
      (objed--change-to :beg pos :ibeg pos))))





(defun objed-contents-object ()
  "Switch to reference of an object.

Reference is how you would refer to an object. As an example for
a defun it would be the function name. This defaults to the
textual content of an object via the content object."
  (interactive)
  (let ((ref (objed--object :ref)))
    (if ref (objed--switch-to ref)
      (setq objed--content-bounds
            (objed--bounds))
      (objed--switch-to 'content))
    (goto-char (objed--beg))
    (force-mode-line-update)))

(defun objed-occur ()
  "Complete initial lines and jump to object."
  (interactive)
  (let* ((lines (objed--collect-object-lines))
         (ivy-sort-function-alist nil)
         (completion-table
          ;; avoid auto sorting of completing read
          (lambda (string pred action)
            ;; FIX: its upside down for sections
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action action lines string pred))))
         (res (objed--with-allow-input
               (cdr (assoc (completing-read "Objects: " completion-table)
                           lines)))))
    (when res
      (goto-char res)
      (objed--init objed--object)
      (goto-char (objed--beg)))))



(defun objed--merge-marked ()
  "Merge marked objects.

Unmarks all and marks the text between the first and last marked
object instead and moves to its start."
  (interactive)
  (if (cdr objed--marked-ovs)
      (let (first last)
        (setq last (pop objed--marked-ovs))
        (setq objed--marked-ovs (nreverse objed--marked-ovs))
        (setq first (pop objed--marked-ovs))
        (let ((beg (overlay-start first))
              (end (overlay-end last)))
          ;; reset marked
          (delete-overlay first)
          (delete-overlay last)
          (dolist (ov objed--marked-ovs)
            (delete-overlay ov))
          (setq objed--marked-ovs nil)
          (objed--change-to :beg beg :end end)
          (goto-char beg))
        (message "Merged marked objects before execution."))
    (message "At least two marked objects needed.")))


(defun objed-unmark (&optional arg)
  "Unmark objects.

Like `objed-mark' but unmarks. With positive numeric prefix
argument ARG unmark that many objects."
  (interactive "p")
  (let ((arg (or arg 1)))
    (dotimes (_ arg)
      (objed--unmark-object)
      (objed--goto-next))))

(defun objed-toggle-mark ()
  "Like `objed-mark', but don't move on to next object."
  (interactive)
  (objed--toggle-mark))


(defun objed-insert (&optional read)
  "Insert stuff.

When READ is non-nil read insert action, otherwise default to
inserting objed-register (see `objed-copy')."
  (interactive "P")
  (let ((action
         (or (and read
                  (read-char-choice
                   "[f]ile, [r]egsiter, [o]bjed register " '(?f ?r ?o)))
             ?o)))
    (cl-case action
      (?f
       (call-interactively 'insert-file))
      (?r
       (insert-register
        (register-read-with-preview "Inser register: ")))
      (?o
       (insert-register :objed-register)))))



(defun objed-electric-event (beg end &optional event)
  "Wrap region between BEG and END using `elec-pair'.

EVENT is used for wrapping according to
`electric-pair-post-self-insert-function' and defaults to
`last-command-event'. If event is not an electric event fallback
to sourround region string representation of event."
  (interactive "r")
  (require 'elec-pair)
  (let ((electric-pair-mode t)
        (last-command-event (or event last-command-event))
        (epos nil)
        ;; make sure to go to beginning
        (rbeg (if (> beg end) end beg))
        (rend (if (> beg end) beg end)))
    (if (not (memq (car (electric-pair-syntax-info last-command-event))
                   '(?\( ?\) ?\" ?\$)))
        (objed--surround beg end
                         (char-to-string last-command-event))
      (save-mark-and-excursion
       ;; skip ws optionally?
       (push-mark (objed--skip-backward rend 'ws) t t)
       (goto-char rbeg)
       (objed--skip-ws)
       (insert last-command-event)
       ;; todo: additional expansion/insertion
       (setq epos (point))
       (electric-pair-post-self-insert-function))
      ;; leave point like electric would for region
      (goto-char epos))))

(defun objed-indent (beg end)
  "Indent region between BEG and END.

Moves point over any whitespace afterwards."
  (interactive "r")
  (indent-region beg end))

(defun objed-open-line ()
  "Open line."
  (interactive)
  (let ((indent (current-column)))
    (save-excursion
      (insert "\n")
      (insert (make-string indent ?\s))))
  (objed--reset))

(defun objed--switch-and-move (o dir)
  "Switch to object O and move it in direction DIR."
  (interactive)
  (unless (eq objed--object o)
    (objed--switch-to o))
  (cond ((eq dir 'forward)
         (objed-move-object-forward))
        ((eq dir 'backward)
         (objed-move-object-backward))))

;; TODO: toggle like fill/unfill
(defun objed-reformat-op (beg end)
  "Reformat object between BEG and END."
  (interactive "r")
  (cond ((memq objed--object (list 'paragraph 'sentence 'line 'textblock))
         (fill-paragraph)
         (objed--init objed--object))
        ((memq objed--object (list 'sexp 'bracket))
         (save-excursion
           (goto-char beg)
           (indent-pp-sexp))
         (objed--init objed--object))
        (t
         (objed-indent beg end))))


(defun objed-pipe-region (beg end cmd &optional variant)
  "Pipe region text between BEG and END through a shell CMD.

VARIANT is either the char r or e to either replace the text with the result
or to echo it."
  (interactive (let ((variant (read-char-choice "Replace [r] Echo [e] "
                                                (list ?r ?e)))
                     (cmd (read-shell-command "Shell command: ")))

                 (list (region-beginning) (region-end)
                       cmd
                       variant)))
  (let* ((cbuf (current-buffer))
         (obuf (generate-new-buffer " *objed-pipe-out*"))
         (args (split-string cmd " "))
         (cmd-name (pop args))
         (skip-nline nil)
         (ecode (with-temp-buffer
                  (insert-buffer-substring cbuf beg end)
                  ;; make sure text ends with newline so
                  ;; it get processed by command
                  (unless (memq (char-before)
                                (list 10 13))
                    (setq skip-nline t)
                    (insert "\n"))
                  (apply 'call-process-region (point-min) (point) cmd-name
                         nil (list obuf) nil args)))
         (output (with-current-buffer obuf
                   ;; dont include newline if manually added
                   (when skip-nline
                     (skip-chars-backward "\n"))
                   (buffer-substring (point-min) (point)))))
    (cond ((string= "" output)
           (kill-buffer obuf)
           (message
            "Command had empty ouput. Exit code %s." ecode))
          ((string= output
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))
           (kill-buffer obuf)
           (message
            "Command did not change text. Exit code %s" ecode))
          ((not (zerop ecode))
           (pop-to-buffer obuf)
           (message
            "Command exited with exit code %s." ecode))
          (t
           (cl-case variant
             (?r
              (kill-buffer obuf)
              (delete-region beg end)
              (insert output)
              (message "Replaced region with output."))
             (?e
              (pop-to-buffer obuf)))))
    (objed--exit-objed)))

(defun objed-comment-or-uncomment-region (beg end &optional arg)
  "Similar to `comment-or-uncomment-region'.

Comments empty line, too. BEG, END and ARG are passed to
`comment-or-uncomment-region'."
  (interactive "*r\nP")
  (if (and (eq (save-excursion (goto-char beg)
                               (beginning-of-line))
               (save-excursion (goto-char end)
                               (beginning-of-line)))
           (save-excursion
             (beginning-of-line)
             (looking-at "^ *$")))
      (progn (goto-char (save-excursion (beginning-of-line)
                                        ;; dummy so comment region does its
                                        ;; thing
                                        (insert "a")
                                        (prog1 (point-marker)
                                          (comment-or-uncomment-region beg end arg))))
             (delete-char -1))
    (comment-or-uncomment-region beg end arg)))


(defun objed-case-op (beg end case)
  "Change case of region.

Use region between BEG and END. CASE is a character which
determines the case operation:

u: upcase
d: downcase
c: capitalize."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-char-choice "Upcase [u], Downcase [d], Capitalize [c]: "
                           (list ?u ?d ?c))))
  (cl-case case
    (?u
     (upcase-region beg end))
    (?d
     (downcase-region beg end))
    (?c
     (capitalize-region beg end))))




;; ** Minibuffer commands

(defun objed-ipipe-commit ()
  "Commit current minibuffer input.

Exit if there is no content."
  (interactive)
  (if (string= "" (minibuffer-contents))
      (progn
        (remove-hook 'after-change-functions
                     #'objed--ipipe-schedule-timer t)
        (exit-minibuffer))
    ;; when timer did not run yet, update manually
    (when objed--ipipe-timer
      (objed--ipipe-reset-timer)
      (objed--ipipe-update-display))
   ;; cache current string
  (setq objed--ipipe-region-string
        (overlay-get objed--ipipe-overlay 'after-string))
  (cl-pushnew (minibuffer-contents)
              objed-ipipe-hist :test #'string=)
  (delete-minibuffer-contents)))

(defun objed-ipipe-complete ()
  "Completion used by `objed-ipipe'.

Completes shell and region commands."
  (interactive)
  (let ((bounds (or (bounds-of-thing-at-point 'symbol)
                                           (cons (point)
                                                 (point))))
        (sym (or (thing-at-point 'symbol) "")))
    (cl-destructuring-bind (beg . end) bounds
      (completion-in-region
       beg end (append
                (locate-file-completion-table
                 exec-path exec-suffixes sym 'identity t)
                (or objed--cmd-cache
                    (progn
                      (mapatoms #'objed--init-cmd-cache)
                      objed--cmd-cache)))))))

;; ** Entry command

(defun objed-ipipe (beg end)
  "Pipe region between BEG, END through commands.

Commands can be shell commands or region commands."
  (interactive "r")
  ;; init
  (setq objed--ipipe-last "")
  (setq objed--ipipe-overlay (make-overlay beg end))
  (objed--ipipe-reset-timer)
  (overlay-put objed--ipipe-overlay 'invisible t)
  (overlay-put objed--ipipe-overlay 'after-string
               (setq objed--ipipe-region-string
                     (buffer-substring-no-properties beg end)))
  (unwind-protect
      (minibuffer-with-setup-hook
          (lambda ()
            (add-hook 'after-change-functions 'objed--ipipe-schedule-timer nil t))
        ;; on success replace region with current overlay
        (save-restriction
          (narrow-to-region beg end)
          (when (read-from-minibuffer "Command (leave blank to accept current state): " nil
                                      objed-ipipe-minibuffer-map nil 'objed-ipipe-hist)
            (objed--ipipe-reset-timer)
            (delete-region beg end)
            (save-excursion
              (insert (overlay-get objed--ipipe-overlay 'after-string))))))
    (objed--ipipe-reset-timer)
    (dolist (buf (buffer-list))
      (when (minibufferp buf)
        (remove-hook 'after-change-functions 'objed--ipipe-schedule-time t)))
    (delete-overlay objed--ipipe-overlay)))

(defun objed-run-or-eval (beg end)
  "Evalate region between BEG and END using `eval-in-repl'."
  (interactive "r")
  (if (not (require 'eval-in-repl nil t))
      (error "The library eval-in-repl couldn't be found")
    (let* ((name (symbol-name (cdr (assq major-mode objed--eir-alist))))
           (lib (intern (concat "eval-in-repl-" name)))
           (cmd (intern (concat "eir-eval-in-" name))))
      (cond  ((and (eq lib 'eval-in-repl-ielm)
                   (not objed-use-ielm-for-eval-p))
              (eval-region beg end t))
             ((and (require lib nil t)
                   (commandp cmd))
              (call-interactively cmd))))))

(defun objed-switch-goto-beg (_obj beg _end)
  "Move to BEG.

This function move to the beginning of any selected
object via `objed-switch-functions'."
  (goto-char beg))

(defun objed--object-dispatch (name)
  "Dispatch according to object NAME.

Uses `objed--dispatch-alist' and defaults to
update to given object."
  (let* ((cmd (key-binding
               (vector
                (if (>= (length (this-command-keys-vector)) 2)
                    (aref (this-command-keys-vector) 0)
                  ;; for testing purposes...
                  last-command-event))))
         (binding (assq cmd objed--dispatch-alist)))
    (cond (binding
           (funcall (cdr binding) name))
          (t
           (if objed--buffer
               (objed--switch-to name)
             (objed--init name))
           (when objed--object
             (let ((switcher (or (assq name objed-switch-alist)
                                 (assq t objed-switch-alist))))
               (when switcher
                 (funcall (cdr switcher) (objed--beg) (objed--end))))
             (run-hook-with-args 'objed-switch-functions name
                                 (objed--beg) (objed--end)))))))


(defun objed--switch-to-object-for-cmd (cmd)
  "Guess which object to use.

CMD is the command for which object should be guessed. Returns
cons of guessed object and its state."
  (let ((objed--block-p t)
        (o (cdr (assq cmd objed-cmd-alist)))
        (initf (cdr (assq cmd objed--after-init-alist))))
    (if o
        (objed--switch-to o (if (eq cmd #'back-to-indentation)
                                'inner 'whole))
      (objed--update-current-object))
    (when initf (funcall initf objed--opoint))))


(defun objed--point-in-periphery ()
  "Return non-nil if point is in current lines periphery."
  (<= (point) (save-excursion (back-to-indentation) (point))))


(defun objed--define-prefix (key cmd)
  "Create a prefix keymap for `objed-map'.

The keymap will be accessible with KEY and bound to prefix command
CMD.

The keymap is initilized with basic bindings for numeric
arguments and help.

Other single character keys are bound to `objed-undefined'."
  (let ((map (define-prefix-command cmd)))
    ;; init as prefix
    (define-key objed-map (kbd key) map)
     ;; basic bindings
    (dolist (char (number-sequence ?a ?z))
      (define-key map (kbd (format "%c" char)) 'objed-undefined))
    (let (loop)
        (define-key map "-" 'negative-argument)
        ;; Make plain numbers do numeric args.
        (setq loop ?0)
        (while (<= loop ?9)
          (define-key map (char-to-string loop) 'digit-argument)
          (setq loop (1+ loop))))

      (define-key map (kbd "C-h") 'objed-describe-prefix-bindings)
      map))

(autoload 'dired-jump "dired-x" nil t)

(defun objed--backward-until (name)
  "Activate part from point backward until object NAME."
  (let* ((start (point))
         (o (objed--until name t)))
    (objed--switch-to
     name nil
     (objed-make-object
      :beg  (point)
      :end start
      :ibeg (objed--min o)
      :iend start))))

(defun objed--forward-until (name)
  "Activate part from point forward until object NAME."
  (let* ((start (point))
         (o (objed--until name)))
    (objed--switch-to
     name nil
     (nreverse
      (objed-make-object
       :ibeg start
       :iend (point)
       :beg start
       :end (objed--max o))))))

(defun objed--mark-all-inside (name)
  "Mark all objects of current type inside object NAME."
  (objed-unmark-all)
  (save-excursion
    (save-restriction
      ;; narrow to object we search for objects in
      (when (objed--save-state
             (when (and (objed--switch-to name)
                        ;; make sure we found one sourrounding point
                        (< (objed--min) (point) (objed--max)))
               (narrow-to-region (objed--min) (objed--max))
               (goto-char (point-min))))
        ;; objed-mark-object
        (let ((n (objed--do-all 'objed--mark-object)))
          (if (> n 1)
              n
            (prog1 nil
              (objed-unmark-all))))))))

(defun objed--ace-switch-object (name)
  "Switch to objed NAME using avy."
  (objed-ace name))

(defun objed--ace-switch-in-current (obj)
  "Ace for OBJ inside current object."
  (let ((reg (objed--current)))
    (apply #'objed-ace (cons obj reg))))

(defun objed--until (n &optional back)
  "Get object N and move point accordingly.

By default search object in forward direction. If BACK is non-nil
search backwards."
  (let* ((objed--object n)
         (objed--obj-state 'whole)
         (o (objed--get back)))
    (prog1 o
      (if (objed--distant-p o)
          (if back
              (goto-char (objed--end o))
            (goto-char (objed--min o)))
        (if back
            (goto-char (objed--alt-beg o))
          (goto-char (objed--alt-end o)))))))

(defun objed--toggle-ends (init)
  "Toggle point between ends of an object.

INIT should be a function which initializes the object. Its start
and end postion will be used for toggle."
  (cond ((eq last-command 'ignore)
         (let ((pos (objed--other)))
           (objed--update-current-object
            (objed--copy-object objed--look-around))
           (objed--change-to :iend pos :end pos)
           (goto-char (objed--beg)))
         ;; set it to same name(ignore by push-state)
         (setq this-command 'objed-look-around))
        ((eq last-command 'objed-look-around)
         (let ((pos (objed--other)))
           (objed--update-current-object
            (objed--copy-object objed--look-around))
           (objed--change-to :ibeg pos :beg pos)
           (goto-char (objed--end))
           (setq this-command 'ignore)))
        (t
         (when (save-excursion (funcall init))
           (setq objed--look-around (objed--copy-object))
           (objed--change-to :ibeg (point) :beg (point))
           (goto-char (objed--end))
           ;; dont save state of modified object
           (setq this-command 'ignore)))))



;; * Init active state

(defun objed--activate (cmd &rest _)
  "Activate `objed' with command CMD.

See `objed-cmd-alist'."
  (when (and objed-mode
             (funcall objed-init-p-function)
             (eq real-this-command cmd))
      (objed--init cmd)))

(defun objed--save-start-position (&rest _)
  "Save position of point via `objed--opoint'."
  (setq objed--opoint (point)))

(defun objed--goto-start (&optional _)
  "Goto start of current object if there is one."
  (when objed--current-obj
    (goto-char (objed--beg))))

(defun objed--object-until-bol (pos)
  "Activate leading part from POS."
  (unless (<= pos (objed--indentation-position))
    (objed--reverse))
  (goto-char (objed--beg))
  (objed--change-to :end pos :iend pos))

(defun objed--object-trailing-line (pos)
  "Activate trailing part from POS."
  (unless (eq objed--obj-state 'inner)
    (objed--reverse))
  (objed--change-to :beg pos :ibeg pos))

(defun objed--until-start (pos)
  "Activate from part from POS until start."
  (objed--change-to :end pos :iend pos))

(defun objed--until-end (pos)
  "Activate part from POS until end."
  ;; workaround: end-of-buffer behaves weird opoint is wrong use the
  ;; mark instead
  (if (eq this-command #'end-of-buffer)
      (objed--change-to :beg (mark) :ibeg (mark))
    (objed--change-to :beg pos :ibeg pos)))

(defun objed--insert-keys-rebound-p ()
  "Return non-nil when any self insertion key is rebound."
  (cl-dolist (char (string-to-list "abcdefghijklmnopqrstuvwxyz"))
    (let ((binding (key-binding (vector char))))
      (when (not (string-match "insert" (symbol-name binding)))
        (cl-return binding)))))

(defun objed-init-p ()
  "Default for `objed-init-p-function'."
  (and (not (minibufferp))
       (not (and (bobp) (eobp)))
       ;; don't interfere with other special modes
       ;; like hydra
       (not overriding-terminal-local-map)
       (not objed--block-p)
       (not objed-disabled-p)
       ;; don't activate when completing the regular Emacs way
       (not (get-buffer-window "*Completions*" 0))
       ;; don't activate during a company completion
       (not (bound-and-true-p company-candidates))
       ;; FIXME: temp workaround for starting commit
       ;; message in insertion mode
       (not (eq last-command 'magit-commit-create))
       ;; dont activate when insertion keys are bound to non insert commands
       (not (objed--insert-keys-rebound-p))
       (or (not objed-disabled-modes)
           (not (apply 'derived-mode-p objed-disabled-modes)))
       (or (memq  major-mode '(messages-buffer-mode help-mode))
           (not (derived-mode-p 'comint-mode 'special-mode 'dired-mode)))))


(defun objed-init (&optional obj fallback)
  "Function for activating objed by hooks.

Initialize with OBJ which defaults to `objed--object' which falls
back to char if unset. For meaning of FALLBACK see
`objed--init'."
  (when (funcall objed-init-p-function)
    (objed--init (or obj objed--object 'char)
                 fallback)))

(defun objed--init-later (&rest _)
  "Init after command loop returned to top level."
  (run-at-time 0 nil #'objed-init))

(defun objed--init-later-with (obj fallback)
  "Return a closure for later init.

The returned function can be used for command advices.
For the meaning of OBJ and FALLBACK see `objed-init'."
  (lambda (&rest _)
    (run-at-time 0 nil #'objed-init obj fallback)))

(defun objed--init (&optional sym fallback)
  "Initialize `objed'.

SYM is a symbol (command or object symbol) used to initialize
or object position data.

FALLBACK if given is a symbol defining the fallback object which
will be used if object is not found at current position.

By default if the object isn't found at point any next and after
that any previous instance of this object is used."
  ;; if anything went wrong make sure to start with clean state
  (when objed--buffer
    (objed--reset))

  ;; (setq objed--current-obj nil)
  ;; (setq objed--obj-state 'whole)
  (setq objed--buffer (current-buffer))
  (add-hook 'pre-command-hook 'objed--push-state nil t)
  (add-hook 'post-command-hook 'objed--check-buffer)
  ;; the user might not use `objed-mode' at all
  ;; so this hook might not be present already...
  (add-hook 'minibuffer-setup-hook 'objed--reset)

  (pcase-dolist
      (`(,var . ,val)
       `((hl-line-range-function . objed-hl-function)
         (suggest-key-bindings . nil)
         (which-key-sort-order . ,objed-which-key-order)
         (which-key-replacement-alist
          . ,(when (bound-and-true-p which-key-replacement-alist)
               (append objed--wk-replacement-alist
                       which-key-replacement-alist)))))
    (push
     (if (local-variable-p var)
         (cons var (symbol-value var))
       var)
     objed--saved-vars)
    (set (make-local-variable var) val))

  (when objed-use-hl
    (setq objed--hl-line-keep-p
          hl-line-mode)
    (unless objed--hl-cookie
      (setq objed--hl-cookie
            (face-remap-add-relative 'hl-line
                                     'objed-hl)))
    (hl-line-mode 1))

  ;; init cursor
  (setq objed--saved-cursor
        (or (frame-parameter nil 'cursor-color)
            (face-attribute 'cursor :background nil 'default)))
  (setq objed--saved-cursor-type (frame-parameter nil 'cursor-type))
  (setq-local cursor-type objed-cursor-type)
  (set-cursor-color objed-cursor-color)

  ;; init object
  (prog1 (cond ((commandp sym)
                ;; TODO: fallback here, too
                (objed--switch-to-object-for-cmd sym))
               ((symbolp sym)
                ;; for region object fallback to char
                ;; if there is no mark in the buffer
                (when (and (eq sym 'region)
                           (not (mark)))
                  (setq sym 'char))
                (if fallback
                    (let ((obatp (objed--inside-object-p sym)))
                      (if obatp
                          (objed--update-current-object obatp)
                        (objed--switch-to fallback)))
                  (objed--switch-to sym)))
               (t
                (unless objed--object
                  (setq objed--object 'char))
                ;; uses objed--object
                (objed--update-current-object sym)))

    ;; make sure the object is highlighted
    (hl-line-highlight)

    ;; transient map
    (fset #'objed--exit-objed
          (set-transient-map objed-map
                             #'objed--keep-transient-p
                             #'objed--reset))
    (setq objed-active-p t)
    (run-hooks 'objed-init-hook)))

(defun objed-init-mode-line ()
  "Init mode line."
  ;; FIXME: obsolete and remove the variable users should use init hook
  (when objed-modeline-hint
    (funcall objed-modeline-setup-func objed-mode-line-format)))

(defun objed-init-which-key ()
  "Show top level help."
  ;; FIXME: obsolete and remove the variable users should use init hook
  ;; show which key after redisplay if active
  (when objed-auto-wk-top-level
    (run-at-time 0 nil #'objed-show-top-level)))

(defun objed--setup-mode-line (format &optional reset)
  "Default function to setup the mode line hint.
FORMAT is used for `mode-line-format'.

Adds `objed-mode-line-format' at front. If RESET is non-nil the
mode line hint is removed again."
  (cond (reset
         (setq mode-line-format
              (delete format mode-line-format)))
        (t
         (push format mode-line-format))))


(defun objed--keep-transient-p ()
  "Return non-nil if `objed-map' should stay active.

Reinitializes the current object in case the current command is
one of `objed-keeper-commands'."
  (let ((ocmd (lookup-key objed-map (this-command-keys-vector))))
    (or (commandp ocmd)
        objed--with-allow-input
        (and this-command
             (or (memq this-command objed-keeper-commands)
                 (assq this-command objed-cmd-alist))
             (prog1 #'ignore
               (add-hook 'post-command-hook 'objed--reinit-object-one-time nil t))))))


(defun objed--reinit-object-one-time ()
  "To be used with `post-command-hook'.

Reinitializes current object and removes itself from the hook."
  (when (and objed--buffer
             this-command)
    (with-current-buffer objed--buffer
      (remove-hook 'post-command-hook 'objed--reinit-object-one-time t)
      (objed--switch-to-object-for-cmd this-command))))


(defun objed-hl-function ()
  "Function used as default for `hl-line-range-function'."
  (when (and objed--buffer objed--current-obj)
    (let ((curr (objed--current)))
      (cons (car curr) (cadr curr)))))


;; * Help commands


(defun objed--describe-bindings (map &optional key)
  "Decsribe bindings of keymap MAP which was activated with KEY."
  (with-temp-buffer
    (let ((key (or key
                   ;; for top level make one up
                   [modal]))
          (imap (make-sparse-keymap)))
      (define-key imap key map)
      (set-transient-map imap)
      (describe-bindings key (current-buffer))))
  (objed--exit-objed))


(defun objed--maybe-which-key (map desc &optional nowait iregex)
  "Show which key popup if user configured to use it.

MAP is the keymap which bindings should be displayed and DESC is
a string to show as the description of the popup.

Waits `which-key-idle-delay' before displaying the popup unless
NOWAIT is non-nil. IREGEX is a regular expressions of bindings to
ignore in `which-key' popup. Any binding whose description
matches IREGEX is not displayed."
  (when (and objed-use-which-key-if-available
             (require 'which-key nil t)
             (bound-and-true-p which-key-mode)
             (or nowait (sit-for which-key-idle-delay)))
    (prog1 t
      (setq which-key--using-top-level desc)
      (let ((which-key-replacement-alist
             (if iregex
                 (append `(((nil . ,iregex) . t))
                         which-key-replacement-alist)
               (append objed--wk-replacement-alist
                       which-key-replacement-alist))))
        (which-key--create-buffer-and-show nil map)))))

;; * Basic Movement, Block Objects (textblocks)

(defun objed--get-block-objects-for-context (&optional ignore)
  "Get list of objects for current context.

If IGNORE is non-nil it should be an object of
`objed--block-objects' which should be ignored."
  (let ((os (cond ((eq major-mode 'org-mode)
                   (let ((os nil))
                     ;; TODO: sort by object size?
                     (dolist (o objed--block-objects (nreverse os))
                       (unless (memq o '(indent textblock))
                         (push o os)))))
                  ((and (derived-mode-p 'prog-mode)
                        (or (objed--in-string-or-comment-p)
                            (objed--at-comment-p)))
                   objed--block-objects)
                  (t
                   ;; performance is poor on slow
                   ;; machines when searching after
                   ;; every char with objed--get in case
                   ;; there are no outlines/mode not active
                   (remq 'textblock objed--block-objects)))))
    (remq ignore
          (if (save-excursion
                (and (or (not (derived-mode-p 'text-mode))
                         (derived-mode-p 'sgml-mode))
                     (progn (unless (eolp) (objed--skip-ws))
                            ;; if line is a comment line search
                            ;; for textblocks (parag. inside comments)
                            (objed--in-comment-p))))
              (progn (unless (eolp) (objed--skip-ws))
                     (append (remq 'textblock  os)
                             (list 'comment 'textblock)))
            os))))

(defun objed--get-blocks (ignore collf &optional istate)
  "Get line based object states.

If IGNORE is non-nil it should be a symbol for a block object
which should be ignored. COLLF is a function which should be used
to collect positions used for sorting the results in ascending
order. ISTATE is the object state to use and defaults to whole."
  (objed--save-state
     (let ((os (objed--get-block-objects-for-context ignore))
           (states nil)
           (oos (list (objed--current)))
           (nos nil))
     (while os
       (when (and (ignore-errors
                      (objed--switch-to (car os) istate))
                  (not (member (objed--current) oos)))
         (push (objed--current) oos)
         (push (cons (funcall collf)
                     (objed--get-current-state))
               states))
       (pop os))
     ;; use start pos for sorting...
    (dolist (ps (sort states (lambda (a b)
                               (<= (car a) (car b))))
                nos)
      (push (cdr ps) nos)))))


;; * Context Objects (programming constructs)

(defun objed--get-context-state (from)
  "Get state to be used by expand commands.

FROM is a list of objects to use for guessing the object at point."
  (let (o)
    (objed--save-state
     (save-excursion
       (cond ((setq o (objed--at-p
                       (remq objed--object
                             ;; maybe at other at line beginning as well
                             ;; add section back here as well?
                             '(defun))))
              (objed--switch-to o))
             ((setq o (objed--at-p (remq objed--object from)))
              (objed--switch-to o))
             ((setq o (save-excursion
                        (when (<= (point) (objed--indentation-position))
                          (objed--skip-ws))
                        (or (objed--at-p (remq objed--object from))
                            ;; never test for struct inside, its to broad
                            (objed--in-p (remq objed--object from)))))
              (when (<= (point) (objed--indentation-position))
                (objed--skip-ws))
              (objed--switch-to o))
             (t
              (goto-char (objed--beg))
              (skip-chars-backward " \t")
              (when (setq o (or (objed--at-p objed--context-objects)
                                (objed--in-p objed--context-objects)))
                (let ((c (objed--current)))
                  (objed--switch-to o)
                  (when (equal (objed--current) c)
                    ;; FIX: more general
                    (forward-char -1)
                    (when (setq o (or (objed--at-p objed--context-objects)
                                      (objed--in-p objed--context-objects)))
                      (objed--switch-to o)))))))
       (when o (objed--get-current-state))))))


(defun objed-context-object ()
  "Guess and activate object at point.

Tries to guess the object at point and moves to start of it.

Repeated calls will continue guessing objects and try to expand
to an object containing the current one."
  ;; stop with defun for now
  (unless (eq objed--object 'defun)
    (let ((s nil))
      ;; TODO: make direction, position dependend and stay at beg/end?
      (if (setq s (objed--get-context-state objed--context-objects))
          (progn (objed--restore-state s)
                 (force-mode-line-update)
                 (goto-char (objed--beg)))
        ;; fallback if nothing else found
        (let ((fallback (objed--get-object 'defun 'whole)))
          (if (and fallback
                   (< (objed--alt-beg fallback)
                      (objed--beg)))
              (objed--switch-to 'defun 'inner)
            (or (objed--switch-to 'defun 'whole)
                (objed--switch-to 'line 'inner))))))))


(defun objed--sexp-fallback (&optional pos)
  "Return fallback object for sexp at POS."
  (let ((pos (or pos (point))))
    (goto-char pos)
    (or (objed--at-p '(bracket string tag))
        (and (or (not (= 0 (skip-syntax-forward "'")))
                 (not (= 0 (skip-syntax-backward "'"))))
             (objed--at-p '(bracket string)))
        (cond ((or (looking-at "\\<[a-z]")
                   (looking-back "[a-z]\\>" (1- (point))))
               'word)
              ((or (looking-at "\\_<[a-z]")
                   (looking-back "[a-z]\\_>" (1- (point))))
               'identifier)))))


(defun objed--maybe-switch-to-sexp-fallback (&optional pos)
  "Switch to sexp fallback at POS."
  (when (eq objed--object 'sexp)
    (let ((fallback (objed--sexp-fallback pos)))
      (when fallback
        (objed--switch-to fallback)))))

(defun objed--toggle-state ()
  "Toggle state of object."
  (objed--reverse))


(defun objed--get-ident-format ()
  "Get format string for identifier."
  (let ((sym (or (symbol-at-point)
                 (and (re-search-forward "\\_<" nil t)
                      (symbol-at-point)))))
    (when sym
      (format "\\_<%s\\_>" sym))))




;; * Region Commands as Operations

(defun objed--init-cmd-cache (sym)
  "Add SYM to `objed--cmd-cache' if it is a region command."
  (require 'help)
  ;; don't trigger autoloads
  (let ((spec (when (not (and (symbolp sym)
                              (autoloadp (indirect-function sym))))
                (cadr (interactive-form sym)))))
    (when (or (and spec (stringp spec)
                   (string-match "\\`\\*?r" spec))
              (and (commandp sym)
                   (string-match "\\`(\\(start\\|begi?n?\\) end"
                                 (format "%s" (help-function-arglist sym t)))))
      (push sym objed--cmd-cache))))

(defun objed--read-cmd (&optional force)
  "Completing read an operation from available region commands.

Uses `objed--cmd-cache' to save previous results. If FORCE is
non-nil, invalidate the cache.

Returns symbol of choosen region command."
  (objed--with-allow-input
   (when (or force (not objed--cmd-cache))
     (setq objed--cmd-cache nil)
     (mapatoms #'objed--init-cmd-cache))
   (intern (completing-read "Operation: " objed--cmd-cache))))


(defun objed--create-op (f arg)
  "Create an operation.

An operation is a function which acts on an object. It recieves
two arguments beeing the buffer positions of an object. Provided
function F is a command or a function from which to create the
operation function. ARG is the prefix argument to be used by the
operation."
  (if (commandp f)
      (objed--create-op-from-rcmd f arg)
    ;; try to pass arg
     (lambda (beg end)
       (condition-case nil
           (objed--with-allow-input
            (funcall f beg end arg))
         ((wrong-number-of-arguments)
          (when arg (message "Prefix arg ignored, not supported by operation."))
          (objed--with-allow-input
           (funcall f beg end)))))))


(defun objed--create-op-from-rcmd (rcmd &optional prefix)
  "Create an operation from a region command.

The returned function (operation) recieves two positions as
arguments and should have the same effect on the text between
those positions as the region command would have had on selected
text between those positions.

Any arguments of region command RCMD will be determined the same
way as if RCMD would be called interactively and thus might query
the user for input at first use.

Because any interactive input is captured before the operation is
used this might not work correctly when interactive input depends on
the positional arguments of the region command.

When PREFIX is given it will be used by RCMD as
`current-prefix-arg'."
  (let ((current-prefix-arg prefix)
        (handler (if (fboundp #'funcall-interactively)
                     #'funcall-interactively #'funcall)))
    (lambda (beg end)
      (save-mark-and-excursion
       ;; in case the region commands expects an active region...
       (goto-char beg)
       (push-mark end t t)
       (objed--with-allow-input
        (let ((this-command rcmd)
              (oargs (cddr
                      (advice-eval-interactive-spec
                       (cadr (interactive-form rcmd))))))
          (apply handler rcmd beg end oargs)))))))


;; * State Info

(defun objed--get-current-state ()
  "Return current state data."
  (list (current-buffer)
        (point) objed--object
        (objed--copy-object)
        objed--obj-state
        (and objed--marked-ovs
             (let ((os nil))
               (dolist (ov objed--marked-ovs (nreverse os))
                 (push (cons (overlay-start ov)
                             (overlay-end ov))
                       os))))))

(defun objed--push-state ()
  "When using objed commands, push state to `objed--last-states'.

To be used in `pre-command-hook'."
  (unless (eq real-this-command 'objed-last)
    (let ((curr (objed--get-current-state)))
      (unless (equal curr (car objed--last-states))
        (push (objed--get-current-state)
              objed--last-states)))))


(defun objed--restore-state (&optional state)
  "Restore state STATE.

STATE defaults to first of `objed--last-states'. Note that the
state is only restored correctly if the buffer was not modified."
  (if (and (or state (setq state (pop objed--last-states)))
           (eq (current-buffer) (pop state)))
      (let* ((pos (pop state))
             (obj (pop state))
             (range (pop state))
             (os (pop state))
             (ovps (pop state)))
        (prog1 (objed--switch-to obj os range)
          (goto-char pos)
          ;; FIXME
          (objed-unmark-all)
          (when ovps
            (objed--mark-ovps ovps))))
    (prog1 nil
      (when (and (called-interactively-p 'any)
                 (eq real-this-command 'objed-last))
        (message "No previous state to restore.")))))


;; * Operation definitions


(defun objed-electric-pair (beg end)
  "Wrap region between BEG, END.

If `current-prefix-arg' query for strings to wrap the region with
else query for key event and use `electric'."
  (if current-prefix-arg
      ;; TODO: offer default, omit <> in html
      (let ((left (read-string "Left side: "))
            (right (read-string "Right side: ")))
        (goto-char end)
        (insert right)
        (goto-char beg)
        (insert left))
  (let ((event (or objed--electric-event
                   (setq objed--electric-event
                         (read-event "Wrap with: ")))))
    (objed-electric-event beg end event))))


(defun objed--surround (beg end str)
  "Surround region BEG,END with string STR."
  (goto-char end)
  (insert str)
  (save-excursion
    (goto-char beg)
    (insert str)))

;; adapted from lispy
;; TODO: flash region
(defun objed--eval-func (beg end &optional replace)
  "Evaluate code between BEG and END.

If REPLACE is non-nil replace the region with the result."
  (if (and (= beg (point-min))
           (= end (point-max)))
      (ignore-errors
        (eval-region beg end) t)
    (let ((p (point))
          (eval-sexp-fu-flash-mode nil))
      (goto-char beg)
      (let ((e-sexp (read (current-buffer)))
            (regionp (/= (point) end))
            (str nil))
        (goto-char p)
        (when (and (consp e-sexp)
                   (not regionp))
          ;; treat variable and face definitions special so they can be
          ;; redefined; ignore reader macro if included in region
          (cond ((and (memq (car e-sexp) (list 'defvar 'defcustom 'defvar-local))
                      (consp (cdr e-sexp))
                      (boundp (cadr e-sexp)))
                 (set (cadr e-sexp) (eval (car (cddr e-sexp)) lexical-binding)))
                ((eq (car e-sexp) 'defface)
                 (elisp--eval-defun-1 (macroexpand e-sexp)))
                ((memq (car e-sexp) '(\, \,@))
                 (setq e-sexp (cadr e-sexp)))))
        (when (setq str (condition-case e
                            (if regionp
                                (progn (eval-region beg end t)
                                       t)
                              (prin1-to-string (eval e-sexp lexical-binding)))
                          (error (prog1 nil
                                   (message (error-message-string e))))))
          (if (not (stringp str))
              t
            (let ((message-log-max nil))
              (message str))
            (if (not replace)
                str
              (when str
                (prog1 str
                  (delete-region beg end)
                  (insert str))))))))))


(defun objed--duplicate-1 (f beg end &optional arg)
  "Duplicate and apply F an region between BEG and END.

Apply function F on region before duplicating it. ARG is passed
to F as third argument."
  (let* ((end (set-marker (make-marker) end))
         (reg (buffer-substring beg end)))
    (goto-char end)
    (unless (bolp)
      (newline-and-indent))
    (save-excursion
      (insert reg)
      (apply f (list beg end arg)))
    (skip-chars-forward " \t")))

(defun objed-duplicate-down (beg end &optional arg)
  "Duplicate region between BEG and END below.

If numeric ARG is given duplicate ARG times."
  (objed--duplicate-1 'ignore beg end arg))

(defun objed-comment-duplicate (beg end &optional arg)
  "Comment and duplicate region between BEG and END.

ARG has the same meaning as for `comment-region.'"
  (let ((comment-combine-change-calls nil))
    (objed--duplicate-1 #'comment-region beg end arg)))

(defun objed--replace-object ()
  "Replace current object(s) with string queried from user."
  (let* ((beg (objed--beg))
         (end (objed--end))
         (str (objed--with-allow-input
               (read-string "Replace with: "
                            nil nil (buffer-substring beg end))))
         (n (objed--do (apply-partially
                        #'objed--replace-region-with-string str)
                       this-command)))
    (message "Replaced %s objects." n)))


(defun objed--replace-region-with-string (str beg end)
  "Use string STR to replace region BEG, END."
  (save-excursion
    (goto-char beg)
    (search-forward (buffer-substring beg end))
    (replace-match str)))


;; * Ipipe
;; inspired by jq-mode

;; ** Minibuffer setup

(defun objed--ipipe-reset-timer ()
  "Reset timer used `objed--ipipe-timer'."
  (when objed--ipipe-timer
    (cancel-timer objed--ipipe-timer))
  (setq objed--ipipe-timer nil))

(defun objed--ipipe-schedule-timer (&rest _)
  "Schedule timer `objed--ipipe-timer'."
  (let ((mini (window-buffer (active-minibuffer-window))))
    (objed--ipipe-reset-timer)
    (setq objed--ipipe-timer
          (run-with-idle-timer
           objed--ipipe-schedule-time nil
           (lambda (buf)
             (with-current-buffer buf
               (objed--ipipe-update-display)))
           mini))))

(defun objed--ipipe-update-display ()
  "Update display.

 Assumes current buffer is minibuffer."
  (let ((cmd (minibuffer-contents))
        (res nil))
    ;; only update display if user provides new input
    (if (and (minibufferp)
             ;; make sure we are in an ipipe session
             (eq (current-local-map) objed-ipipe-minibuffer-map))
          (if (string= "" cmd)
              ;; after commit or delete update the overlay
              (overlay-put objed--ipipe-overlay 'after-string
                           objed--ipipe-region-string)
            ;; save last input and update if possible
            (setq objed--ipipe-last cmd)
            (if (setq res (objed--ipipe-to-string cmd objed--ipipe-region-string))
                (overlay-put objed--ipipe-overlay 'after-string res)
              (overlay-put objed--ipipe-overlay 'after-string
                           objed--ipipe-region-string)))
      (when (minibufferp)
        (remove-hook 'after-change-functions 'objed--ipipe-schedule-time t)))))


(defun objed--ipipe-parse (str)
  "Parse string STR.

Returns cons cell with cmd as car and possible arguemtns as cdr."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (re-search-forward "\\(.*?\\)\\( \\|\\'\\)" nil t)
      (cons (match-string 1)
            (buffer-substring (point) (line-end-position))))))


(defun objed--ipipe-to-string (cmdline str)
  "Pipe string STR through CMDLINE.

Return restult if any or nil."
  (let* ((parsed (objed--ipipe-parse cmdline))
         (cmd (car-safe parsed)))
    (cond ((and cmd (executable-find cmd))
           (with-temp-buffer
             (let ((exit (call-process-region
                          str nil
                          shell-file-name
                          nil t nil
                          shell-command-switch
                          cmdline)))
               (when (= 0 exit)
                 (buffer-string)))))
          ((or (and (intern-soft (concat cmd "-region"))
                    (setq cmd (concat cmd "-region")))
               (intern-soft cmd))
           (when (commandp (setq cmd (intern cmd)))
             (with-temp-buffer
               (insert str)
               (goto-char (point-min))
               (push-mark (point-max) t t)
               (when (ignore-errors (call-interactively cmd) t)
                 (buffer-string))))))))

(defun objed--forward-slurp-object (&optional arg)
  "Slurp the next sexp into the currently selected object.

With ARG, slurp all proceeding sexps into object."
  (let ((start (objed--obeg))
        (end (objed--oend)))
    (goto-char end)
    ;; Signal any errors that we might get first, before mucking with
    ;; the buffer's contents.
    (save-excursion (forward-sexp))
    (let ((close (char-before)))
      ;; Skip intervening whitespace if we're slurping into an empty
      ;; string.
      (if (and (= (+ start 2) end)
               (eq (save-excursion (objed--skip-ws) (point))
                   (save-excursion (forward-sexp) (backward-sexp) (point))))
          (delete-region (- (point) 1)
                         (save-excursion (objed--skip-ws) (point)))
        (delete-char -1))
      (condition-case nil
          (progn
            (forward-sexp)
            (if arg
                (while t (forward-sexp))))
        (scan-error nil))
      (insert close)
      (unless (eq 'string objed--object)
        (indent-region start (point))))))

(defun objed--forward-barf-object (&optional arg)
  "Barf the last sexp out of the currently selected object.

With ARG, barf all sexps out of current object to the right."
  (let ((end (objed--iend))
        (start (objed--ibeg)))
    (goto-char end)
    ;; Signal any errors that we might get first, before mucking
    ;; with the buffer's contents.
    (save-excursion
      (backward-sexp (and (numberp arg) arg))
      (unless (>= (point) start)
        (error "Not enough content for forward barfing out of string")))
    (let ((close (char-after)))
      (delete-char 1)
      (if (or (not arg) (numberp arg))
          (backward-sexp arg)
        (while (>= (point) start)
          (backward-sexp)))
      (objed--skip-ws t)
      (insert close))
    (if (eq (point)
            (save-excursion (forward-sexp) (backward-sexp) (point)))
        (insert ?\s))
    (unless (eq 'string objed--object)
      (indent-region (point) end))))

(defun objed--backward-slurp-object (&optional arg)
  "Slurp the preceding sexp into the currently selected object.

With ARG, slurp all preceding sexps into object."
  (let ((start (objed--obeg))
        (end (objed--oend)))
    (goto-char start)
    ;; Signal any errors that we might get first, before mucking with
    ;; the buffer's contents.
    (save-excursion (backward-sexp))
    (let ((open (char-after)))
      ;; Skip intervening whitespace if we're slurping into an empty
      ;; string.
      (if (and (= (+ start 2) end)
               (eq (save-excursion (objed--skip-ws t) (point))
                   (save-excursion (backward-sexp) (forward-sexp) (point))))
          (delete-region (save-excursion (objed--skip-ws t) (point))
                         (+ (point) 1))
        (delete-char 1))
      (condition-case nil
          (progn
            (backward-sexp)
            (if arg
                (while t (forward-sexp))))
        (scan-error nil))
      (insert open)
      (unless (eq 'string objed--object)
        (indent-region (point) end)))))

(defun objed--backward-barf-object (&optional arg)
  "Barf the first sexp out of the currently selected object.

With ARG, barf all sexps out of current object to the left."
  (let ((end (objed--iend))
        (start (objed--ibeg)))
    (goto-char start)
    ;; Signal any errors that we might get first, before mucking
    ;; with the buffer's contents.
    (save-excursion
      (forward-sexp (and (numberp arg) arg))
      (unless (<= (point) end)
        (error "Not enough content for backward barfing out of string")))
    (let ((open (char-before)))
      (delete-char -1)
      (if (or (not arg) (numberp arg))
          (forward-sexp arg)
        (while (<= (point) end)
          (forward-sexp)))
      (objed--skip-ws)
      (if (eq (point)
              (save-excursion (backward-sexp) (forward-sexp) (point)))
          (insert ?\s))
      (insert open))
    (unless (eq 'string objed--object)
      (indent-region start (point)))))


;; * Exit active state

(defun objed--line-p (text)
  "Determine how TEXT spans over lines.

Return non-nil if text spans over an entire single line or
multiple ones."
  (with-temp-buffer
    (insert text)
    (not (= -1  (forward-line -1)))))

(defun objed-exit-op (exit &optional text range)
  "Handle exit of an operation.

EXIT is the operation name or exit code. If TEXT is given it
carries the textual content of the object the operation acted on
and RANGE hold the object position data."
  ;; TODO: improve exit behaviour for default operations
  (let ((exitf (cdr (assq exit objed--exit-alist))))
    (cond ((eq 'keep exit)
           (ignore))
          ((eq 'mc exit)
           (when (require 'multiple-cursors nil t)
             (if (> (mc/num-cursors) 1)
                 (run-at-time 0 nil 'multiple-cursors-mode)
               (multiple-cursors-mode 0)))
           (objed--exit-objed))
          ((eq 'current exit)
           ;; use the markers for updated object
           (objed--update-current-object
            (objed-make-object :beg (car range)
                               :end (cadr range))))
          ((eq 'exit exit)
           (objed--exit-objed))
          ((functionp exitf)
           (funcall exitf text))
          ((eq 'current exitf)
           (objed--update-current-object
            (objed-make-object :beg (car range)
                               :end (cadr range))))
          ((eq 'exit exitf)
           (objed--exit-objed))
          (exitf
           (objed--switch-to exitf))
          ((or (eq exit 'ignore)
               (bound-and-true-p multiple-cursors-mode)))
          ((not objed--buffer)
           ;; FIXME: why stay map active after electric insertion?
           (objed--exit-objed)
           ;; let op exit itself if it wants to
           (ignore))
          (t
           (let ((co (and range (= (car range) (cadr range)) ; object vanished
                          (objed--get-continuation objed--object))))
             (if co
                 (apply #'objed--switch-to co)
               ;; stay active with most appr. obj
               ;; use a line when we acted on lines
               (if (and text (objed--line-p text))
                   (objed--switch-to 'line)
                 (objed--switch-to 'char))))))
    (when (and range
               (not (eq exitf 'current))
               (not (eq exit 'current))
               (not (eq exit 'keep)))
      (set-marker (car range) nil)
      (set-marker (cadr range) nil))))


(defun objed--get-continuation (obj)
  "Return continuation data for OBJ."
  (let ((shifted (memq 'shift (event-modifiers last-input-event)))
        (no (cond ((and (objed--inner-p)
                        ;; balanced objects
                        (memq objed--object '(string bracket defun)))
                   nil)
                  ((memq obj '(char word defun sentence line paragraph))
                   ;; keepers
                   objed--object)
                  ((memq obj (append objed--block-objects
                                     (list 'comment
                                           'region)))
                   ;; liners
                   'line)
                  (t
                   ;; sexp as default
                   'sexp))))
    (when no
      (let* ((objed--object no)
             (objd (if shifted (objed--get-prev)
                     (objed--get)))
             (end (and objd
                       (if shifted (objed--beg objd)
                         (objed--end objd)))))
        (when end
          (list no
                objed--obj-state
                (objed-make-object :beg (point)
                                   :end end)))))))

(defun objed--check-buffer ()
  "Check if current buffer is still the `objed--buffer'.

Reset and reinitilize objed if appropriate."
  (unless (or objed--with-allow-input
              (not objed--buffer))
    (when (not (eq (current-buffer) objed--buffer))
      (objed--reset--objed-buffer)
      (when (window-live-p (get-buffer-window (current-buffer)))
        (with-selected-window (get-buffer-window (current-buffer))
          (objed-init))))))

(defun objed--reset--objed-buffer ()
  "Reset `objed--buffer'."
  ;; things that need to be reset in objed buffer
  (when (buffer-live-p objed--buffer)
    (with-current-buffer objed--buffer
      ;; safety check
      ;; TODO: prevent this from happening
      (unless (and (markerp (objed--beg))
                   (eq (marker-buffer (objed--beg))
                       (current-buffer)))
        (setq objed--current-obj nil))
      ;; reset object as well?
      ;;(setq objed--object nil)
      (when objed--marked-ovs
        (dolist (ov objed--marked-ovs)
          (delete-overlay ov))
        (setq objed--marked-ovs nil))

      (when objed--hl-cookie
        (face-remap-remove-relative objed--hl-cookie)
        (setq objed--hl-cookie nil))

      (when objed-modeline-hint
        (funcall objed-modeline-setup-func objed-mode-line-format 'reset))

      (when (> (length objed--last-states) objed-states-max)
        (setq objed--last-states
              (cl-subseq objed--last-states 0 objed-states-max)))

      (unless objed--hl-line-keep-p
        (hl-line-mode -1))

      (while objed--saved-vars
        (let ((setting (pop objed--saved-vars)))
          (if (consp setting)
              (set (car setting) (cdr setting))
            (kill-local-variable setting))))
      (remove-hook 'pre-command-hook 'objed--push-state t)
      (setq objed-active-p nil)
      (run-hooks 'objed-exit-hook))))

(defun objed--reset ()
  "Reset variables and state information."
  (if (eq (cadr overriding-terminal-local-map)
          objed-map)
      (objed--exit-objed)
    (when objed--buffer
      (setq objed--opoint nil)
      (setq objed--electric-event nil)

      (when objed--saved-cursor
        (set-cursor-color objed--saved-cursor))
      (when objed--saved-cursor-type
        (setq-local cursor-type objed--saved-cursor-type))
      (objed--reset--objed-buffer)
      (remove-hook 'post-command-hook 'objed--check-buffer)
      (setq objed--block-p nil)
      (setq objed--buffer nil))))



;; * OP execution


(defun objed--do (action &optional exit)
  "Execute ACTION on current object(s).

Apply action to current object(s) and exit with EXIT which is the
symbol used for op exit and defaults to `this-command' (see
ON got applied."
  (let ((exit (or exit this-command)))
    (cond (objed--marked-ovs
           (objed--do-objects action exit))
          (t
           (objed--do-object action exit)))))

(defun objed--do-object (action exit)
  "Apply ACTION on current object and exit with EXIT."
  (let ((range (objed--current)))
    (when range
      (let* ((text (apply #'buffer-substring range))
             (range (list (set-marker (make-marker) (car range))
                          (set-marker (make-marker) (cadr range)))))
        (prog1 1
          ;; WHY: if passing the markers, prepend check
          ;; in kill-region fails.
          (funcall action
                   (marker-position (car range))
                   (marker-position (cadr range)))
          (objed-exit-op exit text range))))))

(defun objed--do-objects (action exit)
  "Apply ACTION on marked objects and exit with EXIT."
  (let* ((ovs (copy-sequence objed--marked-ovs))
         (appendp (memq action '(kill-region copy-region-as-kill)))
         (n 0)
         (mc (and (eq exit 'mc)
                  (require 'multiple-cursors nil t)))
         (last-command last-command)
         (pos (set-marker (make-marker) (overlay-start (car ovs)))))
    ;; move to last ov
    (goto-char pos)
    (save-excursion
      ;; Use the order they were "marked".
      (dolist (ov (nreverse ovs))
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (when (and beg end)
            (goto-char beg)
            (funcall action beg end)
            (when (and mc
                       (not (= pos (point))))
              (mc/create-fake-cursor-at-point))
            (cl-incf n))
          (when appendp
            (setq last-command 'kill-region))
          (delete-overlay ov))))
    (prog1 n
      ;; TODO: configure exit behavior for
      ;; multiple objects
      (setq objed--marked-ovs nil)
      (objed-exit-op exit))))


(defun objed--ov-sequence-p (ovs)
  "Return non-nil if OVS build a sequence.

OVS are overlays. If the overlays are only sepearated by
whitespace they build a sequence."
  (let ((posns nil))
    (dolist (ov ovs)
      (goto-char (overlay-end ov))
      (skip-chars-forward " \t\r\n")
      (push (point) posns))
    ;; skip last
    (pop posns)
    ;; get same order
    (setq posns (nreverse posns))
    ;; skip first
    (pop ovs)
    (not
     ;; empty if sequence...
     (cl-remove-if
      (lambda (ov)
        (= (overlay-start ov)
           (pop posns)))
      ovs))))

(defun objed--install-advices-for (cmds obj)
  "Given a list of commands CMDS install advices for OBJ.

See `objed-cmd-alist'."
  (let ((alist nil)
        (cmd nil))
    (while (setq cmd (pop cmds))
      (push (cons cmd obj) alist))
    (objed--install-advices alist)))

(defun objed--install-advices (alist &optional do-not-save)
    "Install advices according to ALIST.

If DO-NOT-SAVE is non-nil don't store ALIST entries in
`objed-cmd-alist'."
    (dolist (cmd2obj alist)
      (unless do-not-save (push cmd2obj objed-cmd-alist))
      (advice-add (car cmd2obj) :after
                  (apply-partially #'objed--activate (car cmd2obj)))
      (advice-add (car cmd2obj) :before 'objed--save-start-position)))

(defun objed--remove-advices (alist)
  "Remove advices accroding to ALIST.

See `objed-cmd-alist'."
  (dolist (cmd2obj alist)
    (advice-remove (car cmd2obj)
                   (apply-partially #'objed--activate (car cmd2obj)))
    (advice-remove (car cmd2obj) 'objed--save-start-position)))


(provide 'objed)
;;; objed.el ends here
