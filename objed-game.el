;;; objed-game.el --- Objed tutorial game -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Version: 0.8.4
;; File: objed-game.el
;; Keywords: games
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
;; URL: https://gitlab.com/tygrdev/key-game-example

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Train yourself to use Objed!

;;; Code:

;;;; Requirements

(require 'key-game)
(require 'objed)

;;;; key-game-objed

;;;###autoload (autoload 'objed-game "objed-game.el" "Objed tutorial game.")
(key-game objed-game
  "Objed tutorial game."
  :title "Objed Tutorial Game"
  :message "Welcome to the Objed game!"
  :setup (objed-local-mode)
  :mode lisp-interaction-mode
  objed-game-intro
  objed-game-level-1
  objed-game-level-2
  objed-game-level-3
  objed-game-level-4
  objed-game-level-4a
  objed-game-level-5
  objed-game-level-6
  objed-game-combined)

;;;; Intro

(key-game-level objed-game-intro
  "Level 0: Introduction"
  :next objed-game-level-1
  :intro objed-game-intro-frame)

(key-game-frame objed-game-intro-frame
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                           INTRODUCTION

**********************************************************************

Objed is a global minor-mode to navigate and edit text
objects. Objed enables modal editing and composition of commands,
too. It combines ideas of other Editors like Vim or Kakoune and
tries to align them with regular Emacs conventions.

For more information also see:

- My Blog: https://www.with-emacs.com/categories/objed/
- Project Readme: https://github.com/clemera/objed/blob/master/README.asc
- Project News: https://github.com/clemera/objed/blob/master/News.asc.

Text objects are textual patterns like a line, a top level
definition, a word, a sentence or any other unit of text. When
objed-mode or objed-local-mode is enabled, certain editing
commands (configurable) will activate objed and enable its modal
editing features. When active, keys which would usually insert a
character are mapped to objed commands. Other keys and commands
will continue to work as they normally would and exit this
editing state again.


>> Type \\[keyboard-quit] in order to activate objed, choosing
   the initial object based on `last-command' and
   `objed-cmd-alist'

>> Type \\[keyboard-quit] again to quit objed mode
"))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (goto-char (point-min)))

;;;; Level 1:

(key-game-level objed-game-level-1
  "Level 1: Entering Objed"
  :intro objed-game-level-1-intro
  :next objed-game-level-2
  objed-game-level-1-1
  objed-game-level-1-2
  objed-game-level-1-3
  objed-game-level-1-4
  objed-game-level-1-5
  objed-game-level-1-6
  objed-game-level-1-7)

(key-game-frame objed-game-level-1-intro
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                       ENTERING OBJED MODE

**********************************************************************

When you type \\[keyboard-quit], notice that objed shows the
current object type in the `mode-line'. The textual content of
the object is highlighted visually in the buffer and the cursor
color is changed, too.

\\[keyboard-quit] is not the only way to enter
`objed-mode'. Most other text movement commands also activate
objed with the relevant object type.

>> Type \\[forward-word] to move the cursor forward a single word
   and activate `objed-mode' with the `word' object type

>> Type \\[next-line] to move the cursor forward a single line
   and activate `objed-mode' with the `line' object type

>> Type \\[beginning-of-buffer] to move the cursor to the
   beginning of the buffer and activate `objed-mode' with the
   `buffer' object type.

>> Type \\[right-char] to move the cursor forward a single
   character and activate `objed-mode' with the `char' object
   type

>> Type \\[forward-sexp] to move the cursor forward to the next
   s-expression and activate `objed-mode' with the `sexp' object
   type

Once in `objed-mode', you can change the object type by using a
standard movement key.
"))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (goto-char (point-min)))

(key-game-frame objed-game-level-1-1
  :read-only t
  :commands (next-line)
  (insert "\n\n               \nSelect this line and enter objed mode.")
  (goto-char 18))

(key-game-frame objed-game-level-1-2
  :read-only t
  :commands (previous-line)
  (insert "\n\nSelect this line and enter objed mode.\n             "))

(key-game-frame objed-game-level-1-3
  :read-only t
  :commands (forward-word)
  (insert  "\n\nSelect the word \"cat\" and enter objed mode.")
  (goto-char 18))

(key-game-frame objed-game-level-1-4
  :read-only t
  :commands (backward-word)
  (insert "\n\nSelect the word \"dog\" and enter objed mode.")
  (goto-char 24))

(key-game-frame objed-game-level-1-5
  :read-only t
  :commands (right-char)
  (insert "\n\nSelect the character 'a' and enter objed mode.")
  (goto-char 24))

(key-game-frame objed-game-level-1-6
  :read-only t
  :commands (left-char)
  (insert "\n\nSelect the character 'b' and enter objed mode.")
  (goto-char 26))

(key-game-frame objed-game-level-1-7
  :read-only t
  :commands (forward-sexp)
  (insert "\n\nSelect the s-expression (emacs-init-time) and enter objed mode.")
  (goto-char 26))

;;;; Level 2:

(key-game-level objed-game-level-2
  "Level 2: Exiting objed"
  :intro objed-game-level-2-intro
  :next objed-game-level-3
  objed-game-level-2-1
  objed-game-level-2-2
  objed-game-level-2-3
  objed-game-level-2-4
  objed-game-level-2-5)

(key-game-frame objed-game-level-2-intro
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys (concat "\\<objed-mode-map>
**********************************************************************

                         EXITING OBJED

**********************************************************************

By default important editing keys like Space, DEL or Return are
not bound to modal commands and will execute the regular command
and exit objed. Character movement exits objed, as well. This
makes it convenient to move around and continue by
adding/deleting text.


You can also type " (propertize "g" 'font-lock-face 'help-key-binding) "
or \\[keyboard-quit] to exit `objed-mode'
")))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (goto-char (point-min)))

(key-game-frame objed-game-level-2-1
  :keys ("SPC")
  (insert "\n\nExit objed mode and add a space between applepear.")
  (goto-char 48)
  (objed-activate 'char))

(key-game-frame objed-game-level-2-2
  :commands (forward-char)
  :read-only t
  (insert "\n\nMove the cursor one character to the right and exit objed mode.")
  (goto-char 48)
  (objed-activate 'char))

(key-game-frame objed-game-level-2-3
  :commands (backward-char)
  :read-only t
  (insert "\n\nMove the cursor to the left one character and exit objed mode.")
  (goto-char 48)
  (objed-activate 'char))

(key-game-frame objed-game-level-2-4
  :keys ("<return>")
  (insert "\n\nCreate a new line at the end of this sentence and exit objed mode.\n")
  (goto-char 69)
  (objed-activate 'line))

(key-game-frame objed-game-level-2-5
  :read-only t
  :commands ((or objed-quit keyboard-quit))
  (insert "\n\nExit objed mode without moving the cursor.\n")
  (goto-char 30)
  (objed-activate 'line))


;;;; Level 3:

(key-game-level objed-game-level-3
  "Level 3: Object state"
  :intro objed-game-level-3-intro
  :next objed-game-level-4
  objed-game-level-3-1
  objed-game-level-3-2
  objed-game-level-3-3
  objed-game-level-3-4
  objed-game-level-3-5
  objed-game-level-3-6
  objed-game-level-3-7)

(key-game-frame objed-game-level-3-intro
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                           OBJECT STATE

**********************************************************************

In `objed-mode', the object state is either inner or whole and is
indicated in the modeline by (i) or (w) after the object
type. With inner state, anything that would be considered
delimiters or padding around an object is excluded.

While in `objed-mode', the following commands can be used to
modify the state of a selected object:

- \\<objed-map>\\[objed-expand-context] `objed-expand-context'

  Activate the inner part of the object at point and move to the
  start. This is useful to act on the content of the string,
  brackets and so on. On repeat expand to other objects around
  current position

- \\[objed-toggle-state] `objed-toggle-state'

  Toggle object state. Switches between inner and whole object
  state

- \\[objed-toggle-side] `objed-toggle-side'

  Move point to the other side of the current object
"))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (goto-char (point-min)))


(key-game-frame objed-game-level-3-1
  :read-only t
  :commands (forward-sexp objed-toggle-state)
  (insert "\n\nSelect the inner part of the expression (emacs-init-time)")
  (goto-char 42))

(key-game-frame objed-game-level-3-2
  :read-only t
  :commands (forward-word objed-toggle-side)
  (insert "\n\nSelect the word \"hippo\" then move the cursor to the other side.")
  (goto-char 18))

(key-game-frame objed-game-level-3-3
  :read-only t
  :commands (forward-sexp objed-toggle-state)
  (insert "\n\nSelect the inner part of the string in the following expression.

  (message \"The current major mode is %s\" major-mode)")
  (goto-char 79))

(key-game-frame objed-game-level-3-4
  :read-only t
  :commands ((or objed-toggle-state objed-expand-context))
  (insert "\n\nSelect the outer part of the string \"barbecue\".")
  (goto-char 44)
  (objed-activate 'string)
  (objed-toggle-state))

(key-game-frame objed-game-level-3-5
  :read-only t
  :commands (next-line objed-toggle-state)
  (insert "\n\n               \n    Select the inner part of this line.    ")
  (goto-char 18))

(key-game-frame objed-game-level-3-6
  :read-only t
  :commands (objed-toggle-side)
  (insert "\n\nMove the cursor to the opposite side of the defun.

  (defun my-icomplete-setup ()
    (setq-local completion-styles '(basic flex))) ")
  (goto-char 57)
  (objed-activate 'sexp))

(key-game-frame objed-game-level-3-7
  :read-only t
  :commands (objed-expand-context objed-expand-context)
  (insert "\n\nExpand the context of the selection to include the entire `add-hook'

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (emacs-init-time \"Emacs loaded in %.2fs\")))
          100) ")
  (goto-char 136)
  (objed-activate 'bracket))

;;;; Level 3:

(key-game-level objed-game-level-4
  "Level 4: Switch types"
  :intro objed-game-level-4-intro
  :next objed-game-level-4a
  objed-game-level-4-1
  objed-game-level-4-2
  objed-game-level-4-3
  objed-game-level-4-4
  objed-game-level-4-5
  objed-game-level-4-6)

(key-game-frame objed-game-level-4-intro
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys "\\<objed-mode-map>
**********************************************************************

                       SWITCH OBJECT TYPES

**********************************************************************

While in `objed-mode', you can use basic movement commands
without the modifier keys to change the object type and move the
cursor \\<objed-map>

- \\[objed-right-char]/\\[objed-left-char]
  Move forward/backward one char and activate the char object

- \\[objed-objed--forward-sexp]/\\[objed-objed--backward-sexp]
  Move forward/backward one sexp and activate the sexp object

- \\[objed-forward-word]/\\[objed-backward-word]
  Move forward/backward one word and activate the word object

- \\[objed-next-line]/\\[objed-previous-line]
  Move to the next/previous line and activate the line object

- \\[objed-forward-symbol]/\\[objed-previous-symbol]
  Move to the next/previous symbol object

- \\[objed-backward-paragraph]/\\[objed-forward-paragraph]
  Move forward/backward paragraph and switch to paragraph object
"))
  (let ((fill-column 70))
    (fill-region (point-min) (point-max)))
  (goto-char (point-min)))

(key-game-frame objed-game-level-4-1
  :read-only t
  :commands (objed-right-char)
  (insert "\n\nMove the cursor one character to the right without exiting objed mode.")
  (goto-char 30)
  (objed-activate 'word))

(key-game-frame objed-game-level-4-2
  :read-only t
  :commands (objed-left-char)
  (insert "\n\nMove the cursor one character to the left without exiting objed mode.")
  (goto-char 30)
  (objed-activate 'word))

(key-game-frame objed-game-level-4-3
  :read-only t
  :commands ((or objed-objed--forward-sexp objed-forward-symbol))
  (insert "\n\nSelect the symbol `major-mode' without exiting objed mode.

  (message \"The current major mode is %s\" major-mode)")
  (goto-char 104)
  (objed-activate 'sexp))

(key-game-frame objed-game-level-4-4
  :read-only t
  :commands (objed-objed--backward-sexp)
  (insert "\n\nSelect the string in the following expression without exiting
objed mode.

  (message \"The current major mode is %s\" major-mode)")
  (goto-char 120)
  (objed-activate 'sexp))

(key-game-frame objed-game-level-4-5
  :read-only t
  :commands (objed-forward-word)
  (insert "\n\nSelect the following word without exiting objed mode")
  (goto-char 28)
  (objed-activate 'line))

(key-game-frame objed-game-level-4-6
  :read-only t
  :commands (objed-backward-word)
  (insert "\n\nSelect the previous word without exiting objed mode")
  (goto-char 35)
  (objed-activate 'line))

;;;; Level 4a:

(key-game-level objed-game-level-4a
  "Level 4a: Switch types - Avy"
  :intro objed-game-level-4a-intro
  :next objed-game-level-5
  objed-game-level-4a-1
  objed-game-level-4a-2
  objed-game-level-4a-3
  objed-game-level-4a-4
  objed-game-level-4a-5
  objed-game-level-4a-6
  objed-game-level-4a-7)

(key-game-frame objed-game-level-4a-intro
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys (concat "\\<objed-mode-map>
**********************************************************************

                       SWITCH OBJECT TYPES (Avy)

**********************************************************************

Another way to switch object types is by using one of three
prefix keys, followed by the single letter identifier of the
object type to switch to. Here is a list of all the available
object types:

\\{objed-object-map}

The prefix keys are as follows

- " (propertize "o" 'font-lock-face 'help-key-binding) "

  Switch to the nearest instance of the type selected.

  For example, press " (propertize "o l" 'font-lock-face 'help-key-binding) "
  to switch to the line object type selecting the current line.

- " (propertize "#" 'font-lock-face 'help-key-binding) "

  Switch to another object using `avy'.

  This is similar to " (propertize "c" 'font-lock-face 'help-key-binding) "
  except instead of selecting the nearest textual object, it will
  use `avy' to select any object instance in the visible portion
  of the buffer.

- " (propertize "=" 'font-lock-face 'help-key-binding) "

  Switch to another object inside the current one using `avy'.

  This is similar to " (propertize "#" 'font-lock-face 'help-key-binding) "
  except instead of allowing you to select any object in the
  visible portion of the buffer, it will restrict `avy' to find
  instances within the currently selected object.
")))
  (let ((fill-column 70))
    (fill-region 1443 (point-max)))
  (goto-char (point-min)))

(key-game-frame objed-game-level-4a-1
  :read-only t
  :keys ("o m")
  (insert "\n\nSelect the nearest email object and activate objed.

not-an-email
not-an-email
not-an-email
test@mail.com
not-an-email
not-an-email
not-an-email")
  (goto-char (point-min))
  (objed-activate 'line))

(key-game-frame objed-game-level-4a-2
  :read-only t
  :keys ("o s")
  (insert "\n\nSelect the nearest \"string\" in this sentence.")
  (goto-char (point-min))
  (objed-activate 'char))

(key-game-frame objed-game-level-4a-3
  :read-only t
  :keys ("o u")
  (insert "\n\nJump to the nearest url in this buffer.\n\nhttps://google.com")
  (goto-char (point-min))
  (objed-activate 'char))

(key-game-frame objed-game-level-4a-4
  :read-only t
  :keys ("# w")
  :condition (= 129 (point))
  (insert "\n\nJump to the word \"secret\", using avy, in the following sentence:

Frank knew there was a correct time and place to reveal his
secret and this wasn't it.")
  (goto-char (point-min))
  (objed-activate 'line))

(key-game-frame objed-game-level-4a-5
  :read-only t
  :keys ("# l")
  :condition (= 342 (point))
  (insert "\n\nJump to the line containing the word \"pink\", using avy, in
 the following paragraph:

They had always called it the green river. It made sense. The
river was green. The river likely had a different official name,
but to everyone in town, it was and had always been the green
river. So it was with great surprise that on this day the green
river was a fluorescent pink.\n")
  (goto-char (point-min))
  (objed-activate 'word))

(key-game-frame objed-game-level-4a-6
  :read-only t
  :keys ("= w")
  :condition (= 117 (point))
  (insert "\n\nJump to the word \"brick\", using avy, within the currently selected line.

Today is the day I'll finally know what brick tastes like.
")
  (goto-char 83)
  (objed-activate 'line))

(key-game-frame objed-game-level-4a-7
  :read-only t
  :keys ("# d")
  :condition (= 134 (point))
  (insert "

Jump to the defun named \"green\" using avy.

(defun blue ()
  \"Make the cursor blue.\"
  (interactive)
  (set-cursor-color \"blue\"))

(defun green ()
  \"Make the cursor green.\"
  (interactive)
  (set-cursor-color \"green\"))

(defun gray ()
  \"Make the cursor gray.\"
  (interactive)
  (set-cursor-color \"gray\"))

(defun orange ()
  \"Make the cursor orange.\"
  (interactive)
  (set-cursor-color \"orange\"))

")
  (goto-char (point-min))
  (objed-activate 'word))

;;;; Level 5

(key-game-level objed-game-level-5
  "Level 5: Movement"
  :intro objed-game-level-5-intro
  :next objed-game-level-6
  objed-game-level-5-1
  objed-game-level-5-2
  objed-game-level-5-3
  objed-game-level-5-4
  objed-game-level-5-5)

(key-game-frame objed-game-level-5-intro
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys (concat "\\<objed-map>
**********************************************************************

                       MOVEMENT COMMANDS

**********************************************************************

Once you have an object type selected, you may use these commands
to move the cursor around a buffer.

- \\[objed-previous]​/\\[objed-next]
  Move to the start of previous/next instance of current object
  type

- \\[objed-top-object]/\\[objed-bottom-object]
  Goto first/last instance of current object type

- \\[objed-last] `objed-last'
  Pop to last state, which restores the last position and any
  object data.")))
  (let ((fill-column 70))
    (fill-region 1443 (point-max)))
  (goto-char (point-min)))


(key-game-frame objed-game-level-5-1
  :read-only t
  :commands (objed-bottom-object)
  (insert "\n\nNavigate to the last email address in the list.

not-an-email
test@test.com
mail@gmail.com
not-an-email
mail@yahoo.com
test@gmail.com
not-an-email")
  (goto-char 80)
  (objed-activate 'email))


(key-game-frame objed-game-level-5-2
  :read-only t
  :commands (objed-previous objed-previous)
  (insert "\n\nNavigate two email addresses up in the list.

not-an-email
test@test.com
mail@gmail.com
not-an-email
mail@yahoo.com
test@gmail.com")
  (objed-activate 'email))

(key-game-frame objed-game-level-5-3
  :read-only t
  :mode fundamental-mode
  :keys ("o =" "]")
  (insert "\n\nSelect the nearest " (propertize "face then move" 'face 'error)
          " to the next instance of that face.\n\n\n"
          (propertize "Example warning" 'face 'warning)
          "\n\n"
          (propertize "Example error" 'face 'error)
          "\n\n"
          (propertize "Example emphasis" 'face 'bold-italic))
  (goto-char 24)
  (objed-activate 'char))

(key-game-frame objed-game-level-5-4
  :read-only t
  :commands (next-line objed-last)
  (insert "\n\nMove forward one line then move back to the last selected object.\n\n")
  (goto-char 15)
  (objed-activate 'word))

(key-game-frame objed-game-level-5-5
  :read-only t
  :commands (objed-top-object)
  (insert "\n\nNavigate to the first email address in the list.

not-an-email
test@test.com
mail@gmail.com
not-an-email
mail@yahoo.com
test@gmail.com")
  (objed-activate 'email))

;;;; Level 6:

(key-game-level objed-game-level-6
  "Level 6: Moving objects"
  :intro objed-game-level-6-intro)

(key-game-frame objed-game-level-6-intro
  :read-only t
  :mode fundamental-mode
  (insert (substitute-command-keys (concat "\\<objed-map>
**********************************************************************

                           MOVING OBJECTS

**********************************************************************

* INDENTING
-----------


- \\[objed-indent-left]/\\[objed-indent-right]
  Move/indent all lines in object right/leftward

- \\[objed-indent-to-left-tab-stop]/\\[objed-indent-to-right-tab-stop]
  Move/indent all lines in object to right/leftward to tab stop.

* MOVING
--------

The currently selected textual object can be moved forward or
backward with the following commands.

- " (propertize "S-left" 'font-lock-face 'help-key-binding) "/" (propertize "S-right" 'font-lock-face 'help-key-binding) "/" (propertize "S-up" 'font-lock-face 'help-key-binding) "/" (propertize "S-down" 'font-lock-face 'help-key-binding) "/\\[objed-move-object-forward]/\\[objed-move-object-backward]
  Move current object type forward/backward

- \\[objed-move-char-forward]/\\[objed-move-char-backward]
  Switch to char object and move it forward/backward

- \\[objed-move-word-forward]/\\[objed-move-word-backward]
  Switch to word object and move it forward/backward

- \\[objed-move-line-forward]/\\[objed-move-line-backward]
  Switch to line object and move it forward/backward

* BARF/SLURP

Barf and slurp are inverse operations which are commonly used in
lisp programs.

- \\[objed-forward-barf-sexp]/\\[objed-forward-slurp-sexp]
  Slurp following sexp into current object/Barf last sexp out of
  current object.

")))
  (let ((fill-column 70))
    (fill-region 1443 (point-max)))
  (goto-char (point-min)))

;;;; Combined:

(key-game-level objed-game-combined
  "Combined: All levels"
  objed-game-level-1-1
  objed-game-level-1-2
  objed-game-level-1-3
  objed-game-level-1-4
  objed-game-level-1-5
  objed-game-level-1-6
  objed-game-level-1-7

  objed-game-level-2-1
  objed-game-level-2-2
  objed-game-level-2-3
  objed-game-level-2-4
  objed-game-level-2-5

  objed-game-level-3-1
  objed-game-level-3-2
  objed-game-level-3-3
  objed-game-level-3-4
  objed-game-level-3-5
  objed-game-level-3-6
  objed-game-level-3-7

  objed-game-level-4-1
  objed-game-level-4-2
  objed-game-level-4-3
  objed-game-level-4-4
  objed-game-level-4-5
  objed-game-level-4-6

  objed-game-level-4a-1
  objed-game-level-4a-2
  objed-game-level-4a-4
  objed-game-level-4a-5
  objed-game-level-4a-6
  objed-game-level-4a-7

  objed-game-level-5-1
  objed-game-level-5-2
  objed-game-level-5-3
  objed-game-level-5-4
  objed-game-level-5-5)

(provide 'objed-game)

;;; objed-game.el ends here
