;;; motion-selection-mode.el --- A minor mode to add a text editing grammar -*- lexical-binding: t -*-

;; Author: Alexis Purslane <alexispurslane@pm.me>
;; URL: https://github.com/alexispurslane/motion-selection-mode
;; Package-Requires: ((emacs "29.3") (god-mode))
;; Version: 0.1
;; Keywords: tools

;; This file is not part of GNU Emacs

;;; Commentary:

;; Motion-Selection mode introduces a truly vanilla-Emacs friendly text
;; editing grammar, that works with modifier keys or using modal
;; editing via god mode, and which preserves all the same
;; keybindings and mnemonics, and all the same concepts and
;; behavior, as regular Emacs text editing, while also giving you
;; the advantages of a full grammar for text editing.

;;; License:

;; Copyright (C) 2024-2025 Alexis Purslane <alexispurslane@pm.me>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:
(require 'god-mode)

(defvar-local motion-selection--last-point-position nil)
(defvar motion-selection--intentional-region-active nil)
(defvar motion-selection--god-command-in-progress nil)
(defgroup motion-selection nil
    "Customization group for `motion-selection-mode'."
    :group 'editing
    :group 'god)
(defcustom motion-selection-excluded-major-modes '(dired-mode)
    "The list of modes that auto-selection will not occur in."
    :type 'listp
    :group 'motion-selection)
(defcustom motion-selection-excluded-commands '(forward-char
                                                backward-char
                                                xref-find-apropos
                                                xref-find-references
                                                embark-find-definition
                                                elisp-def
                                                eglot-find-declaration
                                                eglot-find-implementation
                                                eglot-find-typeDefinition
                                                isearch-repeat-forward
                                                isearch-repeat-backward
                                                search-forward
                                                next-error
                                                previous-error
                                                search-backward
                                                self-insert-command
                                                eat-self-input
                                                backward-up-list
                                                down-list
                                                puni-up-list
                                                next-line
                                                previous-line
                                                pixel-scroll-precision
                                                pop-global-mark
                                                expreg-expand
                                                expand-region
                                                contract-region
                                                puni-expand-region
                                                puni-contract-region
                                                expreg-contract
                                                undo
                                                mark-sexp
                                                mark-defun
                                                mark-word
                                                mark-paragraph
                                                mark-page
                                                mark-end-of-sentence
                                                eshell-self-insert-command)
    "The list of commands that should not trigger automatic selection.

There's that much of a perfect pattern to which things belong
here and which don't, it's just about what seems
intuitive/useful. Generally, commands that you generally use just
to move around a buffer, or which simply accidentally move you
around a buffer as a side effect, shouldn't select things, but
commands that correspond more directly to text objects you might
want to actually manipulation should select things."
    :type 'listp
    :group 'motion-selection)
(defcustom motion-selection-auto-timer-time 0.6
    "The amount of idle time to wait until doing something.

Influences several things, including several `motion-selection-mode'
features, including `isearch-forward-auto-timer' and backward
timer, auto-deselection, etc."
    :type 'numberp
    :group 'motion-selection)

(defun motion-selection-god-mode-update-cursor-type ()
    (setq cursor-type
          (cond
           ((or god-local-mode buffer-read-only) 'box)
           (overwrite-mode 'hollow)
           (t 'bar))))

(defun motion-selection-god-mode-toggle-on-overwrite ()
    "Toggle god-mode on overwrite-mode."
    (if (bound-and-true-p overwrite-mode)
            (god-local-mode-pause)
        (god-local-mode-resume)))

;; if we pause in adding to our search term for more than
;; half a second, assume we're done searching and want to
;; now proceed through the search results; we can always
;; hit i again to change it
(defun motion-selection-isearch-auto-timer (fun)
    "Runs isearch and activates god mode after a pause in typing.

This allows you to use isearch more efficiently and effectively
for avy-timer-like navigation without having to reach for the
control keys. It feels almost telepathic to me."
    (lambda ()
        (interactive)
        (run-with-idle-timer motion-selection-auto-timer-time nil
                             (lambda ()
                                 (message "Search finished, activating god search mode")
                                 (when isearch-mode
                                     (god-mode-isearch-activate))))
        (funcall fun)))

(defun motion-selection--deselect ()
    "Deselect the current selection if the user does not appear to be
in the process of running a command on it."
    (interactive)
    (when (or executing-kbd-macro
              (and god-local-mode
                   ;; if the minibuffer is open and focused,
                   ;; then the user is probably in the middle
                   ;; of running a command by name on the
                   ;; current selection, so don't deselect
                   (not (minibuffer-window-active-p (selected-window)))
                   ;; if the which key buffer is open the user
                   ;; is probably trying to figure out what to
                   ;; do/enter, so don't deselect what they
                   ;; selected yet
                   (not (and (boundp 'which-key--buffer)
                             (get-buffer-window which-key--buffer)))
                   ;; if god-mode-self-insert is running right
                   ;; now, that means the user is in the middle
                   ;; of entering a god mode command on this
                   ;; region; don't deselect
                   (not (and (bound-and-true-p god-local-mode)
                             (eq this-command 'god-mode-self-insert)))
                   ;; copied from which-key's code to detect
                   ;; the current prefix key if the user has
                   ;; entered one or more prefix keys they're
                   ;; in the middle of executing a non-god-mode
                   ;; command on the region, also don't
                   ;; deselect
                   (let ((this-command-keys (this-single-command-keys)))
                       (when (and (vectorp this-command-keys)
                                  (> (length this-command-keys) 0)
                                  (eq (aref this-command-keys 0) 'key-chord)
                                  (bound-and-true-p key-chord-mode))
                           (setq this-command-keys (this-single-command-raw-keys)))
                       (length= this-command-keys 0))))
        ;; deselect the current region and update the internal selection variables
        (pop-mark)
        (setq motion-selection--last-point-position (point))))

(defun motion-selection--get-parent (mode)
    "Get the ultimate `derived-mode-parent' of MODE."
    ;; this could easily be recursive but elisp doesn't have TCO
    (let ((last-mode nil))
        (while mode
            (setq last-mode mode)
            (setq mode (get mode 'derived-mode-parent)))
        last-mode))
(defun motion-selection--motion-selection ()
    "Meant to be run in `post-command-hook'.

If the last command moved the point, and the user has not
intentionally set down a mark themself, and we are not in one of
the excluded modes, and the command was not one of the excluded
commands, put the mark where the point was before this command
ran, so that we effectively select the text objects the user
moves over, and start an idle timer to automatically deselect the
selection we made if the user doesn't appear to want to run any
commands on this selection."
    (cond
     ((eq this-command 'set-mark-command)
      (setq motion-selection--intentional-region-active t))
     ((and motion-selection--last-point-position
           (not motion-selection--intentional-region-active)
           (> (abs (- motion-selection--last-point-position (point))) 1)
           (not (memq this-command motion-selection-excluded-commands))
           (not (memq major-mode motion-selection-excluded-major-modes))
           (not (and (boundp 'rectangle-mark-mode) rectangle-mark-mode))
           (not isearch-mode)
           (or (not (eq (motion-selection--get-parent major-mode) 'special-mode))
               (not buffer-read-only)))
      (let ((inhibit-message t))
          (unless executing-kbd-macro
              (run-with-idle-timer
               motion-selection-auto-timer-time nil
               (lambda ()
                   (if defining-kbd-macro
                           ;; FIXME [#A]: Do something so that this timer being run is recorded by the kmacro in progress
                           (setq unread-command-events (listify-key-sequence (kbd "C-S-D")))
                       (motion-selection--deselect)))))
          ;; we set two marks here so that if there are
          ;; commands that take multiple regions, this will
          ;; count as the end of the previous region and the
          ;; beginning of the next, so that the text objects
          ;; we proceed over linearly can act as the
          ;; arguments to a command automatically.
          (push-mark motion-selection--last-point-position)
          (push-mark motion-selection--last-point-position nil t))))
    (setq motion-selection--last-point-position (point)))

(defun motion-selection--escape-dwim ()
    "Kill, exit, escape, stop, everything, now, and put me back in the
current buffer in God Mode."
    (interactive)
    (when completion-in-region-mode       (corfu-quit))
    (when overwrite-mode                  (overwrite-mode -1))
    (cond ((region-active-p)              (deactivate-mark))
          ((not god-local-mode)           (god-mode-all 1))
          (isearch-mode                   (isearch-exit))
          ((eq last-command 'mode-exited) nil)
          ((> (minibuffer-depth) 0)       (abort-recursive-edit))
          (current-prefix-arg             nil)
          ((> (recursion-depth) 0)        (exit-recursive-edit))
          (buffer-quit-function           (funcall buffer-quit-function))
          ((string-match "^ \\*" (buffer-name (current-buffer)))
           (bury-buffer))))

(defun motion-selection-single-shot-char (char)
    "Reads a single CHAR from the minibuffer and executes it in the current
buffer exactly as if you'd typed it with god mode off, including running
any commands it may have triggered. Useful with, e.g.,
`org-use-speed-commands'."
    (interactive (list (read-char-from-minibuffer "Perform char: ")))
    (god-local-mode -1)
    (execute-kbd-macro (vector char))
    (god-local-mode 1))

;;;###autoload
(define-minor-mode motion-selection-mode
    "A modal editing minor mode for Emacs built on top of `god-mode'
that gives Emacs users a Vim-like text editing grammar by making
motion commands select the text objects that they step over, and
then treating region commands as general-purpose verbs to act on
;; text objects, not unlike the grammar of meow, but still using the
entirely Emacs-native set of commands, concepts, mnemonics, and
keymaps, requiring no extra integration with the rest of emacs at
all."
    :global t
    :lighter " Motion-Selection"
    :group 'motion-selection
;;;;; God Mode Basics
    (setq god-exempt-major-modes nil)
    (setq god-exempt-predicates nil)
    ;; Enable god mode globally
    (god-mode)

    ;; We want an in-line visual indication of whether we're in god
    ;; mode or not
    (add-hook 'post-command-hook #'motion-selection-god-mode-update-cursor-type)

    ;; Kakoune-style motion-selection--motion-selection
    (add-hook 'deactivate-mark-hook (lambda ()
                                        (setq motion-selection--intentional-region-active nil)))

    (add-hook 'post-command-hook 'motion-selection--motion-selection)

    ;; When we're overwriting text, we want to actually be
    ;; able to edit text automatically without interference
    (add-hook 'overwrite-mode-hook #'motion-selection-god-mode-toggle-on-overwrite)

    (global-set-key (kbd "<escape>") 'motion-selection--escape-dwim)
    ;; next we need to provide a way to exit god mode! We use
    ;; god-mode-all with exemption off to make it easier to keep
    ;; track of when it's on and off.
    (define-key god-local-mode-map (kbd "i") #'god-mode-all)
    (define-key god-local-mode-map (kbd "I") (lambda ()
                                                 (interactive)
                                                 (deactivate-mark)
                                                 (god-mode-all -1)))
    ;; isearch is a crucial movement primitive in emacs,
    ;; equivalent to `f' as well as `/' in evil, and possibly
    ;; even superior to avy as well
    ;; (http://xahlee.info/emacs/misc/ace_jump_avy_vs_isearch.html)
    ;; according to some user interface theories, so we need
    ;; to configure it well, so it's fast and convenient to
    ;; use
;;;;; ISearch
    ;; first, we ensure that we can use god mode with it, so
    ;; we can use it without modifier keys
    (require 'god-mode-isearch)
    (define-key god-mode-isearch-map (kbd "i") #'god-mode-isearch-disable)
    (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
    (add-hook 'isearch-mode-hook #'god-mode-isearch-disable)

    ;; next we ensure that we can get out of isearch the way we get out of everything else
    (define-key god-mode-isearch-map (kbd "<escape>") 'motion-selection--escape-dwim)
    ;; next, we extend the flexibility of traditional
    ;; non-regexp isearch so that things can be reached more
    ;; easily with it
    (setq search-whitespace-regexp ".*?")
    (setq isearch-lax-whitespace t)
    ;; normal sentence behavior please, emacs
    (setq sentence-end-double-space nil)
    ;; The ability to immediately hit `i' and start replacing
    ;; marked text means that you can replicate vim's `c'
    ;; command trivially, which only exists in vim because
    ;; you can't go from visual mode to insert mode without
    ;; losing your selection
    (delete-selection-mode t)
    ;; Emacs provides a lot of powerful movement and text
    ;; object commands, equivalent to vim probably, or
    ;; roughly thereto, but many of them don't have
    ;; bindings. Let's fix that
    (define-key global-map (kbd "C-r") (motion-selection-isearch-auto-timer #'isearch-backward))
    (define-key global-map (kbd "C-s") (motion-selection-isearch-auto-timer #'isearch-forward))
    (define-key global-map (kbd "C-S-Q") 'motion-selection-single-shot-char)
    (define-key global-map (kbd "C-S-D") 'motion-selection--deselect)
    (advice-add 'org-speed-move-safe :before (defun motion-selection--org-speed-move-safe (&rest r) "Don't select anything when we're moving using org speed mode" (motion-selection-mode -1)))
    (advice-add 'org-speed-move-safe :after (defun motion-selection--org-speed-move-safe-undo (&rest r) "Turn selection on after." (motion-selection-mode 1)))

    (when (not repeat-mode)
        (repeat-mode 1)))

(provide 'motion-selection-mode)

;;; motion-selection-mode.el ends here
