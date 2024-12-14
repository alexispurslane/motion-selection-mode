;;; prometheus-mode.el --- A mode to add a text editing grammar to Emacs, built on top of god-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Alexis Purslane <alexispurslane@pm.me>

;; Author: Alexis Purslane <alexispurslane@pm.me>
;; URL: https://github.com/alexispurslane/prometheus-mode
;; Version: 0.1
;; Package-Requires: (god-mode whole-line-or-region (emacs "24.4"))

;; This file is not part of GNU Emacs

;;; Commentary:

;; Prometheus mode introduces a truly vanilla-Emacs friendly text
;; editing grammar, that works with modifier keys or using modal
;; editing via god mode, and which preserves all the same
;; keybindings and mnemonics, and all the same concepts and
;; behavior, as regular emacs text editing, while also giving you
;; the advantages of a full grammar for text editing.

;;; License:

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

(defvar prometheus--last-point-position nil)
(defvar prometheus--intentional-region-active nil)
(defvar prometheus--god-command-in-progress nil)
(defgroup prometheus nil
    "Customization group for `prometheus-mode'"
    :group 'editing
    :group 'god)
(defcustom prometheus-excluded-major-modes '(magit-mode dired-mode gnus-group-mode gnus-summary-mode)
    "The list of modes that auto-selection will not occur in, because
it doesn't make sense."
    :type 'listp
    :group 'prometheus)
(defcustom prometheus-auto-timer-time 0.6
    "The amount of idle time to wait until doing something, across
several `prometheus-mode' features, including
`isearch-forward-auto-timer' and backward timer,
auto-deselection, etc."
    :type 'numberp
    :group 'prometheus)
;;;###autoload
(define-minor-mode prometheus-mode
    "A modal editing minor mode for Emacs built on top of `god-mode'
that gives Emacs users a Vim-like text editing grammar by making
motion commands select the text objects that they step over, and
then treating region commands as general-purpose verbs to act on
text objects, not unlike the grammar of meow, but still using the
entirely Emacs-native set of commands, concepts, mnemonics, and
keymaps, requiring no extra integration with the rest of emacs at
all."
    :global t
    :lighter " Prometheus"
    :group 'prometheus
;;;;; God Mode Basics
    ;; Enable god mode globally
    (god-mode)
    
    ;; We want an in-line visual indication of whether we're
    ;; in god mode or not
    (defun prometheus-god-mode-update-cursor-type ()
        (setq cursor-type
              (cond
               ((or god-local-mode buffer-read-only) 'box)
               (overwrite-mode 'hollow)
               (t 'bar))))

    (add-hook 'post-command-hook #'prometheus-god-mode-update-cursor-type)
    
    ;; Kakoune-style prometheus--motion-selection
    (make-variable-buffer-local 'prometheus--last-point-position)
    (add-hook 'deactivate-mark-hook (lambda ()
                                        (setq prometheus--intentional-region-active nil)))
    (defun prometheus--deselect ()
        ;; if after the designated time has elapsed, the user
        ;; hasn't run any commands, and doesn't seem to be in
        ;; the process of running any commands, in what was
        ;; just selected, automatically deselect it; it's
        ;; likely that was just a trailing selection as a
        ;; result of moving through the buffer!
        (if (and god-local-mode
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
                     (length= this-command-keys 0)))
                ;; deselect the current region and update the internal selection variables
                (progn
                    (pop-mark)
                    (setq-local prometheus--last-point-position (point)))
            ;; come back later
            (run-with-idle-timer
             (* 1.5 prometheus-auto-timer-time) nil
             #'prometheus--deselect)))
    (defun prometheus--motion-selection ()
        (cond
         ((eq this-command 'set-mark-command)
          (setq prometheus--intentional-region-active t))
         ((and prometheus--last-point-position
               (not prometheus--intentional-region-active)
               (not (memq this-command '(forward-char
                                         backward-char
                                         self-insert-command
                                         eat-self-input
                                         next-line
                                         previous-line
                                         pixel-scroll-precision
                                         pop-global-mark
                                         expreg-expand
                                         expreg-contract
                                         undo)))
               (not (memq major-mode prometheus-excluded-major-modes))
               (not rectangle-mark-mode)
               (not isearch-mode))
          (let ((inhibit-message t))
              (run-with-idle-timer
               (* 1.5 prometheus-auto-timer-time) nil
               #'prometheus--deselect)
              ;; we set two marks here so that if there are
              ;; commands that take multiple regions, this will
              ;; count as the end of the previous region and the
              ;; beginning of the next, so that the text objects
              ;; we proceed over linearly can act as the
              ;; arguments to a command automatically.
              (push-mark prometheus--last-point-position)
              (push-mark prometheus--last-point-position nil t))))
        (setq-local prometheus--last-point-position (point)))
    (add-hook 'post-command-hook 'prometheus--motion-selection)
    ;; When we're overwriting text, we want to actually be
    ;; able to edit text automatically without interference
    (defun prometheus-god-mode-toggle-on-overwrite ()
        "Toggle god-mode on overwrite-mode."
        (if (bound-and-true-p overwrite-mode)
                (god-local-mode-pause)
            (god-local-mode-resume)))

    (add-hook 'overwrite-mode-hook #'prometheus-god-mode-toggle-on-overwrite)
    
    ;; Extend quake's version of `escape-dwim' to cancel god
    ;; mode just like it does evil mode, and quit
    ;; autocompletion as well since that's something it
    ;; doesn't take care of unlike evil mode's equivalent
    (advice-add 'escape-dwim :around (lambda (oldfun)
                                         (when (fboundp 'corfu-quit)
                                             (corfu-quit))
                                         (cond
                                          ((and (not evil-mode) (not god-local-mode))
                                           (god-local-mode 1)
                                           ;; reset
                                           ;; everything, so
                                           ;; we get
                                           ;; intuitive
                                           ;; behavior (no
                                           ;; selections
                                           ;; accidentally
                                           ;; automatically
                                           ;; started at
                                           ;; point when we
                                           ;; escape)
                                           (pop-mark)
                                           (setq prometheus--last-point-position nil)
                                           (setq prometheus--intentional-region-active nil))
                                          (overwrite-mode (overwrite-mode -1))
                                          (t (funcall oldfun)))))
    ;; next we need to provide a way to exit god mode!
    (define-key god-local-mode-map (kbd "i") #'god-local-mode)
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
    ;; if we pause in adding to our search term for more than
    ;; half a second, assume we're done searching and want to
    ;; now proceed through the search results; we can always
    ;; hit i again to change it
    (defun isearch-forward-auto-timer ()
        (interactive)
        (run-with-idle-timer prometheus-auto-timer-time nil
                             (lambda ()
                                 (message "Search finished, activating god search mode")
                                 (god-mode-isearch-activate)))
        (isearch-forward))
    (defun isearch-backward-auto-timer ()
        (interactive)
        (run-with-idle-timer prometheus-auto-timer-time nil
                             (lambda ()
                                 (message "Search finished, activating god search mode")
                                 (god-mode-isearch-activate)))
        (isearch-backward))
    ;; next we ensure that we can get out of isearch the way we get out of everything else
    (define-key god-mode-isearch-map (kbd "<escape>") #'escape-dwim)
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
    (define-key global-map (kbd "RET") (lambda () (interactive) (newline) (god-local-mode -1)))
    (define-key global-map (kbd "<backspace>") (lambda () (interactive) (delete-char -1) (god-local-mode -1)))
    (define-key global-map (kbd "C-r") #'isearch-backward-auto-timer)
    (define-key global-map (kbd "C-s") #'isearch-forward-auto-timer)
    ;; being able to apply a region command to a whole line by
    ;; default saves us from learning more commands, and saves
    ;; keystrokes
    (whole-line-or-region-global-mode))

(provide 'prometheus-mode)

;;; prometheus-mode.el ends here
