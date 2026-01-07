;;; org-clock-today.el --- Show total clocked time of the current day in the mode line -*- lexical-binding: t -*-

;; Copyright © 2016-2019 Tijs Mallaerts
;;
;; Author: Tijs Mallaerts <tijs.mallaerts@gmail.com>

;; Package-Requires: ((emacs "25"))
;; Version: 0.0.2
;; URL: https://github.com/mallt/org-clock-today-mode

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Show the total clocked time of the current day in the mode line

;;; Code:

(require 'org-clock)

(defgroup org-clock-today nil
  "Org clock today customizations."
  :group 'org-clock)

(defcustom org-clock-today-hide-default-org-clock-mode-line nil
  "Controls the visibility of the default org clock mode line string."
  :type 'boolean
  :group 'org-clock-today)

(defcustom org-clock-today-count-subtree nil
  "If non-nil, count total minutes of the current subtree as well."
  :type 'boolean
  :group 'org-clock-today)

(defcustom org-clock-today-show-mode-line-when-clock-is-inactive nil
  "If non-nil, show the mode line when no org clocks are active."
  :type 'boolean
  :group 'org-clock-today)

(defcustom org-clock-today-max-hours-per-day nil
  "Maximum hours to work per day.
If non-nil, display the remaining time until the configured maximum in the
mode-line. Value is the number of hours (may be integer or float). Set to
nil to disable. When the worked time exceeds the configured maximum, the
remaining time is shown as 0:00 (i.e. clamped to zero)."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Hours"))
  :group 'org-clock-today)

(defvar org-clock-today-string "" "The lighter.")
(defvar org-clock-today-subtree-time nil "Clock count extracted from subtree.")
(defvar org-clock-today-buffer-time nil "Clock count extracted from buffer.")
(defvar org-clock-today--timer nil)

(defun org-clock-today--total-minutes ()
  "Return the total minutes as a formatted duration string.
This keeps backward compatibility with the original implementation used
for mode-line display of subtree/buffer times." 
  (let ((org-clock-report-include-clocking-task t))
    (org-duration-from-minutes (org-clock-sum-today))))

(defun org-clock-today--total-minutes-number ()
  "Return the total minutes worked today as an integer.
This is used for numeric calculations such as remaining time until max."
  (let ((org-clock-report-include-clocking-task t))
    (org-clock-sum-today)))

(defun org-clock-today--display-default ()
  "Default function to return string for displaying clocks.
If `org-clock-today-max-hours-per-day' is set, append the remaining time
until the configured maximum in the form " (remaining: H:MM)". The
remaining time is clamped to zero when the worked time exceeds the max."
  (let* ((max-str
          (when org-clock-today-max-hours-per-day
            (let* ((max-minutes (round (* 60 org-clock-today-max-hours-per-day)))
                   (worked (org-clock-today--total-minutes-number))
                   (remaining (- max-minutes worked))
                   (remaining-clamped (max 0 remaining)))
              (org-duration-from-minutes remaining-clamped))))
    (concat
     " "
     (when org-clock-today-count-subtree
       (concat org-clock-today-subtree-time " "))
     org-clock-today-buffer-time
     (when max-str (concat " (remaining: " max-str ")")))))

(defcustom org-clock-today-display-format #'org-clock-today--display-default
  "Function to call when building string for mode-line."
  :type '(choice
          (const :tag "Do nothing" ignore)
          (function :tag "Custom function"))
  :group 'org-clock-today)

(defun org-clock-today--update-mode-line ()
  "Calculate the total clocked time of today and update the mode line."
  (setq org-clock-today-string
        (if (org-clock-is-active)
            (with-current-buffer (org-clock-is-active)
              (when org-clock-today-count-subtree
                (save-excursion
                  (save-restriction
                    (goto-char org-clock-marker)
                    (org-narrow-to-subtree)
                    (setq org-clock-today-subtree-time
                          (org-clock-today--total-minutes)))))
              (setq org-clock-today-buffer-time
                    (org-clock-today--total-minutes))
              (funcall org-clock-today-display-format))
          (if org-clock-today-show-mode-line-when-clock-is-inactive
              (concat (funcall org-clock-today-display-format) " (inact)")
            "")))
  (force-mode-line-update))

(defun org-clock-today--start-timer ()
  "Start the timer that will update the mode line every 60 seconds."
  (setq org-clock-today--timer
        (run-at-time 0 60 'org-clock-today--update-mode-line)))

(defun org-clock-today--stop-timer ()
  "Stop the timer."
  (org-clock-today--update-mode-line)
  (cancel-timer org-clock-today--timer))

(defun org-clock-today--maybe-clear-org-mode-line-string ()
  "Clear the org mode line string depending on the defcustom setting."
  (when org-clock-today-hide-default-org-clock-mode-line
    (setq org-mode-line-string "")
    (force-mode-line-update)))

(defun org-clock-today-toggle-count-subtree ()
  "Toggle count total minutes in subtree or buffer."
  (interactive)
  (setq org-clock-today-count-subtree (not org-clock-today-count-subtree))
  (unless org-clock-today-count-subtree
    (setq org-clock-today-subtree-time nil))
  (org-clock-today--update-mode-line))

;;;###autoload
(define-minor-mode org-clock-today-mode
  "Minor mode to show the total clocked time of the current day in the mode line."
  :lighter org-clock-today-string
  :global t
  (if org-clock-today-mode
      (progn
        (add-hook 'org-clock-in-hook 'org-clock-today--start-timer)
        (add-hook 'org-clock-out-hook 'org-clock-today--stop-timer)
        (advice-add 'org-clock-update-mode-line :after
                    'org-clock-today--maybe-clear-org-mode-line-string))
    (remove-hook 'org-clock-in-hook 'org-clock-today--start-timer)
    (remove-hook 'org-clock-out-hook 'org-clock-today--stop-timer)
    (advice-remove 'org-clock-update-mode-line
                   'org-clock-today--maybe-clear-org-mode-line-string)))

(provide 'org-clock-today)

;;; org-clock-today.el ends here
