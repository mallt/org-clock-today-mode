;;; org-clock-today.el --- Minor mode to show the total clocked time of the current day in the mode line -*- lexical-binding: t -*-

;; Copyright © 2016 Tijs Mallaerts
;;
;; Author: Tijs Mallaerts <tijs.mallaerts@gmail.com>

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

;; Minor mode to show the total clocked time of the current day in the mode line

;;; Code:

(require 'org-clock)

(defgroup org-clock-today nil
  "Org clock today customizations."
  :group 'convenience)

(defcustom org-clock-today-hide-default-org-clock-mode-line nil
  "Controls the visibility of the default org clock mode line string."
  :type 'boolean
  :group 'org-clock-today)

(defvar org-clock-today-string "")
(defvar org-clock-today-timer nil)

(defun update-org-clock-today-string ()
  "Calculate the total clocked time of today and update the mode line."
  (interactive)
  (setq org-clock-today-string
        (if (org-clock-is-active)
            (with-current-buffer (org-clock-is-active)
              (let* ((current-sum (org-clock-sum-today))
                     (open-time-difference (time-subtract
                                            (float-time)
                                            (float-time org-clock-start-time)))
                     (open-seconds (time-to-seconds open-time-difference))
                     (open-minutes (/ open-seconds 60))
                     (total-minutes (+ current-sum
                                       open-minutes)))
                (concat " " (org-minutes-to-clocksum-string total-minutes))))
          ""))
  (force-mode-line-update))

(defun start-org-clock-today-timer ()
  "Start the timer that will update the mode line every 60 seconds."
  (interactive)
  (setq org-clock-today-timer
        (run-at-time 0 60 'update-org-clock-today-string)))

(defun stop-org-clock-today-timer ()
  "Stop the timer."
  (interactive)
  (update-org-clock-today-string)
  (cancel-timer org-clock-today-timer))

(defun org-clock-today-maybe-clear-org-mode-line-string ()
  "Clear the org mode line string depending on the defcustom setting."
  (when org-clock-today-hide-default-org-clock-mode-line
    (setq org-mode-line-string "")
    (force-mode-line-update)))

;;;###autoload
(define-minor-mode org-clock-today-mode
  "Minor mode to show the total clocked time of the current day in the mode line."
  :lighter org-clock-today-string
  :global t
  (if org-clock-today-mode
      (progn
        (add-hook 'org-clock-in-hook 'start-org-clock-today-timer)
        (add-hook 'org-clock-out-hook 'stop-org-clock-today-timer)
        (advice-add 'org-clock-update-mode-line :after
                    'org-clock-today-maybe-clear-org-mode-line-string))
    (remove-hook 'org-clock-in-hook 'start-org-clock-today-timer)
    (remove-hook 'org-clock-out-hook 'stop-org-clock-today-timer)
    (advice-remove 'org-clock-update-mode-line
                   'org-clock-today-maybe-clear-org-mode-line-string)))

;;; org-clock-today.el ends here
