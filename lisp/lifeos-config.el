;;; lifeos-config.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author:  <lab-user@Genesis>
;; Maintainer:  <lab-user@Genesis>
;; Created: July 20, 2025
;; Modified: July 20, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/lab-user/lifeos-config
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(after! hydra

  ;; 1) A var to hold the state between hydras
  (defvar lifeos--hydra-transcient-state ""
    "Name of the state chosen by the first hydra.")

  ;; 2) The priority-chooser hydra
  (defhydra hydra-life-os-priority-chooser
    (:color blue :hint nil :foreign-keys run)
    "
 State: %(symbol-value 'lifeos--hydra-transient-state) | Select Priority
 "
    ("a" (life-os--quick-capture-action lifeos--hydra-transient-state "A") "High"  :exit t)
    ("b" (life-os--quick-capture-action lifeos--hydra-transient-state "B") "Medium" :exit t)
    ("c" (life-os--quick-capture-action lifeos--hydra-transient-state "C") "Low"   :exit t)
    ("q" nil "Quit"))

  ;; 3) The state-selector hydra, which immediately jumps into the priority hydra
(defhydra hydra-life-os-state-selector
  (:color blue
          :hint nil
          :foreign-keys run)
  "
 Select State
 "
  ("t" (progn (setq lifeos--hydra-transient-state "TODO") (hydra-life-os-priority-chooser/body)) "TODO")
  ("n" (progn (setq lifeos--hydra-transient-state "NEXT") (hydra-life-os-priority-chooser/body)) "NEXT")
  ("a" (progn (setq lifeos--hydra-transient-state "APPT") (hydra-life-os-priority-chooser/body)) "APPT")
  ("r" (progn (setq lifeos--hydra-transient-state "REVIEW") (hydra-life-os-priority-chooser/body)) "REVIEW")
  ("g" (progn (setq lifeos--hydra-transient-state "GAP") (hydra-life-os-priority-chooser/body)) "GAP")
  ("q" nil "Quit" :color blue))
) ; End of (after! hydra ...) block

(map! :leader :prefix "j"
      :desc "Generate Daily Plan"     "g" #'life-os-generate-daily-plan
      :desc "Begin New Session"       "b" #'life-os-begin-new-session
      :desc "End and Review sessoin"  "e" #'life-os-end-session-review
      :desc "AI Strategic Planner"    "P" #'life-os-plan-my-schedule
      :desc "Quick Capture Task"      "t" #'hydra-life-os-state-selector/body
      :desc "Capture Wizard (Full)"   "c" #'life-os-interactive-capture
      :desc "Promote Note to Task"    "x" #'life-os-promote-note-to-task
      :desc "Process Item at Point"   "p" #'life-os-process-item-at-point
      :desc "Schedule Task at Point"  "s" #'life-os-schedule-task-at-point
      :desc "Set Deadline at Point"   "d" #'life-os-deadline-task-at-point
      ;; Your other verified keybindings will be added here later
      )

(after! org
  (add-hook 'org-after-todo-state-change-hook #'life-os--update-parent-on-confirmation))

(defun life-os-schedule-task-at-point ()
  "Launch the LifeOS scheduler and apply the selected date as SCHEDULED."
  (interactive)
  (require 'org) ; Ensure org functions are available
  (unless (org-at-heading-p)
    (user-error "Not at a headline."))
  (let ((selection (life-os-launch-scheduler)))
    (when selection
      (let* ((date-obj (alist-get :date selection))
             (org-date-str (format-time-string "<%Y-%m-%d %a>" date-obj)))
        (org-schedule nil org-date-str)
        (message "Task scheduled for: %s" org-date-str)))))

(defun life-os-deadline-task-at-point ()
  "Launch the LifeOS scheduler and apply the selected date as a DEADLINE."
  (interactive)
  (require 'org) ; Ensure org functions are available
  (unless (org-at-heading-p)
    (user-error "Not at a headline."))
  (let ((selection (life-os-launch-scheduler)))
    (when selection
      (let* ((date-obj (alist-get :date selection))
             (org-date-str (format-time-string "<%Y-%m-%d %a>" date-obj)))
        (org-deadline nil org-date-str)
        (message "Deadline set for: %s" org-date-str)))))

(provide 'lifeos-config)
;;; lifeos-config.el ends here
