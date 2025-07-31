;;; ~/projects/life-os/lisp/lifeos-config.el --- LifeOS v7.3 Configuration Layer
;;; -*- lexical-binding: t; -*-
;;
;; This file configures the user interface for the LifeOS.
;; It binds the core logic functions from `lifeos.el` to interactive commands
;; and keybindings, creating the system's control surface.
;;
;; This file is part of the LifeOS v7.3 "Socratic Loop" implementation.
;;

(after! hydra
  ;; --- LifeOS v7.3 Hydra-Driven Capture (Refactored for Two-Stage Lifecycle) ---

  (defvar lifeos--hydra-capture-type ""
    "Buffer-local variable to hold the selected capture type (e.g., 'IDEA').")

  (defhydra hydra-life-os-priority-chooser (:color blue :hint nil :foreign-keys run)
    "
   Capture Type: %(symbol-value 'lifeos--hydra-capture-type) | Select Priority
   "
    ("a" (life-os--quick-capture-action lifeos--hydra-capture-type "A") "#A Crisis" :exit t)
    ("b" (life-os--quick-capture-action lifeos--hydra-capture-type "B") "#B High"   :exit t)
    ("c" (life-os--quick-capture-action lifeos--hydra-capture-type "C") "#C Medium" :exit t)
    ("d" (life-os--quick-capture-action lifeos--hydra-capture-type "D") "#D Low"    :exit t)
    ("e" (life-os--quick-capture-action lifeos--hydra-capture-type "E") "#E Reward" :exit t)
    ("q" nil "Quit" :color blue))

  (defhydra hydra-life-os-capture-type-selector (:color blue :hint nil :foreign-keys run)
    "
   Select Capture Type
   "
    ("i" (progn (setq lifeos--hydra-capture-type "IDEA") (hydra-life-os-priority-chooser/body)) "Idea")
    ("l" (progn (setq lifeos--hydra-capture-type "LINK") (hydra-life-os-priority-chooser/body)) "Link")
    ("n" (progn (setq lifeos--hydra-capture-type "NOTE") (hydra-life-os-priority-chooser/body)) "Note")
    ("t" (progn (setq lifeos--hydra-capture-type "TASK") (hydra-life-os-priority-chooser/body)) "Task")
    ("q" nil "Quit" :color blue)))

;; --- Keybindings for LifeOS v7.3 (Socratic Loop & Core Functions) ---
(map! :leader
      :prefix ("j" . "LifeOS")

      ;; --- Daily Workflow (The Socratic Loop) ---
      :desc "Begin New Session"         "b" #'life-os-begin-new-session
      :desc "Generate Socratic Worksheet" "w" #'life-os-generate-worksheet
      :desc "Generate Daily Schedule"     "p" #'life-os-generate-daily-schedule
      :desc "End Session Review"            "e" #'life-os-end-session-review

      ;; --- Capture & Triage ---
      :desc "Quick Capture (Hydra)"     "t" #'hydra-life-os-capture-type-selector/body
      :desc "Capture Wizard (Full)"     "c" #'life-os-interactive-capture
      :desc "Refile & Transition State" "r" #'life-os-refile-and-transition-state

      ;; --- On-Demand Utilities ---
      :desc "Schedule Task at Point"    "s" #'life-os-schedule-task-at-point
      :desc "Set Deadline at Point"     "d" #'life-os-deadline-task-at-point
      :desc "Promote Note to Task"      "x" #'life-os-promote-note-to-task
      :desc "Process Item at Point"     "g" #'life-os-process-item-at-point ; 'g' for "goto"

      ;; --- Manual Generation (for testing/recovery) ---
      :prefix ("g" . "Generate")
      :desc "Full Progress Snapshot"    "p" #'life-os-generate-full-progress-snapshot
      :desc "Daily Command Center (DCC)"  "d" #'life-os-generate-dcc)


;; --- Hooks for Hierarchical Action Engine ---
;; This hook is crucial for the confirmation workflow.
(add-hook 'org-after-todo-state-change-hook #'life-os--update-parent-on-confirmation)

(provide 'lifeos-config)
;;; lifeos-config.el ends here
