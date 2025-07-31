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
  ;; This Hydra implements the "Capture State" portion of the Two-Stage Lifecycle.
  ;; It passes the chosen capture type (e.g., "IDEA") to the `life-os--quick-capture-action` backend.

  (defvar lifeos--hydra-capture-type ""
    "Buffer-local variable to hold the selected capture type (e.g., 'IDEA').")

  (defhydra hydra-life-os-priority-chooser (:color blue :hint nil :foreign-keys run)
    "
   Capture Type: %(symbol-value 'lifeos--hydra-capture-type) | Select Priority
   "
    ("a" (life-os--quick-capture-action lifeos--hydra-capture-type "A") "#A High"   :exit t)
    ("b" (life-os--quick-capture-action lifeos--hydra-capture-type "B") "#B Medium" :exit t)
    ("c" (life-os--quick-capture-action lifeos--hydra-capture-type "C") "#C Low"    :exit t)
    ("q" nil "Quit" :color blue))

  (defhydra hydra-life-os-capture-type-selector (:color blue :hint nil :foreign-keys run)
    "
   Select Capture Type
   "
    ("i" (progn (setq lifeos--hydra-capture-type "IDEA") (hydra-life-os-priority-chooser/body)) "Idea")
    ("l" (progn (setq lifeos--hydra-capture-type "LINK") (hydra-life-os-priority-chooser/body)) "Link")
    ("n" (progn (setq lifeos--hydra-capture-type "NOTE") (hydra-life-os-priority-chooser/body)) "Note")
    ("t" (progn (setq lifeos--hydra-capture-type "TASK") (hydra-life-os-priority-chooser/body)) "Task")
    ;; The "Wizard" option provides a full, interactive capture for more complex items.
    ("w" #'life-os-interactive-capture "Wizard" :exit t)
    ("q" nil "Quit" :color blue)))

;; --- Keybindings for LifeOS v7.3 (Socratic Loop & Core Functions) ---
(map! :leader
      :prefix ("j" . "LifeOS")

      ;; --- Daily Workflow (The Socratic Loop) ---
      ;; Note: life-os-begin-new-session is not bound here, assuming it's part of XMonad init.
      :desc "Generate Socratic Worksheet" "w" #'life-os-generate-worksheet
      :desc "Generate Daily Schedule"     "p" #'life-os-generate-daily-schedule
      ;; Note: life-os-end-session-review is not bound here, assuming XMonad trigger.

      ;; --- Capture & Triage ---
      :desc "Quick Capture (Hydra)"     "c" #'hydra-life-os-capture-type-selector/body ; 'c' for Capture
      :desc "Refile & Transition State" "r" #'life-os-refile-and-transition-state     ; 'r' for Refile/Triage

      ;; --- On-Demand Utilities ---
      :desc "Promote Note to Task"      "x" #'life-os-promote-note-to-task          ; 'x' for eXcalate
      :desc "Go To Item at Point"     "g" #'life-os-process-item-at-point         ; 'g' for Go To

      ;; --- Manual Generation (for testing/recovery) ---
      :prefix ("g" . "Generate")
      :desc "Full Progress Snapshot"    "p" #'life-os-generate-full-progress-snapshot
      :desc "Daily Command Center (DCC)"  "d" #'life-os-generate-dcc)

;; --- Hooks for Hierarchical Action Engine ---
;; This hook is a placeholder for the logic to automatically update parent tasks
;; when a confirmation child task is marked DONE. The v7.3 implementation
;; will integrate this into the Triage function or a dedicated hook.
;; (add-hook 'org-after-todo-state-change-hook #'life-os--update-parent-on-confirmation)

(provide 'lifeos-config)
;;; lifeos-config.el ends here
