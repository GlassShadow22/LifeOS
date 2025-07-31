;;; ~/projects/life-os/lisp/lifeos-config.el --- LifeOS v7.3 Configuration Layer
;;; -*- lexical-binding: t; -*-
;;
;; This file configures the user interface for the LifeOS.
;; It binds the core logic functions from `lifeos.el` to interactive commands
;; and keybindings, creating the system's control surface.
;;
;; This file is part of the LifeOS v7.3 "Socratic Loop" implementation.
;;

;;; Code:

;; Ensure the logic layer is available (already loaded by config.el, but good for clarity if loaded directly)
;; (require 'lifeos) ; Assumes lifeos.el is in load-path

;; --- Keybindings for LifeOS v7.3 (Socratic Loop & Core Functions) ---
;; This uses Doom Emacs's `map!` macro for keybinding definitions.
;; The prefix `SPC j` is designated for LifeOS Journal/Workflow commands.

(map! :leader
      :prefix ("j" . "LifeOS Journal/Workflow")

      ;; --- Terraforming & Core Triage (Priority 1) ---
      :desc "Refile and Transition State" "r t" #'life-os-refile-and-transition-state-wrapper

      ;; --- Nightly Auditor Persona (Priority 2) ---
      :desc "Generate Full Progress Snapshot (Manual)" "n f" #'life-os-generate-full-progress-snapshot-wrapper
      :desc "Generate Daily Context Canvas" "n d" #'life-os-generate-daily-context-canvas-wrapper

      ;; --- Socratic Loop Engine (Priority 3) ---
      :desc "Generate Daily Worksheet" "w" #'life-os-generate-daily-worksheet-wrapper

      ;; --- Session Management ---
      ;; Using 's' as a prefix for session commands
      :prefix ("s" . "Session Management")
      :desc "Begin New Session" "b" #'life-os-begin-new-session-wrapper
      :desc "End Session Review" "e" #'life-os-end-session-review-wrapper
      ;; Exit the 's' prefix context (optional, implicit at end of map! or next :prefix)
      :prefix nil

      ;; --- Scheduling & Time Management ---
      ;; Changed keybinding for schedule function to avoid conflict with 's' prefix
      :desc "Schedule Task (via Scheduler)" "S" #'life-os-schedule-task-at-point ; Changed from "s"
      ;; Assuming a similar function exists for deadlines
      :desc "Schedule Deadline (via Scheduler)" "D" #'life-os-deadline-task-at-point ; Example, adjust as needed

      ;; --- Review Cycles ---
      ;; Assuming these wrapper functions exist in lifeos.el
      :desc "Run Daily Review" "r d" #'life-os-run-daily-review-wrapper
      :desc "Run Weekly Review" "r w" #'life-os-run-weekly-review-wrapper
      :desc "Run Monthly Review" "r m" #'life-os-run-monthly-review-wrapper
      :desc "Run Annual Review" "r y" #'life-os-run-annual-review-wrapper

      ;; --- Utilities ---
      ;; Direct call to utility functions (example names, adjust as per actual functions)
      :desc "Promote Note to Task" "p n" #'life-os-promote-note-to-task ; Example
      :desc "Process Item at Point (ID Link)" "p i" #'life-os-process-item-at-point ; Example
      ;; Add more utility bindings as needed
      )

;; --- Hooks for Hierarchical Action Engine ---
;; This hook is crucial for the confirmation workflow described in the Operational Manual.
;; It should be idempotent (safe to add multiple times if this file is reloaded).
(add-hook 'org-after-todo-state-change-hook #'life-os--update-parent-on-confirmation)

(provide 'lifeos-config)
;;; lifeos-config.el ends here
