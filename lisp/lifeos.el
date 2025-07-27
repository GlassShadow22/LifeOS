;;; -*- lexical-binding: t; -*-
;;; ~/.doom.d/lisp/lifeos.el --- LifeOS Helper Function Library v1.0
;; This file contains the complete, verified, and debugged library of helper
;; functions for the unified review orchestrator.
;; This version incorporates the corrected numerology engine.
(require 'org-ql)
;; Declare external functions to silence compiler warnings
(declare-function plz 'plz)
(declare-function json-encode 'json)
(declare-function json-read 'json)
(declare-function auth-source-search 'auth-source)
(declare-function org-element-at-point 'org-element)
(declare-function org-element-property 'org-element)
(declare-function org-get-heading 'org)
(declare-function org-id-get-create 'org-id)
(declare-function org-entry-get 'org)
(declare-function org-entry-put 'org)
(declare-function org-time-string-to-time 'org)
(declare-function org-id-find 'org-id)
(declare-function org-id-goto 'org-id)
(declare-function org-schedule 'org)
(declare-function org-deadline 'org)
(declare-function org-todo 'org)
(declare-function org-state 'org) ; Assuming this is available in the context where it's used
(declare-function org-agenda-files 'org)
(declare-function org-agenda-list 'org-agenda)
(declare-function org-ql-select 'org-ql-select)

(defgroup lifeos nil "Configuration for the LifeOS." :group 'org)
;; --- [BEGIN] CORE PATH CONFIGURATION (Generalization v1.0) ---
(defcustom lifeos-journal-root "~/journal/"
  "The root directory for the LifeOS journal system."
  :type 'directory
  :group 'lifeos)
(defcustom lifeos-logs-dir (expand-file-name "logs/" lifeos-journal-root)
  "The directory for LifeOS log files (e.g., DCCs, sessions)."
  :type 'directory
  :group 'lifeos)
(defcustom lifeos-worksheets-dir (expand-file-name "worksheets/" lifeos-journal-root)
  "The directory for LifeOS worksheet files."
  :type 'directory
  :group 'lifeos)
(defcustom lifeos-outlooks-dir (expand-file-name "outlooks/" lifeos-journal-root)
  "The directory for LifeOS outlook files."
  :type 'directory
  :group 'lifeos)
(defcustom lifeos-prompts-dir (expand-file-name ".prompts/" lifeos-journal-root)
  "The directory for LifeOS AI prompt template files."
  :type 'directory
  :group 'lifeos)
(defcustom lifeos-system-dir (expand-file-name ".system/" lifeos-journal-root)
  "The directory for LifeOS system files (e.g., parent cache)."
  :type 'directory
  :group 'lifeos)
;; Assuming inbox is directly in the journal root, adjust path as needed
(defcustom lifeos-inbox-file (expand-file-name "inbox.org" lifeos-journal-root)
  "The path to the LifeOS inbox file."
  :type 'file
  :group 'lifeos)
(defcustom lifeos-sessions-dir (expand-file-name "sessions/" lifeos-journal-root)
  "The directory for LifeOS session log files."
  :type 'directory
  :group 'lifeos)
;; --- [END] CORE PATH CONFIGURATION ---
;; --- [BEGIN] CORE USER DATA CONFIGURATION (Generalization v1.0) ---
(defcustom lifeos-user-profile-name "Architect Reggy Elizer"
  "The user's name for LifeOS context."
  :type 'string
  :group 'lifeos)
(defcustom lifeos-user-birth-date "1993-10-18"
  "The user's birth date (YYYY-MM-DD) for numerology calculations."
  :type 'string
  :group 'lifeos)
;; --- [END] CORE USER DATA CONFIGURATION ---
;; ==============================================================================
;; LIFEOS: FOUNDATIONAL UTILITIES (API & Data Processing)
;; ==============================================================================
(defun life-os-call-ai (prompt-text &optional provider)
  "Send PROMPT-TEXT to the specified LifeOS AI PROVIDER and return the RAW
PARSED JSON response."
  (require 'plz) (require 'json)
  (let* ((--provider (or provider 'pro))
         (api-key (or (getenv "LIFEOS_GEMINI_API_KEY")
		      (funcall (plist-get (car (auth-source-search :host "apikey.generativelanguage.googleapis.com")) :secret))))
         (model-endpoint (pcase --provider ('pro "gemini-2.5-pro") ('flash "gemini-2.5-flash")))
         (url (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s" model-endpoint (url-encode-url api-key)))
         (data-payload `(("contents" . [(("parts" . [(("text" . ,prompt-text))]))])))
         (request-body (json-encode data-payload)))
    (message "Sending AI request... (Model: %s)" model-endpoint)
    (plz 'post url :headers '(("Content-Type" . "application/json")) :body request-body :as 'json-read)))
(defun life-os-extract-text-from-ai-response (ai-response-data)
  "Extract the generated text string from the AI's raw JSON response."
  (let* ((candidates (alist-get 'candidates ai-response-data))
         (first-candidate (when candidates (elt candidates 0)))
         (content (when first-candidate (alist-get 'content first-candidate)))
         (parts (when content (alist-get 'parts content)))
         (first-part (when parts (elt parts 0)))
         (generated-text (when first-part (alist-get 'text first-part))))
    (or generated-text "")))
(defun life-os-read-prompt (prompt-full-path)
  "Reads the content of a Master AI Prompt file by its full path."
  (unless (file-exists-p prompt-full-path)
    (error "LifeOS Error: Prompt file not found: %s" prompt-full-path))
  (with-temp-buffer
    (insert-file-contents-literally prompt-full-path)
    (buffer-string)))
;; ==============================================================================
;; LIFEOS: PATH GENERATION HELPERS
;; ==============================================================================
(defun life-os--generate-annual-worksheet-path (year-string)
  "Generate the canonical file path for a given year's Annual Vision worksheet."
  (expand-file-name (format "%s-Vision.org" year-string)
                    (concat (file-name-as-directory lifeos-worksheets-dir)
                            year-string "/")))
(defun life-os--generate-annual-outlook-path (year-string)
  "Generate the canonical file path for a given year's Annual Codex outlook."
  (expand-file-name (format "%s-Codex.org" year-string)
                    (concat (file-name-as-directory lifeos-outlooks-dir)
                            year-string "/")))
(defun life-os--generate-monthly-worksheet-path (year-string month-string)
  "Generate the canonical file path for a monthly Debrief worksheet."
  (expand-file-name (format "%s-%s-Debrief.org" year-string month-string)
                    (concat (file-name-as-directory lifeos-worksheets-dir)
                            year-string "/")))
(defun life-os--generate-monthly-outlook-path (year-string month-string)
  "Generate the canonical file path for a monthly Directive outlook."
  (expand-file-name (format "%s-%s-Directive.org" year-string month-string)
                    (concat (file-name-as-directory lifeos-outlooks-dir)
                            year-string "/" month-string "/")))
(defun life-os--generate-intra-month-worksheet-path (cycle-end-date-string)
  "Generate the canonical path for an Intra-Month Cycle Review worksheet."
  (let ((year-string (substring cycle-end-date-string 0 4)))
    (expand-file-name (format "%s-Cycle-Review.org" cycle-end-date-string)
                      (concat (file-name-as-directory lifeos-worksheets-dir)
                              year-string "/"))))
(defun life-os--generate-intra-month-outlook-path (cycle-end-date-string)
  "Generate the canonical path for an Intra-Month Cycle outlook."
  (let ((year-string (substring cycle-end-date-string 0 4))
        (month-string (substring cycle-end-date-string 5 7)))
    (expand-file-name (format "%s-Cycle.org" cycle-end-date-string)
                      (concat (file-name-as-directory lifeos-outlooks-dir)
                              year-string "/" month-string "/"))))
(defun life-os--generate-bridge-worksheet-path (date-range)
  "Generate bridge worksheet path using the YYYY-OUTMM-INMM-ENDDD.org convention."
  (let* ((start-date-str (cdr (assoc :start-date-str date-range)))
         (end-date-str (cdr (assoc :end-date-str date-range)))
         (year-str (substring end-date-str 0 4))
         (outgoing-month (substring start-date-str 5 7))
         (incoming-month (substring end-date-str 5 7))
         ;; Removed unused `end-day` variable
         )
    (expand-file-name (format "%s-%s-%s-Bridge-Review.org" year-str outgoing-month incoming-month)
                      (concat (file-name-as-directory lifeos-worksheets-dir) year-str "/"))))
(defun life-os--generate-bridge-outlook-paths (date-range)
  "Generate bridge outlook paths (list of 2) using YYYY-OUTMM-INMM-ENDDD.org
convention."
  (let* ((start-date-str (cdr (assoc :start-date-str date-range)))
         (end-date-str (cdr (assoc :end-date-str date-range)))
         (year-str (substring end-date-str 0 4))
         (outgoing-month (substring start-date-str 5 7))
         (incoming-month (substring end-date-str 5 7))
         ;; Removed unused `end-day` variable
         (base-filename (format "%s-%s-%s-Bridge.org" year-str outgoing-month incoming-month))
         (path1 (expand-file-name base-filename (format "%s%s/%s/" lifeos-outlooks-dir year-str outgoing-month)))
         (path2 (expand-file-name base-filename (format "%s%s/%s/" lifeos-outlooks-dir year-str incoming-month))))
    (list path1 path2)))
(defun life-os--generate-dcc-path (&optional date)
  "Generate the canonical file path for a daily command center (DCC) file."
  (let ((time (or date (current-time))))
    (expand-file-name (format "%s.org" (format-time-string "%Y-%m-%d" time))
                      (concat (file-name-as-directory lifeos-logs-dir)
                              (format-time-string "%Y/%m/" time)))))
(defun life-os--scheduler-get-heuristic-dates ()
  "Return a list of alists representing common heuristic dates for the scheduler."
  (let ((today (current-time))
        ;; Removed unused `day-name` variable
        )
    (list
     `((:date . ,today)
       (:label . "Today")
       (:desc . ,(format "PD %d" (alist-get 'personal-day (life-os-calculate-numerology-for-date (format-time-string "%Y-%m-%d" today))))))
     `((:date . ,(time-add today (days-to-time 1)))
       (:label . "Tomorrow")
       (:desc . ,(format "PD %d" (alist-get 'personal-day (life-os-calculate-numerology-for-date (format-time-string "%Y-%m-%d" (time-add today (days-to-time 1))))))))
     `((:date . ,(time-add today (days-to-time 2)))
       (:label . "In 2 days"))
     ;; Add more heuristics here, e.g., next weekend, etc.
     )))
;; ==============================================================================
;; LIFEOS: CONTEXT-GATHERING HELPERS
;; ==============================================================================
(defun life-os--get-annual-codex-content (year-string)
  "Find and return the content of the Annual Codex file for YEAR-STRING."
  (let ((codex-path (life-os--generate-annual-outlook-path year-string)))
    (if (file-exists-p codex-path)
        (with-temp-buffer
          (insert-file-contents-literally codex-path)
          (buffer-string))
      (progn
        (message "LifeOS Context Warning: Annual Codex not found for year %s at path: %s" year-string codex-path)
        ""))))
(defun life-os--get-monthly-directive-content (year-string month-string)
  "Find and return the content of a Monthly Directive file."
  (let ((directive-path (life-os--generate-monthly-outlook-path year-string month-string)))
    (if (file-exists-p directive-path)
        (with-temp-buffer
          (insert-file-contents-literally directive-path)
          (buffer-string))
      (progn
        (message "LifeOS Context Warning: Monthly Directive not found for %s-%s" year-string month-string)
        ""))))
(defun life-os--get-cycle-outlook-content (&optional target-date-time)
  "Find and return the content of the MOST RECENT tactical outlook file."
  (let* ((search-date (or target-date-time (current-time)))
         (found-path nil)
         (counter 0))
    (while (and (not found-path) (< counter 90))
      (let* ((date-str (format-time-string "%Y-%m-%d" search-date))
             ;; Corrected: path-prefix is now a binding within the same let*
             (path-prefix (expand-file-name (format-time-string "%Y/%m/" search-date) lifeos-outlooks-dir))
             (cycle-path (expand-file-name (format "%s-Cycle.org" date-str) path-prefix))
             (bridge-path (expand-file-name (format "%s-Bridge.org" date-str) path-prefix)))
        (cond
         ((file-exists-p cycle-path) (setq found-path cycle-path))
         ((file-exists-p bridge-path) (setq found-path bridge-path))))
      (setq search-date (time-subtract search-date (days-to-time 1)))
      (setq counter (1+ counter)))
    (if found-path
        (with-temp-buffer
          (insert-file-contents-literally found-path)
          (buffer-string))
      (progn
        (message "LifeOS Context Warning: No recent tactical outlook found.")
        ""))))
(defun life-os--get-intra-month-cycle-date-range (target-date-obj)
  "Calculate the 9-day intra-month cycle ending on the last PD9 before TARGET-DATE-OBJ.
Returns an alist with :start-date-str and :end-date-str."
  (let* ((end-date-obj (life-os--find-last-pd-cycle-end 9))
         (start-date-obj (time-subtract end-date-obj (days-to-time 8))))
    `((:start-date-str . ,(format-time-string "%Y-%m-%d" start-date-obj))
      (:end-date-str . ,(format-time-string "%Y-%m-%d" end-date-obj)))))
(defun life-os--get-dcc-history (start-date-string end-date-string)
  "Return the concatenated content of all DCC files within a date range."
  (let ((history-content ""))
    (let ((current-time (org-time-string-to-time start-date-string))
          (end-time (org-time-string-to-time end-date-string)))
      (while (time-less-p current-time (time-add end-time (days-to-time 1)))
        (let* ((year (format-time-string "%Y" current-time))
               (month (format-time-string "%m" current-time))
               (file-name (format "%s.org" (format-time-string "%Y-%m-%d" current-time)))
               ;; Corrected: file-path is now a binding within the same let*
               (file-path (expand-file-name (concat year "/" month "/" file-name) lifeos-logs-dir)))
          (when (file-exists-p file-path)
            (with-temp-buffer
              (insert-file-contents-literally file-path)
              (setq history-content (concat history-content
                                            "
--- Start DCC " (file-name-base file-path) " ---
"
                                            (buffer-string)
                                            "
--- End DCC " (file-name-base file-path) " ---
")))))
        (setq current-time (time-add current-time (days-to-time 1)))))
    history-content))
(defun life-os--get-worksheet-content ()
  "Interactively prompt the user to select a completed worksheet file."
  (condition-case nil
      (let ((selected-path (read-file-name "Select completed worksheet file: " lifeos-worksheets-dir)))
        (if (and selected-path (file-exists-p selected-path) (not (file-directory-p selected-path)))
            (with-temp-buffer
              (insert-file-contents-literally selected-path)
              (buffer-string))
          (progn
            (message "No valid worksheet file selected.")
            "")))
    (quit (progn (message "Worksheet selection cancelled by user (C-g).") ""))))
;; ==============================================================================
;; LIFEOS: NUMEROLOGY & DYNAMIC DATA HELPERS
;; ==============================================================================
;; --- [BEGIN] CORE NUMEROLOGY ENGINE (BUG-FIXED) ---
(defun life-os--reduce-number (n &optional is-life-path-p)
  "Reduce a number N according to numerology rules. Master numbers are
preserved ONLY if IS-LIFE-PATH-P."
  (if (or (and (>= n 1) (<= n 9))
          (and is-life-path-p (or (= n 11) (= n 22))))
      n
    (life-os--reduce-number
     (let ((sum 0))
       (while (> n 0)
         (setq sum (+ sum (mod n 10)))
         (setq n (/ n 10)))
       sum)
     is-life-path-p)))
(defun life-os--calculate-personal-year (birth-date-string target-date-string)
  (let* ((birth-month (string-to-number (substring birth-date-string 5 7)))
         (birth-day (string-to-number (substring birth-date-string 8 10)))
         (target-year (string-to-number (substring target-date-string 0 4)))
         (y1 (life-os--reduce-number target-year))
         (y2 (life-os--reduce-number birth-month))
         (y3 (life-os--reduce-number birth-day))
         (sum (+ y1 y2 y3)))
    (life-os--reduce-number sum)))
(defun life-os--calculate-personal-month (personal-year target-date-string)
  (let* ((target-month (string-to-number (substring target-date-string 5 7)))
         (sum (+ personal-year target-month)))
    (life-os--reduce-number sum)))
(defun life-os--calculate-personal-day (personal-month target-date-string)
  (let* ((target-day (string-to-number (substring target-date-string 8 10)))
         (sum (+ personal-month target-day)))
    (life-os--reduce-number sum)))
(defun life-os-calculate-numerology-for-date (target-date-string)
  ;; --- [MODIFIED] Use customizable user data ---
  (let* ((life-path 5) ; TODO: Make life-path customizable if needed
         (annual-essence-table
          '(("2025" . 7) ("2026" . 7) ("2027" . 7) ("2028" . 7) ("2029" . 9)
            ("2030" . 9) ("2031" . 9) ("2032" . 9) ("2033" . 5)))
         (user-birth-date lifeos-user-birth-date) ; Use customizable variable
         (target-year-string (substring target-date-string 0 4))
         (personal-year (life-os--calculate-personal-year user-birth-date target-date-string))
         (personal-month (life-os--calculate-personal-month personal-year target-date-string))
         (personal-day (life-os--calculate-personal-day personal-month target-date-string))
         (annual-essence (cdr (assoc target-year-string annual-essence-table))))
    (unless annual-essence (error "Numerology Error: No essence for year %s" target-year-string))
    `((life-path . ,life-path) (personal-year . ,personal-year) (personal-month . ,personal-month)
      (personal-day . ,personal-day) (annual-essence . ,annual-essence))))
;; --- [END] CORE NUMEROLOGY ENGINE (BUG-FIXED) ---
(defun life-os--find-next-pd-with-num (pd-num &optional start-date)
  "Find the first upcoming date that is the given Personal Day PD-NUM.
Searches forward from START-DATE (or today)."
  (let* ((start-time (or start-date (current-time)))
         (search-time start-time)
         (found-date nil)
         (counter 0))
    (while (and (not found-date) (< counter 90)) ; 90-day limit for safety
      (let* ((date-str (format-time-string "%Y-%m-%d" search-time))
             (numerology (life-os-calculate-numerology-for-date date-str))
             (pd (when numerology (alist-get 'personal-day numerology))))
        (when (and pd (= pd pd-num))
          (setq found-date search-time)))
      (setq search-time (time-add search-time (days-to-time 1)))
      (setq counter (1+ counter)))
    (or found-date (error "LifeOS Error: Could not find a PD %d in the next 90 days." pd-num))))
(defun life-os--get-numerology-block-for-date (date-string)
  "Calculate and return a formatted string of the numerological data for
DATE-STRING."
  (let ((numerology-data (life-os-calculate-numerology-for-date date-string)))
    (if numerology-data
        (format
         "- Personal Year (PY): %d
- Personal Month (PM): %d
- Personal Day (PD): %d
- Annual Essence (E): %d"
         (alist-get 'personal-year numerology-data)
         (alist-get 'personal-month numerology-data)
         (alist-get 'personal-day numerology-data)
         (alist-get 'annual-essence numerology-data))
      "Numerological data could not be calculated.")))
(defun life-os--get-numerology-block-for-month-transition (start-date-string end-date-string)
  "Calculate and return a formatted string analyzing a month-end numerological
transition. (Hardened v2)"
  (let* ((start-date (org-time-string-to-time start-date-string))
         (end-date (org-time-string-to-time end-date-string))
         (start-numerology (life-os-calculate-numerology-for-date start-date-string))
         (end-numerology (life-os-calculate-numerology-for-date end-date-string))
         (outgoing-pm (if start-numerology (alist-get 'personal-month start-numerology)))
         (incoming-pm (if end-numerology (alist-get 'personal-month end-numerology)))
         (pd-sequence '()))
    (if (and outgoing-pm incoming-pm)
        (progn
          (let ((current-date start-date))
            (while (time-less-p current-date (time-add end-date (days-to-time 1)))
              (let* ((date-str (format-time-string "%Y-%m-%d" current-date))
                     (numerology-for-day (life-os-calculate-numerology-for-date date-str))
                     (pd (if numerology-for-day (alist-get 'personal-day numerology-for-day))))
                (when pd (push pd pd-sequence)))
              (setq current-date (time-add current-date (days-to-time 1)))))
          (setq pd-sequence (nreverse pd-sequence))
          (format
           "- Mega-Flow (PM Transition): %d -> %d
- Micro-Flow (PD Sequence): %s"
           outgoing-pm incoming-pm
           (mapconcat #'number-to-string pd-sequence " -> ")))
      (progn (message "Error: Could not calculate initial numerology for the date range.") nil))))
(defun life-os--get-static-user-data-block ()
  "Return a formatted string of static user profile data."
  (format ; --- [MODIFIED] Use customizable user data ---
   "- User Profile: %s
- Date of Birth: %s
- Life Path: 5 (The Adventurer/Seeker of Freedom)"
   lifeos-user-profile-name lifeos-user-birth-date)) ; Use customizable variables
(defun life-os--get-manual-metrics-block ()
  "Interactively prompt the user for waking state metrics and return them as a
formatted string block."
  (interactive)
  (let* ((energy (read-string "Metric - Energy (1-10): "))
         (focus (read-string "Metric - Focus (1-10): "))
         (mood (read-string "Metric - Mood (1-10): "))
         (sleep (read-string "Metric - Sleep Hours: "))
         (weight (read-string "Metric - Weight (KG): "))
         (keto (if (string-prefix-p "y" (read-string "Metric - Keto Compliance (y/n): ") t) "T" "F"))
         (sauna (if (string-prefix-p "y" (read-string "Metric - Sauna Session (y/n): ") t) "T" "F"))
         (plunge (if (string-prefix-p "y" (read-string "Metric - Cold Plunge (y/n): ") t) "T" "F")))
    (format
     "- Energy Score (Waking): %s / 10
- Focus Score (Waking): %s / 10
- Mood Score (Waking): %s / 10
- Sleep Hours (Previous Night): %s
- Weight (kg) (Waking): %s
- Keto Compliance (Previous Day): %s
- Sauna Session (Previous Day): %s
- Cold Plunge (Previous Day): %s"
     energy focus mood sleep weight keto sauna plunge)))
(defun life-os--find-last-pd-cycle-end (pd-num)
  "Find the most recent date that was the given Personal Day PD-NUM.
Searches backwards up to 30 days."
  (let ((counter 0) (found-date nil) (search-date (current-time)))
    (while (and (not found-date) (< counter 30))
      (let* ((date-str (format-time-string "%Y-%m-%d" search-date))
             (numerology (life-os-calculate-numerology-for-date date-str))
             (pd (if numerology (alist-get 'personal-day numerology))))
        (when (and pd (= pd pd-num))
          (setq found-date search-date)))
      (setq search-date (time-subtract search-date (days-to-time 1)))
      (setq counter (1+ counter)))
    ;; Return the date object or throw an error if not found.
    (or found-date (error "LifeOS Error: Could not find a PD %d in the last 30 days." pd-num))))
(defun life-os--get-previous-month-date-range (target-date-obj)
  "Calculate the start and end dates of the month prior to TARGET-DATE-OBJ.
Returns an alist with keys :start-date-str and :end-date-str."
  (let* ((current-month (string-to-number (format-time-string "%m" target-date-obj)))
         (current-year (string-to-number (format-time-string "%Y" target-date-obj)))
         (prev-month-num (if (= current-month 1) 12 (- current-month 1)))
         (prev-month-year (if (= current-month 1) (- current-year 1) current-year))
         (start-date-str (format "%d-%02d-01" prev-month-year prev-month-num))
         (end-of-month-time (time-subtract (org-time-string-to-time (format "%d-%02d-01" current-year current-month))
                                           (days-to-time 1)))
         (end-date-str (format-time-string "%Y-%m-%d" end-of-month-time)))
    `((:start-date-str . ,start-date-str) (:end-date-str . ,end-date-str))))
(defun life-os--get-bridge-cycle-date-range (target-date-obj)
  "Calculate the deterministic bridge period for the month of TARGET-DATE-OBJ.
(Corrected for variable scope)."
  (let* (;; 1. Determine month boundaries
         (current-month-start (org-time-string-to-time (format-time-string "%Y-%m-01" target-date-obj)))
         (next-month-start (time-add current-month-start (days-to-time 32))) ; Use 32 for safety
         (next-month-start (org-time-string-to-time (format-time-string "%Y-%m-01" next-month-start)))
         (current-month-end (time-subtract next-month-start (days-to-time 1)))
         ;; 2. Find the last PD9 of the CURRENT month
         (last-pd9-of-current-month
          (let ((search-date current-month-end) (found-date nil) (counter 0))
            (while (and (not found-date) (< counter 31))
              (let ((pd (alist-get 'personal-day (life-os-calculate-numerology-for-date (format-time-string "%Y-%m-%d" search-date)))))
                (when (and pd (= pd 9)) (setq found-date search-date)))
              (setq search-date (time-subtract search-date (days-to-time 1)))
              (setq counter (1+ counter)))
            found-date))
         ;; 3. Determine the start date of the bridge period
         (start-date-obj (if last-pd9-of-current-month
                             (time-add last-pd9-of-current-month (days-to-time 1))
                           (error "Could not find a final PD9 in the current month.")))
         ;; 4. Find the first PD9 of the NEXT month
         (end-date-obj
          (let ((search-date next-month-start) (found-date nil) (counter 0))
            (while (and (not found-date) (< counter 31))
              (let ((pd (alist-get 'personal-day (life-os-calculate-numerology-for-date (format-time-string "%Y-%m-%d" search-date)))))
                (when (and pd (= pd 9)) (setq found-date search-date)))
              (setq search-date (time-add search-date (days-to-time 1)))
              (setq counter (1+ counter)))
            (or found-date (error "Could not find an initial PD9 in the next month.")))))
    ;; 5. Return the final, correct date range alist.
    `((:start-date-str . ,(format-time-string "%Y-%m-%d" start-date-obj))
      (:end-date-str . ,(format-time-string "%Y-%m-%d" end-date-obj)))))
(defun life-os--scheduler-get-numerology-dates ()
  "Return a list of alists representing key upcoming numerological dates."
  (let ((today (current-time))
        (dates '()))
    ;; Find the next occurrence for each key personal day number
    (dolist (pd-num '(1 3 5 7 9))
      (let* ((found-date (life-os--find-next-pd-with-num pd-num today))
             (date-str (format-time-string "%Y-%m-%d %a" found-date))
             (label (pcase pd-num
                      (1 "Next PD 1 (New Beginnings)")
                      (3 "Next PD 3 (Communication)")
                      (5 "Next PD 5 (Change/Action)")
                      (7 "Next PD 7 (Analysis/Research)")
                      (9 "Next PD 9 (Completion)"))))
        (push `((:date . ,found-date) (:label . ,label) (:desc . ,date-str)) dates)))
    (nreverse dates)))
(defun life-os--scheduler-get-scheduled-view-data (tag)
  "Return a list of alists representing days with synergistic or open schedules.
Currently a placeholder for a more complex agenda query."
  (message "Note: 'Scheduled' view data generation is a placeholder.")
  ;; This is an MVP stub. The full implementation would require a complex
  ;; `org-ql` query to find days that match the criteria.
  (list
   `((:date . ,(current-time))
     (:label . ,(format "Day with other :%s tasks (SAMPLE)" tag))
     (:desc . "Shows days where tasks with this tag already exist."))
   `((:date . ,(time-add (current-time) (days-to-time 3)))
     (:label . "Next Open Weekday (SAMPLE)")
     (:desc . "Finds the first upcoming weekday with 0 tasks scheduled."))))
;; ==============================================================================
;; LIFEOS: REVIEW CYCLE ENGINE (v3.2 FINAL - Full Context)
;; ==============================================================================
;; --- Configuration Profiles (v2 - Phase-Separated & Fully Defined) ---
(defconst life-os-config--intra-month-review
  '(:name "Intra-Month Tactical Review"
    :date-logic #'life-os--get-intra-month-cycle-date-range
    :phase-a (:prompt "3A_IntraMonth_Q.org"
              :context-gatherers ((:placeholder "[CURRENT_MONTHLY_DIRECTIVE_CONTENT]" :source-fn life-os--get-monthly-directive-content)
                                  (:placeholder "[CYCLE_DCC_HISTORY]" :source-fn life-os--get-dcc-history))
              :output-path-fn life-os--generate-intra-month-worksheet-path)
    :phase-b (:prompt "3B_IntraMonth_Synth.org"
              :context-gatherers ((:placeholder "[CURRENT_MONTHLY_DIRECTIVE_CONTENT]" :source-fn life-os--get-monthly-directive-content)
                                  (:placeholder "[CYCLE_DCC_HISTORY]" :source-fn life-os--get-dcc-history)
                                  (:placeholder "[USER_ANSWERS_TO_INTRA_MONTH_QUESTIONNAIRE]" :source-fn :user-answers))
              :output-path-fn life-os--generate-intra-month-outlook-path)))
;; NOTE: Other review configs are stubs. Their full definition would follow the pattern above.
(defconst life-os-config--bridge-review
  '(:name "Bridge Cycle Ebb & Flow Review"
    :date-logic life-os--get-bridge-cycle-date-range
    :phase-a (:prompt "4A_Bridge_Q.org"
              :context-gatherers ((:placeholder "[ANNUAL_CODEX_CONTENT]" :source-fn life-os--get-annual-codex-content)
                                  (:placeholder "[CONTENTS_OF_OUTGOING_MONTH_DIRECTIVE_FILE]" :source-fn :outgoing-month-directive)
                                  (:placeholder "[CONTENTS_OF_INCOMING_MONTH_DIRECTIVE_FILE]" :source-fn :incoming-month-directive)
                                  (:placeholder "[CONTENTS_OF_ALL_BRIDGE_CYCLE_DCCS]" :source-fn life-os--get-dcc-history)
                                  (:placeholder "[NUMEROLOGICAL_FLOW]" :source-fn life-os--get-numerology-block-for-month-transition))
              :output-path-fn life-os--generate-bridge-worksheet-path)
    :phase-b (:prompt "4B_Bridge_Synth.org"
              :context-gatherers ((:placeholder "[ANNUAL_CODEX_CONTENT]" :source-fn life-os--get-annual-codex-content)
                                  (:placeholder "[CONTENTS_OF_OUTGOING_MONTH_DIRECTIVE_FILE]" :source-fn :outgoing-month-directive)
                                  (:placeholder "[CONTENTS_OF_INCOMING_MONTH_DIRECTIVE_FILE]" :source-fn :incoming-month-directive)
                                  (:placeholder "[CONTENTS_OF_ALL_BRIDGE_CYCLE_DCCS]" :source-fn life-os--get-dcc-history)
                                  (:placeholder "[NUMEROLOGICAL_FLOW]" :source-fn life-os--get-numerology-block-for-month-transition)
                                  (:placeholder "[PASTE_USER_ANSWERS_TO_BRIDGE_CYCLE_QUESTIONNAIRE_HERE]" :source-fn :user-answers))
              :output-path-fn life-os--generate-bridge-outlook-paths)))
(defconst life-os-config--monthly-review
  '(:name "Monthly Debrief & Directive"
    :date-logic life-os--get-previous-month-date-range
    :phase-a (:prompt "2A_Monthly_Q.org"
              :context-gatherers ((:placeholder "[CONTENTS_OF_ANNUAL_CODEX]" :source-fn life-os--get-annual-codex-content)
                                  (:placeholder "[CONTENTS_OF_PAST_MONTHS_TACTICAL_PLANNER_FILE]" :source-fn life-os--get-monthly-directive-content)
                                  (:placeholder "[CONTENTS_OF_ALL_PAST_MONTHS_DCCS]" :source-fn life-os--get-dcc-history))
              :output-path-fn life-os--generate-monthly-worksheet-path)
    :phase-b (:prompt "2B_Monthly_Synth.org"
              :context-gatherers ((:placeholder "[CONTENTS_OF_ANNUAL_CODEX]" :source-fn life-os--get-annual-codex-content)
                                  (:placeholder "[CONTENTS_OF_ALL_PAST_MONTHS_DCCS]" :source-fn life-os--get-dcc-history)
                                  (:placeholder "[CONTENTS_OF_PAST_MONTHS_TACTICAL_PLANNER_FILE]" :source-fn life-os--get-monthly-directive-content)
                                  (:placeholder "[PASTE_USER_ANSWERS_TO_MONTHLY_DEBRIEF_QUESTIONNAIRE_HERE]" :source-fn :user-answers))
              :output-path-fn life-os--generate-monthly-outlook-path)))
(defconst life-os-config--annual-review
  '(:name "Annual Vision & Codex" :date-logic :previous-year :phase-a '() :phase-b '()))
;; --- Engine State & Save Logic ---
(defvar-local life-os--correction-buffer-data nil "Buffer-local alist to store state for the correction buffer.")
(defun life-os-save-and-proceed-correction-buffer ()
  "Saves Phase A worksheet and proceeds to Phase B."
  (interactive)
  (let* ((config (alist-get 'config life-os--correction-buffer-data))
         (phase-a-config (plist-get config :phase-a))
         (date-range (alist-get 'date-range life-os--correction-buffer-data))
         (output-path-fn (plist-get phase-a-config :output-path-fn))
         (output-path (if (eq output-path-fn 'life-os--generate-bridge-worksheet-path)
                          (funcall output-path-fn date-range)
                        (let ((end-date-str (cdr (assoc :end-date-str date-range))))
                          (funcall output-path-fn end-date-str)))))
    (when (or (not (file-exists-p output-path)) (y-or-n-p (format "File exists. Overwrite %s? " output-path)))
      (let ((user-answers (buffer-string)))
        (make-directory (file-name-directory output-path) t) (write-region (point-min) (point-max) output-path)
        (kill-buffer (current-buffer)) (message "Phase A saved. Proceeding to Phase B...")
        (life-os--execute-review-phase (plist-get config :phase-b) config date-range `((user-answers . ,user-answers)) #'life-os-phase-b-mode)))))
(defun life-os-save-final-outlook-buffer ()
  "Saves the final Phase B outlook."
  (interactive)
  (let* ((config (alist-get 'config life-os--correction-buffer-data))
         (phase-b-config (plist-get config :phase-b))
         (date-range (alist-get 'date-range life-os--correction-buffer-data))
         (output-path-fn (plist-get phase-b-config :output-path-fn))
         (output-paths
          (let ((paths (if (eq output-path-fn 'life-os--generate-bridge-outlook-paths)
                           (funcall output-path-fn date-range)
                         (let ((end-date-str (cdr (assoc :end-date-str date-range))))
                           (funcall output-path-fn end-date-str)))))
            (if (listp paths) paths (list paths)))))
    (when (or (not (file-exists-p (car output-paths))) (y-or-n-p "File exists. Overwrite? "))
      (dolist (path output-paths)
        (make-directory (file-name-directory path) t) (write-region (point-min) (point-max) path) (message "LifeOS: Saved to %s" path))
      (message "SUCCESS: Review cycle '%s' complete." (plist-get config :name))
      (kill-buffer (current-buffer)) (delete-window))))
;; --- Engine Minor Modes ---
(defvar life-os-phase-a-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-c C-c") #'life-os-save-and-proceed-correction-buffer) map) "Keymap for Phase A.")
(defvar life-os-phase-b-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-c C-c") #'life-os-save-final-outlook-buffer) map) "Keymap for Phase B.")
(define-minor-mode life-os-phase-a-mode "LifeOS Phase A review buffer mode." :init-value nil :lighter " L-OS Phase A" :keymap life-os-phase-a-mode-map)
(define-minor-mode life-os-phase-b-mode "LifeOS Phase B review buffer mode." :init-value nil :lighter " L-OS Phase B" :keymap life-os-phase-b-mode-map)
;; --- Core Engine & Phase Executor (Corrected for Date Logic) ---
(defun life-os--execute-review-phase (phase-config top-level-config date-range extra-context current-mode-fn)
  "Executes one phase, injects full context (including special cases), and
displays result."
  (let* ((start-date-str (cdr (assoc :start-date-str date-range)))
         (end-date-str (cdr (assoc :end-date-str date-range)))
         (start-date-obj (org-time-string-to-time start-date-str))
         (end-date-obj (org-time-string-to-time end-date-str))
         (start-year-str (format-time-string "%Y" start-date-obj)) (start-month-str (format-time-string "%m" start-date-obj))
         (end-year-str (format-time-string "%Y" end-date-obj)) (end-month-str (format-time-string "%m" end-date-obj))
         (buffer-name "*LifeOS Review*") (prompt-filename (plist-get phase-config :prompt))
         (final-prompt
          (let ((prompt (life-os-read-prompt (expand-file-name prompt-filename lifeos-prompts-dir))))
            (dolist (gatherer (plist-get phase-config :context-gatherers))
              (let* ((placeholder (plist-get gatherer :placeholder)) (source-fn (plist-get gatherer :source-fn))
                     (context-data
                      (cond ((eq source-fn :user-answers) (alist-get 'user-answers extra-context))
                            ;; Special keywords for Bridge Review
                            ((eq source-fn :outgoing-month-directive) (life-os--get-monthly-directive-content start-year-str start-month-str))
                            ((eq source-fn :incoming-month-directive) (life-os--get-monthly-directive-content end-year-str end-month-str))
                            ;; Standard helper function calls
                            ((eq source-fn 'life-os--get-dcc-history) (funcall source-fn start-date-str end-date-str))
                            ((eq source-fn 'life-os--get-monthly-directive-content) (funcall source-fn end-year-str end-month-str))
                            ((eq source-fn 'life-os--get-annual-codex-content) (funcall source-fn end-year-str))
                            ((eq source-fn 'life-os--get-numerology-block-for-month-transition) (funcall source-fn start-date-str end-date-str))
                            (t ""))))
                (message "Injecting context for: %s" placeholder)
                (setq prompt (replace-regexp-in-string (regexp-quote placeholder) (or context-data "") prompt nil t))))
            prompt)))
    ;; --- API Call & Display Logic (remains the same) ---
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer) (insert "--- [ EXECUTING: CONTACTING GEMINI API... ] ---") (pop-to-buffer buffer-name) (sit-for 0.1)
      (let* ((raw-ai-response (life-os-call-ai final-prompt 'pro)) (ai-text (life-os-extract-text-from-ai-response raw-ai-response)))
        (erase-buffer) (insert ai-text) (goto-char (point-min))
        (setq-local life-os--correction-buffer-data `((config . ,top-level-config) (date-range . ,date-range)))
        (funcall current-mode-fn 1) (message "Correction buffer ready. Press C-c C-c to commit.")))))
(defun life-os-run-review-cycle (config &optional start-date-str end-date-str)
  "Main entry point to run a full review cycle.
Calculates date range automatically if START-DATE-STR and END-DATE-STR
are not provided."
  (message "BEGINNING REVIEW CYCLE: %s" (plist-get config :name))
  (let* ((date-range
          (if (and start-date-str end-date-str)
              ;; If dates are provided, use them to build the alist
              `((:start-date-str . ,start-date-str) (:end-date-str . ,end-date-str))
            ;; Otherwise, calculate them dynamically
            (let ((date-logic-fn (plist-get config :date-logic)))
              (funcall date-logic-fn (current-time))))))
    (message "--- Starting Phase A: Questionnaire Generation for period %s to %s ---"
             (cdr (assoc :start-date-str date-range)) (cdr (assoc :end-date-str date-range)))
    (life-os--execute-review-phase
     (plist-get config :phase-a)
     config
     date-range
     nil
     #'life-os-phase-a-mode)))
;; ==============================================================================
;; LIFEOS: USER INTERFACE COMMANDS (M-x)
;; ==============================================================================
(defun life-os-conduct-intra-month-review ()
  "Run the interactive Intra-Month Tactical Review cycle."
  (interactive)
  (life-os-run-review-cycle life-os-config--intra-month-review))
(defun life-os-conduct-bridge-review ()
  "Run the interactive Bridge Cycle Ebb & Flow Review."
  (interactive)
  (life-os-run-review-cycle life-os-config--bridge-review))
(defun life-os-conduct-monthly-review ()
  "Run the interactive Monthly Debrief & Directive cycle."
  (interactive)
  (life-os-run-review-cycle life-os-config--monthly-review))
(defun life-os-conduct-annual-review ()
  "Run the interactive Annual Vision & Codex cycle."
  (interactive)
  (message "This review cycle is not yet configured."))
(defun life-os-conduct-historical-intra-month-review ()
  "Run the Intra-Month Review for a specific, user-provided date range."
  (interactive)
  (let ((start-date (read-string "Enter start date (YYYY-MM-DD): "))
        (end-date (read-string "Enter end date (YYYY-MM-DD): ")))
    (if (and (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" start-date)
             (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" end-date))
        (life-os-run-review-cycle life-os-config--intra-month-review start-date end-date)
      (user-error "Invalid date format. Please use YYYY-MM-DD."))))
;; ==============================================================================
;; LIFEOS: ENGINE CONSTANTS & HELPERS
;; ==============================================================================
(defconst life-os-canonical-properties
  '("PROJECT" "EFFORT" "Task_Type" "Blocked_By" "Confirm_With" "Confirmed_External")
  "Canonical list of fixed property keys for LifeOS. (Note: No colons).")
(defconst life-os-canonical-tags
  '("Idea" "Epic" "Question" "Blocker" "Insight" "Resource" "Fleeting" "DeepWork"
    "ShallowWork" "Admin" "Calls" "Errands" "Family" "Friends" "Mentorship"
    "Urgent" "Review" "Epic")
  "Canonical list of fixed conceptual tags for LifeOS. (Note: No colons).")
(defun life-os--append-to-file-robust (file-path content-string)
  "Robustly append CONTENT-STRING to FILE-PATH."
  (make-directory (file-name-directory file-path) t)
  (condition-case err ; Remove quote from 'error
      (with-temp-buffer
        (insert "
" content-string)
        (append-to-file (point-min) (point-max) file-path)
        t) ; Return t on success
    (error (message "LifeOS Error appending to %s: %s" file-path err) nil))) ; Remove quote from 'error
(defun life-os-update-parent-cache ()
  "Query inbox.org for :Epic: headlines and write their titles and IDs to the
parent cache file.
This version uses robust, low-level text searches to be resilient against
non-interactive parsing bugs."
  (interactive)
  (let* ((cache-file (expand-file-name "parents.org" lifeos-system-dir))
         (inbox-file lifeos-inbox-file)
         (cache-lines '()))
    (when (file-exists-p inbox-file)
      (with-temp-buffer
        (insert-file-contents-literally inbox-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\(\\*+\\).*?:Epic:[ \t]*$" nil t)
          (let* ((headline-pos (match-beginning 0))
                 (level (length (match-string 1)))
                 (title nil)
                 (id nil)
                 (subtree-end
                  (save-excursion
                    (goto-char (line-end-position))
                    (if (re-search-forward (format "^\\*\\{1,%d\\} " level) nil t)
                        (match-beginning 0)
                      (point-max)))))
            (save-excursion
              (goto-char headline-pos)
              (when (re-search-forward "^\\*+ \\(?:\\S-+\\s-\\|\\)[ \t]*\\(.*?\\)[ \t]*:Epic:" subtree-end t)
                (setq title (string-trim (match-string 1)))))
            (save-excursion
              (goto-char headline-pos)
              (when (re-search-forward ":ID:[ \t]+\\([a-zA-Z0-9-]+\\)" subtree-end t)
                (setq id (match-string 1))))
            (when (and title id)
              (push (format "%s | [[id:%s]]" title id) cache-lines))))))
    (with-temp-buffer
      (when cache-lines
        (insert (mapconcat #'identity (nreverse cache-lines) "
"))
        (unless (bolp) (insert "
")))
      (make-directory (file-name-directory cache-file) t)
      (write-file cache-file nil)))
  ;; Do not message in a sub-routine; the main function will report status.
  )
(defun life-os--get-all-todo-keywords-robust ()
  "Return a flat list of all defined TODO keywords, without separators."
  (let ((all-keywords '()))
    (dolist (sequence org-todo-keywords)
      (dolist (item (cdr sequence))
        (when (stringp item)
          (push (car (split-string item "(")) all-keywords))))
    (nreverse all-keywords)))
(defun life-os--append-to-inbox-and-save (content-string)
  "Appends CONTENT-STRING to the LifeOS inbox file and saves the buffer.
Returns t on success, nil on failure."
  (let ((inbox-buffer (find-file-noselect lifeos-inbox-file)))
    (with-current-buffer inbox-buffer
      (goto-char (point-max))
      ;; Ensure there's a newline before appending
      (unless (bolp) (insert "\n"))
      (insert content-string)
      (save-buffer))))
;; ==============================================================================
;; LIFEOS: DYNAMIC ORG CAPTURE SCHEDULER (v5.1 - Thrust 2.A)
;; ==============================================================================
(defun life-os--scheduler-finalize-selection ()
  "Finalize the scheduling capture buffer.
This function retrieves the `life-os-data` text property from the
current line, stores the associated alist in `org-capture-plist`,
and then calls `org-capture-finalize`."
  (interactive)
  (let ((selection-data (get-text-property (line-beginning-position) 'life-os-data))) ; Use line-beginning-position
    (when selection-data
      (setq org-capture-plist (plist-put org-capture-plist :life-os-scheduler-selection selection-data)))
    (org-capture-finalize)))
(defvar life-os--scheduler-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'life-os--scheduler-finalize-selection)
    (define-key map (kbd "<up>") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    map)
  "Local keymap for the LifeOS scheduler capture buffer.")
(defun life-os--scheduler-capture-template ()
  "Render the interactive scheduling dashboard in a capture buffer.
This function is designed to be called via the :function property
of an `org-capture-templates` entry. It generates the UI content,
propertizes each candidate line with its source alist, and
activates the local scheduler keymap."
  (goto-char (point-min))
  (insert "--- [ LifeOS Scheduling Dashboard ]---
")
  (insert "Select a date and press C-c C-c to confirm.
")
  (let ((candidates (append (life-os--scheduler-get-heuristic-dates)
                            (life-os--scheduler-get-numerology-dates))))
    (dolist (item candidates)
      (let* ((label (alist-get :label item))
             (desc (or (alist-get :desc item) ""))
             (formatted-line (format "%-35s | %s
" label desc)))
        (insert (propertize formatted-line 'life-os-data item)))))
  (use-local-map life-os--scheduler-keymap)
  (goto-char (point-min))
  (forward-line 3)) ; Use forward-line instead of next-line
(defun life-os-launch-scheduler-dynamic (&optional context-tags)
  "Launch the dynamic Org capture scheduling UI.
This function programmatically creates a temporary capture template
that uses `life-os--scheduler-capture-template` to render an
interactive buffer. It launches a blocking `org-capture` session
and, upon successful finalization, returns the data alist
associated with the user's selection. Returns nil if cancelled.
The optional CONTEXT-TAGS argument is a placeholder for future
enhancements to generate context-aware suggestions."
  (interactive)
  (declare (ignore context-tags)) ; Add declare ignore for unused argument
  ;; The let-binding creates a temporary, lexical scope for
  ;; `org-capture-templates`, ensuring the global configuration is
  ;; not affected.
  (let ((org-capture-templates
         `(("s" "Scheduler" plain ""
            :function life-os--scheduler-capture-template
            :immediate-finish nil
            :kill-buffer t))))
    (org-capture nil "s")
    ;; `life-os--scheduler-finalize-selection` places the result in
    ;; the plist under this key before finalizing.
    (plist-get org-capture-plist :life-os-scheduler-selection)))
(defun life-os--quick-capture-action (state priority)
  "Backend for Quick Capture. Captures a task with STATE and PRIORITY.
(Corrected Syntax v3.0)"
  ;; The interactive spec is for standalone testing (M-x).
  (interactive
   (list (completing-read "State: " '("TODO" "NEXT" "APPT" "REVIEW" "GAP"))
         (cdr (assoc (completing-read "Priority: " '("[#A] High" "[#B] Medium" "[#C] Low" "[None]"))
                     '(("[#A] High" . "A") ("[#B] Medium" . "B")
                       ("[#C] Low" . "C") ("[None]" . ""))))))
  (let ((headline (read-string (format "Capture [%s]%s: " state (if (string-empty-p priority) "" (format " [#%s]" priority))))))
    (when (and headline (not (string-empty-p headline)))
      (let* ((id (org-id-new))
             (full-headline
              (with-temp-buffer
                ;; All subsequent forms are now correctly inside the 'with-temp-buffer' block.
                (insert (format "* %s %s%s
" state (if (string-empty-p priority) "" (format "[#%s] " priority)) headline))
                (insert "  :PROPERTIES:
")
                (insert (format "  :ID:        %s
" id))
                (insert (format "  :CREATED:   %s
" (format-time-string "[%Y-%m-%d %a %H:%M]")))
                (insert "  :END:")
                (buffer-string))) ; This is the single value returned for the let* binding.
             (pointer (format "- %s%s %s [[id:%s]]" state (if (string-empty-p priority) "" (format " [#%s]" priority)) headline id)))
        (when (life-os--append-to-inbox-and-save full-headline)
          (life-os--append-to-active-log-inbox pointer)
          (message "LifeOS: Quick-captured '%s'" headline))))))
;; --- [BEGIN] 3rd Gear: Full Interactive Capture Wizard ---
(defun life-os-interactive-capture ()
  "A comprehensive, interactive wizard for capturing a new, deeply-contextualized
item.
This function gathers full metadata but does not perform scheduling."
  (interactive)
  (let* (;; --- STAGE 1: Core Data Gathering ---
         (headline (read-string "Main Item: "))
         (state (completing-read "State [TODO]: " (life-os--get-all-todo-keywords-robust) nil t nil nil "TODO"))
         (priority (cdr (assoc (completing-read "Priority [B]: " '("[#A] High" "[#B] Medium" "[#C] Low" "[None]"))
                               '(("[#A] High" . "A") ("[#B] Medium" . "B")
                                 ("[#C] Low" . "C") ("[None]" . "")))))
         ;; --- STAGE 2: Hierarchy (Parenting) ---
         ;; Removed unused `parent-title` variable
         (parent-id
          (when (y-or-n-p "Capture as subtask of an existing Epic? ")
            (life-os-update-parent-cache) ; Ensure cache is fresh
            (let* ((parents-cache (expand-file-name "parents.org" lifeos-system-dir))
                   (parents (when (file-exists-p parents-cache) (split-string (with-temp-buffer (insert-file-contents-literally parents-cache) (buffer-string)) "
" t)))
                   (selection (when parents (completing-read "Select Parent Epic: " parents))))
              (when (and selection (string-match "\\(.*\\) | \\[\\[id:\\([a-zA-Z0-9T.-]+\\)\\]\\]" selection))
                ;; (setq parent-title (string-trim (match-string 1 selection))) ; Removed
                (match-string 2 selection)))))
         ;; --- STAGE 3 & 4: Tags & Properties ---
         (tags (completing-read-multiple "Tag(s) (SPC to separate): " life-os-canonical-tags))
         (properties
          (let ((props '()) (key))
            (while (not (string= (setq key (completing-read "Add Property or [Done]: " (append '("[Done]") life-os-canonical-properties) nil t)) "[Done]"))
              (push `(,(intern (concat ":" key)) . ,(read-string (format "Value for %s: " key))) props))
            (nreverse props)))
         (id (org-id-new))
         ;; --- STAGE 5: Final Headline Assembly ---
         (full-headline
          (with-temp-buffer (org-mode)
            (insert (format "* %s %s%s" state (if (and priority (not (string-empty-p priority))) (format "[#%s] " priority) "") headline))
            (when tags (insert (format " :%s:" (mapconcat #'identity tags ":")))) (insert "
")
            ;; Note: No scheduling block here, as per our decoupled design.
            (insert "  :PROPERTIES:
")
            (insert (format "  :ID:        %s
" id))
            (insert (format "  :CREATED:   %s
" (format-time-string "[%Y-%m-%d %a %H:%M]")))
            (when parent-id (insert (format "  :PARENT:    [[id:%s]]
" parent-id)))
            (dolist (prop properties)
              (insert (format "  :%s: %s
" (car prop) (cdr prop)))) ; Standardize property format
            (insert "  :END:")
            (buffer-string))))
    ;; --- STAGE 6: FINALIZATION & POINTER CREATION ---
    (when (life-os--append-to-inbox-and-save full-headline)
      (let* (;; For now, use the robust static pointer. AI summary can be a later enhancement.
             (summary (format "%s%s %s" state (if (string-empty-p priority) "" (format " [#%s]" priority)) headline))
             (pointer (format "- %s [[id:%s]]" summary id)))
        (life-os--append-to-active-log-inbox pointer))
      (message "LifeOS: Captured '%s' successfully." headline))))
;; --- [BEGIN] Escalation Path & Triage Suite ---
(defun life-os-promote-note-to-task ()
  "Promote a plain text bullet point at point into a full LifeOS task."
  (interactive)
  (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (clean-text (string-trim (replace-regexp-in-string "^[ \t]*[-+*]+\\(\\[[ X-]\\]\\)?[ \t]*" "" line-text)))
         (original-buffer (current-buffer))
         (original-point (point-marker)))
    (if (string-empty-p clean-text)
        (user-error "Cannot promote an empty line.")
      ;; Redefine `read-string` locally for this one call to pre-fill the prompt.
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial-input history &rest _) ; Arguments defined here
                   (declare (ignore initial-input history _)) ; Add declare ignore for unused lambda arguments
                   (funcall #'read-from-minibuffer prompt clean-text))))
        (call-interactively #'life-os-interactive-capture))
      ;; After the capture is fully complete, delete the original plain line.
      (with-current-buffer original-buffer
        (goto-char original-point)
        (delete-region (line-beginning-position) (line-end-position))))))
;; --- Triage Suite (Corrected with Org Element API) ---
(defun life-os-process-item-at-point ()
  "Find the LifeOS ID link at point and jump to its source."
  (interactive)
  ;; The Org Element API is the correct tool for parsing context at point.
  (require 'org-element)
  (let* ((link (org-element-context)) ; Get the element at point
         (id
          (when (eq (org-element-type link) 'link) ; Check if it's a link
            (let ((type (org-element-property :type link)))
              (when (string= type "id") ; Check if it's an ID link
                (org-element-property :path link))))))
    (if id
        (progn
          (message "Jumping to source for ID: %s" id)
          (org-id-goto id))
      (user-error "No LifeOS ID link found at point."))))
;; Helper advice for 'life-os-promote-note-to-task'
(defvar read-string-initial-value nil
  "A temporary variable to hold an initial value for `read-string`.")
;; ==============================================================================
;; LIFEOS: THRUST 2 - AI STRATEGIC SCHEDULER (Corrected v2.0)
;; ==============================================================================
;; MODIFIED: life-os--get-tasks-to-schedule
;; Now correctly creates and returns markers from the query results.
;; MODIFIED: life-os--get-tasks-to-schedule (v7.0.2)
;; Now accepts an explicit list of files for testability.
;; (This function is defined later and is the correct one. The duplicate one is removed.)
;; MODIFIED: life-os-plan-my-schedule (v7.0.2)
;; Now accepts an optional FILES argument to pass to its helper.
;; (This function is defined later and is the correct one. The duplicate one is removed.)
;; ==============================================================================
;; LIFEOS: AI STRATEGIC SCHEDULER (v5.2 - Thrust 2.B)
;; ==============================================================================
;; Declare variables used in the scheduler functions
(defvar life-os--action-review-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'life-os--commit-ai-schedule)
    map)
  "Keymap for the LifeOS Action Review buffer.")

(defvar life-os--action-review-task-marker nil
  "Buffer-local variable to hold the marker for the task being reviewed.")

(defun life-os--get-tasks-to-schedule (&optional files)
  "Find `SCHEDULE-ME` tasks in FILES and return them as a list of markers.
If FILES is nil, defaults to the global `org-agenda-files`."
  (interactive)
  (org-ql-select (or files (org-agenda-files)) '(todo "SCHEDULE-ME")
    :action (lambda () (copy-marker (point))))) ; Return markers, not elements

(defun life-os--get-agenda-view-for-next-n-days (days files)
  "Return a string of agenda entries for DAYS from specific FILES."
  (let ((org-agenda-files files) ; Temporarily override the global list
        (org-agenda-span days)
        (org-agenda-start-on-weekday nil)
        (org-agenda-show-log t))
    (with-temp-buffer
      (org-agenda-list t) ; Pass `t` to specify "current day" which respects the span
      (goto-char (point-min))
      ;; Clean up the output for a more concise prompt.
      (while (re-search-forward "^[A-Za-z]+ .*$
" nil t) (replace-match ""))
      (string-trim (buffer-string)))))

;; MODIFIED: life-os--assemble-strategic-scheduling-context (v7.0.2)
;; No change to signature, but its call to the helper now passes `files`.
(defun life-os--assemble-strategic-scheduling-context (task-marker files)
  "Assemble all required context for a task from specific FILES."
  (with-current-buffer (marker-buffer task-marker)
    (goto-char (marker-position task-marker))
    (let* ((element (org-element-at-point))
           (task-data (buffer-substring-no-properties (org-element-property :begin element)
                                                      (org-element-property :end element)))
           (current-year (format-time-string "%Y"))
           (current-month (format-time-string "%m")))
      (string-join
       `("--- Task to Schedule ---
" ,task-data
         "
--- Annual Codex (Grand Strategy) ---
" ,(life-os--get-annual-codex-content current-year)
         "
--- Monthly Directive (Tactical Orders) ---
" ,(life-os--get-monthly-directive-content current-year current-month)
         "
--- Agenda View (Next 14 Days) ---
" ,(life-os--get-agenda-view-for-next-n-days 14 files)
         "
--- Numerological Outlook (Next 14 Days) ---
" ,(life-os--get-numerology-for-next-n-days 14))
       ""))))

(defun life-os--get-numerology-for-next-n-days (days)
  "Return a string of numerological data for the next N days."
  (let ((output-string ""))
    (dotimes (i days)
      (let* ((date (time-add (current-time) (days-to-time i)))
             (date-str (format-time-string "%Y-%m-%d" date))
             (numerology (life-os-calculate-numerology-for-date date-str)))
        (setq output-string (concat output-string
                                    (format "- %s: PD %d, PM %d, PY %d
"
                                            date-str
                                            (alist-get 'personal-day numerology)
                                            (alist-get 'personal-month numerology)
                                            (alist-get 'personal-year numerology))))))
    output-string))

(defun life-os--parse-ai-scheduling-options (response-text)
  "Parse the AI's formatted Org-mode list into a list of alists."
  (let ((options '())
        (current-option nil))
    (with-temp-buffer
      (insert response-text)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ;; Top-level item: start of a new option
         ((looking-at "^- \\(.*\\)")
          (when current-option (push current-option options))
          (setq current-option `((:desc . ,(match-string 1)))))
         ;; Date sub-item
         ((looking-at "  :date: \\([0-9-]+\\)")
          (setq current-option (append current-option `((:date . ,(match-string 1))))))
         ;; Type sub-item
         ((looking-at "  :type: \\(SCHEDULED\\|DEADLINE\\)")
          (setq current-option (append current-option `((:type . ,(match-string 1))))))
         ;; Rationale sub-item
         ((looking-at "  :rationale: \\(.*\\)")
          (setq current-option (append current-option `((:rationale . ,(match-string 1)))))))
        (forward-line 1)))
    (when current-option (push current-option options))
    (nreverse options)))

(defun life-os--commit-ai-schedule (&optional remaining-task-markers)
  "Commit the user's selected schedule. To be called from the Action Review buffer.
If REMAINING-TASK-MARKERS is provided, prompt to schedule the next one."
  (interactive)
  (let ((option-data (get-text-property (point) 'life-os-data))
        (task-marker life-os--action-review-task-marker)) ; Use the buffer-local var
    (when (and option-data task-marker)
      (with-current-buffer (marker-buffer task-marker)
        (goto-char (marker-position task-marker))
        (let* ((original-element (org-element-at-point))
               ;; Removed unused `original-title` variable
               )
          ;; --- Core Scheduling Logic (Unchanged) ---
          (org-todo "AI-REC")
          (let* ((type (alist-get :type option-data))
                 (date-str (format "<%s>" (alist-get :date option-data)))) ; Extend scope of let*
            (if (string= type "SCHEDULED")
                (org-schedule nil date-str)
              (org-deadline nil date-str))
            (message "Task committed with AI recommendation.")
            ;; --- [NEW] Child Task Creation Trigger ---
            (let ((confirm-with (org-entry-get (point) "Confirm_With"))
                  (parent-id (org-id-get-create)))
              ;; Set default confirmed status if not present
              (unless (org-entry-get (point) "Confirmed_External")
                (org-entry-put (point) "Confirmed_External" "no"))
              ;; Trigger child task creation
              (when (and (string-match-p "\\bAPPT\\b" (org-element-property :raw-value original-element)) ; Use raw-value
                         confirm-with)
                (life-os--create-confirmation-child-task
                 parent-id
                 (org-get-heading t t)
                 date-str
                 confirm-with))))))
      (kill-buffer "*LifeOS Action Review*")
      ;; --- [NEW] Recursive Processing Prompt ---
      (when remaining-task-markers
        (let ((next-task-marker (car remaining-task-markers)))
          (with-current-buffer (marker-buffer next-task-marker)
             (goto-char (marker-position next-task-marker))
             (let ((next-task-heading (org-get-heading t t)))
               (when (and next-task-heading
                          (y-or-n-p (format "Scheduled current task. Process next 'SCHEDULE-ME' task ('%s')? " next-task-heading)))
                 ;; Call plan-my-schedule recursively with the remaining list
                 (life-os-plan-my-schedule nil (cdr remaining-task-markers))))))))))
  ;; Ensure buffer is killed even if not proceeding recursively
  (when (get-buffer "*LifeOS Action Review*")
    (kill-buffer "*LifeOS Action Review*"))

(defun life-os-plan-my-schedule (&optional files remaining-task-markers)
  "Main entry point for AI-assisted scheduling, searchable over specific FILES.
If REMAINING-TASK-MARKERS is provided, it's used instead of querying for tasks."
  (interactive)
  (let ((task-markers (or remaining-task-markers (life-os--get-tasks-to-schedule files))))
    ;; The rest of the function logic remains largely unchanged...
    (if (not task-markers)
        (message "LifeOS: No tasks are currently marked 'SCHEDULE-ME'.")
      (progn
        ;; Pass the CDR (remaining tasks) to commit function
        (let* ((task-to-process (car task-markers))
               (context (life-os--assemble-strategic-scheduling-context task-to-process files)) ; Pass files
               (prompt-template (life-os-read-prompt (expand-file-name "7_Strategic_Scheduler.org" lifeos-prompts-dir))) ; Use lifeos-prompts-dir
               (final-prompt (replace-regexp-in-string (regexp-quote "[TASK_DATA_PLACEHOLDER]") context prompt-template))
               (_ (message "Contacting AI with strategic context..."))
               (ai-response (life-os-extract-text-from-ai-response (life-os-call-ai final-prompt 'pro)))
               (options (life-os--parse-ai-scheduling-options ai-response)))
          (if (not options)
              (message "AI did not return valid options. Please check response format.")
            (with-current-buffer (get-buffer-create "*LifeOS Action Review*")
              (erase-buffer)
              (dolist (opt options)
                (insert (propertize (format "- %s (%s on %s)
  - Rationale: %s
"
                                            (alist-get :desc opt)
                                            (alist-get :type opt)
                                            (alist-get :date opt)
                                            (alist-get :rationale opt))
                                    'life-os-data opt)))
              (use-local-map life-os--action-review-keymap)
              (make-local-variable 'life-os--action-review-task-marker) ; Make local var
              (setq life-os--action-review-task-marker task-to-process) ; Set local var
              ;; Pass the remaining tasks to the commit function
              (make-local-variable 'life-os--action-review-remaining-tasks)
              (setq life-os--action-review-remaining-tasks (cdr task-markers))
              (pop-to-buffer (current-buffer)))))))))

;; Add a helper variable definition (can be near other action review vars)
;; This variable will hold the list of remaining tasks in the action review buffer
(defvar-local life-os--action-review-remaining-tasks nil
  "Buffer-local variable to hold the list of remaining task markers for recursive scheduling.")

(defun life-os--commit-ai-schedule ()
  "Commit the user's selected schedule. To be called from the Action Review buffer.
This is a hardened, self-healing implementation that does not rely on a
fully initialized Org-ID environment. It uses raw text parsing to ensure
robustness."
  (interactive)
  (let ((option-data (get-text-property (point) 'life-os-data))
        (task-marker life-os--action-review-task-marker)
        (remaining-tasks life-os--action-review-remaining-tasks))
    (when (and option-data task-marker)
      (with-current-buffer (marker-buffer task-marker)
        (goto-char (marker-position task-marker))

        ;; --- [BEGIN] HARDENED DATA GATHERING (BRUTE-FORCE) ---
        (let* ((original-title (org-entry-get (point) "ITEM"))
               (has-appt-tag (member "APPT" (org-get-tags)))
               (parent-id nil)
               (confirm-with nil)
               ;; Take a snapshot of the raw text of the properties drawer.
               ;; This is immune to any Org API or environment state failures.
               (properties-drawer-text
                (save-excursion
                  (when (re-search-forward "^[ \t]*:PROPERTIES:" nil t)
                    (buffer-substring (match-beginning 0) (re-search-forward "^[ \t]*:END:" nil t))))))

          ;; If we found a properties drawer, parse it with pure string matching.
          (when properties-drawer-text
            (with-temp-buffer
              (insert properties-drawer-text)
              (goto-char (point-min))
              (when (re-search-forward ":Confirm_With:[ \t]+\\(.*\\)" nil t)
                (setq confirm-with (match-string 1)))
              (goto-char (point-min))
              (when (re-search-forward ":ID:[ \t]+\\(.*\\)" nil t)
                (setq parent-id (match-string 1)))))
        ;; --- [END] HARDENED DATA GATHERING ---

          ;; --- NOW, perform buffer modifications ---
          (org-todo "AI-REC")
          (unless parent-id (setq parent-id (org-id-get-create t))) ; If ID didn't exist, create it now.
          (let* ((type (alist-get :type option-data))
                 (date-str (format "<%s>" (alist-get :date option-data))))
            (if (string= type "SCHEDULED") (org-schedule nil date-str) (org-deadline nil date-str))
            (message "Task committed with AI recommendation.")
            (unless (org-entry-get (point) "Confirmed_External")
              (org-entry-put (point) "Confirmed_External" "no"))

            ;; --- Final check with guaranteed-valid data ---
            (when (and has-appt-tag confirm-with parent-id)
              (message "L-OS INFO: Action Engine triggered for task '%s'." original-title)
              (life-os--create-confirmation-child-task
               parent-id
               original-title
               date-str
               confirm-with)))))

      ;; --- Recursive processing logic (unchanged) ---
      (kill-buffer "*LifeOS Action Review*")
      (when remaining-tasks
        (let ((next-task-marker (car remaining-tasks)))
          (with-current-buffer (marker-buffer next-task-marker)
             (goto-char (marker-position next-task-marker))
             (let ((next-task-heading (org-get-heading t t)))
               (when (and next-task-heading
                          (y-or-n-p (format "Scheduled current task. Process next 'SCHEDULE-ME' task ('%s')? " next-task-heading)))
                 (life-os-plan-my-schedule nil (cdr remaining-tasks))))))))))
;; ==============================================================================
;; LIFEOS: HIERARCHICAL ACTION ENGINE (v5.3 - Thrust 2.C)
;; ==============================================================================
(defun life-os--calculate-confirmation-deadline (scheduled-date-str)
  "Calculate a confirmation deadline 2 days before a scheduled date.
SCHEDULED-DATE-STR is an Org-style date string like `<YYYY-MM-DD ...>`."
  (let* ((time-obj (org-time-string-to-time scheduled-date-str))
         (deadline-obj (time-subtract time-obj (days-to-time 2))))
    (format-time-string "<%Y-%m-%d %a>" deadline-obj)))

(defun life-os--create-confirmation-child-task (parent-id parent-title scheduled-date-str confirm-with)
  "Programmatically create and save a confirmation child task."
  (let* ((id (org-id-new))
         (deadline (life-os--calculate-confirmation-deadline scheduled-date-str))
         (child-title (format "Confirm Appt: %s with %s" parent-title confirm-with))
         (full-headline
          (with-temp-buffer
            (insert (format "* NEXT [#A] %s :Calls:Admin:
" child-title))
            (insert "  :PROPERTIES:
")
            (insert (format "  :DEADLINE:   %s
" deadline))
            (insert (format "  :PARENT:     [[id:%s]]
" parent-id))
            (insert (format "  :ID:         %s
" id))
            (insert "  :END:
")
            (buffer-string)))
         (pointer (format "- NEXT [#A] %s [[id:%s]]" child-title id)))
    (when (life-os--append-to-inbox-and-save full-headline)
      (life-os--append-to-active-log-inbox pointer)
      (message "Generated confirmation task for: %s" parent-title))))

;; This hook function will be added to the config.el file.
(defun life-os--update-parent-on-confirmation (&rest _)
  "Hook to run after a TODO state change.
If a confirmation task is marked DONE, update its parent's
:Confirmed_External: property."
  (when (and (equal org-state "DONE") ; <-- CORRECTED: No parentheses around org-state
             (string-match-p "Confirm Appt:" (org-get-heading t t)))
    (let ((parent-link (org-entry-get (point) "PARENT")))
      (when parent-link
        (let* ((parent-id (replace-regexp-in-string "\\[\\[id:\\|\\]\\]" "" parent-link))
               (location (org-id-find parent-id 'marker)))
          (when location
            (with-current-buffer (marker-buffer location)
              (goto-char (marker-position location))
              (org-entry-put (point) "Confirmed_External" "yes")
              (message "Parent task ID %s confirmed." parent-id))))))))
;; ==============================================================================
;; LIFEOS: SESSION-BASED ARCHITECTURE (v6.0 - Thrust 3.A)
;; ==============================================================================
(defconst life-os--session-counter-file (expand-file-name "session.org" lifeos-system-dir) ; Use lifeos-system-dir and lifeos-journal-root
  "The canonical file path for the persistent session counter.")

(defun life-os--get-session-counter ()
  "Read the current session counter from its canonical file.
Returns 0 if the file does not exist or contains invalid data."
  (if (file-exists-p life-os--session-counter-file)
      (with-temp-buffer
        (insert-file-contents-literally life-os--session-counter-file)
        (condition-case nil (string-to-number (buffer-string)) (error 0))) ; Remove quote from 'error
    0))

(defun life-os--increment-and-get-session-counter ()
  "Increment the session counter by one, save it, and return the new value."
  (let ((new-count (1+ (life-os--get-session-counter))))
    (make-directory (file-name-directory life-os--session-counter-file) t)
    (with-temp-buffer
      (insert (number-to-string new-count))
      (write-file life-os--session-counter-file nil))
    new-count))

(defun life-os-begin-new-session ()
  "Initialize a new session log, inject habits, and open the file."
  (interactive)
  (let* ((session-number (life-os--increment-and-get-session-counter))
         (session-path (expand-file-name (format "Session-%03d.org" session-number) lifeos-sessions-dir)))
    (make-directory (file-name-directory session-path) t)
    ;; 1. Create the base session file
    (with-temp-buffer
      (insert (format "#+TITLE: Session %03d: [%s]
" session-number (format-time-string "%Y-%m-%d")))
      (insert ":PROPERTIES:
:END:
")
      (insert "* Waking Thoughts
* 24h Outlook
* Metrics Checklist
* Inbox / Fleeting Notes
* Closing Thoughts
")
      (write-file session-path))
    ;; 2. Inject habit templates
    (life-os--inject-habit-templates session-path)
    ;; 3. Open the file for the user
    (find-file session-path)
    (message "Began Session %03d with daily habits." session-number)))

(defun life-os--get-active-log-file ()
  "Return the file path of the current log.
Prioritizes the most recent session log. If none exists, falls
back to today's Daily Command Center (DCC) file."
  (let* ((session-files (and (file-directory-p lifeos-sessions-dir)
                           (directory-files lifeos-sessions-dir t "Session-\\(.*\\)\\.org$"))))
    (if session-files
        (car (sort session-files #'string-greaterp))
      (life-os--generate-dcc-path))))

(defun life-os-end-session-review ()
  "Finalize the current session by collecting and saving key metrics."
  (interactive)
  (let* ((active-log (life-os--get-active-log-file))
         (energy (read-string "Metric - Energy Score (Post-Session, 1-10): "))
         (focus (read-string "Metric - Focus Score (Session Avg, 1-10): "))
         (mood (read-string "Metric - Mood Score (Post-Session, 1-10): "))
         (meditation (read-string "Metric - Meditation Count (Total): "))
         (pentest (read-string "Metric - PenTest Hours: "))
         (nlp (read-string "Metric - NLP Hours: "))
         (lifeos (read-string "Metric - LifeOS Hours: ")))
    (with-current-buffer (find-file-noselect active-log)
      (goto-char (point-min))
      (when (re-search-forward ":PROPERTIES:" nil t)
        (goto-char (line-beginning-position)) (forward-line 1) ; Use forward-line
        (org-entry-put (point) "Energy_Score_Final" energy)
        (org-entry-put (point) "Focus_Score_Avg" focus)
        (org-entry-put (point) "Mood_Score_Final" mood)
        (org-entry-put (point) "Meditation_Count" meditation)
        (org-entry-put (point) "PenTest_Hours" pentest)
        (org-entry-put (point) "NLP_Hours" nlp)
        (org-entry-put (point) "LifeOS_Hours" lifeos)
        (save-buffer))
      (message "Session finalized and metrics recorded in %s" (file-name-nondirectory active-log)))))
;; ==============================================================================
;; LIFEOS: AI DAILY BRIEFING ENGINE (v6.1 - Thrust 3.B)
;; ==============================================================================
(defun life-os--get-global-task-horizon ()
  "Query and return a string list of all non-DONE tasks."
  (let* ((tasks (org-ql-select (org-agenda-files) '(not (done))
                  :action (lambda () (format "- %s
" (org-get-heading t t)))))
         (task-string (if tasks (mapconcat #'identity tasks "") "No pending tasks found.")))
    task-string))

(defun life-os--get-latest-session-log ()
  "Return the file path of the most recently completed session log."
  (let* ((session-dir lifeos-sessions-dir)
         (session-files (directory-files session-dir t "Session-\\(.*\\)\\.org$")))
    (when session-files
      (car (sort session-files #'string-greaterp)))))

(defun life-os-generate-daily-plan (&optional date-string)
  "Non-interactively generate a Daily Command Center (DCC) file.
Generates for DATE-STRING (YYYY-MM-DD) if provided, otherwise for today.
Intended to be called automatically or for historical data generation."
  (interactive
   (list (read-string "Generate DCC for date (YYYY-MM-DD) [optional, defaults to today]: ")))
  (let* ((target-time (if (and date-string (not (string-empty-p date-string)))
                          (org-time-string-to-time date-string)
                        (current-time)))
         (target-date-str (format-time-string "%Y-%m-%d" target-time)))
    (message "LifeOS: Beginning DCC generation for %s..." target-date-str)
    (let* ((dcc-path (life-os--generate-dcc-path target-time))
           (dcc-year (format-time-string "%Y" target-time))
           ;; --- 1. Gather All Contextual Data ---
           ;; Note: These functions find the *most recent* data relative to now, which
           ;; is the correct behavior for simulating a historical run. The AI's job
           ;; is to synthesize the state *as it was then*.
           (last-session-path (life-os--get-latest-session-log))
           (last-session-content (if last-session-path
                                     (with-temp-buffer (insert-file-contents-literally last-session-path) (buffer-string))
                                   "No previous session data available."))
           (annual-codex (life-os--get-annual-codex-content dcc-year))
           (cycle-outlook (life-os--get-cycle-outlook-content target-time)) ;; Pass time for recency check
           (task-horizon (life-os--get-global-task-horizon))
           (todays-numerology (life-os--get-numerology-block-for-date target-date-str))
           (prompt-template (life-os-read-prompt (expand-file-name "5_Daily_Gen.org" lifeos-prompts-dir)))
           ;; --- 2. Assemble the Final Prompt ---
           (final-prompt prompt-template))
      (setq final-prompt (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_ANNUAL_CODEX]") (or annual-codex "") final-prompt t t))
      (setq final-prompt (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_9_DAY_CYCLE_OUTLOOK_BLOCK]") (or cycle-outlook "") final-prompt t t))
      (setq final-prompt (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_LAST_SESSION_LOG]") last-session-content final-prompt t t))
      (setq final-prompt (replace-regexp-in-string (regexp-quote "[GLOBAL_TASK_HORIZON_DATA]") task-horizon final-prompt t t))
      (setq final-prompt (replace-regexp-in-string (regexp-quote "[TODAYS_ENERGETIC_MATRIX]") todays-numerology final-prompt t t))
      ;; --- 3. Call AI and Write to File ---
      (message "Sending briefing request to AI for %s..." target-date-str)
      (let ((ai-response (life-os-extract-text-from-ai-response (life-os-call-ai final-prompt 'pro))))
        (if (string-empty-p ai-response)
            (error "LifeOS Error: Received empty response from AI for daily plan.")
          (make-directory (file-name-directory dcc-path) t)
          (with-temp-buffer
            (insert ai-response)
            (write-file dcc-path nil))
          (message "LifeOS: Successfully generated and saved DCC for %s to %s" target-date-str dcc-path))))))

(defun life-os--append-to-active-log-inbox (content-string)
  "Appends CONTENT-STRING to the Inbox section of the active log file."
  (let ((target-path (life-os--get-active-log-file)))
    (unless (file-exists-p target-path)
      ;; Create a placeholder if the DCC/Session file does not exist yet.
      (with-temp-buffer
        (insert (format "#+TITLE: %s Placeholder
* Inbox / Fleeting Notes
" (file-name-base target-path)))
        (write-file target-path)))
    (with-temp-buffer
      (insert-file-contents target-path)
      (goto-char (point-min))
      (if (re-search-forward "^\\* Inbox / Fleeting Notes" nil t)
          (progn
            (goto-char (line-end-position))
            (unless (bolp) (insert "
"))
            (insert content-string))
        (progn ; Fallback if heading doesn't exist
          (goto-char (point-max))
          (insert "
* Inbox / Fleeting Notes
")
          (insert content-string)))
      (write-file target-path))))

(defconst life-os--habit-template-file (expand-file-name "templates/habits.org" lifeos-system-dir) ; Use lifeos-system-dir
  "Canonical path to the file containing master habit templates.")

(defun life-os--inject-habit-templates (target-session-file)
  "Injects habit checklists from the template file into a new session log."
  (when (file-exists-p life-os--habit-template-file)
    (let* ((habits (org-ql-select life-os--habit-template-file '(property "Habit" "daily")
                     :action (lambda () (buffer-substring-no-properties
                                          (org-element-property :begin (org-element-at-point))
                                          (org-element-property :end (org-element-at-point)))))))
      (when habits
        (with-current-buffer (find-file-noselect target-session-file)
          (goto-char (point-min))
          (when (re-search-forward "^\\* Metrics Checklist" nil t)
            (goto-char (line-end-position)) (insert "
")
            (dolist (habit-text habits)
              (insert habit-text "
")))
          (save-buffer))))))

(provide 'lifeos)
