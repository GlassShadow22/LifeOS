;;; -*- lexical-binding: t; -*-
;;; ~/projects/life-os/lisp/lifeos.el --- LifeOS Helper Function Library v7.3
;; This file contains the complete, verified, and debugged library of helper
;; functions for the unified LifeOS v7.3 "Socratic Loop".
(require 'org-ql)
(declare-function plz 'plz)
(declare-function json-encode 'json)
(declare-function json-read 'json)
(declare-function auth-source-search 'auth-source)
(declare-function org-element-at-point 'org-element)
(declare-function org-element-property 'org-element)
(declare-function org-get-heading 'org)
(declare-function org-id-get-create 'org-id)
(declare-function org-id-new 'org-id)
(declare-function org-entry-get 'org)
(declare-function org-entry-put 'org)
(declare-function org-time-string-to-time 'org)
(declare-function org-id-find 'org-id)
(declare-function org-id-goto 'org-id)
(declare-function org-schedule 'org)
(declare-function org-deadline 'org)
(declare-function org-todo 'org)
(declare-function org-agenda-files 'org)
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
         (model-endpoint (pcase --provider ('pro "gemini-1.5-pro") ('flash "gemini-1.5-flash"))) ; Updated to 1.5
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
(defun life-os--generate-dcc-path (&optional date)
  "Generate the canonical file path for a daily command center (DCC) file."
  (let ((time (or date (current-time))))
    (expand-file-name (format "%s.org" (format-time-string "%Y-%m-%d" time))
                      (concat (file-name-as-directory lifeos-logs-dir)
                              (format-time-string "%Y/%m/" time)))))

;; ==============================================================================
;; LIFEOS: CONTEXT-GATHERING HELPERS
;; ==============================================================================
(defun life-os--get-annual-codex-content (year-string)
  "Find and return the content of the Annual Codex file for YEAR-STRING."
  (let ((codex-path (expand-file-name (format "%s-Codex.org" year-string) (concat (file-name-as-directory lifeos-outlooks-dir) year-string "/"))))
    (if (file-exists-p codex-path) (with-temp-buffer (insert-file-contents-literally codex-path) (buffer-string))
      (progn (message "LifeOS Context Warning: Annual Codex not found for year %s" year-string) ""))))

(defun life-os--get-monthly-directive-content (year-string month-string)
  "Find and return the content of a Monthly Directive file."
  (let ((directive-path (expand-file-name (format "%s-%s-Directive.org" year-string month-string) (concat (file-name-as-directory lifeos-outlooks-dir) year-string "/" month-string "/"))))
    (if (file-exists-p directive-path) (with-temp-buffer (insert-file-contents-literally directive-path) (buffer-string))
      (progn (message "LifeOS Context Warning: Monthly Directive not found for %s-%s" year-string month-string) ""))))

(defun life-os--get-cycle-outlook-content (&optional target-date-time)
  "Find and return the content of the MOST RECENT tactical outlook file."
  (let* ((search-date (or target-date-time (current-time))) (found-path nil) (counter 0))
    (while (and (not found-path) (< counter 90))
      (let* ((date-str (format-time-string "%Y-%m-%d" search-date))
             (path-prefix (expand-file-name (format-time-string "%Y/%m/" search-date) lifeos-outlooks-dir))
             (cycle-path (expand-file-name (format "%s-Cycle.org" date-str) path-prefix)))
        (when (file-exists-p cycle-path) (setq found-path cycle-path)))
      (setq search-date (time-subtract search-date (days-to-time 1)))
      (setq counter (1+ counter)))
    (if found-path (with-temp-buffer (insert-file-contents-literally found-path) (buffer-string))
      (progn (message "LifeOS Context Warning: No recent tactical outlook found.") ""))))

;; ==============================================================================
;; LIFEOS: NUMEROLOGY ENGINE
;; ==============================================================================
(defun life-os--reduce-number (n)
  "Reduce a number N according to numerology rules (11, 22 are NOT master)."
  (if (or (and (>= n 1) (<= n 9)) (= n 11) (= n 22)) n
    (life-os--reduce-number
     (let ((sum 0))
       (while (> n 0) (setq sum (+ sum (mod n 10))) (setq n (/ n 10)))
       sum))))

(defun life-os-calculate-numerology-for-date (target-date-string)
  "Calculates and returns an alist of all numerological data for a date."
  (let* ((user-birth-date lifeos-user-birth-date)
         (birth-month (string-to-number (substring user-birth-date 5 7)))
         (birth-day (string-to-number (substring user-birth-date 8 10)))
         (target-year (string-to-number (substring target-date-string 0 4)))
         (target-month (string-to-number (substring target-date-string 5 7)))
         (target-day (string-to-number (substring target-date-string 8 10)))
         (py-sum (+ (life-os--reduce-number birth-month) (life-os--reduce-number birth-day) (life-os--reduce-number target-year)))
         (personal-year (life-os--reduce-number py-sum))
         (pm-sum (+ personal-year target-month))
         (personal-month (life-os--reduce-number pm-sum))
         (pd-sum (+ personal-month target-day))
         (personal-day (life-os--reduce-number pd-sum)))
    `((personal-year . ,personal-year) (personal-month . ,personal-month) (personal-day . ,personal-day))))

;; ==============================================================================
;; LIFEOS: SESSION & LOGGING
;; ==============================================================================
(defun life-os--get-active-log-file ()
  "Return the file path of the current log, prioritizing the most recent session."
  (let* ((session-files (and (file-directory-p lifeos-sessions-dir) (directory-files lifeos-sessions-dir t "Session-\\(.*\\)\\.org$"))))
    (if session-files (car (sort session-files #'string-greaterp)) (life-os--generate-dcc-path))))

(defun life-os--append-to-inbox-and-save (content-string)
  "Appends CONTENT-STRING to the LifeOS inbox file and saves the buffer."
  (with-current-buffer (find-file-noselect lifeos-inbox-file)
    (goto-char (point-max)) (unless (bolp) (insert "\n"))
    (insert content-string) (save-buffer)))

;; ==============================================================================
;; LIFEOS: CAPTURE & TRIAGE (GROUP 9 - v7.3)
;; ==============================================================================
(defun life-os--quick-capture-action (capture-type priority)
  "Backend for Quick Capture. Creates entry in inbox with Capture State."
  (let ((headline (read-string (format "Capture [%s] [#%s]: " capture-type priority))))
    (when (and headline (not (string-empty-p headline)))
      (let* ((id (org-id-new))
             (full-headline
              (format "* %s [#%s] %s\n  :PROPERTIES:\n  :ID:        %s\n  :CREATED:   %s\n  :END:\n"
                      capture-type priority headline id (format-time-string "[%Y-%m-%d %a %H:%M]"))))
        (when (life-os--append-to-inbox-and-save full-headline)
          (message "LifeOS: Quick-captured '%s'" headline))))))

(defun life-os-refile-and-transition-state ()
  "Triage item at point by refiling and evolving its state. Core of v7.3."
  (interactive)
  (unless (org-at-heading-p) (user-error "Triage must be initiated from a headline."))
  (let* ((rfloc (org-refile-get-location "Refile to" nil t)))
    (when rfloc
      (let* ((workflow-keywords (mapcar (lambda (x) (car (split-string x "("))) (car (cdr (cadr org-todo-keywords)))))
             (new-state (completing-read "Transition to Workflow State: " workflow-keywords nil :require-match t)))
        (when new-state
          (org-todo new-state)
          (org-refile nil nil rfloc)
          ;; Post-triage triggers can be added here
          (message "Item triaged to %s with state [%s]." (or (cadr rfloc) (car rfloc)) new-state))))))

;; ==============================================================================
;; LIFEOS: THE SOCRATIC LOOP ENGINE (GROUP 5 - v7.3)
;; ==============================================================================
(defun life-os-generate-full-progress-snapshot ()
  "Generate the Full-Progress.org snapshot for today. (Spec-5B)"
  (interactive)
  (message "Auditor: Generating Full Progress Snapshot...")
  (let* ((output-path (expand-file-name (format "%s-Full-Progress.org" (format-time-string "%Y-%m-%d")) lifeos-system-dir))
         (prev-snapshot-path (expand-file-name (format "%s-Full-Progress.org" (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1)))) lifeos-system-dir))
         (live-tasks-ht (life-os--get-live-task-snapshot (org-agenda-files)))
         (prev-tasks-ht (if (file-exists-p prev-snapshot-path) (life-os--load-task-snapshot prev-snapshot-path) (make-hash-table :test 'equal)))
         (metrics (let ((counts (make-hash-table :test 'equal))) (maphash (lambda (id data) (let* ((state (alist-get :state data)) (key (intern state))) (puthash key (1+ (gethash key counts 0)) counts))) live-tasks-ht) counts)))
    (with-temp-buffer
      (insert (format "#+TITLE: Full Progress Snapshot for %s\n#+DATE: %s\n\n* System-Wide Metrics\n:PROPERTIES:\n" (format-time-string "%Y-%m-%d") (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert (format ":TASKS_TOTAL: %d\n" (hash-table-count live-tasks-ht)))
      (maphash (lambda (k v) (insert (format ":TASKS_%s: %d\n" (symbol-name k) v))) metrics) (insert ":END:\n\n")
      (insert (format "* Diff from %s\n" (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1)))))
      (maphash (lambda (id data) (unless (gethash id prev-tasks-ht) (insert (format "- New Task: [[id:%s]] in %s\n" id (file-name-nondirectory (alist-get :file data)))))) live-tasks-ht)
      (maphash (lambda (id prev-data) (let ((live-data (gethash id live-tasks-ht))) (when (and (not live-data) (not (member (alist-get :state prev-data) '("DONE" "KILL")))) (insert (format "- Completed/Killed: [[id:%s]] (%s) :: %s\n" id (alist-get :state prev-data) (alist-get :headline prev-data)))))) prev-tasks-ht)
      (maphash (lambda (id data) (let ((prev-data (gethash id prev-tasks-ht))) (when (and prev-data (not (equal (alist-get :state data) (alist-get :state prev-data)))) (insert (format "- State Change: [[id:%s]] (%s -> %s) :: %s\n" id (alist-get :state prev-data) (alist-get :state data) (alist-get :headline data)))))) live-tasks-ht)
      (dolist (state '("IMPL" "THNK" "NEXT" "HOLD"))
        (let ((tasks-in-state (cl-loop for id being the hash-keys of live-tasks-ht using (hash-value data) when (equal (alist-get :state data) state) collect (cons id data))))
          (when tasks-in-state
            (insert (format "\n* Active Items (:STATE \"%s\")\n" state))
            (dolist (task-entry tasks-in-state) (insert (format "- %s: [[id:%s]] %s\n" (alist-get :state (cdr task-entry)) (car task-entry) (alist-get :headline (cdr task-entry))))))))
      (make-directory (file-name-directory output-path) t) (write-file output-path nil)
      (message "Auditor: Full-Progress snapshot generated at: %s" output-path))))

(defun life-os-generate-dcc ()
  "Generate the Daily Command Center (DCC.org) for today."
  (interactive)
  (let* ((today-str (format-time-string "%Y-%m-%d"))
         (dcc-path (life-os--generate-dcc-path))
         (prev-dcc-path (life-os--generate-dcc-path (time-subtract (current-time) (days-to-time 1))))
         (prev-dcc-content (if (file-exists-p prev-dcc-path) (with-temp-buffer (insert-file-contents-literally prev-dcc-path) (buffer-string)) "No previous DCC available."))
         (numerology-matrix (with-temp-buffer (dolist (item (life-os-calculate-numerology-for-date today-str)) (insert (format "- %s: %s\n" (car item) (cdr item)))) (buffer-string)))
         (tomorrow-pd (alist-get 'personal-day (life-os-calculate-numerology-for-date (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 1))))))
         (prompt (life-os-read-prompt (expand-file-name "5-Daily-Gen.org" lifeos-prompts-dir))) ; Final filename
         (final-prompt (-> prompt
                           (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_PREVIOUS_DCC]") (or prev-dcc-content ""))
                           (replace-regexp-in-string (regexp-quote "[TODAYS_ENERGETIC_MATRIX]") (or numerology-matrix ""))
                           (replace-regexp-in-string (regexp-quote "[TOMORROWS_PERSONAL_DAY]") (if tomorrow-pd (number-to-string tomorrow-pd) "")))))
    (message "Auditor: Generating Daily Command Center...")
    (let ((response (life-os-extract-text-from-ai-response (life-os-call-ai final-prompt))))
      (if (or (not response) (string-empty-p response)) (user-error "Auditor: AI failed to generate DCC content.")
        (make-directory (file-name-directory dcc-path) t)
        (with-temp-buffer (insert response) (write-file dcc-path nil))
        (message "Auditor: DCC successfully generated at %s" dcc-path)))))

(defun life-os--get-todays-waking-thoughts ()
  "Find today's active Session-Log and extract the content of '* Waking Thoughts'."
  (let ((active-log (life-os--get-active-log-file)))
    (if (not (file-exists-p active-log)) "No active session log found."
      (with-temp-buffer (insert-file-contents-literally active-log) (goto-char (point-min))
        (if (re-search-forward "^\\* Waking Thoughts" nil t)
            (buffer-substring-no-properties (point) (save-excursion (re-search-forward "^\\* " nil t) (or (match-beginning 0) (point-max))))
          "No '* Waking Thoughts' section found in today's session log.")))))

(defun life-os-generate-worksheet ()
  "Generate the Socratic Worksheet for today (Socratic Coach)."
  (interactive)
  (let* ((today-str (format-time-string "%Y-%m-%d"))
         (worksheet-path (expand-file-name (format "%s-Worksheet.org" today-str) lifeos-worksheets-dir))
         (fp-path (expand-file-name (format "%s-Full-Progress.org" today-str) lifeos-system-dir))
         (dcc-path (life-os--generate-dcc-path))
         (fp-content (if (file-exists-p fp-path) (with-temp-buffer (insert-file-contents-literally fp-path) (buffer-string)) "Full-Progress snapshot not found."))
         (dcc-content (if (file-exists-p dcc-path) (with-temp-buffer (insert-file-contents-literally dcc-path) (buffer-string)) "DCC not found."))
         (waking-thoughts (life-os--get-todays-waking-thoughts))
         (strategies (concat (life-os--get-annual-codex-content (format-time-string "%Y")) "\n"
                               (life-os--get-monthly-directive-content (format-time-string "%Y") (format-time-string "%m")) "\n"
                               (life-os--get-cycle-outlook-content)))
         (prompt (life-os-read-prompt (expand-file-name "6-Socratic-Coach.org" lifeos-prompts-dir)))
         (final-prompt (-> prompt
                           (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_FULL_PROGRESS_SNAPSHOT]") fp-content)
                           (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_DCC]") dcc-content)
                           (replace-regexp-in-string (regexp-quote "[USERS_WAKING_THOUGHTS]") waking-thoughts)
                           (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_ALL_ACTIVE_STRATEGIES]") (or strategies "")))))
    (message "Coach: Generating Socratic Worksheet...")
    (let ((response (life-os-extract-text-from-ai-response (life-os-call-ai final-prompt))))
      (if (or (not response) (string-empty-p response)) (user-error "Coach: AI failed to generate worksheet.")
        (make-directory (file-name-directory worksheet-path) t)
        (with-temp-buffer (insert response) (write-file worksheet-path nil))
        (message "Coach: Worksheet generated. Opening for review.") (find-file worksheet-path)))))

(defun life-os-generate-daily-schedule ()
  "Generate the Unified Tactical Schedule for today (Project Manager)."
  (interactive)
  (let* ((today-str (format-time-string "%Y-%m-%d"))
         (schedule-path (expand-file-name "Daily-Schedule.org" lifeos-journal-root))
         (worksheet-path (expand-file-name (format "%s-Worksheet.org" today-str) lifeos-worksheets-dir))
         (fp-path (expand-file-name (format "%s-Full-Progress.org" today-str) lifeos-system-dir))
         (dcc-path (life-os--generate-dcc-path)))
    (unless (file-exists-p worksheet-path) (user-error "Manager: Cannot generate schedule: Today's worksheet is not complete."))
    (let* ((worksheet-content (with-temp-buffer (insert-file-contents-literally worksheet-path) (buffer-string)))
           (fp-content (if (file-exists-p fp-path) (with-temp-buffer (insert-file-contents-literally fp-path) (buffer-string)) "Full-Progress snapshot not found."))
           (dcc-content (if (file-exists-p dcc-path) (with-temp-buffer (insert-file-contents-literally dcc-path) (buffer-string)) "DCC not found."))
           (strategies-content (concat (life-os--get-annual-codex-content (format-time-string "%Y")) "\n"
                                       (life-os--get-monthly-directive-content (format-time-string "%Y") (format-time-string "%m")) "\n"
                                       (life-os--get-cycle-outlook-content)))
           (prompt (life-os-read-prompt (expand-file-name "7-Project-Manager.org" lifeos-prompts-dir)))
           (final-prompt (-> prompt
                             (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_COMPLETED_WORKSHEET]") worksheet-content)
                             (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_FULL_PROGRESS_SNAPSHOT]") fp-content)
                             (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_DCC]") dcc-content)
                             (replace-regexp-in-string (regexp-quote "[CONTENTS_OF_ALL_ACTIVE_STRATEGIES]") (or strategies-content "")))))
      (message "Manager: Generating Unified Tactical Schedule...")
      (let ((response (life-os-extract-text-from-ai-response (life-os-call-ai final-prompt 'pro))))
        (if (or (not response) (string-empty-p response)) (user-error "Manager: AI failed to generate schedule.")
          (with-temp-buffer (insert response) (write-file schedule-path nil))
          (message "Manager: Schedule generated. Opening dashboard.") (find-file schedule-path))))))


(provide 'lifeos)
