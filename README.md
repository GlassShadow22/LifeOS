# LifeOS: A Session-Based, AI-Augmented Environment in Emacs

LifeOS transforms personal information management from a passive archival activity into an active, intelligent, and conversational partnership. It is a complete system, built within Doom Emacs, that leverages a session-based workflow and AI-driven synthesis to create a powerful environment for focus and productivity.

## Core Philosophy

- **Frictionless Capture:** Get ideas out of your head and into the system instantly.
- **Scheduled Synthesis:** Enforce a disciplined rhythm of review, transforming raw data into structured plans.
- **AI as Chief of Staff:** Utilize AI for non-interactive, data-heavy synthesis to produce high-signal briefings.
- **Session-Based Environment:** Frame work in discrete, data-rich session logs that become the ground truth for automated review.

## Architecture

LifeOS is a five-layer system designed for clarity and separation of concerns:

1.  **Automation Layer (`systemd`):** External triggers for non-interactive functions.
2.  **Logic Layer (`lifeos.el`):** The backend engine containing all core functions.
3.  **AI Layer (`prompts/`):** The master prompt library defining all AI personas and behaviors.
4.  **Data Layer (`~/journal/`):** The user's collection of Org files; the "world" the system operates on.
5.  **Interface Layer (`config.el`):** The frontend connecting user input to backend logic.

## Setup & Installation

1.  **Dependencies:** This system is built for Doom Emacs and requires `org-ql`, `plz`, and `json`.
2.  **Clone:** Clone this repository. Place `lifeos.el` in your `~/.doom.d/lisp/` directory and copy the `prompts/` directory to your journal root.
3.  **Configure:** Integrate the contents of `config.el` into your personal configuration. You **must** set the `lifeos-journal-root` and other path variables.
4.  **API Key:** Add your Gemini API key to your `~/.authinfo.gpg` file with the host `apikey.generativelanguage.googleapis.com`.
5.  **Directory Structure:** The system requires a specific directory structure within your journal root. Create the following: `/logs/sessions/`, `/outlooks/`, `/worksheets/`, `/templates/`, and `/.system/`.

---