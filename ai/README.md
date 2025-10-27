# AI Collaboration Blueprint

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [AI Collaboration Blueprint](#ai-collaboration-blueprint)
  - [How to Use This System](#how-to-use-this-system)
    - [For Maintainers (Using the General AI)](#for-maintainers-using-the-general-ai)
    - [For Developers (Configuring the Specialist AI)](#for-developers-configuring-the-specialist-ai)
    - [For Maintainers (Updating the Blueprints)](#for-maintainers-updating-the-blueprints)
  - [Multi-AI "Factory" Workflows](#multi-ai-factory-workflows)
    - [Example 1: New Feature (From Idea to Code)](#example-1-new-feature-from-idea-to-code)
      - [Phase 1: The Blueprint (Interaction with General AI)](#phase-1-the-blueprint-interaction-with-general-ai)
      - [Phase 2: The Implementation (Interaction with Specialist AI)](#phase-2-the-implementation-interaction-with-specialist-ai)
    - [Example 2: Bug Fixing & Refactoring](#example-2-bug-fixing--refactoring)
      - [Phase 1: The Analysis (Interaction with General AI)](#phase-1-the-analysis-interaction-with-general-ai)
      - [Phase 2: The Repair (Interaction with Specialist AI)](#phase-2-the-repair-interaction-with-specialist-ai)

<!-- markdown-toc end -->

This directory serves as the "brain" for AI-assisted development in the Spacemacs project. It follows a "two-AI" model:

1.  **The Strategist (`general_ai.md`):** A high-level briefing for generalist AIs (like Gemini) used for planning, architecture, and documentation.
2.  **The Specialist (`coding_ai.md`):** A detailed, rule-based instruction set for specialist coding AIs (like GitHub Copilot).

These files are the **Single Source of Truth** for our AI collaboration.

## How to Use This System

### For Maintainers (Using the General AI)

When planning new features, refactoring, or writing documentation, provide the `general_ai.md` file as context.

**Example (Gemini):**
> "Please review the `general_ai.md` file. Acting as an **Architect,** propose a new layer structure for..."

### For Developers (Configuring the Specialist AI)

For developers using AI tools, this directory is the source for the rules.

**GitHub Copilot (Repository Configuration):**

This repository is configured so that GitHub's native tools (like Copilot in PRs) automatically use our rules. The file at `.github/copilot-instructions.md` is a **synced copy** of `ai/coding_ai.md`.

**Other AI Tools (in Emacs, etc.):**

If you use a different AI tool inside Spacemacs, you can manually load the `ai/coding_ai.md` or `ai/general_ai.md` files as context for your prompts.

### For Maintainers (Updating the Blueprints)

The files in the `ai/` directory are the **Single Source of Truth**.

The file in `.github/` is a **synced copy** used for tool integration and cross-platform compatibility (to avoid Symlink problems on Windows).

**!! IMPORTANT !!**
If you edit `ai/coding_ai.md`, you **MUST** run the sync script to propagate your changes.

**How to run the script (from the repository root):**
```bash
./ai/sync-blueprints.sh
```
After running the script, `git add` and `git commit` both your changes in `ai/coding_ai.md` and the newly updated copy in `.github/copilot-instructions.md`.

---

## Multi-AI "Factory" Workflows

The true power of this system is chaining the AIs together. Use the **General AI** to create a high-level plan (the "Blueprint"), then feed that blueprint to the **Specialist AI** for flawless implementation and fix the details iteratively. Please remember that AIs will make mistakes. So go ahead and review the code and test it. Please do not push code you do not understand!

### Example 1: New Feature (From Idea to Code)

#### Phase 1: The Blueprint (Interaction with General AI)

**Your Prompt to the General AI:**
> "I want a 'Layer Toggler' to enable/disable layers in `.spacemacs` without manual editing.
>
> 1.  **As a Requirements Engineer,** define the MVP requirements.
> 2.  **As a UI Designer,** create an ASCII-mockup for the buffer.
> 3.  **As an Architect,** outline the technical implementation plan."

**General AI Response (The Blueprint):**
> Here is the blueprint for the 'Layer Toggler':
>
> **1. Requirements (Requirements Engineer):**
> * Must read and parse `~/.spacemacs`.
> * Must identify the `dotspacemacs-configuration-layers` list.
> * Must distinguish between active (e.g., `org`) and inactive (e.g., `;; python`) layers.
> * User must be able to toggle a layer's state via a keypress.
> * A "save" action must overwrite the `.spacemacs` file with the changes.
>
> **2. UI Mockup (UI Designer):**
> ```text
> +--- Spacemacs Layer Toggler ---+
> | [x] org                     |
> | [x] git                     |
> | [ ] python                  |
> +--- (t)oggle [s]ave [q]uit ---+
> ```
>
> **3. Technical Plan (Architect):**
> * Create an interactive command `my/toggle-layers`.
> * This command will read the dotfile, parse the layer list, and generate the UI in a new, read-only buffer (`*layer-toggler*`).
> * It must define local keymaps for `t` (toggle), `s` (save), and `q` (quit).
> * The `save` function will rewrite the `~/.spacemacs` file, commenting or un-commenting the layer lines as needed.

#### Phase 2: The Implementation (Interaction with Specialist AI)

**Your Prompt to the Specialist AI:**
> "Here is a detailed blueprint from the General AI. Please implement it.
>
> [Paste the entire "Blueprint" response from Phase 1 here]
>
> Your tasks:
> 1.  **As a Coder,** write the complete Elisp code for the `my/toggle-layers` command and all required helper functions based on this plan.
> 2.  **As a Documentation Writer,** provide a `spacemacs/set-leader-keys` example to bind this new command to `SPC a L`."

**Specialist AI Response:**
> ```elisp
> ;; -*- lexical-binding: t; -*-
>
> (defun my/toggle-layers ()
>   "Read .spacemacs and present a buffer to toggle layers."
>   (interactive)
>   (let* ((dotfile-path "~/.spacemacs")
>          (dotfile-content (f-read-text dotfile-path))
>          ;; ...parsing logic here...
>         )
>     (with-current-buffer (get-buffer-create "*layer-toggler*")
>       ;; ...UI generation logic here...
>       (use-local-map
>        (let ((map (make-sparse-keymap)))
>          (define-key map (kbd "t") 'my/layer-toggle-at-point)
>          (define-key map (kbd "s") 'my/layer-save-buffer)
>          (define-key map (kbd "q") 'quit-window)
>          map))
>       (setq buffer-read-only t)
>       (pop-to-buffer (current-buffer)))))
>
> ;; ...implementation of helper functions...
>
> ;; Keybinding Example:
> (spacemacs/set-leader-keys "a L" 'my/toggle-layers)
> ```
>
> **Explanation:**
> -   **Concept:** This command uses a temporary buffer...
> -   **Pitfalls:** This assumes a simple `.spacemacs` format...

### Example 2: Bug Fixing & Refactoring

#### Phase 1: The Analysis (Interaction with General AI)

**Your Prompt to the General AI:**
> "This old function blocks Emacs and sometimes errors:
> `(defun my/old-search () (interactive) (message (concat "Found: " (shell-command-to-string "grep -r 'TODO' ~/projects"))))`
>
> 1.  **As an Architect,** what is the core problem here?
> 2.  **As a Requirements Engineer,** define the requirements for a modern, non-blocking solution."

**General AI Response (The Blueprint):**
> **1. Analysis (Architect):**
> * **Problem:** `shell-command-to-string` is a **synchronous** (blocking) process. Emacs freezes until the `grep` command completes, which could take minutes. The error occurs if `grep` finds nothing and returns `nil`, which `concat` cannot handle.
> * **Solution:** The function must be rewritten to be **asynchronous**.
>
> **2. Requirements (Requirements Engineer):**
> 1.  The function MUST use `start-process` to run `grep` asynchronously.
> 2.  It MUST define a "process sentinel" (a callback function).
> 3.  When the process ends, the sentinel MUST collect the output and place it in a new buffer (e.g., `*grep-results*`).

#### Phase 2: The Repair (Interaction with Specialist AI)

**Your Prompt to the Specialist AI:**
> "Here is the analysis and blueprint from the General AI for fixing my broken function.
>
> **Old Code:**
> `(defun my/old-search () (interactive) (message (concat "Found: " (shell-command-to-string "grep -r 'TODO' ~/projects"))))`
>
> **Blueprint:**
> [Paste the "Blueprint" response from Phase 1 here]
>
> Your tasks:
> 1.  **As a Refactorer,** rewrite `my/old-search` as `my/async-search`, implementing the asynchronous plan exactly.
> 2.  **As a Test Engineer,** explain how I would manually test that this async function works and no longer blocks."

**Specialist AI Response:**
> **1. Refactored Code (Refactorer):**
> ```elisp
> ;; -*- lexical-binding: t; -*-
>
> (defun my/async-search-sentinel (process event)
>   "Sentinel function to handle output from async-search."
>   (when (memq (process-status process) '(exit signal))
>     (with-current-buffer (get-buffer-create "*grep-results*")
>       (insert (with-current-buffer (process-buffer process)
>                 (buffer-string)))
>       (pop-to-buffer (current-buffer)))
>     (delete-process process)))
>
> (defun my/async-search ()
>   "Run 'grep -r TODO ~/projects' asynchronously."
>   (interactive)
>   (let ((process (start-process "my-grep"
>                                 "*grep-results*" ; Output buffer
>                                 "grep" "-r" "TODO" "~/projects")))
>     (set-process-sentinel process 'my/async-search-sentinel)
>     (message "Async grep started...")))
> ```
>
> **2. Manual Test Plan (Test Engineer):**
> 1.  Run `M-x my/async-search`.
> 2.  **Verify:** Emacs does *not* freeze. You can still type and move.
> 3.  **Verify:** After a few moments, the `*grep-results*` buffer appears with your TODO items.
>
> **Explanation:**
> -   **Concept:** We use `start-process` to launch `grep` in the background...
