(defvar exwm-terminal-command "xterm"
  "Terminal command to run.")

(defvar exwm-locking-command "slock"
  "Command to run when locking session")

(defvar exwm-hide-tiling-modeline nil
  "Whether to hide modeline.")

(defvar exwm-leader-key nil
  "Key to use for EXWM global commands")

(defvar exwm-workspace-switch-wrap t
  "Whether `exwm/exwm-workspace-next' and `exwm/exwm-workspace-prev' should wrap.")

(defvar exwm-workspace-number nil
  "Number of workspaces. Defaults to the number of connected displays if `nil'.")

(defvar exwm-randr-command nil
  "`xrandr' command to set up displays prior to EXWM init.")
