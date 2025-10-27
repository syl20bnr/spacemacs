#!/bin/sh
#
# ================================================
#  AI BLUEPRINT SYNC SCRIPT (Spacemacs)
# ================================================
#
# This script copies the master coding blueprint from the `ai/` directory
# (the Single Source of Truth) to the `.github/` directory where
# GitHub tools (like Copilot) expect it.
#
# WHY: This avoids cross-platform symlink issues (especially for Windows).
#
# WHEN TO RUN: Run this script EVERY time you modify the master file
#              `ai/coding_ai.md`.
#
# USAGE: From the repository root, run:
#        ./ai/sync-blueprints.sh
#

# --- Configuration ---
AI_DIR="ai"
CODING_MASTER="coding_ai.md"
GITHUB_DIR=".github"

# --- Script ---
echo "Syncing AI Coding Blueprint for GitHub..."

# 1. Sync for GitHub Copilot (Repository Level)
cp -v "$AI_DIR/$CODING_MASTER" "$GITHUB_DIR/copilot-instructions.md"

echo ""
echo "Sync complete. Please 'git add' and 'git commit' the following file:"
echo "  - $GITHUB_DIR/copilot-instructions.md"
echo "  - (and your changes in $AI_DIR/$CODING_MASTER)"
