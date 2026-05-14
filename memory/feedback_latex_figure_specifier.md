---
name: LaTeX figure placement specifier
description: Use [pos=H] not [H] for figure environments in battery_paper.tex
type: feedback
---

Always use `[pos=H]` as the float specifier for `\begin{figure}` in this project's LaTeX files (e.g., `battery_paper.tex`). Do NOT change it to `[H]`.

**Why:** The document class or template uses `pos=H` as the correct specifier for forced placement. The user confirmed this is intentional.

**How to apply:** When writing or editing any `\begin{figure}` environment in this project, always write `\begin{figure}[pos=H]`.
