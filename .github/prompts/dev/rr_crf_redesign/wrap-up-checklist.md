# RR/CRF Redesign Wrap-up Checklist

Use this checklist before returning to a new implementation slice or after
mentor feedback changes the plan.

## 1. Verify Current Worktree

- Confirm the active worktree is `/Users/miao/Developer/creahia`.
- Confirm the active branch is `feat/unify_rr_and_crf`.
- Check whether there are uncommitted files before editing docs.
- Note if the local branch is ahead of or behind the remote.

## 2. Compare Implementation with Planning Docs

Review current code and tests against:

- `00-roadmap.md`
- the relevant step file
- `mentor-feedback.md`
- `decision-log.md`

Classify each mismatch as one of:

- implementation completed, document outdated
- implementation missing, document still correct
- implementation and document disagree, needs user confirmation

## 3. Update Planning State

- Update `00-roadmap.md` for overall status and next slices.
- Update the relevant step file with implementation details and verification.
- Update `decision-log.md` only for confirmed decisions, not open questions.
- Keep unresolved questions in a clearly labeled confirmation section.

## 4. Validate

For documentation-only updates:

- run a markdown/content sanity check if available
- run code tests only when the document update depends on current code behavior

For code-affecting wrap-up:

- run the narrow test filter for the touched area
- record any tests that were not run

## 5. Choose the Next Slice

Before implementation resumes, identify:

- the next smallest HIA behavior to validate
- the affected files/functions
- the expected PAF/CRF behavior
- the narrow test or manual check that proves the slice works
