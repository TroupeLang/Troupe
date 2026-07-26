# Contributing to Troupe

Thank you for considering a contribution to Troupe! To ensure a smooth and productive workflow, please follow the guidelines below when opening pull requests or submitting code.

This file covers the pull-request policy. For the test-suite layout and a worked example of adding
a built-in function, see [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md); for build and test commands,
see [docs/DEVELOPMENT.md](docs/DEVELOPMENT.md).

## Pull Request Policy

To maintain a clean and maintainable codebase, all pull requests must adhere to the following rules:

### 1. No Unnecessary Syntactic Changes

Pull requests **must not include formatting-only changes** such as:
- Whitespace adjustments
- Reindentation
- Reordering of imports (unless semantically necessary)
- Other non-functional code style modifications

If such changes are necessary:
- They **must be clearly justified** in the pull request description.
- They **must not be combined** with any functional or semantic changes.

### 2. Minimal and Focused Changes

We prefer small, focused pull requests that:
- Address a single issue or feature
- Are easy to understand and review
- Minimize the potential for unintended side effects

Avoid combining unrelated changes into a single pull request.

### 3. Separation of Concerns

If your contribution includes both syntactic and semantic changes:
- **Split them into separate pull requests** whenever possible.

This helps reviewers understand and evaluate changes more effectively.

### 4. Cosmetic changes

Commits with purely cosmetic changes (code cleanup, etc) should
clearly marked as such, e.g., with the tag `style:` 

## Code Style

Please follow the established coding style of the project. Use existing code as a reference when in doubt.

## Tests

Include tests for any new functionality or bug fixes when applicable.

## Review Process

All pull requests will be reviewed by maintainers. Changes may be requested before merging.

---

Thank you for helping us improve the project!
