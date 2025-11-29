# Copilot Code Review Instructions

## Repository Overview

The `installr` package is an R package designed for Windows that automates the installation and updating of software, with a special focus on R itself. The package provides:

1. Easy R updates on Windows via the `updateR()` function
2. Installation functions for R development software (git, Rtools, etc.)
3. Installation functions for reproducible research tools (MikTeX, pandoc, etc.)

## Code Style Guidelines

- Follow R best practices and conventions
- Use roxygen2 documentation for all exported functions
- Line length should not exceed 100 characters (see `.lintr` configuration)
- Functions should handle Windows-specific paths and URLs appropriately

## Testing Guidelines

- Tests are located in the `tests/` directory and use the `testthat` framework
- Many functions are Windows-only, so tests should be skipped on non-Windows platforms when appropriate
- Mock external downloads and web requests in tests

## Review Focus Areas

When reviewing code changes, please pay special attention to:

1. **URL handling**: Ensure download URLs are valid and use HTTPS where possible
2. **Windows compatibility**: Functions should work correctly on Windows operating systems
3. **Error handling**: Proper error messages and graceful failure handling
4. **Documentation**: All exported functions should have complete roxygen2 documentation
5. **Backwards compatibility**: Changes should not break existing functionality

## Dependencies

- Core dependencies: `stringr`, `utils`
- Suggested packages are used for optional functionality (see DESCRIPTION file)
- Minimize adding new dependencies when possible
