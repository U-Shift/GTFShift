# Contributing to GTFShift

Thank you for your interest in contributing to **GTFShift**! We welcome contributions of all kinds, including bug reports, feature requests, documentation improvements, and code contributions.

## Reporting Bugs and Requesting Features

Before opening a new issue, please search existing [GitHub Issues](https://github.com/U-Shift/GTFShift/issues) to check if the problem or feature request has already been reported.

- **Bug Reports**: Please include a minimal, reproducible example (`reprex`) demonstrating the issue, along with your R session information (`sessionInfo()`).
- **Feature Requests**: Describe the proposed functionality, the use case it addresses, and how it fits within the scope of **GTFShift**.

## Pull Requests

1. Fork the repository on [GitHub](https://github.com/U-Shift/GTFShift) and create a new branch from `main` (or `dev`) for your changes.
2. Make your changes in a focused and well-documented manner:
   - Follow the existing coding style in the package (e.g., use `<-` for assignment, document functions using `roxygen2`).
   - Run `roxygen2::roxygenize()` (or `devtools::document()`) if you update or add function documentation.
   - Add unit tests in `tests/testthat/` for any new functionality or bug fixes.
3. Run `devtools::check()` locally and ensure all checks and tests pass without errors or warnings.
4. Submit a Pull Request describing the changes made and referencing any related issues.

## Questions?

If you have any questions, feel free to open an issue on [GitHub](https://github.com/U-Shift/GTFShift/issues) or contact the package maintainer listed in `DESCRIPTION`.

