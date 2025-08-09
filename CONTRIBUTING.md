# Contributing to CIEL

Thank you for your interest in contributing to the CIEL programming language! This document outlines the guidelines and requirements for contributing to this project.

## Code Style and Formatting

This repository uses automated code formatting and style enforcement to maintain consistency across the codebase. Please ensure your contributions adhere to these standards.

### ClangFormat

The repository includes a `.clang-format` file that defines the code formatting standards for all C/C++ source files. The configuration is based on the LLVM style.
Please ensure that your code is formatted according to these rules before submitting a pull request.

### EditorConfig

The repository includes an `.editorconfig` file that defines basic formatting rules for various file types. This ensures consistency across different editors and IDEs.

Most modern editors automatically respect EditorConfig settings. If your editor doesn't support it natively, please install the appropriate EditorConfig plugin.

## Contribution Workflow

### 1. Fork and Clone

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/ciel.git
   cd ciel
   ```

### 2. Create a Feature Branch

**Important**: Never work directly on the `main` branch. Always create a new branch for your changes:

```bash
# Create and switch to a new branch
git checkout -b feature/your-feature-name

# Or for bug fixes
git checkout -b fix/issue-description
```

Use descriptive branch names that clearly indicate the purpose of your changes:
- `feature/add-while-loop-support`
- `fix/memory-leak-in-parser`
- `docs/update-installation-guide`

### 3. Make Your Changes

1. Write your code following the project's coding standards
2. Ensure your code is properly formatted using clang-format
3. Add or update tests as necessary
4. Update documentation if needed

### 4. Commit Your Changes

**All commits must be signed**. This is a mandatory requirement for security and authenticity purposes.

Please follow the [conventional commit](https://www.conventionalcommits.org/en/v1.0.0/) guidelines.

### 5. Push and Create Pull Request

1. **Push your branch** to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Create a Pull Request**:
   - Navigate to the original repository on GitHub
   - Click "New Pull Request"
   - Select your branch as the source
   - Provide a clear title and description of your changes
   - Reference any related issues using `#issue-number`


