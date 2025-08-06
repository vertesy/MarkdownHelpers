# AGENTS Instructions for MarkdownHelpers

Welcome! This repository hosts **MarkdownHelpers**, a small R package that provides helper
functions for generating and formatting Markdown content.  The codebase is lightweight
and mostly aimed at supporting other tools such as `MarkdownReports` and `ggExpress`.

## Repository Structure
- `R/MarkdownHelpers.R` – all package functions live in this single file.  Each
  function is documented with roxygen comments.
- `man/` – generated documentation files.  Run `devtools::document()` to
  regenerate them when functions change.
- `DESCRIPTION`, `NAMESPACE`, `CITATION.cff` – standard package metadata.
- `Development/` – scripts used by the author for building and experimenting; not
  required for normal use.

## Dependencies
This package depends on several other @vertesy projects:
- [Stringendo](https://github.com/vertesy/Stringendo)
- [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2)
- [ReadWriter](https://github.com/vertesy/ReadWriter)

Many examples in `README.md` also make use of `MarkdownReports`, `ggExpress`,
`Seurat.utils`, and `Seurat.pipeline`.
Install the dependencies first when working with this package.

## Working with the Code
- Keep all R functions in `R/MarkdownHelpers.R` and use roxygen comments for
  documentation.
- After editing, regenerate docs with `devtools::document()` and update the
  package version in `DESCRIPTION` if needed.

## Testing
Before committing code changes, run an R package build and check from the
repository root:

```bash
R CMD build .
R CMD check --no-manual MarkdownHelpers_*.tar.gz
```

These commands verify that the package builds and the examples run (dependencies
must be installed).  Fix any issues before submitting changes.

## Further Reading
To understand how this package fits into the broader ecosystem, explore the
README and the linked repositories above.  They provide context, advanced usage
patterns, and additional helper functions that extend Markdown-centric workflows.
