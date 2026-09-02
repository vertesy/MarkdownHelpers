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
- In `/Development/MYPACKAGE/Development/Create_the_MYPACKAGE_Package.R")`, `PackageTools::document_and_create_package()` recreates an R package’s metadata and documentation from a configuration file. It runs `devtools::document()` to regenerate package documentation, including the DESCRIPTION and NAMESPACE, from roxygen annotation and `config.R`. 



### Update the Source, Not Just the Documentation

Documentations rebuilt and overwritten from upstream sources: `.Rd` files from roxygen annotations and DESCRIPTION and NAMESPACE from  `config.R` by `PackageTools::document_and_create_package()` relying on  `devtools::document()`  when I manually, regularly run `/Development/MYPACKAGE/Development/Create_the_MYPACKAGE_Package.R")`. Thus  always update the upstream sources first, then fix the downstream documentations correspondingly.



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
