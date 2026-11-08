# llmpublic — Project Config

## Project Identity

| Field | Value |
|-------|-------|
| Package name | `llmpublic` |
| Primary domain | Small generic R helpers: ccusage summaries, static call analysis, markdown search, hover popups |
| Stage | Early development (local, unpublished) |
| Environment | `dev` |
| Targets pipeline | none — small function library, no data pipeline |
| R version | 4.5.x (from `default.R`) |

## Conventions

- Every function takes its paths and data as explicit arguments; no default points at a personal location.
- Tests use small, synthetic, invented fixtures only.
- Use `cli::cli_abort()` (never `stop()`), `checkmate` for argument checks, no silent `tryCatch`.
- No personal data, home paths or real usage figures anywhere in the package.
