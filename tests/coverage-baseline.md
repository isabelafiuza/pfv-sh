# Baseline Test Coverage Report

**Date:** 2026-04-14
**Package:** pfv-sh v0.1.0
**Overall Coverage:** 0.0% (baseline before Epic 04 tests)

## Per-File Coverage

| File                    | Total Lines | Covered | Coverage % |
| ----------------------- | ----------- | ------- | ---------- |
| R/artifact.r            | —           | —       | 0.0%       |
| R/cli.r                 | —           | —       | 0.0%       |
| R/combinacao.r          | —           | —       | 0.0%       |
| R/config-file.r         | —           | —       | 0.0%       |
| R/escrita.r             | —           | —       | 0.0%       |
| R/health-report.r       | —           | —       | 0.0%       |
| R/logging.r             | —           | —       | 0.0%       |
| R/metrics.r             | —           | —       | 0.0%       |
| R/parallel.r            | —           | —       | 0.0%       |
| R/parser.r              | —           | —       | 0.0%       |
| R/pfv-sh.r              | —           | —       | 0.0%       |
| R/predict.r             | —           | —       | 0.0%       |
| R/provenance.r          | —           | —       | 0.0%       |
| R/train.r               | —           | —       | 0.0%       |
| R/utils.r               | —           | —       | 0.0%       |
| R/zzz.r                 | —           | —       | 0.0%       |
| **Total**               | **—**       | **—**   | **0.0%**   |

> Line counts and covered counts will be filled in after tickets 021-024 are implemented
> and `covr::package_coverage()` is run against the complete test suite.

## Top 5 Coverage Gaps

> To be filled in after Epic 04 tests are written and coverage is measured.

1. **R/train.r** — Training pipeline orchestration (`train_main`, `treina_usina`). Core pipeline functions require integration tests with model artifacts.

2. **R/predict.r** — Prediction pipeline orchestration (`predict_main`, `predict_usina`). Requires end-to-end predict pipeline including fitted model artifacts.

3. **R/escrita.r** — Output writing functions (`write_previsao_geracao_fotovoltaica`). Thin wrapper around IO layer; requires end-to-end predict pipeline to exercise.

4. **R/parser.r** — CLI argument parser creation. Only called from `main.r` shim; requires integration test exercising argument parsing.

5. **R/provenance.r** — Provenance tracking functions. Lifecycle-bound; requires full pipeline execution to exercise `create_provenance` / `finalize_provenance` paths.

## Known Gaps

- `main.r` is outside the package boundary and is NOT measured by covr. It contains the CLI entry point shim. The core logic is internalized as `R/cli.r` which IS measured.
- `R/zzz.r` contains `.onLoad` hooks and `globalVariables()` declarations — these are lifecycle functions that covr cannot easily exercise.
- `R/pfv-sh.r` is the package-level documentation file (`"_PACKAGE"`) and contributes no executable lines.

## CI Threshold

- **Minimum threshold:** 75%
- **Current coverage:** 0.0% (pre-Epic 04 baseline)
- **Codecov project target:** 75% with 2% threshold (allows drops down to 73% before failing)
- **Codecov patch target:** 70% with 5% threshold (allows new code down to 65% before failing)

The threshold is enforced in two places:

1. **GitHub Actions workflow** (`test-coverage.yaml`) — the `covr::percent_coverage()` check runs inline after coverage measurement and will `stop()` the workflow if coverage falls below 75%.
2. **Codecov status checks** (`codecov.yml`) — Codecov posts project and patch status checks on pull requests based on the configured targets.
