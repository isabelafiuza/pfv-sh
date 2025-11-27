# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial package structure with train and predict pipelines
- ARIMAX model for solar generation forecasting
- Fisico-Estimado model (linear regression) for solar generation forecasting
- NWP data preprocessing: grid association, interpolation, gap filling
- Automatic identification of solar generation periods
- Configuration file validation and parsing
- Docker support for containerized execution
- Comprehensive roxygen2 documentation
- Unit tests for config and utils modules
- GitHub workflows for CI/CD (lint, test, R-CMD-check, docker)
- Issue and PR templates

### Dependencies
- data.table >= 1.17.0
- forecast >= 8.24.0
- zoo >= 1.8-14
- argparse >= 2.2.5
- lubridate >= 1.9.4
- lgr >= 0.4.4
- pfvIO >= 0.2.2

## [0.0.0.9000] - 2025-11-27

### Added
- Initial development version
- Core forecasting functionality
- Package scaffolding
