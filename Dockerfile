# syntax=docker/dockerfile:1

# =============================================================================
# Stage 1: Builder — installs build tools and restores all R packages via renv
# =============================================================================
FROM rocker/r-ver:4.5.3 AS builder

ARG PFVSH_VERSION=0.2.0
ARG GITHUB_PAT

# ---- System build dependencies -----------------------------------------------
# build-essential / cmake: compile packages with C/C++ code (data.table, forecast)
# gfortran: Fortran compiler required by the forecast package
# libcurl4-openssl-dev / libssl-dev: HTTP/TLS support at build time
# libxml2-dev: xml2 (transitive dep of some packages)
# zlib1g-dev / libzstd-dev: compression libraries
# python3 / python3-dev: argparse R package delegates to Python at build time
# git: renv needs git to install GitHub-sourced packages (pfvIO)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    curl \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libzstd-dev \
    python3 \
    python3-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# ARROW_WITH_ZSTD: ensures ZSTD codec is included when arrow compiles from source.
# RENV_CONFIG_PPM_ENABLED: routes CRAN packages through PPM for pre-built Linux binaries.
ENV ARROW_WITH_ZSTD=ON \
    RENV_CONFIG_PPM_ENABLED=TRUE

WORKDIR /app

# ---- renv bootstrap and package install --------------------------------------
RUN mkdir -p renv
COPY renv.lock       renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile       .Rprofile
COPY DESCRIPTION DESCRIPTION
COPY NAMESPACE   NAMESPACE
COPY R/          R/
COPY main.r      main.r

RUN R -e "renv::restore(confirm = FALSE); renv::install('.', rebuild = TRUE)" && \
    for lib in $(R -s -e "cat(.libPaths(), sep='\n')"); do \
        cp -rLn "$lib"/* /usr/local/lib/R/site-library/ 2>/dev/null || true; \
    done

# =============================================================================
# Stage 2: Runtime — minimal image with only runtime libraries and entrypoint
# =============================================================================
FROM rocker/r-ver:4.5.3

ARG PFVSH_VERSION=0.2.0

LABEL org.opencontainers.image.title="pfv-sh" \
    org.opencontainers.image.description="Previsão semihorária de geração solar fotovoltaica" \
    org.opencontainers.image.vendor="ONS - Operador Nacional do Sistema Elétrico" \
    org.opencontainers.image.source="https://github.com/isabelafiuza/pfv-sh" \
    org.opencontainers.image.licenses="MIT" \
    org.opencontainers.image.version="${PFVSH_VERSION}"

# ---- Runtime system dependencies ---------------------------------------------
# libcurl4-openssl-dev / libssl-dev: arrow C++ library needs these at runtime
#   (missing causes segfault, not a clean R error)
# libzstd-dev: ZSTD codec used by arrow for Parquet compression/decompression
# python3: argparse R package shells out to Python's argparse module at runtime
# libgfortran5: Fortran runtime library required by the forecast package
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libzstd-dev \
    python3 \
    libgfortran5 \
    && rm -rf /var/lib/apt/lists/*

# ---- Copy installed R library from builder -----------------------------------
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=builder /usr/local/lib/R/library       /usr/local/lib/R/library

# ---- Non-root user -----------------------------------------------------------
RUN useradd -r -s /bin/false appuser

# ---- Application code --------------------------------------------------------
WORKDIR /app
COPY --chown=appuser:appuser main.r main.r

# ---- Create data directories -------------------------------------------------
RUN mkdir -p /app/data /app/out /app/artifact && \
    chown -R appuser:appuser /app/data /app/out /app/artifact

# ---- Runtime environment -----------------------------------------------------
ENV LOG_LEVEL=info
ENV PFVSH_PARALLEL=false
ENV PFVSH_RESUME=false
ENV PFVSH_WORKERS=

# ---- Health check ------------------------------------------------------------
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD Rscript -e "library(pfvsh); cat('OK')" || exit 1

USER appuser

ENTRYPOINT ["Rscript", "main.r"]
CMD ["--datadir", "/app/data"]
