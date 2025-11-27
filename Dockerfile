FROM rocker/tidyverse:4.5.2

# Labels for container metadata
LABEL org.opencontainers.image.title="pfv-sh"
LABEL org.opencontainers.image.description="Previsão semihorária de geração solar fotovoltaica"
LABEL org.opencontainers.image.vendor="ONS - Operador Nacional do Sistema Elétrico"
LABEL org.opencontainers.image.source="https://github.com/isabelafiuza/pfv-sh"
LABEL org.opencontainers.image.licenses="MIT"

WORKDIR /app

# Copy package files first (for better layer caching)
COPY DESCRIPTION DESCRIPTION
COPY NAMESPACE NAMESPACE

# Setup renv
RUN mkdir -p renv/
COPY renv.lock renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile .Rprofile

# Restore dependencies (cached layer if renv.lock unchanged)
RUN R -e "renv::restore()"

# Copy R source code
COPY R/ R/
COPY main.r main.r

# Install the package
RUN R -e "install.packages('remotes')" && \
    R -e "remotes::install_local('.', dependencies = FALSE, upgrade = 'never')"

# Create non-root user for security
RUN useradd -m -s /bin/bash appuser && \
    chown -R appuser:appuser /app

# Create directories for data volumes
RUN mkdir -p /app/data /app/out /app/artifact && \
    chown -R appuser:appuser /app/data /app/out /app/artifact

# Switch to non-root user
USER appuser

# Health check - verify R and package load correctly
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD Rscript -e "library(pfv.sh); cat('OK')" || exit 1

# Default environment variables
ENV LOG_LEVEL=info

ENTRYPOINT ["Rscript", "main.r"]
CMD ["--datadir", "/app/data"]