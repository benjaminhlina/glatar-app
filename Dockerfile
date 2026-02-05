FROM rocker/shiny:latest

# https://specs.opencontainers.org/image-spec/annotations/
LABEL \
    org.opencontainers.image.authors="Benjamin Hlina <benjamin.hlina@gmail.com>" \
    org.opencontainers.image.vendor="glatar-app" \
    org.opencontainers.image.version="0.0.0.9999" \
    org.opencontainers.image.source="https://github.com/benjaminhlina/glatar-app" \
    org.opencontainers.image.licenses="MIT"

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    cmake \
    g++ \
    gdal-bin \
    git \
    gfortran \
    libabsl-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgeos-dev \
    libgdal-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libmysqlclient-dev \
    libpng-dev \
    libpq-dev \
    libproj-dev \
    libsqlite3-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libxml2-dev \
    libxt-dev \
    make \
    pandoc \
    pkg-config \
    proj-bin \
    proj-data \
    && rm -rf /var/lib/apt/lists/*


# # ---- remove shiny-server template apps ---
RUN rm -rf /srv/shiny-server/*

# ---- ops for got to install renv ----
RUN R -e "install.packages(c('renv'), repos = 'https://cran.rstudio.com')"

# # ---- Set working directory ----
WORKDIR /srv/shiny-server/GLATAR-App/
# ---- Copy renv files ----
COPY renv.lock renv.lock
COPY renv/ renv/

# ---- Restore R packages ----
ENV RENV_PATHS_CACHE=/renv/cache
ENV RENV_CONFIG_PAK_ENABLED=TRUE
ENV RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org
# RUN R -e "options(renv.verbose = TRUE); renv::restore(prompt = FALSE)"
#RUN R -e "options(renv.verbose = TRUE); \
 # Sys.setenv(RENV_CONFIG_PAK_ENABLED = 'TRUE'); \
  #tryCatch(renv::restore(prompt = FALSE), \
   # error = function(e) { \
    #  message('Error details: ', conditionMessage(e)); \
     # traceback(); \
      #quit(status = 1); \
    #})"
# ---- Restore with pak debugging ----
RUN R -e "options(renv.verbose = TRUE); \
  Sys.setenv(RENV_CONFIG_PAK_ENABLED = 'TRUE'); \
  lockfile <- renv:::renv_lockfile_load(project = getwd()); \
  packages <- names(lockfile[['Packages']]); \
  cat('Total packages to install:', length(packages), '\n'); \
  for (i in seq_along(packages)) { \
    cat(sprintf('Installing %d/%d: %s\n', i, length(packages), packages[i])); \
    tryCatch({ \
      renv::install(packages[i], prompt = FALSE); \
    }, error = function(e) { \
      cat('FAILED ON PACKAGE:', packages[i], '\n'); \
      cat('Error:', conditionMessage(e), '\n'); \
      quit(status = 1); \
    }); \
  }"

# Copy app files
COPY app.R app.R
COPY www/ www/
COPY data/ data/
COPY modules/ modules/
# copy shiny-server config file
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf


# ---- change file ownership and rew -----
RUN chown -R shiny:shiny /srv/shiny-server && \
    chmod -R 755 /srv/shiny-server


# --- copy shiny_entry and change rew ----
COPY shiny_entry.sh /usr/local/bin/shiny_entry.sh
RUN chmod 755 /usr/local/bin/shiny_entry.sh

# Expose port
USER shiny
EXPOSE 3838
CMD ["/usr/local/bin/shiny_entry.sh"]
