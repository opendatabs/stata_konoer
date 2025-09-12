########################################################
#        Renku install section                         #

FROM renku/renkulab-r:4.3.1-0.25.0 as builder

ARG RENKU_VERSION=2.9.4

# Install renku from pypi or from github if a dev version
RUN if [ -n "$RENKU_VERSION" ] ; then \
        source .renku/venv/bin/activate ; \
        currentversion=$(renku --version) ; \
        if [ "$RENKU_VERSION" != "$currentversion" ] ; then \
            pip uninstall renku -y ; \
            gitversion=$(echo "$RENKU_VERSION" | sed -n "s/^[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\(rc[[:digit:]]\+\)*\(\.dev[[:digit:]]\+\)*\(+g\([a-f0-9]\+\)\)*\(+dirty\)*$/\4/p") ; \
            if [ -n "$gitversion" ] ; then \
                pip install --no-cache-dir --force "git+https://github.com/SwissDataScienceCenter/renku-python.git@$gitversion" ;\
            else \
                pip install --no-cache-dir --force renku==${RENKU_VERSION} ;\
            fi \
        fi \
    fi

#             End Renku install section                #
########################################################

FROM renku/renkulab-r:4.3.1-0.25.0

WORKDIR /code
# Install required system dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    libcairo2-dev \
    libxt-dev \
    libcurl4 \
    libcurl4-openssl-dev \
    libssl-dev \
    r-cran-rstan \
    libxml2-dev \
    default-jdk \
    libglpk-dev \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev \
    locales && \
    locale-gen de_DE.UTF-8 && \
    update-locale LANG=de_DE.UTF-8

# Set environment variables for German locale
ENV LANG=de_DE.UTF-8
ENV LANGUAGE=de_DE:de
ENV LC_ALL=de_DE.UTF-8

# Set default RStudio Package Manager Repository
RUN echo "r <- getOption('repos'); \
          r['CRAN'] <- 'https://packagemanager.rstudio.com/cran/__linux__/jammy/2024-11-28'; \
          options(repos = r);" > ~/.Rprofile

COPY install.R /code/
RUN R -f /code/install.R

COPY . /code/

COPY --from=builder ${HOME}/.renku/venv ${HOME}/.renku/venv
