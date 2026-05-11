FROM rocker/shiny:4.3.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . /srv/shiny-server/app

RUN R -e "if (file.exists('/srv/shiny-server/app/renv.lock')) { install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(project = '/srv/shiny-server/app', prompt = FALSE) } else { message('No renv.lock found; skipping renv restore.') }"

RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
