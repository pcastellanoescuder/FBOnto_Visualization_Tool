# Get R devel version

FROM rocker/r-devel

# System libraries of general use

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev

# Install R packages required 

## CRAN

RUN R -e "install.packages(c('shiny', 'DT', 'dplyr', 'shinythemes', 'ontologyIndex', 'shinyWidgets', 'networkD3', 'readxl'), repos = 'http://cran.rstudio.com/')"

## Bioconductor

RUN installGithub.r pcastellanoescuder/fobitools

# Copy the app to the image

COPY /app /fobitoolsgui

# Select port

EXPOSE 3838

# Allow permission

RUN sudo chown -R shiny:shiny /fobitoolsgui

# Run app

CMD ["R", "-e", "shiny::runApp('/fobitoolsgui', host = '0.0.0.0', port = 3838)"]

