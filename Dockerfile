FROM rocker/tidyverse:latest

RUN R -e "install.packages('rigr', repos='https://cloud.r-project.org')" && \
    R -e "library(rigr)"
