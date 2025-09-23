FROM r-base:latest

RUN R -e "install.packages('rigr', repos='https://cloud.r-project.org')"