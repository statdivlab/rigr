FROM r-base:latest

RUN R -e "install.packages('rigr', repos='https://cloud.r-project.org')" && \
    R -e "library(rigr)"
RUN R -e "install.packages('tidyverse', repos='https://cloud.r-project.org')" && \
    R -e "library(tidyverse)"