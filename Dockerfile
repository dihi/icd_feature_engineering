FROM r-base:3.5.3
COPY ./packrat/packrat.lock packrat/

RUN Rscript -e 'install.packages("packrat")'
RUN Rscript -e 'packrat::restore()'
