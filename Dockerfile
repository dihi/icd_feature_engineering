FROM r-base:3.5.3

RUN mkdir /checkpoint/
COPY ./Code/checkpoint_file.R ./checkpoint/
RUN R slave -e 'install.packages("checkpoint")'
WORKDIR /checkpoint/
RUN mkdir  /root/.checkpoint
RUN R -e 'print(list.files("./"))'
RUN R -e 'checkpoint::checkpoint("2019-05-01", forceProject=TRUE)'


