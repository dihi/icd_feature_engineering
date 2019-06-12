FROM r-base:3.5.3
RUN R slave -e 'install.packages(c("optparse", "data.table","lubridate", "glmnet","xgboost","ROCR","MLmetrics","Matrix","ggplot2", "icd"))'
