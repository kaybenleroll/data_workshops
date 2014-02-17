library(rjags);
library(data.table);
library(ggplot2);


mintdata.dt <- readRDS('data.dt.rds');

set.seed(4242);

reduced.data.100.dt  <- mintdata.dt[, .SD[sample(1:dim(.SD)[1], 100)], by = coinid];
reduced.data.10pc.dt <- mintdata.dt[, .SD[sample(1:dim(.SD)[1], round(0.1 * dim(.SD)[1], 0))], by = coinid];
