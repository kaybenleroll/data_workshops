
library(data.table);
library(boot);
library(ggplot2);
library(fBasics);
library(tseries);


data(catsM);
data(lynx);


equity.returns <- readRDS("equity_returns.rds");
