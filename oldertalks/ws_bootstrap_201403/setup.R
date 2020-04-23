
library(data.table);
library(boot);
library(ggplot2);
library(fBasics);
library(tseries);
library(car);
library(MASS);


data(catsM);
data(lynx);
data(Duncan);


equity.returns <- readRDS("equity_returns.rds");
