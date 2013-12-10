###
### Worksheet Data Setup
###

ensure.installed <- function(package){
    if (!package %in% installed.packages()) install.packages(package)
}


ensure.installed("ggplot2");
library(ggplot2);

ensure.installed("gridExtra");
library(gridExtra);

ensure.installed("reshape2");
library(reshape2);


data(AirPassengers);

AP.ts       <- AirPassengers;
MA.month.df <- read.table('Maine.dat', header = TRUE);
MA.month.ts <- ts(MA.month.df$unemploy, start = c(1996, 1), freq = 12)

CBE.df      <- read.table('cbe.dat',         header = TRUE);
AA.df       <- read.table('ApprovActiv.dat', header = TRUE);
motororg.df <- read.table('motororg.dat',    header = TRUE);

SPY.df      <- read.csv('spy.dat', header = TRUE);
