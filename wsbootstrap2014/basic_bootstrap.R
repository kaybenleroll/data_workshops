
source("setup.R", echo = TRUE);


mean.w   <- function(x, w) sum(x * w);
air.boot <- boot(data = aircondit$hours, statistic = mean.w, R = 999, stype = "w");
