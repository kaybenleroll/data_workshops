###
### Worksheet Exercise 5.3
###

source('setup_data.R', echo = TRUE);


App.ts.decomp <- decompose(App.ts);
App.rnd.ts    <- window(App.ts.decomp$random, start = c(1996, 3));

Act.ts.decomp <- decompose(Act.ts);
Act.rnd.ts    <- window(Act.ts.decomp$random, start = c(1996, 3));


AppAct.rnd.ts.acf <- acf(ts.union(App.rnd.ts, Act.rnd.ts), na.action = na.pass, plot = FALSE);
AppAct.rnd.ts.ccf <- ccf(App.rnd.ts, Act.rnd.ts, na.action = na.pass, plot = FALSE);

plot(AppAct.rnd.ts.acf);
plot(AppAct.rnd.ts.ccf);
