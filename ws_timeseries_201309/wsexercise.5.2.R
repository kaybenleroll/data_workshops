###
### Worksheet Exercise 5.2
###

source('setup_data.R', echo = TRUE);


AppAct.ts     <- ts.union(App.ts, Act.ts);
AppAct.ts.acf <- acf(AppAct.ts, plot = FALSE);


plot(AppAct.ts.acf);
