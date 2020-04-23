###
### Worksheet Exercise 5.1
###

source('setup_data.R', echo = TRUE);


App.ts <- ts(AA.df$Approvals, start = c(1996,1), freq=4);
Act.ts <- ts(AA.df$Activity,  start = c(1996,1), freq=4);


ts.plot(App.ts, Act.ts, lty = c(1, 3), ylim = c(0, 16000));
