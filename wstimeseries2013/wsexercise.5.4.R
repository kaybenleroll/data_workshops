###
### Worksheet Exercise 5.4
###

source('setup_data.R', echo = TRUE);


Complaints.ts <- ts(motororg.df$complaints, start = c(1996, 1), freq = 12)
plot(Complaints.ts, xlab = "Time / months", ylab = "Complaints");

Complaints.ts.decomp <- decompose(Complaints.ts);
plot(Complaints.ts.decomp);
