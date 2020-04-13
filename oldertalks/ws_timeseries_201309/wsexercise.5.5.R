###
### Worksheet Exercise 5.5
###

source('setup_data.R', echo = TRUE);


Complaints.hw1 <- HoltWinters(Complaints.ts, beta = FALSE, gamma = FALSE);

print(Complaints.hw1);

plot(Complaints.hw1);
