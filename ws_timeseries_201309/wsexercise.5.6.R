###
### Worksheet Exercise 5.5
###

source('setup_data.R', echo = TRUE);


Complaints.hw2 <- HoltWinters(Complaints.ts, alpha = 0.2, beta = FALSE, gamma = FALSE);

print(Complaints.hw2);
plot(Complaints.hw2);


Complaints.hw3 <- HoltWinters(Complaints.ts, alpha = 0.9, beta = FALSE, gamma = FALSE);

print(Complaints.hw3);
plot(Complaints.hw3);



layout(1:3);
plot(Complaints.hw1);
plot(Complaints.hw2);
plot(Complaints.hw3);
