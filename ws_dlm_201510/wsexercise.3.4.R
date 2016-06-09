source("lib.R");

temperatures <- A %*% cbind(c(100, 50, 50), c(50, 100, 50), c(50, 50, 100)) + b

print(apply(temperatures, 2, max))
