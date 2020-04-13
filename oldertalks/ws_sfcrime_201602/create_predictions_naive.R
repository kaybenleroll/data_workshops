library(data.table)
library(ggplot2)
library(gtools)


load('testtrain.rda')


prop_dt <- train_dt[, .N, by = Category][, .(Category, count = N, prop = N / sum(N))]


prop_column_dt <- dcast(prop_dt[order(Category)], . ~ Category, value.var = 'prop')
prop_column_dt[, `.` := NULL]

pred1_dt <- cbind(test_dt[, .(Id)], prop_column_dt)

write.csv(pred1_dt, file = 'prediction1.csv', row.names = FALSE)


rm(pred1_dt)


counts     <- prop_dt[order(Category), count]
dirichlet1 <- colMeans(gtools::rdirichlet(10000, counts))

dirichlet1_dt <- as.data.table(t(dirichlet1))
setnames(dirichlet1_dt, prop_dt[order(Category)]$Category)

pred2_dt <- cbind(test_dt[, .(Id)], dirichlet1_dt)

write.csv(pred2_dt, file = 'prediction2.csv', row.names = FALSE)

rm(pred2_dt)


dirichlet2 <- colMeans(gtools::rdirichlet(10000, counts + 10))

dirichlet2_dt <- as.data.table(t(dirichlet2))
setnames(dirichlet2_dt, prop_dt[order(Category)]$Category)

pred3_dt <- cbind(test_dt[, .(Id)], dirichlet2_dt)

write.csv(pred3_dt, file = 'prediction3.csv', row.names = FALSE)

rm(pred3_dt)
