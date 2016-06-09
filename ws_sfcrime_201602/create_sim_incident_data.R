require(data.table)
require(ggplot2)
require(RPostgreSQL)


dbconnection <- dbConnect(dbDriver("PostgreSQL"), host = 'localhost', dbname = 'sfcrime', user = 'geospuser', pass = 'geospuser')

data_dt <- dbGetQuery(dbconnection, "SELECT id, incident_ts, label FROM incident_data i WHERE i.label = 'train' ORDER BY incident_ts")
setDT(train_dt)

subset_dt <- train_dt[runif(.N) < 0.2]

train_id <- subset_dt[runif(.N) < 0.5]
check_id <- setdiff(subset_dt$id, train_id);
