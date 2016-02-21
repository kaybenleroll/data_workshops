require(data.table)
require(ggplot2)
require(RPostgreSQL)


train_dt <- fread("train.csv")
test_dt  <- fread("test.csv")


train_dt[, c("incident_ts", "label", "id") := .(as.POSIXct(Dates), "train", sprintf("T%08d", 1:.N))]
test_dt [, c("incident_ts", "label") := .(as.POSIXct(Dates), "test")]


incident_train_dt <- train_dt[, .(id,      label, incident_ts, dow = DayOfWeek, district = PdDistrict, address = Address, lng = X, lat = Y)]
incident_test_dt  <- test_dt [, .(id = Id, label, incident_ts, dow = DayOfWeek, district = PdDistrict, address = Address, lng = X, lat = Y)]

incident_dt <- rbind(incident_train_dt, incident_test_dt);

category_dt <- train_dt[, .(id, category = Category, desc = Descript, res = Resolution)]

dbconnection <- dbConnect(dbDriver("PostgreSQL"), host = 'localhost', dbname = 'sfcrime', user = 'geospuser', pass = 'geospuser')

dbWriteTable(dbconnection, name = 'incident_data', value = incident_dt, append = TRUE, row.names = FALSE)
dbWriteTable(dbconnection, name = 'category_data', value = category_dt, append = TRUE, row.names = FALSE)


dbGetQuery(dbconnection, "ALTER TABLE incident_data ADD COLUMN geom geometry(Point,4326)")
dbGetQuery(dbconnection, "UPDATE incident_data SET geom = st_setsrid(st_makepoint(lng, lat), 4326)")
