# Enter username and password
# Save this file as "connect_to_lakewatch.R"


library(RPostgreSQL)
drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv, host="pgsql.rc.pdx.edu", dbname="lakewatch", user="", password="" )
