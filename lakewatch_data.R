library(dplyr)
library(ggplot2)

source("Rstuff/connect_to_lakewatch.R")

users<-dbGetQuery(con, "SELECT id as user_id, username, clinefinder FROM users")
taxa<-dbGetQuery(con, "SELECT id as taxon_id, taxa_group, scientific_name, common_name, watchlist2 FROM taxa")
lakes<-dbGetQuery(con, "SELECT id as lake_id, name as lake_name, county, latitude, longitude, reachcode FROM lakes")
sites<-dbGetQuery(con, "SELECT id as site_id, user_id, lake_id, name as site_name FROM sites")
samplings<-dbGetQuery(con, "SELECT id as sampling_id, site_id, lake_id, user_id, date as sampling_date, secchi, temperature, substrate, trap, rake, zigzag, notes FROM samplings")
physical_data<-dbGetQuery(con, "SELECT id as phyiscal_datum_id, sampling_id, user_id, sampling_type, depth, value, notes FROM physical_data")
organisms<-dbGetQuery(con, "SELECT id as organism_id, user_id, confidence, sampling_id, sampling_type, taxon_id, count, size, notes FROM organisms")

postgresqlCloseConnection(con)



sites<-sites %.% inner_join(users, by="user_id") %.% inner_join(lakes, by="lake_id")
dat<-sites %.% inner_join(samplings, by="site_id") %.% inner_join(physical_data, by="sampling_id")

dat_samp14_temp<-filter(dat, sampling_id==14 & sampling_type=="Temperature")
p<-ggplot(data=dat_samp14_temp, aes(y=depth, x=value, group=sampling_id))
p<-p + geom_line() + scale_y_reverse() + xlim(c(0,25)) + labs(title=dat$sampling_name, x=expression(Temperature~(degree~C)), y="Depth (m)")
print(p)