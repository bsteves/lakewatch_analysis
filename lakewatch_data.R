library(dplyr)
library(ggplot2)
library(RPostgreSQL)

source("connect_to_lakewatch.R")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host=LW_SERVER, dbname=LW_DATABASE, user=LW_USER, password=LW_PASSWORD)

 

users <- dbGetQuery(con, "SELECT id as user_id, username, clinefinder FROM users")
taxa <- dbGetQuery(con, "SELECT id as taxon_id, taxa_group, scientific_name, common_name, watchlist2 FROM taxa")
lakes <- dbGetQuery(con, "SELECT id as lake_id, name as lake_name, county, latitude, longitude, reachcode FROM lakes")
sites <- dbGetQuery(con, "SELECT id as site_id, user_id, lake_id, name as site_name FROM sites")
samplings <- dbGetQuery(con, "SELECT id as sampling_id, site_id, date as sampling_date, secchi, temperature, substrate, trap, rake, zigzag, notes FROM samplings")
physical_data <- dbGetQuery(con, "SELECT id as phyiscal_datum_id, sampling_id, sampling_type, depth, value, notes FROM physical_data")
organisms <- dbGetQuery(con, "SELECT id as organism_id, confidence, sampling_id, sampling_type, taxon_id, count, size, notes FROM organisms")

postgresqlCloseConnection(con)


dat <- sites %.% inner_join(users, by="user_id") %.% inner_join(lakes, by="lake_id") %.% inner_join(samplings, by="site_id") %.% inner_join(physical_data, by="sampling_id") %.% group_by(site_name)


write.csv(users, "OLW_users.csv")
write.csv(taxa, "OLW_taxa.csv")
write.csv(lakes, "OLW_lakes.csv")
write.csv(sites, "OLW_sites.csv")
write.csv(samplings, "OLW_samplings.csv")
write.csv(physical_data, "OLW_physical_data.csv")
write.csv(organisms, "OLW_organisms.csv")


write.csv(dat, "OLW_combined.csv")

# create a temperature profile plot function
lakeTempPlot <- function(site_dat){ 
  sampling_name <- paste(site_dat$lake_name.x[1], site_dat$site_name[1], sep="-")
  p <- ggplot(data=site_dat, aes(y=value, x=depth, group=sampling_date))
  p <- p + geom_line(aes(color=factor(sampling_date))) 
  p <- p + labs(title=sampling_name, y=expression(Temperature~(degree~C)), x="Depth (m)")
  p <- p + scale_x_reverse()  + coord_flip() + scale_color_discrete(name="Sampling Date")
  p <- p + theme_bw()
  print(p) 
}


# plot the temperature data for each site
plots<-dat  %.% do(lakeTempPlot)

# for (i in 1:nrow(sites)){
#   site_dat<-dat %.% filter(site_id == sites$site_id[i] & !is.na(value))
#   
#   if (nrow(site_dat) < 2) next
#   site_dat<-site_dat %.% arrange(sampling_date, depth)
#   
#   p <- ggplot(data=site_dat, aes(y=value, x=depth, group=sampling_date))
#   p <- p + geom_line(aes(color=factor(sampling_date))) 
#   p <- p + labs(title=site_dat$sampling_name, y=expression(Temperature~(degree~C)), x="Depth (m)")
#   p <- p + scale_x_reverse()  + coord_flip() + scale_color_discrete(name="Sampling Date")
#   p <- p + theme_bw()
#   print(p)
#   
# }


