# Load required packages
library(dplyr)
library(ggplot2)
library(RPostgreSQL)
library(ggmap)

# Connect to the database
source("lakewatch_analysis/connect_to_lakewatch.R")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host=LW_SERVER, dbname=LW_DATABASE, user=LW_USER, password=LW_PASSWORD)

# Pull down some data from the database
users <- dbGetQuery(con, "SELECT id as user_id, username, clinefinder FROM users;")
taxa <- dbGetQuery(con, "SELECT id as taxon_id, taxa_group, scientific_name, common_name, watchlist2 FROM taxa;")
lakes <- dbGetQuery(con, "SELECT id as lake_id, name as lake_name, county, latitude, longitude, reachcode FROM lakes;")
sites <- dbGetQuery(con, "SELECT id as site_id, user_id, lake_id, name as site_name, latitude as site_lat, longitude as site_lon FROM sites;")
samplings <- dbGetQuery(con, "SELECT id as sampling_id, site_id, date as sampling_date, secchi, temperature, substrate, trap, rake, zigzag, notes FROM samplings;")
physical_data <- dbGetQuery(con, "SELECT id as phyiscal_datum_id, sampling_id, sampling_type, depth, value, notes FROM physical_data;")
organisms <- dbGetQuery(con, "SELECT id as organism_id, confidence, sampling_id, sampling_type, taxon_id, count, size, notes FROM organisms;")
secchi <- dbGetQuery(con, "SELECT id as secchi_id, sampling_id, secchi_time, cline_finder, observer, secchi_depth, view_type, plants, hit_bottom, sky_code, wave_code FROM secchi_data;")
# Close the database connection
postgresqlCloseConnection(con)

# Combine the various data frames into one
# Group by site
dat <- sites %>% inner_join(users, by="user_id") %>% inner_join(lakes, by="lake_id") %>% inner_join(samplings, by="site_id") %>% inner_join(physical_data, by="sampling_id") %>% group_by(site_name)

# Export data to CSV
write.csv(users, "OLW_users.csv")
write.csv(taxa, "OLW_taxa.csv")
write.csv(lakes, "OLW_lakes.csv")
write.csv(sites, "OLW_sites.csv")
write.csv(samplings, "OLW_samplings.csv")
write.csv(physical_data, "OLW_physical_data.csv")
write.csv(organisms, "OLW_organisms.csv")
write.csv(dat, "OLW_combined.csv")
write.csv(secchi, "OLW_secchi.csv")

# Create a temperature profile plot function.
# Plots all sampling profiles for a given site
lakeTempPlot <- function(site_dat){ 
  sampling_name <- paste(site_dat$lake_name[1], site_dat$site_name[1], sep="-")
  p <- ggplot(data=site_dat, aes(y=value, x=depth, group=sampling_date))
  p <- p + geom_line(aes(color=factor(sampling_date))) 
  p <- p + labs(title=sampling_name, y=expression(Temperature~(degree~C)), x="Depth (m)")
  p <- p + scale_x_reverse()  + coord_flip() + scale_color_discrete(name="Sampling Date")
  p <- p + theme_bw()
  print(p) 
}

mapSite <- function(site_dat){
  sampling_name <- paste(site_dat$lake_name[1], site_dat$site_name[1], sep="-")
  map <- get_map(location = c(lon = site_dat$longitude[1], lat = site_dat$latitude[1]), zoom = 10)
  m <- ggmap(map)
  m <- m + geom_point(aes(x=longitude, y=latitude), data=site_dat, col="red", size=3)
  m <- m + labs(title=sampling_name)
  print(m)
}

# Plot the temperature data for each site using dplyr do()
plots <- dat  %>% do(lakeTempPlot)
maps <- dat %>% do(mapSite)


# Old loop for creating a plot per site
# for (i in 1:nrow(sites)){
#   site_dat<-dat %>% filter(site_id == sites$site_id[i] & !is.na(value))
#   
#   if (nrow(site_dat) < 2) next
#   site_dat<-site_dat %>% arrange(sampling_date, depth)
#   
#   p <- ggplot(data=site_dat, aes(y=value, x=depth, group=sampling_date))
#   p <- p + geom_line(aes(color=factor(sampling_date))) 
#   p <- p + labs(title=site_dat$sampling_name, y=expression(Temperature~(degree~C)), x="Depth (m)")
#   p <- p + scale_x_reverse()  + coord_flip() + scale_color_discrete(name="Sampling Date")
#   p <- p + theme_bw()
#   print(p)
#   
# }


# Secchi Report

surface_temps <- physical_data %>% group_by(sampling_id) %>% filter(sampling_type=="Temperature") %>% summarise(min_z_temp=first(value))
max_depth_temps <- physical_data %>% group_by(sampling_id) %>% filter(sampling_type=="Temperature") %>% summarise(max_z=max(depth), max_z_temp=last(value))

secchi_report <- secchi %>% inner_join(samplings, by="sampling_id") %>% inner_join(sites, by="site_id") %>%  inner_join(sites) %>% inner_join(lakes) %>% left_join(surface_temps, by="sampling_id") %>% left_join(max_depth_temps, by="sampling_id") %>% select( county, lake_name, site_lat, site_lon, site_name, observer, sampling_date, secchi_time, secchi_depth, max_z, min_z_temp, max_z_temp, view_type, plants, hit_bottom, sky_code, wave_code) %>% arrange(county, lake_name, site_name, sampling_date)


