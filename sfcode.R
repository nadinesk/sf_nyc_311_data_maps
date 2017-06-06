library(openxlsx)
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(rgeos)
library(RColorBrewer)
library(ggthemes)
library(reshape2)
library(dplyr)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
library(viridis)



top2016SF <- read.csv("D:/sfnyc/top2016SF.csv")

top2016SF_10 <- top2016SF %>%
  slice(1:10) %>%
  rename(c('total_count_descriptor' = 'total_count'))

tbl_df(top2016SF_10)


b_desc <- read.csv("D:/sfnyc/bulky_items_desc.csv")

b_desc <- b_desc %>%
  arrange(desc(total_count_descriptor))

tbl_df(b_desc)




sfb1 <- read.csv("D:/sfnyc/sfmo_lt3.csv")
sfb2 <- read.csv("D:/sfnyc/sfmo_gte3.lt6.csv")
sfb3 <- read.csv("D:/sfnyc/sfmo_gte6.lt9.csv")
sfb4 <- read.csv("D:/sfnyc/sfmo_gte9.lte12.csv")
sfb5 <- read.csv("D:/sfnyc/mosf_e12month.csv")

sfba <- rbind(sfb1, sfb2, sfb3, sfb4, sfb5)

sfba <- sfba %>%
arrange(desc(total_count_descriptor))

tbl_df(sfba)


ba <-readOGR("D:/pn","planning_neighborhoods")

ba_wgs84 <- spTransform(ba, CRS("+proj=longlat +datum=WGS84"))

ba_wgs84@data$id = rownames(ba_wgs84@data)
ba_wgs84.points = fortify(ba_wgs84, region="id")
ba_wgs84.df = join(ba_wgs84.points, ba_wgs84@data, by="id")
ba_wgs84.df

tbl_df(ba_wgs84.df)

baidList <- ba_wgs84@data$neighborho

centroids.df <- as.data.frame(coordinates(ba_wgs84))
names(centroids.df) <- c("Longitude", "Latitude")  
id.df <- data.frame(id = baidList, centroids.df)

tbl_df(id.df)


sf_bi <- ggplot(sfba, aes(longitude, latitude))  + #"id" is col in your df, not in the map object 
  expand_limits(x = ba_wgs84.df$long, y = ba_wgs84.df$lat) + 
  geom_polygon(data= ba_wgs84.df, aes(x=long, y=lat, group=group), fill="white", color="pink", size=0.15) +
  geom_point(aes(color=total_count_descriptor, size=total_count_descriptor, alpha=total_count_descriptor) ) + 
  geom_text(data=id.df, aes(label = id, x = Longitude, y = Latitude), size = 3) +
  labs(x = "Longitude", y = "Latitude", title = "San Francisco-Bulky Items") + 
  scale_colour_gradient(low = "#99ff99", high = "black")

sf_bi

dev.copy(png,'D:/sfnyc/sf_bi.png')
dev.off()



