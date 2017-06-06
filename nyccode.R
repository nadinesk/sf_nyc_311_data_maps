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

top2016NYC <- read.csv("D:/sfnyc/top2016NYC.csv")

top2016NYC_10 <- top2016NYC %>%
  slice(1:10)

tbl_df(top2016NYC_10)


h_desc <- read.csv("D:/sfnyc/heat_hw_desc.csv")

h_desc <- h_desc %>%
  arrange(desc(total_count_descriptor))

tbl_df(h_desc)


nyb1 <- read.csv("D:/sfnyc/month1.csv")
nyb2 <- read.csv("D:/sfnyc/month2.csv")
nyb3 <- read.csv("D:/sfnyc/month3.csv")
nyb4 <- read.csv("D:/sfnyc/month4.csv")
nyb5 <- read.csv("D:/sfnyc/month5.csv")
nyb6 <- read.csv("D:/sfnyc/month6.csv")
nyb7 <- read.csv("D:/sfnyc/month7.csv")
nyb8_11 <- read.csv("D:/sfnyc/months8-11.csv")
nyb12 <- read.csv("D:/sfnyc/month12.csv")

nyba <- rbind(nyb1, nyb2, nyb3, nyb4, nyb5, nyb6, nyb7, nyb8_11, nyb12)

nyba <- nyba %>%
arrange(desc(total_count))

tbl_df(nyba)

sum(nyba$total_count)

ny <-readOGR("D:/nybb_17a","nybb")

ny_wgs84 <- spTransform(ny, CRS("+proj=longlat +datum=WGS84"))

ny_wgs84@data$id = rownames(ny_wgs84@data)
ny_wgs84.points = fortify(ny_wgs84, region="id")
ny_wgs84.df = join(ny_wgs84.points, ny_wgs84@data, by="id")
ny_wgs84.df

tbl_df(ny_wgs84.df)

idList <- ny_wgs84@data$BoroName

centroids.df <- as.data.frame(coordinates(ny_wgs84))
names(centroids.df) <- c("Longitude", "Latitude")
id.df <- data.frame(id = idList, centroids.df)

tbl_df(id.df)


ny_hhw <- ggplot(nyba, aes(longitude, latitude))  + #"id" is col in your df, not in the map object 
  expand_limits(x = ny_wgs84.df$long, y = ny_wgs84.df$lat) + 
  geom_polygon(data= ny_wgs84.df, aes(x=long, y=lat, group=group), fill="white", color="pink", size=0.15) +
  geom_point(aes(color=total_count, size=total_count, alpha=total_count) ) + 
  geom_text(data=id.df, aes(label = id, x = Longitude, y = Latitude), size=3) +
  labs(x = "Longitude", y = "Latitude", title = "NYC-Heat/Hot Water") + 
  scale_colour_gradient(low = "#ffb2b2", high = "black")
ny_hhw

table(nyba$total_count)
summary(nyba)

