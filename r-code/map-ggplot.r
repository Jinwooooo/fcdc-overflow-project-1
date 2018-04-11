library(ggplot2)

geo.data <- ggplot2::map_data('world')


ggplot() +  geom_polygon(data=geo.data, aes(x=long, y=lat, group=group))
