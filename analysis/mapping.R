library(viridis)
library(tidyverse)
library(cowplot)     # for theme_map()
library(colorspace)  # for scale_fill_continuous_sequential()
library(sf)       

df<-read_csv("analytic_data.csv")
df
shapes<-sf::st_read(system.file("C://Users//Andrew//Desktop//Statistics and Data Analysis//va_tracking-master//data_files//", package = "sf"))
va <- sf::st_read(system.file("shape/tl_2016_51_cousub.shp", package = "sf"), quiet = TRUE)

va <- sf::st_read(
  "C://Users//Andrew//Desktop//Statistics and Data Analysis//va_tracking-master//data_files//admin_shapefiles//VirginiaCounty.shp")
plot(va)
df
head(va)
va <- va %>% 
  mutate(Name = NAMELSAD )
va <- va %>% 
  left_join(df, by = "Name")
va

# GO TO EXCEL. FIND AND REPLACE city WITH City 
ggplot(va, aes(fill = d_index )) + 
  geom_sf(color = "white") +
  scale_fill_continuous_sequential(
    palette = "Blues", rev = TRUE,
    na.value = "grey60",
    name = "D-Index (Kelly, 1999)",
    #limits = c(9000, 21000),
    # breaks = 3000*c(1:5),
    # labels = c("$9,000", "$12,000", "$15,000", "$18,000", "$21,000")
    guide = guide_colorbar(
      direction = "horizontal",
      label.position = "bottom",
      title.position = "top",
      barwidth = grid::unit(3.0, "in"),
      barheight = grid::unit(0.2, "in")
    )
  ) + 
  theme_map(12) +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.02, 0.8)
  )

