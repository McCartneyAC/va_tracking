library(viridis)
library(tidyverse)
library(cowplot)     # for theme_map()
library(colorspace)  # for scale_fill_continuous_sequential()
library(sf)       


theme_map <- function(font_size = 14, font_family = "", line_size = .5,
                      rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14){
  # work based off of theme_cowplot to get font sizing right
  theme_cowplot(font_size = font_size, font_family = font_family, line_size = line_size,
                rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) %+replace%
    theme(
      line = element_blank(),
      rect = element_blank(),
      
      axis.line =          element_blank(),
      axis.line.x =        NULL,
      axis.line.y =        NULL,
      axis.text =          element_blank(),
      axis.text.x =        NULL,
      axis.text.x.top =    NULL,
      axis.text.y =        NULL,
      axis.text.y.right =  NULL,
      axis.ticks =         element_blank(),
      axis.ticks.length =  unit(0, "pt"),
      axis.title =         element_blank(),
      axis.title.x =       NULL,
      axis.title.x.top =   NULL,
      axis.title.y.right = NULL,
      complete = TRUE
    )
}




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

#### bivariate chloropleth
# create 3 buckets for gini
quantiles_diversity <- df %>%
  pull(d_index) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

# create 3 buckets for mean income
quantiles_electivity <- df %>%
  pull(metric) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high Diversity, high Electivity
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low Diversity, high Electivity
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium Diversity, medium Electivity
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high Diversity, low Electivity
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low Diversity, low Electivity
) %>%
  gather("group", "fill")

# attach bivar color scale
va <- va %>% 
  mutate(
    diversity_quantiles = cut(
      d_index,
      breaks = quantiles_diversity,
      include.lowest = TRUE
    ),
    electivity_quantiles = cut(
      metric,
      breaks = quantiles_electivity,
      include.lowest = TRUE
    ),    
    group = paste(
      as.numeric(diversity_quantiles), "-",
      as.numeric(electivity_quantiles)
    )) %>%
  left_join(bivariate_color_scale, by = "group")


# breathe easy now


map <- va %>% 
  ggplot() + 
  geom_sf(aes(
      fill = fill
    )) + 
  scale_fill_identity() +
  labs(x = NULL,
       y = NULL,
       title = "Virginia: How Course Tracking Relates to Diversity") +
  theme_map()

map

bivariate_color_scale <- bivariate_color_scale %>% 
  separate(group, into = c("Diversity", "Trackedness"), sep = " - ") %>%
  mutate(Diversity = as.integer(Diversity),
         Trackedness = as.integer(Trackedness))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = Diversity,
      y = Trackedness,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "More Diverse ??????",
       y = "More Course Tracks ??????") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 12)
  ) +
  # quadratic tiles
  coord_fixed()
legend
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.5, 0.25, 0.25)


