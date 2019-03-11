## ------------------------------------------------------------------
## Shot Charts
## 
## Generates graphs for each player displaying court coordinates
## for each shot attempted, and whether the shot was made or missed. 
## Also creates a consolidated faceted graph for all 5 players.
##
## Inputs required:
## Shot statistics files:
## - data/andre-iguodala.csv
## - data/draymond-green.csv
## - data/kevin-durant.csv
## - data/klay-thompson.csv
## - data/stephen-curry.csv
##
## Outputs created:
## images/andre-iguodala-shot-chart.pdf
## images/draymond-green-shot-chart.pdf
## images/kevin-durant-shot-chart.pdf
## images/klay-thompson-shot-chart.pdf
## images/stephen-curry-shot-chart.pdf
## images/gsw-shot-charts.pdf
##
## ------------------------------------------------------------------

## script code adapted from workout01 spec example

library(jpeg)
library(grid)

# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"

# create raster object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc")
)

# generate shot charts
shot_chart_iguodala <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shots: Andre Iguodala (2016 season)') +
  theme_minimal()

shot_chart_green <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shots: Draymond Green (2016 season)') +
  theme_minimal()

shot_chart_durant <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shots: Kevin Durant (2016 season)') +
  theme_minimal()

shot_chart_thompson <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shots: Klay Thompson (2016 season)') +
  theme_minimal()

shot_chart_curry <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shots: Stephen Curry (2016 season)') +
  theme_minimal()

#pdf(filename = "../images/gsw-shot-charts.pdf", 
#    width = 6.5, height = 5)
#shot_chart_thompson
#dev.off()
