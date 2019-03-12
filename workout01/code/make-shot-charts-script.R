## ------------------------------------------------------------------
## Shot Charts
## 
## Generates graphs for each player displaying court coordinates
## for each shot attempted, and whether the shot was made or missed. 
## Also creates a consolidated faceted graph for all 5 players.
##
## Inputs:
## - data/shots-data.csv
## - images/nba-court.jpg
##
## Outputs:
## - images/andre-iguodala-shot-chart.pdf
## - images/draymond-green-shot-chart.pdf
## - images/kevin-durant-shot-chart.pdf
## - images/klay-thompson-shot-chart.pdf
## - images/stephen-curry-shot-chart.pdf
## - images/gsw-shot-charts.pdf
## - images/gsw-shot-charts.png
##
## ------------------------------------------------------------------

# load packages
library(ggplot2)
library(jpeg)
library(grid)

# import data
col_classes <- c("NULL", "character", "character", "integer", 
                 "integer", "real", "real", "character",
                 "character", "character", "real", "character",
                 "real", "real", "character", "real")

shots_data <- read.csv("../data/shots-data.csv",
                       colClasses = col_classes,
                       stringsAsFactors = FALSE)

iguodala <- shots_data[shots_data$name == "Andre Iguodala", ]
green <- shots_data[shots_data$name == "Draymond Green", ]
durant <- shots_data[shots_data$name == "Kevin Durant", ]
thompson <- shots_data[shots_data$name == "Klay Thompson", ]
curry <- shots_data[shots_data$name == "Stephen Curry", ]

## court image background and plotting code adapted from 
## workout01 spec example

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

# export charts to pdf files
pdf(file = "../images/andre-iguodala-shot-chart.pdf", 
    width = 6.5, height = 5)
shot_chart_iguodala
dev.off()

pdf(file = "../images/draymond-green-shot-chart.pdf", 
    width = 6.5, height = 5)
shot_chart_green
dev.off()

pdf(file = "../images/kevin-durant-shot-chart.pdf", 
    width = 6.5, height = 5)
shot_chart_durant
dev.off()

pdf(file = "../images/klay-thompson-shot-chart.pdf", 
    width = 6.5, height = 5)
shot_chart_thompson
dev.off()

pdf(file = "../images/stephen-curry-shot-chart.pdf", 
    width = 6.5, height = 5)
shot_chart_curry
dev.off()

# generate faceted chart, all players
facet_shot_chart <- ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  facet_wrap(~ name) +
  ggtitle('Shots: GSW (2016 season)') +
  theme_minimal() +
  theme(legend.position="top", 
        legend.box = "horizontal", 
        legend.title = element_blank())

# export to pdf
pdf(file = "../images/gsw-shot-charts.pdf", 
    width = 8, height = 7)
facet_shot_chart
dev.off()

# export to png
png(filename = "../images/gsw-shot-charts.png",
    width = 8, height = 7, units = "in", res = 72)
facet_shot_chart
dev.off()
