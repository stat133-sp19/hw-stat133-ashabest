## --------------------------------------------------------
## Shots Data
## 
## Generates a CSV data file containing variables to be used
## for data visualization of GSW shots data, along with text
## files for summaries of the given data plus the consolidated
## shots data.
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
## - data/shots-data.csv
## - output/andre-iguodala-summary.txt
## - output/draymond-green-summary.txt
## - output/kevin-durant-summary.txt
## - output/klay-thompson-summary.txt
## - output/stephen-curry-summary.txt
## - output/shots-data-summary.txt
##
## --------------------------------------------------------

# set types for each variable
col_classes <- c("character", "character", "integer", "integer", "real", "real", "character", "character", "character", "real", "character", "real", "real")

# read in csv files as data frames
iguodala <- read.csv("../data/andre-iguodala.csv", colClasses = col_classes, stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", colClasses = col_classes, stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", colClasses = col_classes, stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", colClasses = col_classes, stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", colClasses = col_classes, stringsAsFactors = FALSE)

# add column "name" to containing name of player
iguodala$name = "Andre Iguodala"
green$name = "Draymond Green"
durant$name = "Kevin Durant"
thompson$name = "Klay Thompson"
curry$name = "Stephen Curry"

# make values of shot_made_flag more descriptive
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] = "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] = "shot_yes"

green$shot_made_flag[green$shot_made_flag == "n"] = "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] = "shot_yes"

durant$shot_made_flag[durant$shot_made_flag == "n"] = "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] = "shot_yes"

thompson$shot_made_flag[thompson$shot_made_flag == "n"] = "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] = "shot_yes"

curry$shot_made_flag[curry$shot_made_flag == "n"] = "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] = "shot_yes"

# add column "minute" containing minute number when shot was attempted
iguodala$minute = iguodala$period * 12 - iguodala$minutes_remaining
green$minute = green$period * 12 - green$minutes_remaining
durant$minute = durant$period * 12 - durant$minutes_remaining
thompson$minute = thompson$period * 12 - thompson$minutes_remaining
curry$minute = curry$period * 12 - curry$minutes_remaining

sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

# stack all tables into a single data frame
shots_data <- rbind(iguodala, green, durant, thompson, curry)

write.csv(
  x = shots_data,
  file = '../data/shots-data.csv'
)

sink(file = '../output/shots-data-summary.txt')
summary(shots_data)
sink()
