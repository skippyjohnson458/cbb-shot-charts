
library(tidyverse) #Data manipulation
library(data.table) #Faster loading of CSVs
library(janitor) #Clean names function
library(sportyR) #Geom_basketball function

## Dataframe of downloaded plays from collegebasketballdata.com

data <- fread("plays.csv") 


shots <- data |>
  filter(ShootingPlay == TRUE) |>
  clean_names()

unique(shots$shot_info_shooter_name) #List of all players who have taken shots

shots2 <- shots |>
  filter(!is.na(shot_info_location_x)) |> #Removes Rows with missing shot location data. 
  filter(shot_info_shooter_name == "Alvaro Cardenas") |>
  mutate(
    x_scaled = shot_info_location_x / 10, #Divide by 10 to get feet
    y_scaled = shot_info_location_y / 10, #Same
    x_half = ifelse(x_scaled < 47, 94 - x_scaled, x_scaled) #Reflects the x-axis for shots on the other side of the court
  )


## Geom_basketball puts 0,0 at the center of the court will CBBD puts it at the bottom left, so we need to move the x axis 47 feet and the y axis 25 feet. 

geom_basketball(league = "ncaa", court_units = "ft", display_range = "offense", y_trans = 25, x_trans = 47,
                color_updates = list(
                  offensive_half_court = "#ffffff", 
                  defensive_half_court = "#ffffff",
                  court_apron = "#ffffff",
                  two_point_range = c("#ffffff", "#ffffff"),
                  center_circle_fill = "#ffffff",
                  painted_area = c("#ffffff", "#ffffff"),
                  free_throw_circle_fill = "#ffffff",
                  sideline = "#000000",
                  endline = "#000000",
                  division_line = "#000000",
                  center_circle_outline = "#000000",
                  lane_boundary = c("#000000", "#000000"),
                  three_point_line = c("#000000", "#000000"),
                  free_throw_circle_outline = "#000000",
                  lane_space_mark = "#000000",
                  restricted_arc = "#000000",
                  backboard = "#000000"
                )
) +
  geom_point(aes(shots2$x_half, y = shots2$y_scaled, color = shots2$shot_info_made), size = 2)









