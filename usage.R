###
### COLOR CHECKER
###



## Load required packages ---

library(sp)
library(cli)
library(raster)
library(imager)
library(usethis)


## Load R function to recalibrate colors in images ---

source(file.path("R", "color_checker.R"))


## Import color chart (true colors values) ----

color_chart <- as.data.frame(
  readr::read_csv(
    file = file.path("data", "patch_values.csv")
  )
)


## List pictures to recalibrate ----

images <- list.files(
  path        = file.path("data", "images"),
  pattern     = "jpg$",
  ignore.case = TRUE,
  full.names  = TRUE
)


## Recalibrate colors in listed images ----

color_checker(
  images      = images,
  color_chart = color_chart,
  output_path = file.path("outputs", "recalibrations"),
  cell_size   = 0.2
)
