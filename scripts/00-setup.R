# project setup ####
# package libraries
library(here)

# create folders and files 
dir.create(path = "scripts")
dir.create(path = "reports")
dir.create(path = "data-viz")
dir.create(path = "data-tidy")
file.create("scripts/00-setup.R")
file.create("scripts/01-read.R")
file.create("scripts/02-tidy.R")
file.create("scripts/03-transform.R")
file.create("scripts/04-data-viz.R")

install.packages(c(
  "here",
  "tidyverse",
  "tidycensus"
))