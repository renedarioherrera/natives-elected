# project setup ####
# package libraries
library(here)

# create folders and files 
dir.create(path = "scripts")
dir.create(path = "reports")
dir.create(path = "data-raw")
dir.create(path = "data-viz")
dir.create(path = "data-tidy")
file.create("scripts/00-setup.R")
file.create("scripts/01-read.R")
file.create("scripts/02-tidy.R")
file.create("scripts/03-transform.R")
file.create("scripts/04-data-viz.R")

# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
}

# Usage example
packages<-c(
  "tidycensus",
  "mapview",
  "tigris"
)

check.packages(packages)
