#---- 1. Conceptualisation ----
install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
library(geodata)
library(terra)
#---- Q1 -----
#You will be analysing presence/absence data using a generalized linear model (GLM), what distribution family should you choose?
#GLM with binomial family errors
#---- Data Preparation ----
avi_dat <- read.table('Data/Data_SwissBreedingBirds.csv', header=T, sep=',')
nrow(avi_dat)
summary(avi_dat)
#contains data on 56 bird species across 2535 grid cells
#doesn't include spatial information (lat and long)
#already matched with worldclim data so no matter
