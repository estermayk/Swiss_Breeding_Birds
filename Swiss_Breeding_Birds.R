#---- 1. Conceptualisation ----
install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
library(geodata)
library(terra)

#---- Q1 -----
#You will be analysing presence/absence data using a generalized linear model (GLM), what distribution family should you choose?
#GLM with binomial family errors

#---- 2. Data Preparation ----
avi_dat <- read.table('Data/Data_SwissBreedingBirds.csv', header=T, sep=',')

nrow(avi_dat)

summary(avi_dat)
#contains data on 56 bird species across 2535 grid cells
#doesn't include spatial information (lat and long)
#already matched with worldclim data so no matter

ouzel_cols <- c('Turdus_torquatus', 'bio_5', 'bio_2', 'bio_14', 'blockCV_tile') #selecting the columns we'll use
summary(ouzel_cols)

ouzel_df <- data.frame(avi_dat)[ouzel_cols] #turning it into useable data
summary(ouzel_df)

#now we need to get the worldclim data
output_dir<-"practical1_climdata" #telling it to put the data into this folder

bio_curr <-worldclim_country("Switzerland",version="2.1", var='bio', res=10, lon=5.5, lat=45.5, path=output_dir)[[c(2,5,14)]] #pulling from the packages we already grabbed - current

bio_fut <- cmip6_world(var = "bio", model = "CNRM-CM6-1-HR", ssp = "245", res = 10,  time = "2041-2060",  lon = c(5.96, 10.49),  lat = c(45.82, 47.81),path=output_dir)[[c(2,5,14)]] #and future

# A spatial mask of Switzerland in Swiss coordinates

bg <- rast('/vsicurl/https://damariszurell.github.io/SDM-Intro/CH_mask.tif')

bio_curr <- terra::project(bio_curr, bg)

bio_fut <- terra::project(bio_fut, bg)

#we need to change the projection of our cliamte data to match that of the bg file.

bio_curr <- terra::resample(bio_curr, bg)

bio_fut <- terra::resample(bio_fut, bg)

#we then need to make the resolution equivalent to bg. 

bio_curr <- terra::mask(bio_curr, bg)

bio_fut <- terra::mask(bio_fut, bg)

#we then need to clip the extent to match an outline of Switzerland

names(bio_curr) <- c('bio_2', 'bio_5', 'bio_14')

names(bio_fut) <- c('bio_2', 'bio_5', 'bio_14')

#this code is copied, ally said to do so in interest of time 

#lets plot and save visualisations

png(filename='Figs/bio_curr.png')
plot(bio_curr)
dev.off()

png(filename='Figs/bio_fut.png')
plot(bio_fut)
dev.off()

#---- 3. Model Fitting ----

model <- glm(Turdus_torquatus ~ bio_2 + I(bio_2^2) + bio_5 + I(bio_5^2) + bio_14 + I(bio_14^2), family = 'binomial', data=ouzel_df)
summary(model)

#---- Q2 ----
#From the model summary, which variables look to be having an important effect?
#bio 5 and 14?
#yes

#---- 4. Testing and Critiquing our Model ----
#---- 4.1. Partial Effects ----
#we need to understand how each climatic variable impacts presence/absence of ouzels
#so we need to plot the partial effects - effect of each variable while holding the other variables constant
#using predict function for each variable
#to do this we will generate a new data frame for each in which our focal variable varies but the others are held constant

#for bio_2
bio_2<-seq(min(ouzel_df$bio_2),max(ouzel_df$bio_2),0.1) #created bio_2 in which we sequence between the min and max with regards to bio2 by increments of 0.1

newdata_bio2<-expand.grid(bio_2,mean(ouzel_df$bio_5),mean(ouzel_df$bio_14)) #here we're setting bio5 and 14 to their means so they don't vary

names(newdata_bio2)<-c("bio_2","bio_5","bio_14") #here we're creating a dataset that uses varying bio2 and fixed 5 and 14

#To use predict we need to generate a new dataset. We will set the other two climate variables at their mean.

response<-predict(model,newdata=newdata_bio2,type="response")

#We've told the predict function to make a prediction on the response scale, ie. in terms of presence/absence

plot(newdata_bio2$bio_2,response,type="l")

#for bio_5
bio_5<-seq(min(ouzel_df$bio_5),max(ouzel_df$bio_5),0.1) 

newdata_bio5<-expand.grid(bio_5,mean(ouzel_df$bio_2),mean(ouzel_df$bio_14)) 

names(newdata_bio5)<-c("bio_5","bio_2","bio_14") 

response1<-predict(model,newdata=newdata_bio5,type="response")

plot(newdata_bio5$bio_5,response1,type="l")

#for bio_14
bio_14<-seq(min(ouzel_df$bio_14),max(ouzel_df$bio_14),0.1) 

newdata_bio14<-expand.grid(bio_14,mean(ouzel_df$bio_2),mean(ouzel_df$bio_5)) 

names(newdata_bio14)<-c("bio_14","bio_2","bio_5") 

response1<-predict(model,newdata=newdata_bio14,type="response")

plot(newdata_bio14$bio_14,response1,type="l")

