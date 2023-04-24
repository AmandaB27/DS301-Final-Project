#DS 301 Final Project | Prediction Question
#------------------------------------------------------------------------------#
#Prediction: Predict county's life expectancy given a set of health predictors. 


#SETUP-------------------------------------------------------------------------#
#Checking & setting directory
getwd() #check current working directory

setwd("D:/School Spring 2023/DS 301/Final Project") #change current working directory

#Import data set to the variable life

life = read.csv("Life-Expectancy-Data-Updated.csv",header=TRUE)

head(life)
str(life)


#TRAINING & TESTING SET--------------------------------------------------------#

#Training set
#Want averages across years 2000-2014 for each country
#Get list of all unique counties in the data set
#for each country, select observations from all years 2000-2014
  #Create one observation with average of predictors from 2000-2014


countries = unique(life$Country)#used to find the countries
head(countries)

length(countries) #validate right number of countries



#want to fill a matrix with 179 observations (countries) and 19 predictors
countryAverages = matrix(NA,length(countries),19)
region = rep(NA,179)

## loop over all observations
for(i in 1:length(countries)){
  
  #select observations from all years 2000-2014
  #countryObs is a Boolean list of TRUE/FALSE
  countryObs = (life$Country == countries[i])
  
  #get all observations for that country 
  validObservations = life[countryObs,]
  validObservations = validObservations[validObservations$Year != 2015, ] #remove 2015 data
  region[i] = validObservations$Region[1]
  
  #remove the developing vs developing indicator variable predictors
  validObservations = head(validObservations)[c(-19,-20)]
  
  #placeholder, numerical predictors will change due to loop below 
  #TODO : Try to get this to work if we can, have a second fix below
  #countryAverages[i,] = c(validObservations[1,])
  
  #for each numeric predictor find average across 2000-2014
  #I excluded the first 3 because they are nonnumerical and constant.
  #TODO: Is there a better way to iterate 4:19 in loop?
  for(j in c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)){
    
    #for every predictor j, find average and add to that country's observation
    #countryAverages[i,j] = format(round(mean(validObservations[[j]]),5))
    countryAverages[i,j] = as.numeric(mean(validObservations[[j]]))
    
  }
  
}

countryAverages[,1] = countries
countryAverages[,2] = region
head(countryAverages)
str(countryAverages[,4])
#TODO: want to add back in countries and regions. Can we do this faster/easier?

#create a data frame and assign column names
training = as.data.frame(countryAverages[,-3])
colnames(training) <- c(names(validObservations)[-c(3)])
head(training)
dim(training)


#Testing set
#Get observations for every country from 2015, Removed year as it is unneeded.

test = life[life$Year == 2015, ][-c(3,20,21)]
head(test)
dim(test)

str(test)
str(training)




