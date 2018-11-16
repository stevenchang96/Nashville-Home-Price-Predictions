setwd("~/Library/Mobile Documents/com~apple~CloudDocs/School/UPenn/MUSA 507/Midterm Project/midtermData_forStudents")

library(stargazer)
library(corrplot)
library(caret) 
library(AppliedPredictiveModeling)
library(stargazer)
library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(sf)
options(scipen=999)
library(tidyr)
library(knitr)
library(reshape2)
library(lubridate)
library(dplyr)
library(spdep)
library(leaflet)
library(viridis)
library(spdep)
library(viridis)

#Load Data and name 
dataset<-read_csv("/Users/irondisciple/Library/Mobile Documents/com~apple~CloudDocs/School/UPenn/MUSA 507/Midterm Project/midtermData_forStudents/train.and.test_student.csv")
Nashville<-subset(dataset,dataset$LocationCity=="NASHVILLE")

# Correlation Matrix
Nash<- select(Nashville,-OwnerInstrument, -OwnerName, -OwnerAddress1, -OwnerAddress2,
-OwnerCity, -OwnerState, -OwnerZip, -OwnerCountry, -LocationAddress, -LocationCity,
-TaxDistrict, -ParcelId_property, -District, -LandUseDescription, 
-LandUseFullDescription, -Building_Type, -Story_Height, -Exterior_Wall, 
-Grade, -Frame, -avgHtfl_building, -Phys_Depreciation, -Land_Unit_Type,
-HeatingType, -Fixtures, -Foundation,
-notheated,-CouncilDistrict,-CensusBlock,-Card,-sf_ifla,-fpla,-test,-kenID)


M <- cor(Nash)
corrplot(M, method = "number",tl.cex=0.6)

Nashville$OwnerAddress2<-NULL
Nashville$notheated<-NULL
Nashville$HeatingType<-NULL

Nashville<- 
Nashville%>%
filter(Building_Type== "SINGLE FAMILY" |Building_Type == "RESIDENTIAL CONDO")%>%
mutate(isSingleFamily =as.factor(ifelse(Building_Type== "SINGLE FAMILY" ,1,0)))


# Feature Engineering Yearly Built, Number of Basement, number of Bedrooms and Units in Building
Nashville<- 
Nashville%>% 
mutate(YR_BUILT = as.factor(yearbuilt_building),
NUM_BEDROOMS = as.factor(bedroomsunits_building),
NUMBER_BATHS=as.factor(baths),
NUMBER_OFROOMS = as.factor(roomsunits_building)) %>%
as.data.frame()

# Making Dummy Variables for Houses that are physically depreciated 
Nashville<- 
Nashville%>%
filter(Phys_Depreciation== "Average" |Phys_Depreciation == "Good")%>%
mutate(isVeryGood = as.factor(ifelse(Phys_Depreciation== "Average" ,1,0)))

# Taking Care of Nas
Nashville<-Nashville[,-56]

check<-which(!complete.cases(Nashville))
check

Nashville_NoNA<-Nashville[-check,]

# Summary Statistics and getting the class
stargazer(Nashville_NoNA, type="text", title = "Summary Statistics of All Variables")


# Summary Statistics of Internal Characteristics
Internal_Characteristics<-Nashville_NoNA[c(31,32,33,35:42,47:49,56,57,58)]
stargazer(Internal_Characteristics, type="text", title = "Summary Statistics of Variables with Internal Characteristics")


Amenities<-Nashville_NoNA[c(25,26,27,28,42,49)]
stargazer(Amenities,type="text",title = "Summary Statistics of Variables with Amenities")

Spatial<-Nashville_NoNA[c(17,4:23,29,30,44:46,50:51,53)]
stargazer(Spatial, type="text", title = "Summary Statistics of Variables with Spatial Structure")

#Getting Basemap of Nashville and Mapping SalesPrice of Houses
baseMap <-read_sf("https://data.nashville.gov/api/geospatial/xxxs-vvs4?method=export&format=GeoJSON")

# Dissolving Basemap
ggplot() +
geom_sf(data=st_union(baseMap)) +
labs(title="Nashville basemap")

# Mapping SalesPrice in Nashville
ggplot() + 
geom_sf(data=st_union(baseMap,crs=4326)) +
geom_point(data=Nashville, aes(x = WGS1984X, y = WGS1984Y, colour=factor(ntile(Nashville$SalePrice,5))), 
size = 1) + 
scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
labels=as.character(quantile(Nashville$SalePrice,
c(.1,.2,.4,.6,.8),na.rm=T)),
name="Housing Sales Prices\n(Quintile\nBreaks)") +
labs(title="Housing Sales Prices, Nashville")

# Mapping Square feet of Finished Area in Nashville
ggplot() + 
geom_sf(data=st_union(baseMap,crs=4326)) +
geom_point(data=Nashville, aes(x = WGS1984X, y = WGS1984Y, colour=factor(ntile(Nashville$sf_finished,5))), 
size = 1) + 
scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
labels=as.character(quantile(Nashville$sf_finished,
c(.1,.2,.4,.6,.8),na.rm=T)),
name="Square Feet of Finished Area\n(Quintile\nBreaks)") +
labs(title="Square Feet of Finished Area, Nashville")

# Mapping Neigborhood Accessor
ggplot() + 
geom_sf(data=st_union(baseMap,crs=4326)) +
geom_point(data=Nashville, aes(x = WGS1984X, y = WGS1984Y, colour=factor(ntile(Nashville$NeighborhoodAssessor,5))), 
size = 1) + 
scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
labels=as.character(quantile(Nashville$NeighborhoodAssessor,
c(.1,.2,.4,.6,.8),na.rm=T)),
name="Neighborhood Accessor\n(Quintile\nBreaks)") +
labs(title="Neighborhood Accessor, Nashville")


ggplot() + 
geom_sf(data=st_union(baseMap,crs=4326)) +
geom_point(data=Nashville, aes(x = WGS1984X, y = WGS1984Y, colour=factor(ntile(Nashville$sf_finished_less_ifla,5))), 
size = 1) + 
scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
labels=as.character(quantile(Nashville$sf_finished_less_ifla,
c(.1,.2,.4,.6,.8),na.rm=T)),
name="Total Finished area less an adjustment\n(Quintile\nBreaks)") +
labs(title="Total Finished area less an adjustment, Nashville")

p <- ggplot(Nashville, aes(NUM_BEDROOMS, fill =isSingleFamily))
p + geom_bar()+ ggtitle("NUMBER OF BEDROOMS IN SINGLE FAMILY VS RESIDENTIAL CONDO")+xlab("NUMBER OF BEDROOMS")


# SalesValues, creating Training and Testing Set

SalesValues<-Nashville_NoNA%>%
as.data.frame()%>%
filter(test==0)


inTrain <- createDataPartition(
y = SalesValues$SalePrice, 
p = .75, list = FALSE)

training <- SalesValues[inTrain,]
test<-subset(Nashville,Nashville$test==1)

# Building Regression using the Training as Data

reg<-lm(SalePrice~kenID+LocationZip+CouncilDistrict+CensusBlock+accountnumber_property+
yearbuilt_building+
NeighborhoodAssessor+
sf_fin_less_ifla_less_bfin+sf_sketched+Zone_Assessor+baths+WGS1984X+WGS1984Y+NUMBER_BATHS+effyearbuilt_building+NUM_BEDROOMS+isSingleFamily,data = training)
summary(reg)

# Table of Regression Result using the Training Data
stargazer(reg, type = 'text',title="Summary Statistics of Training Set")


# Predicting for the Observed, since the Observed is 0 and can't be predicted,Inf will be the abs error
regPred <- predict(reg,test) 
regPred

regPred <- 
  data.frame(observedSales = test$SalePrice,
             predictedSales = regPred)
regPred <-
  regPred %>%
  mutate(error = predictedSales - observedSales) %>%
  mutate(absError = abs(predictedSales - observedSales)) %>%
  mutate(percentAbsError = abs(predictedSales - observedSales) / observedSales) 


head(regPred)


head(regPred)

mean(regPred$absError)


set.seed(825)
fitControl<-trainControl(method = "cv",number = 100)

lmFit <- train(SalePrice ~LocationZip+CouncilDistrict+CensusBlock+accountnumber_property+
                 yearbuilt_building+
                 NeighborhoodAssessor+
                 sf_fin_less_ifla_less_bfin+sf_sketched+Zone_Assessor+baths+WGS1984X+WGS1984Y+NUMBER_BATHS+isVeryGood, data = Nashville_NoNA, 
               method = "lm", trControl = fitControl)

stargazer(lmFit, type = 'text',title="Summary Statistics of lmFit")
lmFit

stargazer(lmFit$resample, type="text",title = "Summary Statistics of lmFit")

# Checking to see if the model is Generazable.If our model is generalizable, we should expect relatively comparable goodness of fit metrics across each subset

sd(lmFit$resample[,3])

# One of the folds had a MAE that was much different the others. Looks like 10 is enough fold but maybe it's not enough to make decisions. 

ggplot(as.data.frame(lmFit$resample), aes(MAE)) + 
  geom_histogram(bins=20) +
  labs(x="Mean Absolute Error",
       y="Count")

library(ggplot2)
regDF <- cbind(Nashville$SalePrice, reg$fitted.values)
colnames(regDF) <- c("Observed", "Predicted")
regDF <- as.data.frame(regDF)
ggplot() + 
  geom_point(data=regDF, aes(Observed, Predicted)) +
  stat_smooth(data=regDF, aes(Observed, Observed), method = "lm", se = FALSE, size = 1, colour="red") + 
  stat_smooth(data=regDF, aes(Observed, Predicted), method = "lm", se = FALSE, size = 1, colour="blue") + 
  labs(title="Predicted Sales Volume as a function\nof Observed Sales Volume",
       subtitle="Perfect prediction in red; Actual prediction in blue") +
  theme(plot.title = element_text(size = 18,colour = "black"))


regA<-lm(SalePrice~kenID+LocationZip+CouncilDistrict+CensusBlock+accountnumber_property+
           yearbuilt_building+
           NeighborhoodAssessor+
           sf_fin_less_ifla_less_bfin+sf_sketched+Zone_Assessor+baths+WGS1984X+WGS1984Y+NUMBER_BATHS+effyearbuilt_building+NUM_BEDROOMS+isSingleFamily,data = Nashville_NoNA)

regA_residuals <- data.frame(regA$residuals)
hedLonLat <- data.frame(Nashville_NoNA$WGS1984X,Nashville_NoNA$WGS1984Y)
residualsToMap <- cbind(hedLonLat, regA_residuals )
colnames(residualsToMap) <- c("longitude", "latitude", "residual")

ggplot() + 
  geom_sf(data=st_transform(baseMap,crs=4326)) +
  geom_point(data = residualsToMap, 
             aes(x=longitude, y=latitude, color=residual), 
             size = .1) + 
  labs(title="Regression residuals") +
  scale_color_viridis()


Zipcode<-read_sf("https://data.nashville.gov/api/geospatial/u7r5-bpku?method=export&format=GeoJSON")
reg_Predicted <- data.frame(mean(regPred$absError))
hedLonLat <- data.frame(Nashville$WGS1984X,Nashville$WGS1984Y)
PredictToMap <- cbind(hedLonLat,reg_Predicted)
colnames(PredictToMap) <- c("longitude", "latitude", "MAE")


# Map of Residuals for Test Set
ggplot() + 
  geom_sf(data=st_transform(Zipcode,crs=4326)) +
  geom_point(data =hedLonLat, 
             aes(x=Nashville$WGS1984X, y=Nashville$WGS1984Y, color=mean(regPred$absError)), 
             size = .1) + 
  labs(title="Regression Mean Absolute Error") +
  scale_color_viridis()


# Mean Price per Zipcode

Zipcode<-read_sf("https://data.nashville.gov/api/geospatial/u7r5-bpku?method=export&format=GeoJSON")
reg_Predicted <- data.frame(mean(Nashville$SalePrice))
hedLonLat <- data.frame(Nashville$WGS1984X,Nashville$WGS1984Y)
PredictToMap <- cbind(hedLonLat,reg_Predicted)
colnames(PredictToMap) <- c("longitude", "latitude", "Mean Price per Zipcode")


# Map of Mean Price Per Zipcode
ggplot() + 
geom_sf(data=st_transform(Zipcode,crs=4326)) +
geom_point(data =hedLonLat, 
aes(x=Nashville$WGS1984X, y=Nashville$WGS1984Y, color=mean(Nashville$SalePrice)), 
size = .1) + 
labs(title="Mean Price per Zip Code") +
scale_color_viridis()
