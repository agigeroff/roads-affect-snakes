###poisson modelling

library(lme4) #load glmm package
library(car)
library(glmtoolbox)
library(PerformanceAnalytics)
library(ggplot2)
library(lmerTest)

#import data
snake.data=read.csv("C://Users//User//Desktop//Research//Research_Garter_Snakes//Stats and Analyses//Model Building 2//All Data Poisson.csv")

#scale all continuous predictor variables
snake.data$julian.date=scale(snake.data$ï..date, center=T, scale=T)
snake.data$temperature=scale(snake.data$temp_avg, center=T, scale=T)
snake.data$time=scale(snake.data$avg_time, center=T, scale=T)
snake.data$field.200=scale(snake.data$field_200m, center=T, scale=T)
snake.data$field.900=scale(snake.data$field_900m, center=T, scale=T)
snake.data$field.1000=scale(snake.data$field_1000m, center=T, scale=T)
snake.data$forest.200=scale(snake.data$forest_200m, center=T, scale=T)
snake.data$forest.300=scale(snake.data$forest_300m, center=T, scale=T)
snake.data$forest.700=scale(snake.data$forest_700m, center=T, scale=T)
snake.data$forest.800=scale(snake.data$forest_800m, center=T, scale=T)
snake.data$road.300=scale(snake.data$road_300m, center=T, scale=T)
snake.data$road.500=scale(snake.data$road_500m, center=T, scale=T)
snake.data$road.800=scale(snake.data$road_800m, center=T, scale=T)
snake.data$sides.300=scale(snake.data$sides_300m, center=T, scale=T)
snake.data$sides.500=scale(snake.data$sides_500m, center=T, scale=T)
snake.data$sides.600=scale(snake.data$sides_600m, center=T, scale=T)
snake.data$urban.100=scale(snake.data$urban_100m, center=T, scale=T)
snake.data$urban.200=scale(snake.data$urban_200m, center=T, scale=T)
snake.data$urban.300=scale(snake.data$urban_300m, center=T, scale=T)
snake.data$urban.1000=scale(snake.data$urban_1000m, center=T, scale=T)
snake.data$water.600=scale(snake.data$water_600m, center=T, scale=T)
snake.data$water.800=scale(snake.data$water_800m, center=T, scale=T)
snake.data$water.900=scale(snake.data$water_900m, center=T, scale=T)

#garter count 
##gvif tests to remove extraneous variables

garter.count.test.vif=lm(count_g~julian.date+temperature+time+site+boards+mowed+
                           urban.200+field.1000+forest.300+water.900+road.500+sides.500,
                         data=snake.data)
gvif(garter.count.test.vif) #remove forest
garter.count.test.vif2=lm(count_g~julian.date+temperature+time+site+boards+mowed+
                           urban.200+field.1000+water.900+road.500+sides.500,
                         data=snake.data)
gvif(garter.count.test.vif2) #remove sides
garter.count.test.vif3=lm(count_g~julian.date+temperature+time+site+boards+mowed+
                            urban.200+field.1000+water.900+road.500,
                          data=snake.data)
gvif(garter.count.test.vif3)

##pearson's correlation test

garter.count.cor.continuous=snake.data[,c(31,32,33,36, 38, 42, 48, 53)]
chart.Correlation(garter.count.cor.continuous,histogram=T,pch=19) #remove urban

##full model

garter.count.full.model.road=glmer(count_g~road.500+field.1000+water.900+julian.date+
                               temperature+time+boards+mowed+(1|site),
                              family=poisson,
                             data=snake.data, na.action=na.exclude)

summary(garter.count.full.model.road)

garter.count.full.model.urban=glmer(count_g~urban.200+field.1000+water.900+julian.date+
                                     temperature+time+boards+mowed+(1|site),
                                   family=poisson,
                                   data=snake.data, na.action=na.exclude)
summary(garter.count.full.model.urban)


#redbelly count
##gvif to remove extraneous variables

redbelly.count.test.vif=lm(count_rb~julian.date+temperature+time+site+boards+mowed+
                             urban.100+road.800+sides.600+field.200+forest.800+water.900,
                           data=snake.data)
gvif(redbelly.count.test.vif) # remove forest
redbelly.count.test.vif2=lm(count_rb~julian.date+temperature+time+site+boards+mowed+
                             urban.100+road.800+sides.600+field.200+water.900,
                           data=snake.data)
gvif(redbelly.count.test.vif2)

##pearsons correlation test

redbelly.count.cor.continuous=snake.data[,c(31,32,33,34, 40, 43, 47, 53)]
chart.Correlation(redbelly.count.cor.continuous,histogram=T, pch=19) #remove urban

##full models
redbelly.count.full.model.road=glmer(count_rb~road.800+sides.600+field.200+water.900+
                                       julian.date+temperature+time+boards+mowed+
                                       (1| site),
                                     family=poisson,
                                     data=snake.data,
                                     na.action=na.exclude)
summary(redbelly.count.full.model.road)
redbelly.count.full.model.urban=glmer(count_rb~urban.100+sides.600+field.200+water.900+
                                        julian.date+temperature+time+boards+mowed+
                                        (1|site),
                                      family=poisson,
                                      data=snake.data,
                                      na.action=na.exclude)
summary(redbelly.count.full.model.urban)

#garter SVL

##check assumptions: response variable is gaussian

ggplot(snake.data, aes(x=avg_svl_g)) +
         geom_histogram()
shapiro.test(snake.data$avg_svl_g) #close enough to normal

##gvif to determine if any extraneous variables in full model

garter.SVL.full.model.vif=lm(avg_svl_g~road.800+urban.300+sides.500+field.200+
                               water.600+forest.700+julian.date+temperature+time+
                               site+boards+mowed,
                             data=snake.data)
gvif(garter.SVL.full.model.vif)

garter.SVL.full.model.vif2=lm(avg_svl_g~road.800+urban.300+sides.500+field.200+
                               water.600+julian.date+temperature+time+
                               site+boards+mowed,
                             data=snake.data)
gvif(garter.SVL.full.model.vif2)

garter.SVL.full.model.vif3=lm(avg_svl_g~urban.300+sides.500+field.200+
                                water.600+julian.date+temperature+time+
                                site+boards+mowed,
                              data=snake.data)
gvif(garter.SVL.full.model.vif3)

garter.SVL.full.model.vif4=lm(avg_svl_g~urban.300+field.200+
                                water.600+julian.date+temperature+time+
                                site+boards+mowed,
                              data=snake.data)
gvif(garter.SVL.full.model.vif4)

##pearson's correlation of continuous predictor variables

garter.svl.corr.test=snake.data[,c(31,32,33, 34, 39, 43, 49, 51)]
chart.Correlation(garter.svl.corr.test, historgram=T, pch=19)

##full model

garter.svl.full.model=lmer(avg_svl_g~urban.300+field.200+water.600+julian.date+
                              temperature+time+(1|site)+boards+mowed,
                            data=snake.data,
                            na.action=na.exclude)
summary(garter.svl.full.model)

#redbelly SVL

ggplot(snake.data, aes(x=avg_svl_rb))+
  geom_histogram()
shapiro.test(snake.data$avg_svl_rb) #same here

#gvif tests
redbelly.SVL.full.model.vif=lm(avg_svl_rb~road.300+urban.1000+sides.300+field.900+
                                 water.800+forest.200+julian.date+temperature+time+
                                 site+boards+mowed,
                               data=snake.data)
gvif(redbelly.SVL.full.model.vif)

redbelly.SVL.full.model.vif2=lm(avg_svl_rb~road.300+sides.300+field.900+
                                 water.800+forest.200+julian.date+temperature+time+
                                 site+boards+mowed,
                               data=snake.data)
gvif(redbelly.SVL.full.model.vif2)

#pearson correlation

redbelly.SVL.corr.test=snake.data[,c(31,32,33,35,37,41,50,52)]
chart.Correlation(redbelly.SVL.corr.test,histogram=T,pch=19)

#model

redbelly.svl.full.model=lmer(avg_svl_rb~road.300+sides.300+field.900+water.800+forest.200+
                               julian.date+temperature+time+(1|site)+boards+mowed,
                             data=snake.data,
                             na.action=na.exclude)
summary(redbelly.svl.full.model)

##make figures

garter.count.roads=ggplot(snake.data, aes(x=road.500, y=count_g)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Road Density (km/km^2, 500 m, scaled)") + ylab("Garter Snake Count") +
  geom_smooth(method=lm, col = "black")

redbelly.count.roads=ggplot(snake.data, aes(x=urban.100, y=count_rb)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("Urban Area (%, 100 m, scaled)") + ylab("Redbelly Snake Count") +
  geom_smooth(method=lm, col="black")

garter.count.date=ggplot(snake.data, aes(x=julian.date, y=count_g)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("Julian Date") + ylab("Garter Snake Count")+
  geom_smooth(method=lm, col = "black")

garter.count.time=ggplot(snake.data, aes(x=time,y=count_g)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("Time") + ylab("Garter Snake Count") +
  geom_smooth(method = lm, col = "black")

garter.count.boards=ggplot(snake.data, aes(x=boards, y=count_g)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("# Boards") + ylab("Garter Snake Count") +
  geom_smooth(method=lm, col = "black")

redbelly.count.date=ggplot(snake.data, aes(x=julian.date, y=count_rb)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("Julian Date") + ylab("Redbelly Snake Count") +
  geom_smooth(method=lm,col="black")

redbelly.count.time=ggplot(snake.data, aes(x=time, y=count_rb)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("Time") + ylab("Redbelly Snake Count") +
  geom_smooth(method=lm, col = "black")

redbelly.count.boards=ggplot(snake.data, aes(x=boards, y=count_rb)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("# Boards") + ylab("Redbelly Snake Count") +
  geom_smooth(method=lm, col="black")

redbelly.count.temperature = ggplot(snake.data, aes(x=temperature, y=count_rb)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("Temperature") + ylab("Redbelly Snake Count") +
  geom_smooth(method=lm, col="black")

garter.svl.temperature = ggplot(snake.data, aes(x=temperature, y=avg_svl_g)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("Temperature") + ylab("Garter Snake SVL") +
  geom_smooth(method=lm, col=" black")

garter.svl.boards = ggplot(snake.data, aes(x=boards, y=avg_svl_g)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("# Boards") + ylab("Garter Snake SVL") +
  geom_smooth(method=lm, col = "black")

redbelly.svl.temperature = ggplot(snake.data, aes(x = temperature, y = avg_svl_rb)) +
  geom_point(size=2) +
  theme_classic() +
  xlab("Temperature") + ylab("Redbelly Snake SVL") +
  geom_smooth(method=lm, col="black")