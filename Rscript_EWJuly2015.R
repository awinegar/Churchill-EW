library(ggplot2)
library(gridExtra)
library(reshape2)

#Data
env<- read.csv(file.choose(), as.is=T) #EW_July2015teams_ENV.csv
dip<- read.csv(file.choose()) #EW_July2015teams_DIP.csv
traps<- read.csv(file.choose()) #EW_July2015teams_TRAPS.csv

#Env
fTeam<- factor(env$Team_No)

env.depth<- ggplot(env, aes(x=fTeam, y=Env_depth)) + geom_boxplot()
env.depth<- env.depth + labs(x="EarthWatch Team", y="Env Depth (cm)")
env.depth<- env.depth + theme(axis.text.x = element_text(colour="black", size=16))
env.depth<- env.depth + theme(axis.text.y = element_text(colour="black", size=16))
env.depth<- env.depth + theme(axis.title.x = element_text(size = rel(2), angle=00))
env.depth<- env.depth + theme(axis.title.y = element_text(size = rel(2), angle=90))

phys.depth<- ggplot(env, aes(x=fTeam, y=Phys_depth)) + geom_boxplot()
phys.depth<- phys.depth + labs(x="EarthWatch Team", y="Phys Depth (cm)")
phys.depth<- phys.depth + theme(axis.text.x = element_text(colour="black", size=16))
phys.depth<- phys.depth + theme(axis.text.y = element_text(colour="black", size=16))
phys.depth<- phys.depth + theme(axis.title.x = element_text(size = rel(2), angle=00))
phys.depth<- phys.depth + theme(axis.title.y = element_text(size = rel(2), angle=90))


area<- ggplot(env, aes(x=fTeam, y=S_area)) + geom_boxplot()
area<- area + labs(x="EarthWatch Team", y="Surface Area (m2)")
area<- area + theme(axis.text.x = element_text(colour="black", size=16))
area<- area + theme(axis.text.y = element_text(colour="black", size=16))
area<- area + theme(axis.title.x = element_text(size = rel(2), angle=00))
area<- area + theme(axis.title.y = element_text(size = rel(2), angle=90))

sedge<- ggplot(env, aes(x=fTeam, y=Sedge_per)) + geom_boxplot()
sedge<- sedge + labs(x="EarthWatch Team", y="Percent Sedge")
sedge<- sedge + theme(axis.text.x = element_text(colour="black", size=16))
sedge<- sedge + theme(axis.text.y = element_text(colour="black", size=16))
sedge<- sedge+ theme(axis.title.x = element_text(size = rel(2), angle=00))
sedge<- sedge + theme(axis.title.y = element_text(size = rel(2), angle=90))

shrub<- ggplot(env, aes(x=fTeam, y=Shrub_per)) + geom_boxplot()
shrub<- shrub + labs(x="EarthWatch Team", y="Percent Shrub")
shrub<- shrub + theme(axis.text.x = element_text(colour="black", size=16))
shrub<- shrub + theme(axis.text.y = element_text(colour="black", size=16))
shrub<- shrub + theme(axis.title.x = element_text(size = rel(2), angle=00))
shrub<- shrub + theme(axis.title.y = element_text(size = rel(2), angle=90))

temp<- ggplot(env, aes(x=fTeam, y=Temp_degC)) + geom_boxplot()
temp<- temp + labs(x="EarthWatch Team", y="Temp (deg C)")
temp<- temp + theme(axis.text.x = element_text(colour="black", size=16))
temp<- temp + theme(axis.text.y = element_text(colour="black", size=16))
temp<- temp + theme(axis.title.x = element_text(size = rel(2), angle=00))
temp<- temp + theme(axis.title.y = element_text(size = rel(2), angle=90))

cond<- ggplot(env, aes(x=fTeam, y=SpCond)) + geom_boxplot()
cond<- cond + labs(x="EarthWatch Team", y="Cond (uS/cm)")
cond<- cond + theme(axis.text.x = element_text(colour="black", size=16))
cond<- cond + theme(axis.text.y = element_text(colour="black", size=16))
cond<- cond + theme(axis.title.x = element_text(size = rel(2), angle=00))
cond<- cond + theme(axis.title.y = element_text(size = rel(2), angle=90))

DO.per<- ggplot(env, aes(x=fTeam, y=DO_per)) + geom_boxplot()
DO.per<- DO.per + labs(x="EarthWatch Team", y="Percent DO")
DO.per<- DO.per + theme(axis.text.x = element_text(colour="black", size=16))
DO.per<- DO.per + theme(axis.text.y = element_text(colour="black", size=16))
DO.per<- DO.per + theme(axis.title.x = element_text(size = rel(2), angle=00))
DO.per<- DO.per + theme(axis.title.y = element_text(size = rel(2), angle=90))

algae<- ggplot(env, aes(x=fTeam, y=Tot_algae_ugL)) + geom_boxplot()
algae<- algae + labs(x="EarthWatch Team", y="Total algae (ug/L)")
algae<- algae + theme(axis.text.x = element_text(colour="black", size=16))
algae<- algae + theme(axis.text.y = element_text(colour="black", size=16))
algae<- algae + theme(axis.title.x = element_text(size = rel(2), angle=00))
algae<- algae + theme(axis.title.y = element_text(size = rel(2), angle=90))


plot1<- grid.arrange(env.depth, phys.depth, nrow=1)
plot2<- grid.arrange(area, sedge, shrub, nrow=2)
plot3<- grid.arrange(temp, cond, DO.per, algae, nrow=2)



#Traps

traps.long<- melt(traps, id.vars=c("Site", "Unique_ID", "Team_Word", "Team_No"))
colnames(traps.long) [5]<- 'Species'
colnames(traps.long) [6]<- 'Count'

traps.plot<- ggplot(traps.long, aes(x=Team_Word, y=Count, colour=Species)) + geom_point(size=4)
traps.plot<- traps.plot + facet_wrap(~Site)
traps.plot<- traps.plot + labs(x="EarthWatch Team", y="Total count in traps")
traps.plot<- traps.plot + theme(axis.text.x = element_text(colour="black", size=16))
traps.plot<- traps.plot + theme(axis.text.y = element_text(colour="black", size=16))
traps.plot<- traps.plot + theme(axis.title.x = element_text(size = rel(2), angle=00))
traps.plot<- traps.plot + theme(axis.title.y = element_text(size = rel(2), angle=90))


#DIP
#Come back to. 

#WEATHER FOR TEAM 2
weather<- read.csv(file.choose()) #EWJuly2015_team2_WEATHER

weather.long<- melt(weather, id.vars=c("July"))
colnames(weather.long)[2]<- 'Variable'
colnames(weather.long)[3]<- 'Observation'

weather.plot<- ggplot(weather.long, aes(x=July, y=Observation, colour=Variable)) + geom_point(size=4) + geom_path()
weather.plot<- weather.plot + labs(x="Day of July", y="Deg C or mm Precip")
weather.plot<- weather.plot + theme(axis.text.x = element_text(colour="black", size=16))
weather.plot<- weather.plot + theme(axis.text.y = element_text(colour="black", size=16))
weather.plot<- weather.plot + theme(axis.title.x = element_text(size = rel(2), angle=00))
weather.plot<- weather.plot + theme(axis.title.y = element_text(size = rel(2), angle=90))

