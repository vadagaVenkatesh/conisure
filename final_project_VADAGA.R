# csv includes lastest edition FIFA 2019 players attributes like Age

# Nationality, Overall, Potential, Club, Value, Wage, Preferred Foot, International Reputation, Weak Foot, Skill Moves, Work Rate, Position
#,Jersey Number, Joined, Loaned From, Contract Valid Until, Height, Weight,
#LS, ST, RS, LW, LF, CF, RF, RW, LAM, CAM, RAM, LM, LCM, CM, RCM, RM, LWB, LDM, CDM, RDM, RWB, LB, LCB, CB, RCB, RB,
#Crossing, Finishing, Heading, Accuracy, ShortPassing, Volleys, Dribbling, Curve, FKAccuracy, LongPassing, BallControl,
#Acceleration, SprintSpeed, Agility, Reactions, Balance, ShotPower, Jumping, Stamina, Strength, LongShots, Aggression,
#Interceptions, Positioning, Vision, Penalties, Composure, Marking, StandingTackle, SlidingTackle, 
#GKDiving, GKHandling, GKKicking, GKPositioning, GKReflexes, and Release Clause.

#football mentioned here is also called 
#american soccer
library(tidyverse)
library(UsingR)
library(prob)
library(sampling)
football_data<-data.frame(read.csv("data.csv" ,header = TRUE))

#view(football_data)

summary(football_data)

#cleaning Data
football_data$Value<- (gsub("â,¬","",football_data$Value))
football_data
football_data$Wage<- (gsub("â,¬","",football_data$Wage))
football_data
football_data$Release.Clause<-(gsub("â,¬","",football_data$Release.Clause))
football_data
football_data = football_data[,-c(5,7,11,14,19,24,25,28:54)]
football_data

head(football_data,n=3)
#filtering categorical variable 
#Let's find out the country which produces 
#the most football players 

x<-table(football_data$Nationality)
x<-head(sort(x ,decreasing = TRUE))
x

# In col "preferred.Foot" also counts for the 
#goal Keepers 
#dedute the subsets that where GK is defined 
#Total number of left and right footed displayed
#tabluate or summarize 

p<-as.factor(football_data$Preferred.Foot)
subdf<- subset(p,football_data$Preferred.Foot!="")
subdf
subdf<- table(subdf)
prop.table(subdf)

  
#This means their are 13948 right footed players 
#and 4211 left footed players
#filtering Numerical variable

#Calculation the relation between the player potential
#and the player overall score out of 100
plot(football_data$Potential,football_data$Overall)
#looking at graph overall score of player is less 
#than equal to their individual potential
fivenum(football_data$Jersey.Number,na.rm = TRUE)
#this means that most of the jercy numbers are from
#8 to 26 (50 %)
median(football_data$Jersey.Number,na.rm = TRUE)
#the 17 Number jercy is used by most of the players

plot(x=football_data$Height, xlab="Player_Height" ,y=football_data$Weight,ylab ="Player_weight")
#Their are less players from 5'3 to 5'8 and above 6'3 
#major chunk of players have height from 5'9 to 6'2


#distribution
#tib1<-as_tibble(football_data$Crossing)
#removing goalkeeper stats 'NA'

cross_mean<-mean(football_data$Crossing,na.rm =TRUE)
cross_sd<-sd(football_data$Crossing,na.rm =TRUE)
pdf1<-dnorm(football_data$Crossing,mean = cross_mean,sd = cross_sd)
pdf<-table(pdf1)
sum(pdf)

plot( x = pdf,type = "l",abline(h=0),xlab ="probablity of occurance of a effective cross during any given game" )

#plotting  players in club paris saint germain
psg<-football_data[(football_data$Club=="Paris Saint-Germain"),]
head(psg)
#plottiing player in club Football Club Barcleona
fcb<-football_data[(football_data$Club=="FC Barcelona"),]
head(fcb)
#player haveing best overall performance
ranked<-football_data[(football_data$Overall >= 85|football_data$Vision >=85),]

#show top 18 players of all time in 2019
head(ranked,n =20)


#striker in the dataset 
all_st<-football_data[(football_data$Position %in% "ST"),]
all_st


#centrailzed limit theroem for the football data w.r.t Age element 

x<-football_data$Age
hist(x, prob = TRUE, breaks = 15, las =2)
#Distribution of the data
#normal
par(mfrow = c(1,3))
size<-20
xbar <- numeric(length(football_data$Age))
for(i in 1:length(football_data$Age)) {
  xbar[i] <- mean(rnorm(size, mean = mean(football_data$Age), sd = sd(football_data$Age)))
}
hist(xbar, prob = TRUE, main = "Sample Size = 20")


size<-30
xbar <- numeric(length(football_data$Age))
for(i in 1:length(football_data$Age)) {
  xbar[i] <- mean(rnorm(size, mean = mean(football_data$Age), sd = sd(football_data$Age)))
}
hist(xbar, prob = TRUE, main = "Sample Size = 30")


size<-40
xbar <- numeric(length(football_data$Age))
for(i in 1:length(football_data$Age)) {
  xbar[i] <- mean(rnorm(size, mean = mean(football_data$Age), sd = sd(football_data$Age)))
}
hist(xbar, prob = TRUE, main = "Sample Size = 40")


#sampling the players from brazil
ST<-football_data[(football_data$Nationality %in% "Brazil"),]
ST
#sampling

nrow(ST)
size <-20
sample<-srswor(size ,nrow(ST))
table(sample)

y<-ST[sample!=0,]
y

f<-data.frame(table(y$Age))
colnames(f)<-c("Age","samples")
f

size <-30
sample<-srswor(size ,nrow(ST))
table(sample)

y<-ST[sample!=0,]
y

f<-data.frame(table(y$Age))
colnames(f)<-c("Age","samples")
f


size <-40
sample<-srswor(size ,nrow(ST))
table(sample)

y<-ST[sample!=0,]
y

f<-data.frame(table(y$Age))
colnames(f)<-c("Age","samples")
f

#players playing in club Paris Saint-Germain
psg<-football_data[(football_data$Club =="Paris Saint-Germain"),]
psg
head(psg)

#players playing in country spain
spain<-football_data[(football_data$Nationality == "Spain"),]
spain
head(spain)

#sampling 
##simple random sampling without replacement

age<-football_data[( football_data$Nationality == "Brazil"),]

nrow(age)

size<-21
age_sample <-srswor(size,nrow(age))
x<-table(age_sample)


sample_age<-age[x!=0,]
sample_age
#sample size 21 ;21 sample age
size<-31
age_sample <-srswor(size,nrow(age))
x<-table(age_sample)


sample_age<-age[x!=0,]
sample_age
#sample size 31 ;31 sample age

size<-41
age_sample <-srswor(size,nrow(age))
x<-table(age_sample)


sample_age<-age[x!=0,]
sample_age
#sample size 41 ;41 sample age 


#systematic sampling of brazilian footballers

a<-sample(ceiling(nrow(age)/size),1)

b<-seq(a, by=ceiling(nrow(age)/size), length=size)
ab <-age[b,]
head(ab)

#Extra 

# the Player's Name start with P

table(str_length(football_data$Name))
barplot(table(str_length(football_data$Name)), xlab = "length", ylab = "frequency")
#plotting the length and the freq of the player's name in the data set
plot(table(str_length(football_data$Name)), xlab = "length", ylab = "frequency" )
#smallest player's name in the dataset
football_data$Name[str_length(football_data$Name) == min(str_length(football_data$Name))]
#All the player names which start with the letter P
football_data$Name[str_detect(football_data$Name, "^P")]
