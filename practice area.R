getwd
setwd()

beers <- read.csv("Beers.csv")
breweries <-read.csv("Breweries.csv")

#Rename beers name column to beerName
colnames(beers)[1] <- c("beerName")

#Rename brewery name column to brewerCompany
colnames(breweries)[2] <- c("brewerCompany")

#Rename Brewery_id to Brew_id in beers data, same as in breweries in order to merge data sets
colnames(beers)[5] <- c("Brew_ID")


# Trim the leading white-spce from the State
breweries$State <- trimws(breweries$State)

#Create State Names data frame, inlcuding Washington DC
stateNames <- data.frame(stateName=c(state.name,"Washington DC"),State=c(state.abb,"DC"))


# add the State Name to the Breweries Data Frame
breweries <- merge(breweries,stateNames,by="State", all=TRUE)

uniqueBreweries <- breweries[!duplicated(breweries[c(3,4,5)]),]


################Brewery cleanse

#####probably junk that could go away, need to review##################
#brewuniID<-unique(breweries$Brew_ID)
#brewuniNAME<-unique(breweries$brewerCompany)
#dupbreweries<-duplicated(x = breweries$brewerCompany)
#a<-breweries$brewerCompany[dupbreweries]
##########################################################################


#Merge beers and breweries data together
m1 <- merge(beers,breweries,by="Brew_ID")



#performs search for parenthesis values, removes them from a nondestructive column for beers
m1$beerNameNoY<-gsub("[:(:].*[:):]","",m1$beerName)
#trims white space around newly trimmed data for beers
m1$beerNameNoY<-trimws(m1$beerNameNoY)
#performs search for parenthesis values, removes them from a nondestructive column for brewries
m1$brewerNameNoP<-gsub("[:(:].*[:):]","",m1$brewerCompany)
#trims white space around newly trimmed data for brewries
m1$brewerNameNoP<-trimws(m1$brewerNameNoP)

#eliminates duplicates for beers
m1 <- m1[!duplicated(m1[,c("beerNameNoY","brewerNameNoP","ABV","IBU","Style")]),]










#q1 how many breweries in US???
uniqueBreweries <- uniqueBreweries[!duplicated(uniqueBreweries[,c("brewerCompany","City","stateName")]),]
countBrewByState <- table(uniqueBreweries$stateName)
countBrewByState <- as.data.frame(countBrewByState)
colnames(countBrewByState)=c("stateName","BreweriesCount")
countBrewByState

countBrewByState2 <- table(uniqueBreweries$stateName)
countBrewByState2 <- as.data.frame(countBrewByState2)
colnames(countBrewByState2)=c("stateName","BreweriesCount")
countBrewByState2

#Table with the first 6 observations from m1 (merged data)
head(m1)

#Talble with the last 6 observations from m1 (merged data)
tail(m1)


#q3 na per column
sapply(m1,function(x) sum(is.na(x)))

#Q4 median ABV & IBU for each state, plot a bar chart
med.st.abv<-aggregate(m1$ABV~m1$State,m1,median,na.rm=T)
colnames(med.st.abv)<- c("State","ABV")
med.st.ibu<-aggregate(m1$IBU~m1$State,m1,median,na.rm=T)
colnames(med.st.ibu)<- c("State","IBU")

med.st.abv
med.st.ibu

library(ggplot2)

##Histograms, Raw Data

ggplot(data=med.st.abv, aes(x=med.st.abv$State,y=med.st.abv$ABV)) + geom_bar(stat="identity")
ggplot(data=med.st.ibu, aes(x=med.st.ibu$State,y=med.st.ibu$IBU)) + geom_bar(stat="identity")

####NOTE SD DOES NOT SHOW UP IN IBU BECAUSE OF MISSING DATA, WHAT DO WE DO WITH THIS???

#Q5
z<-which.max(m1$ABV)
paste(m1$stateName[z], "has the highest ABV with a Max ABV of:", m1$ABV[z])

y<-which.max(m1$IBU)
paste(m1$stateName[y], "has the highest IBU with a Max IBU of:", m1$IBU[y])

#Q6
summary(m1$ABV)

#q7
ggplot(data=m1,aes(x=m1$IBU,y=m1$ABV)) + geom_point() + geom_smooth(method="lm")
