
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

###this area cleans the brewrery data of most likely duplicate entry
#loads stringr funtion
library(stringr)
#creates work area for brewries data with b df
b<-breweries
#trims b of white space and converts to lowercase
sapply(b,FUN = trimws)
sapply(b,FUN = tolower)
#pulls first two words from brewery data to eliminate potential duplicates and creates it as a column in b
b$TwoWords<-word(b$brewerCompany,1,2,sep=" ")
#eliminates duplicate data where duplicate brewries share the same name and are in the same city
uniqueBreweries <- b[!duplicated(b[,c("TwoWords", "City")]),]


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


#eliminates duplicates for beers
m1 <- m1[!duplicated(m1[,c("beerNameNoY","brewerCompany","ABV","IBU","Style")]),]










#q1 how many breweries in US???
countBrewByState <- table(uniqueBreweries$stateName)
countBrewByState <- as.data.frame(countBrewByState)
colnames(countBrewByState)=c("stateName","BreweriesCount")
countBrewByState

#Table with the first 6 observations from m1 (merged data)
head(m1)

#Talble with the last 6 observations from m1 (merged data)
tail(m1)


#q3 na per column
sapply(m1,function(x) sum(is.na(x)))

#Q4 median ABV & IBU for each state, plot a bar chart
# ABV Median Calculation per State and Column Renames. NA Values are ignored.
med.st.abv<-aggregate(m1$ABV~m1$State,m1,median,na.rm=T)
colnames(med.st.abv)<- c("State","ABV")

# IBU Median Calculation per State and Column Renames. NA Values are ignored.
med.st.ibu<-aggregate(m1$IBU~m1$State,m1,median,na.rm=T)
colnames(med.st.ibu)<- c("State","IBU")
med.st.ibu<-rbind(med.st.ibu,c("SD","0"))


# ABV Median per State 
med.st.abv

# IBU Median per State
med.st.ibu

library(ggplot2)

##Histograms, Raw Data fill=med.st.ibu$IBU

ggplot(data=med.st.abv, aes(x=med.st.abv$State,y=med.st.abv$ABV, fill=med.st.abv$ABV)) + geom_bar(stat="identity") + 
  ggtitle("Median ABV by state") +
  xlab("State") + ylab("Median ABV") +guides(fill=FALSE) 

ggplot(data=med.st.ibu, aes(x=med.st.ibu$State,y=med.st.ibu$IBU, fill=med.st.ibu$IBU)) + geom_bar(stat = "identity") + 
  ggtitle("Median IBU by state",subtitle = "SD median can not be calculated due to missing IBU data and is represented as 0") +
  xlab("State") + ylab("Median IBU") +guides(fill=FALSE) 

####NOTE SD DOES NOT SHOW UP IN IBU BECAUSE OF MISSING DATA, WHAT DO WE DO WITH THIS???

#Q5
z<-which.max(m1$ABV)
paste(m1$stateName[z], "has the highest ABV with a Max ABV of:", m1$ABV[z])

y<-which.max(m1$IBU)
paste(m1$stateName[y], "has the highest IBU with a Max IBU of:", m1$IBU[y])

#Q6
summary(m1$ABV)

#q7
ggplot(data=m1,aes(x=m1$ABV,y=m1$IBU,colour=m1$IBU)) + geom_point() + geom_smooth(method="lm", formula=y~x) +
  xlab("Alcohol by volume") + ylab("Bitternes") + ggtitle("Beer Bitternes vs Alcohol Content") + 
  scale_colour_gradientn(colours = rainbow(10)) + theme(legend.title=element_blank())

mod = lm(m1$ABV ~ m1$IBU, data = m1)
summary(mod) 
  
