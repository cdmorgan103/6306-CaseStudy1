########################## Data Description ##########################

## Beers.csv:
## Name: Name of the beer.
## Beer ID: Unique identifier of the beer.
## ABV: Alcohol by volume of the beer.
## IBU: International Bitterness Units of the beer.
## Brewery ID: Brewery id associated with the beer.
## Style: Style of the beer.
## Ounces: Ounces of beer.

## Breweries.csv:
## Brew ID: Unique identifier of the brewery.
## Name: Name of the brewery.
## City: City where the brewery is located.
## State: U.S. State where the brewery is located.


########################## Question 1 ##########################
## How many breweries are present in each state?

# Import Breweries.csv data set
# Assign to data frame object
BreweriesDf <- read.csv("Breweries.csv", header = TRUE)

# Load plyr libary to apply count() function
library(plyr)
NumBrewPerState <- count(BreweriesDf, "State")
colnames(NumBrewPerState) <- c("State", "# Breweries")
NumBrewPerState


########################## Question 2 ##########################
## Merge beer data with the breweries data. 
## Print the frst 6 observations and the last six observations 
## to check the merged file.

# Rename first column in Breweries.csv from Brew_ID to
# Brewery_id to match column in Beers.csv prior to merging
names(BreweriesDf)[1] <- "Brewery_id"

# Import Beers.csv data set
# Assign to data frame object
BeersDf <- read.csv("Beers.csv", header = TRUE)

# Merge the Breweries.csv and Beers.csv data frames
BeersBreweriesMergedDf <- merge(BreweriesDf, BeersDf, by = "Brewery_id")

# Use head() function to print first 6 observations and
# tail() function for the last six observations to make
# sure that the merged data frame looks correct
head(BeersBreweriesMergedDf)
tail(BeersBreweriesMergedDf)
View(BeersBreweriesMergedDf)
# Merged data frame does seem to be correct with multiple
# beers with unique Beer_ID numbers assigned to the same
# brewery identified by the Brewery_id number


########################## Question 3 ##########################
## Report the number of NA's in each column.

# Create a simple function to identify NA values and sum
# them, then use the sapply() function to apply this to 
# across all 10 columns of the merged data frame and simplify
# to a single row showing the sum of NA values under the 
# respective column names
sapply(BeersBreweriesMergedDf, function(x) sum(is.na(x)))

# After seeing the result of 62 NA values for the ABV, and 
# 1005 NA values for the IBU columns, we were prompted to
# to take a closer look a the data frame and make sure there
# aren't any other missing or duplicate values that need to
# be addressed and what other data cleaning is needed in general




########################## Question 4 ##########################
## Compute the median alcohol content and international
## bitterness unit for each state. Plot a bar chart to compare.

# Use tapply() function to calculate median ABV by State
# Note that NA values were ignored in this case
# Assign to object for easier handling when creating bar chart
MedianABVByState <- tapply(BeersBreweriesMergedDf$ABV, 
                    BeersBreweriesMergedDf$State, 
                    median, 
                    na.rm = TRUE)
# Convert the MedianABVByState array to a data frame
MedianABVByStateDf <- as.data.frame(MedianABVByState)
# Add column for state in data frame and assign row names
# to the new column
MedianABVByStateDf$State <- rownames(MedianABVByStateDf)

# Load ggplot2 library
# Create bar chart to compare Median ABV by State
library(ggplot2)
theme_set(theme_classic())
g <- ggplot(MedianABVByStateDf, aes(State, MedianABVByState))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title = "Median ABV by State", 
       subtitle = "Median does not include NA values", 
       x = "State",
       y = "Alcohol by Volume, ABV [%]",
       caption = "Source: Median ABV calculated from merged Breweries.csv and Beers.csv dataset") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8))



# Use tapply() function to calculate median IBU by State
# Note that NA values were ignored in this case
# Assign to object for easier handling when creating bar chart
MedianIBUByState <- tapply(BeersBreweriesMergedDf$IBU, 
                           BeersBreweriesMergedDf$State, 
                           median, 
                           na.rm = TRUE)
# Convert the MedianIBUByState array to a data frame
MedianIBUByStateDf <- as.data.frame(MedianIBUByState)
# Add column for state in data frame and assign row names
# to the new column
MedianIBUByStateDf$State <- rownames(MedianIBUByStateDf)

# Create bar chart to compare Median IBU by State
theme_set(theme_classic())
g <- ggplot(MedianIBUByStateDf, aes(State, MedianIBUByState))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title = "Median IBU by State", 
       subtitle = "Median does not include NA values", 
       x = "State",
       y = "International Bitterness Unit, IBU",
       caption = "Source: Median IBU calculated from merged Breweries.csv and Beers.csv dataset") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.8))


########################## Question 5 ########################## 
## Which state has the maximum alcoholic (ABV) beer? 
## Which state has the most bitter (IBU) beer?

# Use which.max() function and ABV column slice to find row
# with maximum ABV value then use column slice with "State"
# label to only output the state name for the chosen row
BeersBreweriesMergedDf[which.max(BeersBreweriesMergedDf$ABV),]["State"]

# Use which.max() function and ABV column slice to find row
# with maximum IBU value then use column slice with "State"
# label to only output the state name for the chosen row
BeersBreweriesMergedDf[which.max(BeersBreweriesMergedDf$IBU),]["State"]

########################## Question 6 ##########################
## Summary statistics for the ABV variable. 

# Apply summary() function to ABV column with the data frame
summary(BeersBreweriesMergedDf$ABV)


########################## Question 7 ##########################
## Is there an apparent relationship between the bitterness 
## of the beer and its alcoholic content? Draw a scatter plot.
## Can use ggplot2 library for graphs.
## Ignore missing values in analysis.
## Make best judgement of relationship and explain answer.

# Use ggplot for scatterplot
ggplot(BeersBreweriesMergedDf, 
       aes (y = ABV, x = IBU)) +
       geom_point(size = 2)


# Same scatterplot as above but color coded by state
ggplot(BeersBreweriesMergedDf, 
       aes (y = ABV, x = IBU, colour = factor(State))) +
  geom_point(size = 3)