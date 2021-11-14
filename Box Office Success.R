library(regclass)
library(arules)
library(lubridate)
library(multcompView)


#almost clean data from Excel
MOVIE <- read.csv("MyMovies.csv",stringsAsFactors = TRUE)

summary(MOVIE)

#Some odd values with Budget, Theaters, and some NAs in Theaters and Runtime
#Subset named MYMOVIE which ensures estimated total budget, number of theaters, and runtime > 0
MYMOVIE <- droplevels(subset(MOVIE,Budget > 0 & Theaters > 0 & !is.na(Theaters) & !is.na(RunTime)) )
summary(MYMOVIE)
MYMOVIE$X <- NULL
#verify that 100% of rows have information
mean(complete.cases(MYMOVIE))

#Don't need movie title in this
MYMOVIE$Movie <- NULL

#Rating:  combine rare levels with a threshold of 20
table(MYMOVIE$Rating)
table(combine_rare_levels(MYMOVIE$Rating,threshold=20)$values)
MYMOVIE$Rating <- combine_rare_levels(MYMOVIE$Rating, threshold=20)$values
#Remove the - in the name
levels(MYMOVIE$Rating) <- gsub("-","", levels(MYMOVIE$Rating ) )
levels(MYMOVIE$Rating)[which(levels(MYMOVIE$Rating) %in% c("Combined"))] <- "Other"
table(MYMOVIE$Rating)


#Convert release date field and extract out month, then get rid of original date information
MYMOVIE$Date <- mdy(MYMOVIE$ReleaseDate)
MYMOVIE$Month <- month(MYMOVIE$Date,label=TRUE)
MYMOVIE$ReleaseDate <- NULL
MYMOVIE$Date <- NULL



#Distribution:  combine rare levels with a threshold
#Also remove - in level names
table(MYMOVIE$Distribution)
table(combine_rare_levels(MYMOVIE$Distribution,threshold=75)$values)
MYMOVIE$Distribution <- combine_rare_levels(MYMOVIE$Distribution, threshold=75)$values
levels(MYMOVIE$Distribution) <- gsub("-","", levels(MYMOVIE$Distribution ) )


#Budget
hist(MYMOVIE$Budget)
hist(log10(MYMOVIE$Budget))

#So skewed.  Create sensible categories
x <- cut(MYMOVIE$Budget, c(0,1e6,5e6,1e7,5e7,1e8,5e8,1e9),include.lowest = TRUE)
summary(x)
x <- cut(MYMOVIE$Budget, c(0,1e6,5e6,1e7,2e7,3e7,4e7,5e7,6e7,7e7,1e8,5e8),include.lowest = TRUE)
summary(x)
MYMOVIE$BudgetCat <- x
#MYMOVIE$Budget <- NULL

#Runtime
hist(MYMOVIE$RunTime)
x <- discretize(MYMOVIE$RunTime,method="frequency",breaks=8)
summary(x)
x <- cut(MYMOVIE$RunTime,c(60,90,95,100,105,110,120,130,150,260),include.lowest = TRUE)
summary(x)
MYMOVIE$RuntimeCat <- x
#MYMOVIE$RunTime <- NULL

#TopBilledStar:  combine rare levels with a threshold
table(MYMOVIE$TopBilledStar)
table(combine_rare_levels(MYMOVIE$TopBilledStar,threshold=25)$values)
MYMOVIE$TopBilledStar <- combine_rare_levels(MYMOVIE$TopBilledStar, threshold=25)$values
levels( MYMOVIE$TopBilledStar )[which(levels(MYMOVIE$TopBilledStar) %in% c("Combined"))] <- "Other"
table(MYMOVIE$TopBilledStar)


#MainGenre:  combine rare levels with a threshold
#Also remove - in level names
table(MYMOVIE$MainGenre)
table(combine_rare_levels(MYMOVIE$MainGenre,threshold=80)$values)

levels( MYMOVIE$MainGenre )[which(levels(MYMOVIE$MainGenre) %in% c("Action","Disaster","Epic","Spy"))] <- "Action"
levels( MYMOVIE$MainGenre )[which(levels(MYMOVIE$MainGenre) %in% c("Romance","Romantic"))] <- "Romance"
levels( MYMOVIE$MainGenre )[which(levels(MYMOVIE$MainGenre) %in% c("Space","Sci-Fi"))] <- "Sci-Fi"

MYMOVIE$MainGenre <- combine_rare_levels(MYMOVIE$MainGenre, threshold=80)$values
levels(MYMOVIE$MainGenre) <- gsub("-","", levels(MYMOVIE$MainGenre ) )
levels( MYMOVIE$MainGenre )[which(levels(MYMOVIE$MainGenre) %in% c("Combined"))] <- "Other"
table(MYMOVIE$MainGenre)


#SecondaryGenre:  combine rare levels with a threshold
#Also change the first level alphabetically to "None" (right now it is "-")
table(MYMOVIE$SecondaryGenre)
table(combine_rare_levels(MYMOVIE$SecondaryGenre,threshold=80)$values)
MYMOVIE$SecondaryGenre <- combine_rare_levels(MYMOVIE$SecondaryGenre, threshold=80)$values
levels(MYMOVIE$SecondaryGenre)
levels( MYMOVIE$SecondaryGenre )[which(levels(MYMOVIE$SecondaryGenre) %in% c("Combined"))] <- "Other"
levels( MYMOVIE$SecondaryGenre )[which(levels(MYMOVIE$SecondaryGenre) %in% c("-"))] <- "None"
table(MYMOVIE$SecondaryGenre)



#Make WeekOfMonth a categorical variable
MYMOVIE$WeekOfMonth <- factor( MYMOVIE$WeekOfMonth )
levels(MYMOVIE$WeekOfMonth) <- c("Fist","Second","Third","Fourth","Fifth")



#Director:  combine rare levels with a threshold
table(MYMOVIE$Director)
table(combine_rare_levels(MYMOVIE$Director,threshold=10)$values)
MYMOVIE$Director <- combine_rare_levels(MYMOVIE$Director, threshold=10)$values
levels( MYMOVIE$Director )[which(levels(MYMOVIE$Director) %in% c("Combined"))] <- "Other"

#Universe - make a yes/no level
table(combine_rare_levels(MYMOVIE$Universe,threshold=10)$values)
MYMOVIE$Universe <- factor(ifelse(MYMOVIE$Universe=="None","No","Yes") ) 
summary(MYMOVIE$Universe)

#Theaters
table( discretize( MYMOVIE$Theaters,breaks=8,method="frequency"))
summary( cut( MYMOVIE$Theaters,c(0,700,1500,2000,2500,2800,3100,3500,5000)))
MYMOVIE$Theaters <- cut( MYMOVIE$Theaters,c(0,700,1500,2000,2500,2800,3100,3500,5000),include.lowest = TRUE)



#Checkpoint
summary(MYMOVIE)
mean(complete.cases(MYMOVIE))


#write this hard work to a file
write.csv(MYMOVIE,file="mycleanmovies.csv",row.names = FALSE)



#Start finding drivers!


MYMOVIE <- read.csv("mycleanmovies.csv",stringsAsFactors = TRUE)

#What's the overall probability of success in this dataset?
mean(MYMOVIE$Success=="Success")


#How much does the probability of success vary across the levels for each group?

examine_movie_driver <- function(driver,sort=TRUE) { 
  mosaic( formula(paste("Success ~",driver)), data=MYMOVIE, inside=TRUE, equal=TRUE )
  SUMMARY <- aggregate(formula(paste('Success=="Success"~', driver)),data=MYMOVIE,FUN=mean)
  AOV <- aov(formula(paste('Success=="Success"~', driver)),data=MYMOVIE)
  if( as.numeric( unlist( summary(AOV)[[1]][5] ) )[1] < .05 ) { 
    TUKEY <- TukeyHSD(AOV)
    LETTERS <- multcompLetters4(AOV,TUKEY) 
    SUMMARY$letters <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )] } else {
      SUMMARY$letters <- rep("a",nrow(SUMMARY))
    }
  names(SUMMARY)[2] <- "ProbSuccess"
  SUMMARY$n <- as.numeric(table(MYMOVIE[,driver]))
  if(sort==TRUE) { 
    SUMMARY <- SUMMARY[order(SUMMARY$ProbSuccess,decreasing=TRUE),] }
  SUMMARY <- SUMMARY[order(SUMMARY$ProbSuccess,decreasing=TRUE),]
  rownames(SUMMARY) <- NULL
  SUMMARY
}



examine_movie_driver("Rating")
examine_movie_driver("Distribution")
examine_movie_driver("BudgetCat")
examine_movie_driver("RuntimeCat")
examine_movie_driver("TopBilledStar")
examine_movie_driver("MainGenre")    #Good
examine_movie_driver("SecondaryGenre")
examine_movie_driver("WeekOfMonth")
examine_movie_driver("Director")
examine_movie_driver("Universe") #Good
examine_movie_driver("Theaters") #Good
MYMOVIE$Month <- factor(MYMOVIE$Month,ordered=TRUE,levels=c(
  "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
))
examine_movie_driver("Month")



#Combinations of drivers that have a particularly high (or low) probability of success?

#Method 1:  Examine a simple partition model to find "pockets" of characteristics

TREE <- rpart(Success ~ ., data=MYMOVIE[,1:13], cp=.001, minbucket=50)
TREE$cptable
#summarize_tree gives a numerical score to each variable as to its importance to the model.  These numbers gauge the "strength" of the characteristic as a driver of success
summarize_tree(TREE)
visualize_model(TREE)


#tree JUST on variables that I'm interested in
TREE <- rpart(Success ~ .,data=MYMOVIE[,c("MainGenre","Universe","Theatres","Success")],cp=.003,minbucket=25)
TREE$cptable
visualize_model(TREE)



##Method 2:  Examine "association rules" (market basket).

#Look at everything
MOVIE.TRANS <- as(MYMOVIE,"transactions")

#Note:  warning here column(s) 3, 4 not logical or factor. Applying default discretization (see '? discretizeDF').
#That's because this only works without warning if EVERYTHING is categories and no numerical columns
#Already created categories for budget and runtime, so exclude columns 3 and 4!
MOVIE.TRANS <- as(MYMOVIE[,-c(3,4)],"transactions")


#Left of = sign is COLUMN name; Right of = sign is category being studied (happen to have same name here)
itemFrequency(MOVIE.TRANS)["Success=Success"]
RULES <- apriori(MOVIE.TRANS,parameter = list(supp=50/length(MOVIE.TRANS),conf=0.25,maxlen=3),
                 appearance = list(default="lhs",rhs="Success=Success"), control=list(verbose=FALSE))
RULES <- RULES[!is.redundant(RULES)]
RULES <- RULES[is.significant(RULES,MOVIE.TRANS)]
length(RULES)
inspect(sort(RULES,by="confidence"))

#Now only columns of desire
MOVIE.TRANS <- as(MYMOVIE[,c("Rating","BudgetCat","RuntimeCat","MainGenre","WeekOfMonth","Month","Success")],"transactions")
#Left of = sign is COLUMN name; Right of = sign is category being studied (happen to have same name here)
itemFrequency(MOVIE.TRANS)["Success=Success"]
RULES <- apriori(MOVIE.TRANS,parameter = list(supp=20/length(MOVIE.TRANS),conf=0.25,maxlen=3),
                 appearance = list(default="lhs",rhs="Success=Success"), control=list(verbose=FALSE))
RULES <- RULES[!is.redundant(RULES)]
RULES <- RULES[is.significant(RULES,MOVIE.TRANS)]
length(RULES)
inspect(sort(RULES,by="confidence"))
