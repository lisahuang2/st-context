#------------------------------#
#### EVAL-AT4 DATA ANALYSIS ####
#------------------------------#


#Set the directory to the location of the data file
setwd("C:/Users/Lisa/Documents/Lisa/Experiments/Stereotyping in context/Eval-AT4/full analysis in R, by congruency")
getwd()



#### Demographic data ####

# Go through the demographics file from each computer and remove blank lines, lines 
# from my own test runs, and lines with no data. 
# In variables with numeric values such as age, replace written words with digit values.
# Remove values for which no exact value was provided. (e.g., dUS = "all my life" or "17-18")


#Read in the demographics csv data files for each computer.
#Then add computer variable.

demC1 = read.csv("demC1.csv", header = TRUE)
View(demC1)
demC1$computer <- "comp1"

demC2 = read.csv("demC2.csv", header = TRUE)
View(demC2)
demC2$computer <- "comp2"

demC3 = read.csv("demC3.csv", header = TRUE)
View(demC3)
demC3$computer <- "comp3"


#Combine the demographics files from each computer
demogRaw <- rbind(demC1, demC2, demC3)
View(demogRaw)

#Save the raw demographics file
save(demogRaw, file="demogRaw.rda")

#Save another copy of the demographic data that will be used for analysis
demog <- demogRaw
save(demog, file="demog.rda")


#Remove unnecessary columns
View(demog)
demog <- demog[,c('Subject','Cond','computer','Date','Time','dintro','dsocial','dage','dsex','dethnic','denglang',
                  'dlongeng','studentid','evalBOB','evalJOHN','evalCHRIS','feelBob','feelJohn','feelChris',
                  'InitRate_eval','InitRate_feel')]



#Look through the data frame to check for missing subject numbers and duplicate subject numbers.

#There were three duplicate subject numbers: 130, 131, 156
#Assign new subject numbers to the duplicates (see the subject exclusions and notes document)

#Subject 130 (in row 80)
demog$Subject[81] = 215

#Subject 131 (in row 83)
demog$Subject[82] = 216

#Subject 156 (in row 154)
demog$Subject[155] = 214



#Now determine how many participants there are.
length(unique(demog$Subject))



#Add condition columns

#Add column for Expectancy Frequency condition
#(this is normally the valence frequency condition, but is now recoded in terms of frequency of expected impression)
demog$expectancy <- ifelse(demog$Cond == 1 |
                             demog$Cond == 2 |
                             demog$Cond == 5 |
                             demog$Cond == 6 |
                             demog$Cond == 9 |
                             demog$Cond == 10, 
                           c("congFreq"), c("congRare"))

#Add people counterbalancing Cond
demog$people <- ifelse(demog$Cond == 1 |
                         demog$Cond == 2 |
                         demog$Cond == 3 |
                         demog$Cond == 4,
                       "PCbob",
                       ifelse(demog$Cond == 5 |
                                demog$Cond == 6 |
                                demog$Cond == 7 |
                                demog$Cond == 8,
                              "PCjohn",
                              "PCchris"))

#Add button order counterbalancing Cond
demog$button <- ifelse(demog$Cond == 1 |
                         demog$Cond == 3 |
                         demog$Cond == 5 |
                         demog$Cond == 7 |
                         demog$Cond == 9 |
                         demog$Cond == 11, 
                       c("posF"), c("negF"))


#Create a new data frame for the scale ratings, and then remove them from the demographics data

scales <- demog[,c('Subject','Cond','expectancy','people','button','InitRate_eval','InitRate_feel',
                   'evalBOB','evalJOHN','evalCHRIS','feelBob','feelJohn','feelChris')]
View(scales)
save(scales, file='scales.rda')

demog <- demog[,c('Subject','Cond','expectancy','people','button','computer','Date','Time','dintro','dsocial',
                  'dage','dsex','dethnic','denglang','dlongeng','studentid')]


#Check that all the variables are in the correct data class

library(dplyr)
sapply(demog, class)

demog$Cond <- as.factor(demog$Cond)
demog$expectancy <- as.factor(demog$expectancy)
demog$people <- as.factor(demog$people)
demog$button <- as.factor(demog$button)
demog$dsex <- as.factor(demog$dsex)


#Summary descriptives

#Mean and SD of age
library(pastecs)
stat.desc(demog$dage)


##frequencies & proportions

#dsex
sexCount <- table(demog$dsex)
sexProp <- prop.table(sexCount)
cbind(sexCount,sexProp)

#dethnic
ethnicCount <- table(demog$dethnic)
ethnicProp <- prop.table(ethnicCount)
cbind(ethnicCount,ethnicProp)

#denglang
engCount <- table(demog$denglang)
engProp <- prop.table(engCount)
cbind(engCount,engProp)


#condition
condCount <- table(demog$Cond)
condProp <- prop.table(condCount)
cbind(condCount, condProp)

#expectancy frequency
expCount <- table(demog$expectancy)
expProp <- prop.table(expCount)
cbind(expCount,expProp)

#people
peopleCount <- table(demog$people)
peopleProp <- prop.table(peopleCount)
cbind(peopleCount,peopleProp)

#button
buttonCount <- table(demog$button)
buttonProp <- prop.table(buttonCount)
cbind(buttonCount,buttonProp)



#Save file
save(demog, file="demog.rda")



##------------------------------------------------------------------------------------------


### LEARNING PHASE ###


#Compile all of the individual subjects' csv files into one folder, then merge into one csv file.
#Delete all files from test runs.
#Make sure there are no duplicate subject numbers. 
#Also make a note of any missing subject numbers.

#***Refer to the subject exclusions and notes document for missing and duplicate subject numbers


#Read in the raw data file

learnRaw = read.csv("learnRaw.csv", header = TRUE)
View(learnRaw)
save(learnRaw, file='learnRaw.rda')

length(unique(learnRaw$Subj))


#Create a second copy in which to analyze the data

learn <- learnRaw
save(learn, file='learn.rda')
View(learn)

#Remove rows with header lines and instruction rows

learn <- subset(learn, Subj != "Subj")
learn <- subset(learn, Correct != "instr")


#Retain only the necessary columns 

learn <- learn[,c("Subj","Cond","Block","Trial","Correct","Resp","Name","RT","Correct.1")]


#Add a column for the expectancy frequency condition
#(in the original analyses, this was the valence frequency condition)

learn$expectancy <- ifelse(learn$Cond == 1 |
                             learn$Cond == 2 |
                             learn$Cond == 5 |
                             learn$Cond == 6 |
                             learn$Cond == 9 |
                             learn$Cond == 10, 
                           c("congFreq"), c("congRare"))


#Add people counterbalancing Cond

learn$people <- ifelse(learn$Cond == 1 |
                         learn$Cond == 2 |
                         learn$Cond == 3 |
                         learn$Cond == 4,
                       "PCbob",
                       ifelse(learn$Cond == 5 |
                                learn$Cond == 6 |
                                learn$Cond == 7 |
                                learn$Cond == 8,
                              "PCjohn",
                              "PCchris"))

#Add button order counterbalancing Cond

learn$button <- ifelse(learn$Cond == 1 |
                         learn$Cond == 3 |
                         learn$Cond == 5 |
                         learn$Cond == 7 |
                         learn$Cond == 9 |
                         learn$Cond == 11, 
                       c("consF"), c("incsF"))



# Rename the variables

library(dplyr)
learn = learn %>% rename('subject'='Subj','condition'='Cond','block'='Block','trialNum'='Trial','trialType'='Correct',
                         'response'='Name','correct'='Correct.1')



# Calculate the percent of correct responses for each participant within each block of trials

#Assign value of 1 to correct responses and 0 to incorrect responses 
learn$correct2 <- ifelse(learn$correct=="TRUE","1",
                         ifelse(learn$correct=="FALSE","0",
                                "null"
                         ))

#Check that there are no null responses

unique(learn$correct2)


#Make sure correct2 variable is numeric
sapply(learn,class)
learn$correct2 <- as.numeric(learn$correct2)


#Calculate the percentages to determine accuracy in each block

pctCorrect <-aggregate(learn$correct2, list(subject = learn$subject, block = learn$block), 
                       FUN=mean, na.rm=TRUE)
View(pctCorrect)

library(dplyr)
pctCorrect = pctCorrect %>% rename('pct'='x')

save(pctCorrect, file = "pctCorrect.rda")


#View percentages in block 10 only

pctBlock10 <- subset(pctCorrect, block == "10")
View(pctBlock10)


#Create a variable that indicates whether the subject passed the learning criteria
#(<75% correct on the last block of trials)
#Assign a value of 1 to good subjects and 0 to bad subjects
#Then make a note of the subjects who did not pass the learning criteria

pctBlock10$pass <- ifelse(pctBlock10$pct >= .75, 1, 
                          ifelse(pctBlock10$pct < .75, 0,
                                 'null'))

#Confirm the number of subjects who did not pass the learning criteria
library(plyr)
count(pctBlock10, 'pass')


save(pctBlock10, file = "pctBlock10.rda")



##------------------------------------------------------------------------------------------



### TEST PHASE ###


#Compile all of the individual subjects' csv files into one folder, then merge into one csv file.
#Delete all files from test runs.
#Make sure there are no duplicate subject numbers. 
#Also make a note of any missing subject numbers.

#***Refer to the subject exclusions and notes document for missing and duplicate subject numbers


#Then read in the raw data file

testRaw = read.csv("testRaw.csv", header = TRUE)
View(testRaw)
save(testRaw, file='testRaw.rda')

length(unique(testRaw$Subj))


#Create a second copy in which to analyze the data

test <- testRaw
View(test)
save(test, file='test.rda')


#Remove rows with header lines and instruction rows

test <- subset(test, Subj != "Subj")
test <- subset(test, Correct != "instr")


#Retain only the necessary columns 

test <- test[,c("Subj","Cond","Trial","Correct","Resp","Name","RT","Correct.1")]


#Add a column for the expectancy frequency condition
#(in the original analyses, this was the valence frequency condition)

test$expectancy <- ifelse(test$Cond == 1 |
                            test$Cond == 2 |
                            test$Cond == 5 |
                            test$Cond == 6 |
                            test$Cond == 9 |
                            test$Cond == 10, 
                          c("congFreq"), c("congRare"))


#Add people counterbalancing Cond

test$people <- ifelse(test$Cond == 1 |
                        test$Cond == 2 |
                        test$Cond == 3 |
                        test$Cond == 4,
                      "PCbob",
                      ifelse(test$Cond == 5 |
                               test$Cond == 6 |
                               test$Cond == 7 |
                               test$Cond == 8,
                             "PCjohn",
                             "PCchris"))

#Add button order counterbalancing Cond

test$button <- ifelse(test$Cond == 1 |
                        test$Cond == 3 |
                        test$Cond == 5 |
                        test$Cond == 7 |
                        test$Cond == 9 |
                        test$Cond == 11, 
                      c("consF"), c("incsF"))


# Rename the variables

library(dplyr)
test = test %>% rename('subject'='Subj','condition'='Cond','trialNum'='Trial','context'='Correct',
                       'response'='Name')


#Add a column to indicate whether the response was the consistent evaluation or the inconsistent evaluation
#(in the original analyses, the response was coded as frequent evaluation or the rare evaluation)

test$eval <- ifelse(test$button == 'consF' & test$response == 'F', 'cons',
                    ifelse(test$button == 'consF' & test$response == 'J', 'incs',
                    ifelse(test$button == 'incsF' & test$response == 'F', 'incs',
                    ifelse(test$button == 'incsF' & test$response == 'J', 'cons',
                           'null'))))


#***Find any null responses

unique(test$eval)
table(test$eval)



#Re-order the variables in the data file 

test <- test[,c('subject','condition','expectancy','people','button','trialNum','context',
                'Resp','response', 'RT','Correct.1','eval')] 

#Verify the number of participants

length(unique(test$subject))

save(test, file="test.rda")



#Remove participants who failed the learning criteria 

test <- test[which(test$subject!=43 & test$subject!=51 & test$subject!=85 & test$subject!=134 & test$subject!=143 &
                     test$subject!=172 & test$subject!=190 & test$subject!=205 & test$subject!=208 & test$subject!=214
                      ),]

#Remove participants with missing test phase data 

test <- test[which(test$subject!=189 & test$subject!=213),]



## Identify and remove subjects who pressed same button throughout the test phase ##

#Assign a value of 0 for F responses and 1 for J responses
test$response2 <- ifelse(test$response == "F","0",
                         ifelse(test$response == "J","1",
                                "null"))

#Check that there are no null responses
unique(test$response2)

#Make sure response2 variable is numeric
sapply(test,class)
test$response2 <- as.numeric(test$response2)


sameButton <-aggregate(test$response2, list(subject = test$subject), 
                       FUN=mean, na.rm=TRUE)
View(sameButton)
save(sameButton, file = "sameButton.rda")


#Participants for whom x = 0 pressed F on all trials, and participants for whom x = 1 
#pressed J on all trials. Make a note of these participants and remove them from the data.

test <- test[which(test$subject!=1 & test$subject!=86),]


# Check that all bad participants have been removed.

length(unique(test$subject))



# Code the trials by expectancy-consistent and expectancy-inconsistent rather than frequent and rare 

test$context2 <- ifelse(test$expectancy == "congFreq" & test$context == "Test Freq", "Test Cons",
                        ifelse(test$expectancy == "congFreq" & test$context == "Test Rare", "Test Incs",
                        ifelse(test$expectancy == "congRare" & test$context == "Test Freq", "Test Incs",
                        ifelse(test$expectancy == "congRare" & test$context == "Test Rare", "Test Cons",
                        ifelse(test$context == "Test Freq+Rare", "Test Cons+Incs",
                        ifelse(test$context == "Test Shared", "Test Shared",
                        ifelse(test$context == "Test Novel", "Test Novel",
                        ifelse(test$context == "Test All", "Test All",
                            "null"))))))))


unique(test$context2)


#Reorder the variables

test <- test[,c('subject','condition','expectancy','people','button','trialNum','context',
                'context2','Resp','response', 'RT','Correct.1','eval','response2')] 


## Separate the data by trial type ##

testCons <- subset(test, context2 == "Test Cons")
View(testCons)
save(testCons, file="testCons.rda")

testIncs <- subset(test, context2 == "Test Incs")
View(testIncs)
save(testIncs, file="testIncs.rda")

testShared <- subset(test, context2 == "Test Shared")
View(testShared)
save(testShared, file="testShared.rda")

testMixed <- subset(test, context2 == "Test Cons+Incs")
View(testMixed)
save(testMixed, file="testMixed.rda")

testNovel <- subset(test, context2 == "Test Novel")
View(testNovel)
save(testNovel, file="testNovel.rda")

testAll <- subset(test, context2 == "Test All")
View(testAll)
save(testAll, file="testAll.rda")



### Chi-square goodness-of-fit test ###

#-----------------------------
# Chisq.test(x,p = y)
# x = vector of observed values
# y = vector of expected proportions

#If expected proportions are equal, then you don't need p:
# chisq.test(x)

#Function for effect size
cohenW = function(chisq, n) {
  w = (sqrt(chisq/n))
  return(w)
}

# cohenW(chisq, n)


#Test Consistent Context
consCt <- table(testCons$eval)   #Obtain counts of each response
consProp <- prop.table(consCt)   #Obtain proportions
rbind(consCt, consProp)

cons <- c(340,14)               #Create vector of counts
exp <- c(.5,.5)
chisq.test(cons, p=exp)

sum(cons)    #To obtain n.
cohenW(300.21,354)         #Obtain effect size


#Test Inconsistent Context
incsCt <- table(testIncs$eval)
incsProp <- prop.table(incsCt) 
rbind(incsCt, incsProp)

incs <- c(29,325)   
exp <- c(.5,.5)
chisq.test(incs, p=exp)

sum(incs)
cohenW(247.5,354)


#Test Shared Context
shrdCt <- table(testShared$eval)
shrdProp <- prop.table(shrdCt) 
rbind(shrdCt, shrdProp)

shrd <- c(245,109)   
exp <- c(.5,.5)
chisq.test(shrd, p=exp)

sum(shrd)
cohenW(52.249,852)



#Test Mixed Context
mixCt <- table(testMixed$eval)
mixProp <- prop.table(mixCt) 
rbind(mixCt, mixProp)

mix <- c(322,386)   
exp <- c(.5,.5)
chisq.test(mix, p=exp)

sum(mix)
cohenW(5.7853,708)


#Test Novel Context
novCt <- table(testNovel$eval)
novProp <- prop.table(novCt) 
rbind(novCt, novProp)

nov <- c(569,139)   
exp <- c(.5,.5)
chisq.test(nov, p=exp)

sum(nov)
cohenW(261.16,708)


#Test All Context
allCt <- table(testAll$eval)
allProp <- prop.table(allCt) 
rbind(allCt, allProp)

all <- c(352,356)   
exp <- c(.5,.5)
chisq.test(all, p=exp)

sum(all)
cohenW(0.022599,708)



### Expectancy Frequency Moderation
### Chi-square test of independence

library(gmodels)

# if inputting from raw data in the data file (as I did below):
# CrossTable(predictor, outcome, fisher = T, chisq = T, expected = T, sresid = T, format = "SPSS")

# if inputting from a contingency table:
# CrossTable(contingencyTable, fisher = T, chisq = T, expected = T, sresid = T, format = "SPSS")


#Consistent trials, frequency moderation
View(testCons)
CrossTable(testCons$expectancy, testCons$eval, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(2.470443, 354)


#Inconsistent trials, frequency moderation
View(testIncs)
CrossTable(testIncs$expectancy, testIncs$eval, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(6.838625, 354)


    #There is a moderation by expectancy frequency, so break the data down by expectancy condition
    #and run an analysis for each condition separately.
    
    #Consistent Frequent condition
    tIncs_cFreq <- subset(testIncs, expectancy == "congFreq")
    View(tIncs_cFreq)
    
    ct <- table(tIncs_cFreq$eval)  
    prop <- prop.table(ct)   
    rbind(ct, prop)
    
    cnt <- c(21,153)             
    exp <- c(.5,.5)
    chisq.test(cnt, p=exp)
    
    sum(cnt)    
    cohenW(100.14,174)       


    #Consistent Rare condition
    tIncs_cRare <- subset(testIncs, expectancy == "congRare")
    View(tIncs_cRare)
    
    ct <- table(tIncs_cRare$eval)  
    prop <- prop.table(ct)   
    rbind(ct, prop)
    
    cnt <- c(8,172)             
    exp <- c(.5,.5)
    chisq.test(cnt, p=exp)
    
    sum(cnt)    
    cohenW(149,172)   
  
    
    #Create graph of the moderation
    
    incsExpect <- c('Consistent is common','Consistent is common','Consistent is rare','Consistent is rare')
    incsEval <- c('Consistent','Inconsistent','Consistent','Inconsistent')
    incsProp <- c(.12, .88, .04, .96)
    
    incsTable <- data.frame(expectancy = incsExpect, evaluation = incsEval, proportion = incsProp)
    View(incsTable)
  
    library(ggplot2)
    
    incsGraph <- ggplot(incsTable, aes(expectancy, proportion, fill = evaluation)) +
                          stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
                          labs(x = 'Expectancy', y = 'Proportion Selected', fill = 'Evaluation Selected')
      
    incsGraph

    
    
#Shared trials, frequency moderation
View(testShared)
CrossTable(testShared$expectancy, testShared$eval, fisher = T, chisq = T, expected = T, 
            prop.c = F, prop.t = F, sresid = T, format = "SPSS")
    
cohenW(32.0352, 354)

    
    #There is a moderation by expectancy frequency, so break the data down by expectancy condition
    #and run an analysis for each condition separately.
    
    #Consistent Frequent condition
    tShrd_cFreq <- subset(testShared, expectancy == "congFreq")
    View(tShrd_cFreq)
    
    ct <- table(tShrd_cFreq$eval)  
    prop <- prop.table(ct)   
    rbind(ct, prop)
    
    cnt <- c(145,29)             
    exp <- c(.5,.5)
    chisq.test(cnt, p=exp)
    
    sum(cnt)    
    cohenW(77.333,174)       
    
    
    #Consistent Rare condition
    tShrd_cRare <- subset(testShared, expectancy == "congRare")
    View(tShrd_cRare)
    
    ct <- table(tShrd_cRare$eval)  
    prop <- prop.table(ct)   
    rbind(ct, prop)
    
    cnt <- c(100,80)             
    exp <- c(.5,.5)
    chisq.test(cnt, p=exp)
    
    sum(cnt)    
    cohenW(2.2222,180)   
    
    
    #Create graph of the moderation
    
    shrdExpect <- c('Consistent is common','Consistent is common','Consistent is rare','Consistent is rare')
    shrdEval <- c('Consistent','Inconsistent','Consistent','Inconsistent')
    shrdProp <- c(.83, .17, .56, .44)
    
    shrdTable <- data.frame(expectancy = shrdExpect, evaluation = shrdEval, proportion = shrdProp)
    View(shrdTable)
    
    library(ggplot2)
    
    shrdGraph <- ggplot(shrdTable, aes(expectancy, proportion, fill = evaluation)) +
                    stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
                    labs(x = 'Expectancy', y = 'Proportion Selected', fill = 'Evaluation Selected')
    
    shrdGraph


#Mixed trials, frequency moderation

View(testMixed)
CrossTable(testMixed$expectancy, testMixed$eval, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(2.895437, 708)



#Novel trials, frequency moderation

View(testNovel)
CrossTable(testNovel$expectancy, testNovel$eval, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(5.438207, n=708)


    #There is a moderation by expectancy frequency, so break the data down by expectancy condition
    #and run an analysis for each condition separately.
    
    #Consistent Frequent condition
    tNov_cFreq <- subset(testNovel, expectancy == "congFreq")
    View(tNov_cFreq)
    
    ct <- table(tNov_cFreq$eval)  
    prop <- prop.table(ct)   
    rbind(ct, prop)
    
    cnt <- c(292,56)             
    exp <- c(.5,.5)
    chisq.test(cnt, p=exp)
    
    sum(cnt)    
    cohenW(160.05,348)       
    
    
    #Consistent Rare condition
    tNov_cRare <- subset(testNovel, expectancy == "congRare")
    View(tNov_cRare)
    
    ct <- table(tNov_cRare$eval)  
    prop <- prop.table(ct)   
    rbind(ct, prop)
    
    cnt <- c(277,83)             
    exp <- c(.5,.5)
    chisq.test(cnt, p=exp)
    
    sum(cnt)    
    cohenW(104.54,360)   

    
    #Create graph of the moderation
    
    novExpect <- c('Consistent is common','Consistent is common','Consistent is rare','Consistent is rare')
    novEval <- c('Consistent','Inconsistent','Consistent','Inconsistent')
    novProp <- c(.84, .16, .77, .23)
    
    novTable <- data.frame(expectancy = novExpect, evaluation = novEval, proportion = novProp)
    View(novTable)
    
    library(ggplot2)
    
    novGraph <- ggplot(novTable, aes(expectancy, proportion, fill = evaluation)) +
      stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
      labs(x = 'Expectancy', y = 'Proportion Selected', fill = 'Evaluation Selected')
    
    novGraph
    

    
#All trials, frequency moderation

View(testAll)
CrossTable(testAll$expectancy, testAll$eval, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(0.09196345, 708)



##------------------------------------------------------------------------------------------


### SCALE RATINGS ###


View(scales)

#Rename subject and condition
library(dplyr)
scales = scales %>% rename('subject'='Subject','condition'='Cond')


# Add variables to indicate the rating in each context
# Recode the ratings of each name (Bob, John, Chris) to cons, incs, and shared.
# See the Excel conditions for reference. 


#Evaluation ratings data


scales$evalCons <- ifelse(scales$condition == '1' | scales$condition == '2' | 
                              scales$condition == '11' | scales$condition == '12', scales$evalBOB,
                   ifelse(scales$condition == '3' | scales$condition == '4' | 
                              scales$condition == '5' | scales$condition == '6', scales$evalJOHN,
                   ifelse(scales$condition == '7' | scales$condition == '8' | 
                              scales$condition == '9' | scales$condition == '10', scales$evalCHRIS,
                   'null'
                           )))


scales$evalIncs <- ifelse(scales$condition == '3' | scales$condition == '4' | 
                            scales$condition == '9' | scales$condition == '10', scales$evalBOB,
                   ifelse(scales$condition == '1' | scales$condition == '2' | 
                            scales$condition == '7' | scales$condition == '8', scales$evalJOHN,
                   ifelse(scales$condition == '5' | scales$condition == '6' | 
                            scales$condition == '11' | scales$condition == '12', scales$evalCHRIS,
                   'null'
                                 )))


scales$evalShared <- ifelse(scales$people == "PCbob", scales$evalCHRIS,
                     ifelse(scales$people == "PCjohn", scales$evalBOB,
                     ifelse(scales$people == "PCchris", scales$evalJOHN,
                     "null")))


#Check that there are no null responses.

unique(scales$evalCons)
unique(scales$evalIncs)
unique(scales$evalShared)



#Feeling thermometer data


scales$feelCons <- ifelse(scales$condition == '1' | scales$condition == '2' | 
                            scales$condition == '11' | scales$condition == '12', scales$feelBob,
                   ifelse(scales$condition == '3' | scales$condition == '4' | 
                            scales$condition == '5' | scales$condition == '6', scales$feelJohn,
                   ifelse(scales$condition == '7' | scales$condition == '8' | 
                            scales$condition == '9' | scales$condition == '10', scales$feelChris,
                   'null'
                                 )))


scales$feelIncs <- ifelse(scales$condition == '3' | scales$condition == '4' | 
                            scales$condition == '9' | scales$condition == '10', scales$feelBob,
                   ifelse(scales$condition == '1' | scales$condition == '2' | 
                            scales$condition == '7' | scales$condition == '8', scales$feelJohn,
                   ifelse(scales$condition == '5' | scales$condition == '6' | 
                            scales$condition == '11' | scales$condition == '12', scales$feelChris,
                   'null'
                                 )))


scales$feelShared <- ifelse(scales$people == "PCbob", scales$feelChris,
                     ifelse(scales$people == "PCjohn", scales$feelBob,
                     ifelse(scales$people == "PCchris", scales$feelJohn,
                     "null")))


#Check that there are no null responses.

scales[which(scales$feelCons == 'null'),]
scales[which(scales$feelIncs == 'null'),]
scales[which(scales$feelShared == 'null'),]



#Check that each variable is the correct data type.
sapply(scales, class)


#Change variables to the correct data type

scales$condition <- factor(scales$condition)
scales$expectancy <- factor(scales$expectancy)
scales$people <- factor(scales$people)
scales$button <- factor(scales$button)

scales$evalCons <- as.integer(scales$evalCons)
scales$evalIncs <- as.integer(scales$evalIncs)
scales$evalShared <- as.integer(scales$evalShared)
scales$feelCons <- as.integer(scales$feelCons)
scales$feelIncs <- as.integer(scales$feelIncs)
scales$feelShared <- as.integer(scales$feelShared)


save(scales, file="scales.rda")


#Exclude bad participants

#Verify number of participants in the file. (It should be the full 213.)
View(scales)
length(unique(scales$subject))


#Remove participants who had missing learning phase data (n=4)

scales <- scales[which(scales$subject!=189 & scales$subject!=213 & scales$subject!=215 & scales$subject!=216),]


#Remove participants who failed the learning criteria (n=10)

scales <- scales[which(scales$subject!=43 & scales$subject!=51 & scales$subject!=85 & scales$subject!=134 & scales$subject!=143 &
                     scales$subject!=172 & scales$subject!=190 & scales$subject!=205 & scales$subject!=208 & scales$subject!=214
),]



#Transform the scales data to long format

library(reshape2)

#transform evaluation ratings into long format
scalesEvalL <- melt(scales, id.vars=c("subject","condition","expectancy"), 
                    measure.vars=c("evalCons","evalIncs","evalShared"),
                    variable.name="context",
                    value.name="rateEval")
View(scalesEvalL)
save(scalesEvalL, file = 'scalesEvalL.rda')


#transform feeling thermometer ratings into long format
scalesFeelL <- melt(scales, id.vars=c("subject","condition","expectancy"), 
                    measure.vars=c("feelCons","feelIncs","feelShared"),
                    variable.name="context",
                    value.name="rateFeel")
View(scalesFeelL)
save(scalesFeelL, file = 'scalesFeelL.rda')



#Descriptive statistics

library(pastecs)

#Attention check - initial ratings
stat.desc(scales[,c("InitRate_eval","InitRate_feel")])

#descriptives of scale ratings - by context
stat.desc(scales[,c("evalCons","evalIncs","evalShared","feelCons","feelIncs","feelShared")])

#descriptives of scale ratings - by expectancy (evaluative ratings)
by(scalesEvalL[,c('rateEval')], scalesEvalL$expectancy, stat.desc)

#descriptives of scale ratings - by expectancy (feeling thermometer ratings)
by(scalesFeelL[,c('rateFeel')], scalesFeelL$expectancy, stat.desc)

#descriptives of scale ratings - by context and expectancy
by(scales[,c("evalCons","evalIncs","evalShared","feelCons","feelIncs","feelShared")], scales$expectancy, stat.desc)



## Analysis of evaluative scale ratings

#Bar graph of the evaluative scale ratings

library(ggplot2)

ggplot(scalesEvalL, aes(context, rateEval, fill = expectancy)) +
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
  labs(x = 'context', y = 'evaluation')


#Run the ANOVA

library(ez)

evalModel <- ezANOVA(data=scalesEvalL,
                     dv = rateEval,
                     wid = subject,
                     between = expectancy,
                     within = context,
                     detailed = T,
                     type = 3) 
evalModel


#Calculate partial eta squared effect sizes
#Partial eta squared = SS effect/(SS effect + SS error)
#eff = SS of effect
#err = SS of error
#function -> eta2p(eff,err)

eta2p = function(eff, err) {
  y = eff/(eff + err)
  return(y)
}

#expectancy
eta2p(24.384553, 239.3072) 

#context
eta2p(2252.630451, 848.8603) 

#context x expectancy
eta2p(4.081037, 848.8603) 


#There is no interaction between expectancy and context, so collapse across expectancy
#and perform a one-way repeated measures ANOVA on context.
evalModel2 <- ezANOVA(data=scalesEvalL,
                     dv = rateEval,
                     wid = subject,
                     within = context,
                     detailed = T,
                     type = 3) 
evalModel2


# Effect size - eta squared = (SSn+SSd)/SSd
# Note that the formula is the same as for the effect size from the factorial anova,
# except it is now just eta-squared rather than partial eta-squared because there is 
# only one factor in the model.

eta2p(2252.392, 852.9414) 


#Post-hoc paired comparisons

pairwise.t.test(scalesEvalL$rateEval, scalesEvalL$context, paired = TRUE, 
                p.adjust.method = 'bonferroni')



## Analysis of feeling thermometer ratings

#Bar graph of the feeling thermometer ratings

library(ggplot2)

ggplot(scalesFeelL, aes(context, rateFeel, fill = expectancy)) +
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
  labs(x = 'context', y = 'rating')


#Run the ANOVA

library(ez)

feelModel <- ezANOVA(data=scalesFeelL,
                     dv = rateFeel,
                     wid = subject,
                     between = expectancy,
                     within = context,
                     detailed = T,
                     type = 3) 
feelModel


#Calculate partial eta squared effect sizes
#Partial eta squared = SS effect/(SS effect + SS error)
#eff = SS of effect
#err = SS of error
#function -> eta2p(eff,err)

eta2p = function(eff, err) {
  y = eff/(eff + err)
  return(y)
}

#expectancy
eta2p(3827.498, 70519.64) 

#context
eta2p(457968.059, 181157.92) 

#context x expectancy
eta2p(2140.682, 181157.92) 


#There is no interaction between expectancy and context, so collapse across expectancy
#and perform a one-way repeated measures ANOVA on context. Then conduct paired comparisons.

feelModel2 <- ezANOVA(data=scalesFeelL,
                      dv = rateFeel,
                      wid = subject,
                      within = context,
                      detailed = T,
                      type = 3) 
feelModel2


# Effect size - eta squared = (SSn+SSd)/SSd
# Note that the formula is the same as for the effect size from the factorial anova,
# except it is now just eta-squared rather than partial eta-squared because there is 
# only one factor in the model.

eta2p(457852.7, 183298.61) 


#Post-hoc paired comparisons

pairwise.t.test(scalesFeelL$rateFeel, scalesFeelL$context, paired = TRUE, 
                p.adjust.method = 'bonferroni')




#-------------------------------------------------------------------------------

#The multilevel modeling approach for repeated measures factors
#(You don't need to run it.)


#Evaluation ratings
library(nlme)
evalModel2 <- lme(rateEval ~ expectancy*context, random = ~1|subject/context, 
                  data = scalesEvalL, method = "ML")
summary(evalModel2)
anova(evalModel2)

  #simple effects contrasts
  library(phia)
  testInteractions(evalModel2, fixed="context", across="expectancy")


#Feeling thermometer ratings
library(nlme)
feelModel2 <- lme(rateFeel ~ expectancy*context, random = ~1|subject/context, 
                  data = scalesFeelL, method = "ML")
summary(feelModel2)
anova(feelModel2)
  
  #simple effects contrasts
  library(phia)
  testInteractions(feelModel2, fixed="context", across="expectancy")









#---------USEFUL FUNCTIONS------------------#

##Package Functions

install.packages("packageName") #Install package
remove.packages("packageName")  #Uninstall package
library(packageName)   #Open libraries
library()   #Shows the installed packages


##Directory Functions

setwd("directory goes here")    #Set the directory to the location of the datafile
getwd()   #Identify the current working directory. This function has no arguments.
dir() # show files in the working directory


##Reading and Saving files

myData = read.csv("datafile.csv", header = TRUE)    #Read in the csv data file.
myData <- read.csv(file.choose(), header = TRUE)    #Read in the csv data file.

load("myData.RDA")    #Load R data
search() # Shows the loaded packages

save(fileName, file="filename.rda")   #Save the data as an R data file


##Viewing Data

View(myData)    #Open up data in a new tab
head(myData)    #Show the first several lines of the data in the console  
names(mydata)   # Lists variables in the dataset 


##Identify data types

sapply(pilotScale, class) #Lists the data type for every variable in the data frame.
class(dataMatrix)         #Asks what data type "dataMatrix" is (data frame)
class(dataMatrix$column)   #Asks what data type is "column" from dataMatrix (character, numeric, integer, logical, factor)
is.numeric(dataMatrix$column)    # Asks whether the specified data is numeric
is.character(dataMatrix$column)  # Asks whether the specified data is character

pilotScale$trialType <- factor(pilotScale$trialType)    #Change data type to factor (to read as a nominal variable)
pilotScale$response <- as.numeric(as.character(pilotScale$response))  #Change from factor to numeric


##Manipulate data in data frame

stcont3pilot$condition <- NULL   #Delete a column
rm(stcont3pilot$condition)  #Delete a column, from dplyr


##Help Functions

?functionName   #To get help with a function or object
help(functionName)   #To get help with a function or object
??functionName    #Search the help pages for anything that has the word "functionName"
help(package=car) # View documentation in package 'car'. You can also type: library(help="car")

args(log) # Description of the command
apropos("age") # Search the word "age" in the objects available in the current R session


##Descriptive Statistics

summary(myData)   #Some descriptive statistics   




##---------R Resources------------------------##


## Look at R materials from PSC204A and PSC205 SEM class

## Other resources include the following:
##  - Software Carpentry R workshop: http://data-lessons.github.io/gapminder-R/index.html
##  - R for Data Science, online book from Ryan: http://r4ds.had.co.nz/
##  - Documentation from the R website itself

##-------------------------------