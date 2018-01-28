#------------------------------#
#### EVAL-AT5 DATA ANALYSIS ####
#------------------------------#


#Set the directory to the location of the data file
setwd("C:/Users/Lisa/Documents/Lisa/Experiments/Stereotyping in context/Eval-AT5/full analysis by congruency")
getwd()



#### Demographic data ####

# Go through the demographics file from each computer and remove blank lines, lines 
# from my own test runs, and lines with no data. 
# In variables with numeric values such as age, replace written words with digit values.
# Remove values for which no exact value was provided. (e.g., dUS = "all my life" or "17-18")


#Read in the demographics csv data files for each computer.
#Then add computer variable.

demJc1 = read.csv("demJc1.csv", header = TRUE)
View(demJc1)
demJc1$computer <- "174Jcomp1"

demJc2 = read.csv("demJc2.csv", header = TRUE)
View(demJc2)
demJc2$computer <- "174Jcomp2"
  
demMc1 = read.csv("demMc1.csv", header = TRUE)
View(demMc1)
demMc1$computer <- "174Mcomp1"

demMc2 = read.csv("demMc2.csv", header = TRUE)
View(demMc2)
demMc2$computer <- "174Mcomp2"

demMc3 = read.csv("demMc3.csv", header = TRUE)
View(demMc3)
demMc3$computer <- "174Mcomp3"


#Combine the demographics files from each computer
demogRaw <- rbind(demJc1, demJc2, demMc1, demMc2, demMc3)
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

#There were three duplicate subject numbers: 140, 141, and 179
#Assign new subject numbers to the duplicates (see the subject exclusions and notes document)

#Subject 140 (in row 61)
demog$Subject[60] = 326

#Subject 141 (in row 152)
demog$Subject[151] = 327

#Subject 179 (in row 82)
demog$Subject[82] = 328


#There was one subject (#1043) who completed the experiment but could not have been signed up for
#this study through SONA. Remove this person from the data.
demog <- demog[-c(185), ]


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
                             c("congRare"), c("congFreq"))

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
sexProp <- prop.table(table(demog$dsex))
cbind(sexCount,sexProp)

#dethnic
ethnicCount <- table(demog$dethnic)
ethnicProp <- prop.table(table(demog$dethnic))
cbind(ethnicCount,ethnicProp)

#denglang
engCount <- table(demog$denglang)
engProp <- prop.table(table(demog$denglang))
cbind(engCount,engProp)


#condition
condCount <- table(demog$Cond)
condProp <- prop.table(table(demog$Cond))
cbind(condCount, condProp)

#valence
expCount <- table(demog$expectancy)
expProp <- prop.table(table(demog$expectancy))
cbind(expCount,expProp)

#people
peopleCount <- table(demog$people)
peopleProp <- prop.table(table(demog$people))
cbind(peopleCount,peopleProp)

#button
buttonCount <- table(demog$button)
buttonProp <- prop.table(table(demog$button))
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
                       c("congRare"), c("congFreq"))


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
                      c("posF"), c("negF"))



# Rename the variables

library(dplyr)
learn = learn %>% rename('subject'='Subj','condition'='Cond','block'='Block','trialNum'='Trial','trialType'='Correct',
                       'response'='Name','correct'='Correct.1')



# For some reason, the trues and falses in the "correct" variable is all capitalized for only subject 328.

learn$correct <- ifelse(learn$correct == "TRUE" | learn$correct == "True", "True",
                        ifelse(learn$correct == "FALSE" | learn$correct == "False", "False",
                               "null"))


# Calculate the percent of correct responses for each participant within each block of trials

#Assign value of 1 to correct responses and 0 to incorrect responses 
learn$correct2 <- ifelse(learn$correct=="True","1",
                         ifelse(learn$correct=="False","0",
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
                        c("congRare"), c("congFreq"))


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
                       c("posF"), c("negF"))


# Rename the variables

library(dplyr)
test = test %>% rename('subject'='Subj','condition'='Cond','trialNum'='Trial','context'='Correct',
                       'response'='Name')


#Add a column to indicate whether the response was the consistent evaluation or the inconsistent evaluation
#(in the original analyses, the response was coded as frequent evaluation or the rare evaluation)

test$eval <- ifelse(test$condition == '1' & test$response == "F", "incs",
                          ifelse(test$condition == "1" & test$response == "J", "cons",
                          ifelse(test$condition == "2" & test$response == "F", "cons",
                          ifelse(test$condition == "2" & test$response == "J", "incs",
                          ifelse(test$condition == "3" & test$response == "F", "incs",
                          ifelse(test$condition == "3" & test$response == "J", "cons",
                          ifelse(test$condition == "4" & test$response == "F", "cons",
                          ifelse(test$condition == "4" & test$response == "J", "incs",
                          ifelse(test$condition == "5" & test$response == "F", "incs",
                          ifelse(test$condition == "5" & test$response == "J", "cons",
                          ifelse(test$condition == "6" & test$response == "F", "cons",
                          ifelse(test$condition == "6" & test$response == "J", "incs",
                          ifelse(test$condition == "7" & test$response == "F", "incs",
                          ifelse(test$condition == "7" & test$response == "J", "cons",
                          ifelse(test$condition == "8" & test$response == "F", "cons",
                          ifelse(test$condition == "8" & test$response == "J", "incs",
                          ifelse(test$condition == "9" & test$response == "F", "incs",
                          ifelse(test$condition == "9" & test$response == "J", "cons",
                          ifelse(test$condition == "10" & test$response == "F", "cons",
                          ifelse(test$condition == "10" & test$response == "J", "incs",
                          ifelse(test$condition == "11" & test$response == "F", "incs",
                          ifelse(test$condition == "11" & test$response == "J", "cons",
                          ifelse(test$condition == "12" & test$response == "F", "cons",
                          ifelse(test$condition == "12" & test$response == "J", "incs",
                               "null"))))))))))))))))))))))))


#***Find any null responses

library(plyr)
count(test, 'eval')


#Re-order the variables in the data file 

test <- test[,c('subject','condition','expectancy','people','button','trialNum','context',
                'Resp','response', 'RT','Correct.1','eval')] 

#Verify the number of participants

length(unique(test$subject))

save(test, file="test.rda")



#Remove participants who failed the learning criteria 

test <- test[which(test$subject!=10 & test$subject!=26 & test$subject!=44 & test$subject!=54 & test$subject!=58 &
                   test$subject!=66 & test$subject!=80 & test$subject!=84 & test$subject!=97 & test$subject!=98 & 
                   test$subject!=103  & test$subject!=107 & test$subject!=121 & test$subject!=126 & test$subject!=133 &
                   test$subject!=194 & test$subject!=302
                   ),]


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

#Subjects who pressed same button - #3

test <- test[which(test$subject!=3 & test$subject!=39 & test$subject!=303),]


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

cons <- c(403,23)               #Create vector of counts
exp <- c(.5,.5)
chisq.test(cons, p=exp)

sum(cons)    #To obtain n.
cohenW(338.97,426)         #Obtain effect size


#Test Inconsistent Context
incsCt <- table(testIncs$eval)
incsProp <- prop.table(incsCt) 
rbind(incsCt, incsProp)

incs <- c(30,396)   
exp <- c(.5,.5)
chisq.test(incs, p=exp)

sum(incs)
cohenW(314.45,426)


#Test Shared Context
shrdCt <- table(testShared$eval)
shrdProp <- prop.table(shrdCt) 
rbind(shrdCt, shrdProp)

shrd <- c(453,399)   
exp <- c(.5,.5)
chisq.test(shrd, p=exp)

sum(shrd)
cohenW(3.4225,852)


#Test Mixed Context
mixCt <- table(testMixed$eval)
mixProp <- prop.table(mixCt) 
rbind(mixCt, mixProp)

mix <- c(342,510)   
exp <- c(.5,.5)
chisq.test(mix, p=exp)

sum(mix)
cohenW(33.127,852)


#Test Novel Context
novCt <- table(testNovel$eval)
novProp <- prop.table(novCt) 
rbind(novCt, novProp)

nov <- c(491,361)   
exp <- c(.5,.5)
chisq.test(nov, p=exp)

sum(nov)
cohenW(19.836,852)


#Test All Context
allCt <- table(testAll$eval)
allProp <- prop.table(allCt) 
rbind(allCt, allProp)

all <- c(322,530)   
exp <- c(.5,.5)
chisq.test(all, p=exp)

sum(all)
cohenW(50.779,852)



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

cohenW(0.05642234, 426)


#Inconsistent trials, frequency moderation
View(testIncs)
CrossTable(testIncs$expectancy, testIncs$eval, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(5.043247, 426)


#There is a moderation by expectancy frequency, so break the data down by expectancy condition
#and run an analysis for each condition separately.

  #Consistent Frequent condition
  tIncs_cFreq <- subset(testIncs, expectancy == "congFreq")
  View(tIncs_cFreq)

  ct <- table(tIncs_cFreq$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(21,193)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(138.24,214)       


  #Consistent Rare condition
  tIncs_cRare <- subset(testIncs, expectancy == "congRare")
  View(tIncs_cRare)

  ct <- table(tIncs_cRare$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(9,203)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(177.53,212)   

  
  #Create graph of the moderation
  
  incsExpect <- c('Consistent is common','Consistent is common','Consistent is rare','Consistent is rare')
  incsEval <- c('Consistent','Inconsistent','Consistent','Inconsistent')
  incsProp <- c(.10, .90, .04, .96)
  
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

cohenW(96.22321, 852)


#There is a moderation by expectancy frequency, so break the data down by expectancy condition
#and run an analysis for each condition separately.

  #Consistent Frequent condition
  tShrd_cFreq <- subset(testShared, expectancy == "congFreq")
  View(tShrd_cFreq)

  ct <- table(tShrd_cFreq$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(299,129)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(67.523,428)       


  #Consistent Rare condition
  tShrd_cRare <- subset(testShared, expectancy == "congRare")
  View(tShrd_cRare)

  ct <- table(tShrd_cRare$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(154,270)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(31.736,424)   


  #Create graph of the moderation
  
  shrdExpect <- c('Consistent is common','Consistent is common','Consistent is rare','Consistent is rare')
  shrdEval <- c('Consistent','Inconsistent','Consistent','Inconsistent')
  shrdProp <- c(.70, .30, .36, .64)
  
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

cohenW(32.53068, 852)


#There is a moderation by expectancy frequency, so break the data down by expectancy condition
#and run an analysis for each condition separately.

  #Consistent Frequent condition
  tMix_cFreq <- subset(testMixed, expectancy == "congFreq")
  View(tMix_cFreq)

  ct <- table(tMix_cFreq$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(131,297)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(64.383,428)       


  #Consistent Rare condition
  tMix_cRare <- subset(testMixed, expectancy == "congRare")
  View(tMix_cRare)

  ct <- table(tMix_cRare$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(211,213)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(.009434,424)   

  
  #Create graph of the moderation
  
  mixExpect <- c('Consistent is common','Consistent is common','Consistent is rare','Consistent is rare')
  mixEval <- c('Consistent','Inconsistent','Consistent','Inconsistent')
  mixProp <- c(.31, .69, .50, .50)
  
  mixTable <- data.frame(expectancy = mixExpect, evaluation = mixEval, proportion = mixProp)
  View(mixTable)
  
  library(ggplot2)
  
  mixGraph <- ggplot(mixTable, aes(expectancy, proportion, fill = evaluation)) +
    stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
    labs(x = 'Expectancy', y = 'Proportion Selected', fill = 'Evaluation Selected')
  
  mixGraph



#Novel trials, frequency moderation

View(testNovel)
CrossTable(testNovel$expectancy, testNovel$eval, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(34.48056, n=852)


#There is a moderation by expectancy frequency, so break the data down by expectancy condition
#and run an analysis for each condition separately.

  #Consistent Frequent condition
  tNov_cFreq <- subset(testNovel, expectancy == "congFreq")
  View(tNov_cFreq)

  ct <- table(tNov_cFreq$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(289,139)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(52.57,428)       


  #Consistent Rare condition
  tNov_cRare <- subset(testNovel, expectancy == "congRare")
  View(tNov_cRare)

  ct <- table(tNov_cRare$eval)  
  prop <- prop.table(ct)   
  rbind(ct, prop)

  cnt <- c(202,222)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    
  cohenW(.9434,424)   

  
  #Create graph of the moderation
  
  novExpect <- c('Consistent is common','Consistent is common','Consistent is rare','Consistent is rare')
  novEval <- c('Consistent','Inconsistent','Consistent','Inconsistent')
  novProp <- c(.68, .32, .48, .52)
  
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

cohenW(6.29595, 852)



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


scales$evalCons <- ifelse(scales$condition == '3' | scales$condition == '4' | 
                            scales$condition == '9' | scales$condition == '10', scales$evalBOB,
                   ifelse(scales$condition == '1' | scales$condition == '2' | 
                            scales$condition == '7' | scales$condition == '8', scales$evalJOHN,
                   ifelse(scales$condition == '5' | scales$condition == '6' | 
                            scales$condition == '11' | scales$condition == '12', scales$evalCHRIS,
                   'null'
                                 )))


scales$evalIncs <- ifelse(scales$condition == '1' | scales$condition == '2' | 
                            scales$condition == '11' | scales$condition == '12', scales$evalBOB,
                   ifelse(scales$condition == '3' | scales$condition == '4' | 
                            scales$condition == '5' | scales$condition == '6', scales$evalJOHN,
                   ifelse(scales$condition == '7' | scales$condition == '8' | 
                            scales$condition == '9' | scales$condition == '10', scales$evalCHRIS,
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


scales$feelCons <- ifelse(scales$condition == '3' | scales$condition == '4' | 
                            scales$condition == '9' | scales$condition == '10', scales$feelBob,
                   ifelse(scales$condition == '1' | scales$condition == '2' | 
                            scales$condition == '7' | scales$condition == '8', scales$feelJohn,
                   ifelse(scales$condition == '5' | scales$condition == '6' | 
                            scales$condition == '11' | scales$condition == '12', scales$feelChris,
                   'null'
                                 )))


scales$feelIncs <- ifelse(scales$condition == '1' | scales$condition == '2' | 
                            scales$condition == '11' | scales$condition == '12', scales$feelBob,
                   ifelse(scales$condition == '3' | scales$condition == '4' | 
                            scales$condition == '5' | scales$condition == '6', scales$feelJohn,
                   ifelse(scales$condition == '7' | scales$condition == '8' | 
                            scales$condition == '9' | scales$condition == '10', scales$feelChris,
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


#Remove participants who had missing learning phase data (n=2)

scales <- scales[which(scales$subject!=326 & scales$subject!=327),]


#Remove participants who failed the learning criteria (n=17)

scales <- scales[which(scales$subject!=10 & scales$subject!=26 & scales$subject!=44 & scales$subject!=54 & scales$subject!=58 &
                     scales$subject!=66 & scales$subject!=80 & scales$subject!=84 & scales$subject!=97 & scales$subject!=98 & 
                     scales$subject!=103  & scales$subject!=107 & scales$subject!=121 & scales$subject!=126 & scales$subject!=133 &
                     scales$subject!=194 & scales$subject!=302
),]



#Transform to long format

library(reshape2)

#transform evaluation ratings into long format
scalesEvalL <- melt(scales, id.vars=c("subject","condition","expectancy"), 
                    measure.vars=c("evalCons","evalIncs","evalShared"),
                    variable.name="context",
                    value.name="rateEval")
View(scalesEvalL)
save(scalesEvalL, file = 'scalesEvalL.rda')

length(unique(scalesEvalL$subject))

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

#descriptives of scale ratings - by expectancy (eval ratings)
by(scalesEvalL[,c('rateEval')], scalesEvalL$expectancy, stat.desc)

#descriptives of scale ratings - by expectancy (feel ratings)
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
eta2p(42.60200, 226.5081) 

#context
eta2p(2584.73763, 623.4577) 

#context x expectancy
eta2p(50.80895, 623.4577) 


#Collapse across expectancy and perform a one-way repeated measures ANOVA on context.

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

eta2p(2579.733, 674.2667) 


#Post-hoc paired comparisons

pairwise.t.test(scalesEvalL$rateEval, scalesEvalL$context, paired = TRUE, 
                p.adjust.method = 'bonferroni')


# "Simple Effects" of the expectancy x context interaction


# Split the data by levels of Context

evalCons <- subset(scalesEvalL[which(scalesEvalL$context == "evalCons"),])
View(evalCons)
evalIncs <- subset(scalesEvalL[which(scalesEvalL$context == "evalIncs"),])
View(evalIncs)
evalShared <- subset(scalesEvalL[which(scalesEvalL$context == "evalShared"),]) 
View(evalShared)


#effsize library for cohen.d function
library(effsize)


#t-test of evalCons

EvalConsT <- t.test(evalCons$rateEval ~ evalCons$expectancy, data=evalCons, paired = F)
EvalConsT
cohen.d(evalCons$rateEval, evalCons$expectancy)


#t-test of evalCons

EvalIncsT <- t.test(evalIncs$rateEval ~ evalIncs$expectancy, data=evalIncs, paired = F)
EvalIncsT
cohen.d(evalIncs$rateEval, evalIncs$expectancy)


#t-test of evalShared

EvalSharedT <- t.test(evalShared$rateEval ~ evalShared$expectancy, data=evalShared, paired = F)
EvalSharedT
cohen.d(evalShared$rateEval, evalShared$expectancy)



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
eta2p(11478.56, 78531.95) 

#context
eta2p(517798.13, 160645.31) 

#context x expectancy
eta2p(12077.36, 160645.31) 



#Collapse across expectancy and perform a one-way repeated measures ANOVA with context. 
#Then conduct paired comparisons.

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

eta2p(515910.7, 172722.67) 


#Post-hoc paired comparisons

pairwise.t.test(scalesFeelL$rateFeel, scalesFeelL$context, paired = TRUE, 
                p.adjust.method = 'bonferroni')



# "Simple Effects" of the expectancy x context interaction


# Split the data by levels of Context

feelCons <- subset(scalesFeelL[which(scalesFeelL$context == "feelCons"),])
View(feelCons)
feelIncs <- subset(scalesFeelL[which(scalesFeelL$context == "feelIncs"),])
View(feelIncs)
feelShared <- subset(scalesFeelL[which(scalesFeelL$context == "feelShared"),]) 
View(feelShared)


#effsize library for cohen.d function
library(effsize)


#t-test of feelCons

feelConsT <- t.test(feelCons$rateFeel ~ feelCons$expectancy, data=feelCons, paired = F)
feelConsT
cohen.d(feelCons$rateFeel, feelCons$expectancy)


#t-test of feelCons

feelIncsT <- t.test(feelIncs$rateFeel ~ feelIncs$expectancy, data=feelIncs, paired = F)
feelIncsT
cohen.d(feelIncs$rateFeel, feelIncs$expectancy)


#t-test of feelShared

feelSharedT <- t.test(feelShared$rateFeel ~ feelShared$expectancy, data=feelShared, paired = F)
feelSharedT
cohen.d(feelShared$rateFeel, feelShared$expectancy)




#-------------------------------------------------------------------------------

#The multilevel modeling approach for repeated measures factors
#(You don't need to run it.)
#(The code might not be accurate.)


#Evaluation ratings

library(nlme)

evalModel2 <- lme(rateEval ~ valence*context, random = ~1|subject/context, 
                  data = scalesEvalL, method = "ML")
summary(evalModel2)
anova(evalModel2)

  #simple effects contrasts
  library(phia)
  testInteractions(evalModel2, fixed="context", across="expectancy")


#Feeling thermometer ratings

library(nlme)

feelModel2 <- lme(rateFeel ~ valence*context, random = ~1|subject/context, 
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