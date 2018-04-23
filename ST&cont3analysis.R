#------------------------------#
#### ST&CONT3 DATA ANALYSIS ####
#------------------------------#

#Packages used
library(reshape2)
library(dplyr)
library(pastecs)
library(plyr)
library(gmodels)
library(ggplot2)


#Read in the csv data file and save as an R file
rawdata = read.csv("rawData.csv", header = TRUE)
save(rawdata, file="rawdata.rda")

#View the data
View(rawdata)

#Save a second copy of the data that will be used for analyses
alldata <- rawdata
save(alldata, file="alldata.rda")
View(alldata)


##### -------------DATA CLEANING ------------------------------------------------------------

### REMOVE UNNECESSARY DATA COLUMNS ###

alldata <- rawdata[,c("date", "time", "group", "subject", "blocknum", "trialnum", 
                      "blockcode", "trialcode", "response", "correct", "latency")]  
head(alldata)


### REMOVE EXCLUDED SUBJECTS ###
# (See the ST&Cont3 DATA ANALYSIS word document and subj notes excel file for full list of exclusions)

#Subject numbers from test runs (1, 4, 327)
alldata <- alldata[which(alldata$subject!=1 & alldata$subject!=4 & alldata$subject!=327), ]

#Subjects who quit right after the demographics questions (not included in total N)
alldata <- alldata[which(alldata$subject!=30 & alldata$subject!=75 & alldata$subject!=122 & alldata$subject!=130 & 
                           alldata$subject!=166 & alldata$subject!=265 & alldata$subject!=279 & alldata$subject!=332 & 
                           alldata$subject!=373 & alldata$subject!=425 & alldata$subject!=434 & alldata$subject!=437 & 
                           alldata$subject!=439 & alldata$subject!=452 & alldata$subject!=603 & alldata$subject!=606 & 
                           alldata$subject!=626 & alldata$subject!=651 & alldata$subject!=663 & alldata$subject!=667 &
                           alldata$subject!=742 & alldata$subject!=781 & alldata$subject!=915 & alldata$subject!=988),]

#Subjects who failed the stereotype attention check
alldata <- alldata[which(alldata$subject!=27 & alldata$subject!=363 & alldata$subject!=420 & alldata$subject!=593 & 
                           alldata$subject!=618 & alldata$subject!=704 & alldata$subject!=997 & alldata$subject!=1210),]

#Subjects who failed the learning criteria in learning phase
alldata <- alldata[which(alldata$subject!=16 & alldata$subject!=18 & alldata$subject!=21 & alldata$subject!=51 & alldata$subject!=53 &
                       alldata$subject!=93 & alldata$subject!=101 & alldata$subject!=108 & alldata$subject!=112 & alldata$subject!=121 &
                       alldata$subject!=124 & alldata$subject!=226 & alldata$subject!=238 & alldata$subject!=247 & alldata$subject!=254 &
                       alldata$subject!=267 & alldata$subject!=314 & alldata$subject!=430 & alldata$subject!=470 & alldata$subject!=503 &
                       alldata$subject!=520 & alldata$subject!=546 & alldata$subject!=555 & alldata$subject!=556 & alldata$subject!=584 &
                       alldata$subject!=628 & alldata$subject!=659 & alldata$subject!=825 & alldata$subject!=851& alldata$subject!=876 & 
                       alldata$subject!=945 & alldata$subject!=950 & alldata$subject!=1090 & alldata$subject!=1093 & alldata$subject!=1145 & 
                       alldata$subject!=1160 & alldata$subject!=1173 & alldata$subject!=1192 & alldata$subject!=1243 &  alldata$subject!=1265),]

#Subjects who failed stereotype attention check AND learning criteria in learning phase
alldata <- alldata[which(alldata$subject!=32 & alldata$subject!=117 & alldata$subject!=129 & alldata$subject!=323 & alldata$subject!=350 &
                       alldata$subject!=379 & alldata$subject!=387 & alldata$subject!=479 & alldata$subject!=535 & alldata$subject!=589 &
                       alldata$subject!=596 & alldata$subject!=693 & alldata$subject!=715 & alldata$subject!=734 & alldata$subject!=796 &
                       alldata$subject!=892 & alldata$subject!=1006 & alldata$subject!=1100 & alldata$subject!=1129 & alldata$subject!=1170 &
                       alldata$subject!=1180 & alldata$subject!=1185),]

#Subjects with no test phase data and subjects who pressed the same button throughout the test phase
alldata <- alldata[which(alldata$subject!=534 & alldata$subject!=622 & alldata$subject!=785),]

#Duplicate subjects
alldata <- alldata[!(alldata$subject==383 & alldata$time=="11:00:34"),]
alldata <- alldata[!(alldata$subject==860 & alldata$time=="20:59:32"),]

#Save file
save(alldata, file="alldata.rda")

# Check that all excluded participants have been removed.
# There should be a total of 255 participants up to this point.
length(unique(alldata$subject))


### ADD CONDITION COLUMNS ###

#Create condition variable. The condition is the remainder of the subject number divided by
#the number of conditions, in this case, 12. Then recode condition 0 as condition 12.
alldata$condition <- alldata$subject %% 12
alldata$condition <- ifelse(alldata$condition == 0, 
                        c(12), c(alldata$condition)) 

#Add stereotype condition column
alldata$stereotype <- ifelse(alldata$condition == 1 |
                                  alldata$condition == 3 |
                                  alldata$condition == 5 |
                                  alldata$condition == 7 |
                                  alldata$condition == 9 |
                                  alldata$condition == 11, 
                                  c("extraverted"), c("introverted"))

#Add people counterbalancing condition
alldata$people <- ifelse(alldata$condition == 1 |
                              alldata$condition == 2 |
                              alldata$condition == 3 |
                              alldata$condition == 4,
                              "P1bob",
                                  ifelse(alldata$condition == 5 |
                                         alldata$condition == 6 |
                                         alldata$condition == 7 |
                                         alldata$condition == 8,
                                         "P1john",
                                              "P1chris"))

#Add button order counterbalancing condition
alldata$button <- ifelse(alldata$condition == 1 |
                              alldata$condition == 2 |
                              alldata$condition == 5 |
                              alldata$condition == 6 |
                              alldata$condition == 9 |
                              alldata$condition == 10, 
                                  c("extraF"), c("introF"))

#Move the stereotype, people, and button columns right after the condition column.
alldata <- alldata[ , c("date", "time", "group", "subject", "condition", "stereotype", 
                               "people", "button", "blocknum", "trialnum", "blockcode", "trialcode", 
                               "response", "correct", "latency")]

#select the subject, blockcode, condition, stereotype, people, and button columns to check that the 
#correct conditions have been assigned to each subject.
viewCond = alldata[ , c("subject", "blockcode", "condition", "stereotype", "people", "button")]
View(viewCond)
save(viewCond, file="viewCond.rda")


#####-----SEPARATE INTO DIFFERENT FILES------------------------------------------------------------

### SEPARATE EACH PART OF THE EXPERIMENT INTO SEPARATE DATA FILES FOR ANALYSIS ###

#Demographic data
demog <- subset(alldata, blocknum == 1)
View(demog)
save(demog, file="demog.rda")

#Stereotype manipulation block
stpCheck <- subset(alldata, blocknum == 2)
View(stpCheck)
save(stpCheck, file="stpCheck.rda")

#Learning phase
learn <- subset(alldata, blocknum %in% c(4:13))
View(learn)
save(learn, file="learn.rda")

#Test phase
testMain <- subset(alldata, blocknum == 15)
View(testMain)
save(testMain, file="testMain.rda")

#Scale ratings
scaleMain <- subset(alldata, blocknum == 16)
View(scaleMain)
save(scaleMain, file="scaleMain.rda")

#Test phase and scale ratings of ingroup and outgroup targets
other <- subset(alldata, blocknum == 17)
View(other)
save(other, file="other.rda")


#####-------DEMOGRAPHIC DATA-----------------------------------------------------------------------------------

View(demog)

#Create a wide format demographics file
library(reshape2)
demogW <- dcast(demog, date + time + group + subject + condition + stereotype + people + button
                          ~ trialcode, value.var = "response")
demogW$instrDemo <- NULL
demogW$intro1 <- NULL
View(demogW)
save(demogW, file="demogW.rda")

#Check that all the variables are in the correct data classs
library(dplyr)
sapply(demogW, class)

#Change variables to the correct data classs
demogW$condition <- as.factor(as.character(demogW$condition))
demogW$stereotype <- as.factor(demogW$stereotype)
demogW$people <- as.factor(demogW$people)
demogW$button <- as.factor(demogW$button)
demogW$dage <- as.numeric(demogW$dage)


### SAMPLE SIZES BY CONDITION (Frequencies & Proportions) ###

#Separated by all 12 experimental conditions
condCount <- table(demogW$condition)
condProp <- prop.table(table(demogW$condition))
cbind(condCount, condProp)

#Separated by stereotype condition
stpCount <- table(demogW$stereotype)
stpProp <- prop.table(table(demogW$stereotype))
cbind(stpCount,stpProp)

#Separated by people counterbalancing condition
peopleCount <- table(demogW$people)
peopleProp <- prop.table(table(demogW$people))
cbind(peopleCount,peopleProp)

#Separated by button counterbalancing condition
buttonCount <- table(demogW$button)
buttonProp <- prop.table(table(demogW$button))
cbind(buttonCount,buttonProp)


### SUMMARY DESCRIPTIVES ###

#Mean and SD of age
mean(demogW$dage)
sd(demogW$dage)

#use pastecs package for more descriptive stats
library(pastecs)
stat.desc(demogW$dage)

#Counts and proportions of ethnicity (dethnic)
ethnicCount <- table(demogW$dethnic)
ethnicProp <- prop.table(table(demogW$dethnic))
cbind(ethnicCount,ethnicProp)

#Counts and proportions of gender (dgender)
genderCount <- table(demogW$dgender)
genderProp <- prop.table(table(demogW$dgender))
cbind(genderCount,genderProp)

#Frequencies and proportions of English as first language (denglang, yes or no)
engCount <- table(demogW$denglang)
engProp <- prop.table(table(demogW$denglang))
cbind(engCount,engProp)

#Frequencies and proportions of born in US (dUS, yes or no)
usCount <- table(demogW$dUS)
usProp <- prop.table(table(demogW$dUS))
cbind(usCount,usProp)

#Save the file
save(demogW, file="demogW.rda")


#####--------TEST PHASE DATA CLEANING------------------------------------------------------------------------

View(testMain)

#Remove all columns except for subject, trialcode, and response. Save as a new data frame.
testMain <- testMain[,c("subject", "condition", "stereotype", "people", "button", "trialcode", "response")]
View(testMain)

#Remove instruction trial rows
testMain <- subset(testMain, trialcode != "testInstr")

#Add a variable to indicate the trial type
testMain$trialType <- ifelse(testMain$trialcode == "P1c1v2" | 
                                   testMain$trialcode == "P1c3v4" |
                                   testMain$trialcode == "P1c5v6" | 
                                   testMain$trialcode == "P1c7v8" | 
                                   testMain$trialcode == "P1c9v10" | 
                                   testMain$trialcode == "P1c11v12", "P1",
                             
                               ifelse(testMain$trialcode == "P2c1v2" | 
                                        testMain$trialcode == "P2c3v4" |
                                        testMain$trialcode == "P2c5v6" | 
                                        testMain$trialcode == "P2c7v8" | 
                                        testMain$trialcode == "P2c9v10" | 
                                        testMain$trialcode == "P2c11v12", "P2",
                             
                               ifelse(testMain$trialcode == "sharedc1v2" | 
                                        testMain$trialcode == "sharedc3v4" |
                                        testMain$trialcode == "sharedc5v6" | 
                                        testMain$trialcode == "sharedc7v8" | 
                                        testMain$trialcode == "sharedc9v10" | 
                                        testMain$trialcode == "sharedc11v12", "shared",
                                      
                               ifelse(testMain$trialcode == "mix1c1v2" | 
                                        testMain$trialcode == "mix1c3v4" |
                                        testMain$trialcode == "mix1c5v6" | 
                                        testMain$trialcode == "mix1c7v8" | 
                                        testMain$trialcode == "mix1c9v10" | 
                                        testMain$trialcode == "mix1c11v12" | 
                                        testMain$trialcode == "mix2c1v2" | 
                                        testMain$trialcode == "mix2c3v4" |
                                        testMain$trialcode == "mix2c5v6" | 
                                        testMain$trialcode == "mix2c7v8" | 
                                        testMain$trialcode == "mix2c9v10" | 
                                        testMain$trialcode == "mix2c11v12", "mixed",      
                                      
                                ifelse(testMain$trialcode == "all1c1v2" | 
                                        testMain$trialcode == "all1c3v4" |
                                        testMain$trialcode == "all1c5v6" | 
                                        testMain$trialcode == "all1c7v8" | 
                                        testMain$trialcode == "all1c9v10" | 
                                        testMain$trialcode == "all1c11v12" | 
                                        testMain$trialcode == "all2c1v2" | 
                                        testMain$trialcode == "all2c3v4" |
                                        testMain$trialcode == "all2c5v6" | 
                                        testMain$trialcode == "all2c7v8" | 
                                        testMain$trialcode == "all2c9v10" | 
                                        testMain$trialcode == "all2c11v12", "all",
                                    "novel")))))  

#Add a variable to indicate the button response letter
testMain$response2 <- ifelse(testMain$response == 33, "F",
                             ifelse(testMain$response == 36, "J",
                                    "null"))

#Find any null responses
library(plyr)
count(testMain, 'response2')

#Add a variable to indicate whether the response was the consistent trait or the inconsistent trait
testMain$response3 <- ifelse(testMain$stereotype == "extraverted" & 
                                 testMain$button == "extraF" & 
                                 testMain$response2 == "F", "cons", 
                                 
                               ifelse(testMain$stereotype == "extraverted" &
                                        testMain$button == "extraF" &
                                        testMain$response2 == "J", "incs",
                               
                               ifelse(testMain$stereotype == "extraverted" &
                                        testMain$button == "introF" &
                                        testMain$response2 == "F", "incs",
                              
                               ifelse(testMain$stereotype == "extraverted" &
                                        testMain$button == "introF" &
                                        testMain$response2 == "J", "cons",
                                
                               ifelse(testMain$stereotype == "introverted" &
                                        testMain$button == "extraF" &
                                        testMain$response2 == "F", "incs",
                                      
                               ifelse(testMain$stereotype == "introverted" &
                                        testMain$button == "extraF" &
                                        testMain$response2 == "J", "cons",
                                      
                               ifelse(testMain$stereotype == "introverted" &
                                        testMain$button == "introF" &
                                        testMain$response2 == "F", "cons",
                                  
                               ifelse(testMain$stereotype == "introverted" &
                                        testMain$button == "introF" &
                                        testMain$response2 == "J", "incs",
                                    
                               "null"))))))))

#Find any null responses
library(plyr)
count(testMain, 'response3')

#Save the file
save(testMain, file="testMain.rda")


## SEPARATE THE DATA BY TRIAL TYPE ##

#Congruent context trials
testCons <- subset(testMain, trialType == "P1")
View(testCons)
save(testCons, file="testCons.rda")

#Incongruent context trials
testIncs <- subset(testMain, trialType == "P2")
View(testIncs)
save(testIncs, file="testIncs.rda")

#Shared context trials
testShared <- subset(testMain, trialType == "shared")
View(testShared)
save(testShared, file="testShared.rda")

#Mixed context trials
testMixed <- subset(testMain, trialType == "mixed")
View(testMixed)
save(testMixed, file="testMixed.rda")

#Novel context trials
testNovel <- subset(testMain, trialType == "novel")
View(testNovel)
save(testNovel, file="testNovel.rda")

#All context trials
testAll <- subset(testMain, trialType == "all")
View(testAll)
save(testAll, file="testAll.rda")


#####--------TEST PHASE ANALYSES OF MAIN TARGET (STEVE) -------------------------------------------

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


## TEST CONGRUENT CONTEXT ##

consCt <- table(testCons$response3)   #Obtain counts of each response before running the chi-square test
consProp <- prop.table(table(testCons$response3))   #Obtain proportions
rbind(consCt, consProp)

cons <- c(465,45)               #Create vector of counts
exp <- c(.5,.5)
chisq.test(cons, p=exp)

sum(cons)    #To obtain n.
cohenW(345.88,510)         #Obtain effect size


## TEST INCONGRUENT CONTEXT ##

incsCt <- table(testIncs$response3)
incsProp <- prop.table(table(testIncs$response3)) 
rbind(incsCt, incsProp)

incs <- c(42,468)   
exp <- c(.5,.5)
chisq.test(incs, p=exp)

sum(incs)
cohenW(355.84,510)


## TEST SHARED CONTEXT ##

shrdCt <- table(testShared$response3)
shrdProp <- prop.table(table(testShared$response3)) 
rbind(shrdCt, shrdProp)

shrd <- c(566,454)   
exp <- c(.5,.5)
chisq.test(shrd, p=exp)

sum(shrd)
cohenW(12.298,1020)


## TEST MIXED CONTEXT ##

mixCt <- table(testMixed$response3)
mixProp <- prop.table(table(testMixed$response3)) 
rbind(mixCt, mixProp)

mix <- c(502,518)   
exp <- c(.5,.5)
chisq.test(mix, p=exp)

sum(mix)
cohenW(.25098,1020)


## TEST NOVEL CONTEXT ##

novCt <- table(testNovel$response3)
novProp <- prop.table(table(testNovel$response3)) 
rbind(novCt, novProp)

nov <- c(650,370)   
exp <- c(.5,.5)
chisq.test(nov, p=exp)

sum(nov)
cohenW(76.863,1020)


## TEST ALL CONTEXT ##

allCt <- table(testAll$response3)
allProp <- prop.table(table(testAll$response3)) 
rbind(allCt, allProp)

all <- c(490,530)   
exp <- c(.5,.5)
chisq.test(all, p=exp)

sum(all)
cohenW(1.5686,1020)


### MODERATION BY STEREOTYPE CONDITION ###
### CHI-SQUARE TEST OF INDEPENDENCE ###

library(gmodels)

# if inputting from raw data in the data file (as I did below):
# CrossTable(predictor, outcome, fisher = T, chisq = T, expected = T, sresid = T, format = "SPSS")

# if inputting from a contingency table:
# CrossTable(contingencyTable, fisher = T, chisq = T, expected = T, sresid = T, format = "SPSS")


## CONGRUENT CONTEXT, MODERATION BY STEREOTYPE ##

head(testCons)
CrossTable(testCons$stereotype, testCons$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(0.7207373, 510)


## INCONGRUENT CONTEXT, MODERATION BY STEREOTYPE ##

head(testIncs)
CrossTable(testIncs$stereotype, testIncs$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(3.846858, 510)

#There is a significant moderation by stereotype, so break the data apart by stereotype condition

  #Extraverted condition
  tIncsExt <- subset(testIncs, stereotype == "extraverted")
  View(tIncsExt)
  
  ct <- table(tIncsExt$response3)  
  prop <- prop.table(table(tIncsExt$response3))   
  rbind(ct, prop)
  
  cnt <- c(17,263)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)
  
  sum(cnt)    
  cohenW(216.13,280)       
  
  #Introverted condition
  tIncsInt <- subset(testIncs, stereotype == "introverted")
  View(tIncsInt)
  
  ct <- table(tIncsInt$response3)  
  prop <- prop.table(table(tIncsInt$response3))   
  rbind(ct, prop)
  
  cnt <- c(25,205)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)

  sum(cnt)    #To obtain n.
  cohenW(140.87,230)      


## SHARED CONTEXT, MODERATION BY STEREOTYPE ##

head(testShared)
CrossTable(testShared$stereotype, testShared$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(42.11659, 1020)

#There is a strong moderation by stereotype, so break the data apart by stereotype condition
  
  #Extraverted condition
  tSharedExt <- subset(testShared, stereotype == "extraverted")
  View(tSharedExt)

  ct <- table(tSharedExt$response3)  
  prop <- prop.table(table(tSharedExt$response3))   
  rbind(ct, prop)
  
  cnt <- c(362,198)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)
  
  sum(cnt)   
  cohenW(48.029,560)       
  
  #Introverted condition
  tSharedInt <- subset(testShared, stereotype == "introverted")
  View(tSharedInt)
  
  ct <- table(tSharedInt$response3)  
  prop <- prop.table(table(tSharedInt$response3))   
  rbind(ct, prop)
  
  cnt <- c(204,256)             
  exp <- c(.5,.5)
  chisq.test(cnt, p=exp)
  
  sum(cnt)    #To obtain n.
  cohenW(5.8783,460)      


## MIXED CONTEXT, MODERATION BY STEREOTYPE ##

head(testMixed)
CrossTable(testMixed$stereotype, testMixed$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(2.518273, 1020)


## NOVEL CONTEXT, MODERATION BY STEREOTYPE ##

head(testNovel)
CrossTable(testNovel$stereotype, testNovel$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(0.1685929, n=1020)


## ALL CONTEXT, MODERATION BY STEREOTYPE ##

head(testAll)
CrossTable(testAll$stereotype, testAll$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(3.559828, 1020)


#####--------SCALE RATINGS OF MAIN TARGET STEVE----------------------------------------------

View(scaleMain)

## CLEAN THE SCALE RATINGS DATA ##

# Add a variable to indicate the trial type

    # people - P1bob, P1john, P1chris
    # trialcode - 
        # P1Bob condition
              # scaleBob = Pcons
              # scaleJohn = Pincs
              # scaleChris = shared
              # scaleBobJohn = mixed
        # P1john condition
              # scaleJohn = Pcons
              # scaleChris = Pincs
              # scaleBob = shared
              # scaleJohnChris = mixed
        # P1chris condition
              # scaleChris = Pcons
              # scaleBob = Pincs
              # scaleJohn = shared
              # scaleChrisBob = mixed
        # all conditions
              # scaleNovel
              # scaleOverall
              # scaleInstr

library(dplyr)

scaleMain$trialType <- ifelse(scaleMain$people == "P1bob" & scaleMain$trialcode == "scaleBob", "Pcons",
                        ifelse(scaleMain$people == "P1bob" & scaleMain$trialcode == "scaleJohn", "Pincs",
                        ifelse(scaleMain$people == "P1bob" & scaleMain$trialcode == "scaleChris", "shared",
                        
                        ifelse(scaleMain$people == "P1john" & scaleMain$trialcode == "scaleJohn", "Pcons",
                        ifelse(scaleMain$people == "P1john" & scaleMain$trialcode == "scaleChris", "Pincs",
                        ifelse(scaleMain$people == "P1john" & scaleMain$trialcode == "scaleBob", "shared",
                           
                        ifelse(scaleMain$people == "P1chris" & scaleMain$trialcode == "scaleChris", "Pcons",
                        ifelse(scaleMain$people == "P1chris" & scaleMain$trialcode == "scaleBob", "Pincs",
                        ifelse(scaleMain$people == "P1chris" & scaleMain$trialcode == "scaleJohn", "shared",
                                   
                        ifelse(scaleMain$trialcode == "scaleBobJohn" | 
                               scaleMain$trialcode == "scaleJohnChris" |
                               scaleMain$trialcode == "scaleChrisBob", "mixed",
                               
                        ifelse(scaleMain$trialcode == "scaleNovel", "novel",
                        ifelse(scaleMain$trialcode == "scaleOverall", "overall",            
                               "scaleInstr"))))))))))))

#Remove all scaleInstr rows.
scaleMain <- subset(scaleMain,  trialType != "scaleInstr")

#Check that each variable is the correct data type.
sapply(scaleMain, class)

#Change the variables to the correct class.
scaleMain$condition <- factor(scaleMain$condition)
scaleMain$stereotype <- factor(scaleMain$stereotype)
scaleMain$people <- factor(scaleMain$people)
scaleMain$button <- factor(scaleMain$button)
scaleMain$trialType <- factor(scaleMain$trialType)
scaleMain$response <- as.numeric(as.character(scaleMain$response))

#Convert the scale ratings data to wide format
library(reshape2)
scaleMainW <- dcast(scaleMain, date + time + group + subject + condition + stereotype + people + button
                     ~ trialType, value.var = "response")
View(scaleMainW)

#Save and export the wide format data to a csv file
save(scaleMainW, file="scaleMainW.rda")
write.csv(scaleMainW, "scaleMainW.csv")


## DESCRIPTIVE STATISTICS OF SCALE RATINGS DATA ##

#Aggregate function on the wide format data for mean and SD
aggregate(scaleMainW[c("Pcons","Pincs","shared","mixed","novel","overall")], 
          scaleMainW["stereotype"], mean, na.rm=TRUE)
aggregate(scaleMainW[c("Pcons","Pincs","shared","mixed","novel","overall")], 
          scaleMainW["stereotype"], sd, na.rm=TRUE)

#For more comprehensive statistics, use stat.desc from the pastecs package with the long format data
library(pastecs)
by(scaleMain$response, list(scaleMain$trialType,scaleMain$stereotype), stat.desc)


## ANALYSIS OF VARIANCE (in the long format data) ##

head(scaleMain)
library(ez)

mixedAOV <- ezANOVA(data = scaleMain, 
                    dv = .(response), 
                    wid = .(subject), 
                    within = .(trialType), 
                    between = .(stereotype), 
                    type = 3, 
                    detailed = TRUE,
                    return_aov = TRUE)
             
#Write a function to calculate partial eta-squared effect sizes
#Partial eta squared = SS effect/(SS effect + SS error)
#eff = SS of effect
#err = SS of error
#function -> eta2p(eff,err)

eta2p = function(eff, err) {
  y = eff/(eff + err)
  return(y)
}

#effect size of trialType (i.e., context)
eta2p(112.1569, 3115.2833) 

#effect size of stereotype
eta2p(92.5419, 491.1444) 

#effect size of trialType x stereotype
eta2p(2153.2265, 3115.2833) 


# *Note - To conduct a simple effects analysis, you have to run the anova using the lm function. 
# Then you can use the package lsmeans or phia to calculate the simple effects.
# It doesn't seem possible to do this with a mixed effects model (i.e., between var x within var interaction). 


## PLOT THE SCALE RATINGS ##

library(ggplot2)

#context variable is 'trialType'
#stereotype variable is 'stereotype'
#extraversion/introversion rating is 'response'

ggplot(scaleMain, aes(trialType, response, fill = stereotype)) +
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
  labs(x = 'Context', y = 'extraversion/introversion rating') +



#####----------ANALYSIS OF THE INGROUP AND OUTGROUP TARGETS---------------------------------------------

View(other)

## CLEAN AND SEPARATE THE FILES ##

#Remove all columns except for subject, condition, stereotype, people, button, trialcode, and response. 
other2 <- other[,c("subject", "condition", "stereotype", "people", "button", "trialcode", "response")]
View(other2)

#Save as a new data frame.
save(other2, file="pilotOther2.rda")

#Remove excluded participants (see separate document on subject exclusions)
other2 <- other2[which(other2$subject!=58 & other2$subject!=73 & other2$subject!=136 & other2$subject!=144 & 
                           other2$subject!=174 & other2$subject!=185 & other2$subject!=195 & other2$subject!=200 & 
                           other2$subject!=215 & other2$subject!=243 & other2$subject!=259 & other2$subject!=338 & 
                           other2$subject!=358 & other2$subject!=388 & other2$subject!=391 & other2$subject!=406 & 
                           other2$subject!=408 & other2$subject!=414 & other2$subject!=448 & other2$subject!=466 &
                           other2$subject!=529 & other2$subject!=544 & other2$subject!=568 & other2$subject!=573 &
                           other2$subject!=575 & other2$subject!=600 & other2$subject!=646 & other2$subject!=648 & 
                           other2$subject!=673 & other2$subject!=674 & other2$subject!=709 & other2$subject!=712 & 
                           other2$subject!=714 & other2$subject!=717 & other2$subject!=735 & other2$subject!=782 & 
                           other2$subject!=804 & other2$subject!=806 & other2$subject!=854 & other2$subject!=860 &
                           other2$subject!=884 & other2$subject!=891 & other2$subject!=936 & other2$subject!=943 &
                           other2$subject!=947 & other2$subject!=954 & other2$subject!=978 & other2$subject!=985 &
                           other2$subject!=1039 & other2$subject!=1057 & other2$subject!=1060 & other2$subject!=1099 &
                           other2$subject!=1102 & other2$subject!=1119 & other2$subject!=1131 & other2$subject!=1143 &
                           other2$subject!=1187 & other2$subject!=1203 & other2$subject!=1227 & other2$subject!=1257),]

# Check that all bad participants have been removed.
# There should be a total of 195 participants up to this point.
length(unique(other2$subject))

#Remove instruction trial rows
other2 <- subset(other2, trialcode != "testOthExtr" & trialcode != "testOthIntr")
                   
#Separate the scale ratings into a separate file and save.
otherScale <- subset(other2, trialcode == "scaleSamOv" | trialcode == "scaleWaltOv")
View(otherScale)
save(otherScale, file="otherScale.rda")

#Remove the scale ratings data from the other2 data frame
other2 <- subset(other2, trialcode != "scaleSamOv" & trialcode != "scaleWaltOv")

#Separate the attention check into a separate file and save.
otherCheck <- subset(other2, trialcode == "attnCheckSam" | trialcode == "attnCheckWalt")
View(otherCheck)
save(otherCheck, file="otherCheck.rda")

#Remove the attention check data from the other2 data frame
other2 <- subset(other2, trialcode != "attnCheckSam" & trialcode != "attnCheckWalt")


#####--------OTHER TARGET TEST PHASE DATA CLEANING-----------------------------------------

#In the other2 data frame, add a variable to indicate the trial type.
other2$trialType <- ifelse(other2$trialcode == "P1sam1v2" | 
                                 other2$trialcode == "P1sam3v4" |
                                 other2$trialcode == "P1sam5v6" | 
                                 other2$trialcode == "P1sam7v8" | 
                                 other2$trialcode == "P1sam9v10" | 
                                 other2$trialcode == "P1sam11v12", "P1sam",
                               
                               ifelse(other2$trialcode == "P2sam1v2" | 
                                other2$trialcode == "P2sam3v4" |
                                other2$trialcode == "P2sam5v6" | 
                                other2$trialcode == "P2sam7v8" | 
                                other2$trialcode == "P2sam9v10" | 
                                other2$trialcode == "P2sam11v12", "P2sam",
                                      
                              ifelse(other2$trialcode == "sharedsam1v2" | 
                                other2$trialcode == "sharedsam3v4" |
                                other2$trialcode == "sharedsam5v6" | 
                                other2$trialcode == "sharedsam7v8" | 
                                other2$trialcode == "sharedsam9v10" | 
                                other2$trialcode == "sharedsam11v12", "sharedsam",
                                             
                              ifelse(other2$trialcode == "mix1sam1v2" | 
                                other2$trialcode == "mix1sam3v4" |
                                other2$trialcode == "mix1sam5v6" | 
                                other2$trialcode == "mix1sam7v8" | 
                                other2$trialcode == "mix1sam9v10" | 
                                other2$trialcode == "mix1sam11v12" | 
                                other2$trialcode == "mix2sam1v2" | 
                                other2$trialcode == "mix2sam3v4" |
                                other2$trialcode == "mix2sam5v6" | 
                                other2$trialcode == "mix2sam7v8" | 
                                other2$trialcode == "mix2sam9v10" | 
                                other2$trialcode == "mix2sam11v12", "mixedsam",
                                                    
                              ifelse(other2$trialcode == "nov1exFsam" | 
                                other2$trialcode == "nov2exFsam" |
                                other2$trialcode == "nov3exFsam" | 
                                other2$trialcode == "nov4exFsam" |
                                other2$trialcode == "nov1inFsam" |
                                other2$trialcode == "nov2inFsam" | 
                                other2$trialcode == "nov3inFsam" |
                                other2$trialcode == "nov4inFsam", "novelsam",
                                 
                              ifelse(other2$trialcode == "P1walt1v2" | 
                                other2$trialcode == "P1walt3v4" |
                                other2$trialcode == "P1walt5v6" | 
                                other2$trialcode == "P1walt7v8" | 
                                other2$trialcode == "P1walt9v10" | 
                                other2$trialcode == "P1walt11v12", "P1walt",
                                       
                              ifelse(other2$trialcode == "P2walt1v2" | 
                                other2$trialcode == "P2walt3v4" |
                                other2$trialcode == "P2walt5v6" | 
                                other2$trialcode == "P2walt7v8" | 
                                other2$trialcode == "P2walt9v10" | 
                                other2$trialcode == "P2walt11v12", "P2walt",
                                              
                              ifelse(other2$trialcode == "sharedwalt1v2" | 
                                other2$trialcode == "sharedwalt3v4" |
                                other2$trialcode == "sharedwalt5v6" | 
                                other2$trialcode == "sharedwalt7v8" | 
                                other2$trialcode == "sharedwalt9v10" | 
                                other2$trialcode == "sharedwalt11v12", "sharedwalt",
                                                     
                              ifelse(other2$trialcode == "mix1walt1v2" | 
                                other2$trialcode == "mix1walt3v4" |
                                other2$trialcode == "mix1walt5v6" | 
                                other2$trialcode == "mix1walt7v8" | 
                                other2$trialcode == "mix1walt9v10" | 
                                other2$trialcode == "mix1walt11v12" | 
                                other2$trialcode == "mix2walt1v2" | 
                                other2$trialcode == "mix2walt3v4" |
                                other2$trialcode == "mix2walt5v6" | 
                                other2$trialcode == "mix2walt7v8" | 
                                other2$trialcode == "mix2walt9v10" | 
                                other2$trialcode == "mix2walt11v12", "mixedwalt",
                                                            
                              ifelse(other2$trialcode == "nov1exFwalt" | 
                                other2$trialcode == "nov2exFwalt" |
                                other2$trialcode == "nov3exFwalt" | 
                                other2$trialcode == "nov4exFwalt" | 
                                other2$trialcode == "nov1inFwalt" |
                                other2$trialcode == "nov2inFwalt" | 
                                other2$trialcode == "nov3inFwalt" |
                                other2$trialcode == "nov4inFwalt", "novelwalt",
                                                           "null"))))))))))
                             
#Check that there are no null responses
library(plyr)
count(other2, 'trialType')

#Add a variable to indicate the target (ingroup, outgroup) of each trial.
other2$target <- ifelse(other2$stereotype == "extraverted" & 
                                   (other2$trialType == "P1sam" | 
                                      other2$trialType == "P2sam" |
                                      other2$trialType == "sharedsam" |
                                      other2$trialType == "novelsam" |
                                      other2$trialType == "mixedsam"), "ingroup",
                            
                            ifelse(other2$stereotype == "extraverted" & 
                                     (other2$trialType == "P1walt" | 
                                      other2$trialType == "P2walt" |
                                      other2$trialType == "sharedwalt" |
                                      other2$trialType == "novelwalt" |
                                      other2$trialType == "mixedwalt"), "outgroup",
                            
                            ifelse(other2$stereotype == "introverted" & 
                                      (other2$trialType == "P1walt" | 
                                        other2$trialType == "P2walt" |
                                        other2$trialType == "sharedwalt" |
                                        other2$trialType == "novelwalt" |
                                        other2$trialType == "mixedwalt"), "ingroup",
                                         
                            ifelse(other2$stereotype == "introverted" & 
                                        (other2$trialType == "P1sam" | 
                                         other2$trialType == "P2sam" |
                                         other2$trialType == "sharedsam" |
                                         other2$trialType == "novelsam" |
                                         other2$trialType == "mixedsam"), "outgroup",
                                               "null"))))

#Check that there are no null responses
count(other2, 'target')
                             
#Add a column to indicate the button response letter
other2$response2 <- ifelse(other2$response == 33, "F", "J")

#Add a column to indicate whether the response was the congruent trait or the incongruent trait
#(Note - the code can be shortened by calling the condition number rather than the stereotype and button response.)
other2$response3 <- ifelse(other2$stereotype == "extraverted" & 
                                  other2$button == "extraF" & 
                                  other2$response2 == "F", "cons", 
                                
                                ifelse(other2$stereotype == "extraverted" &
                                  other2$button == "extraF" &
                                  other2$response2 == "J", "incs",
                                       
                                ifelse(other2$stereotype == "extraverted" &
                                  other2$button == "introF" &
                                  other2$response2 == "F", "incs",
                                              
                                ifelse(other2$stereotype == "extraverted" &
                                  other2$button == "introF" &
                                  other2$response2 == "J", "cons",
                                                     
                                ifelse(other2$stereotype == "introverted" &
                                  other2$button == "extraF" &
                                  other2$response2 == "F", "incs",
                                                            
                                ifelse(other2$stereotype == "introverted" &
                                  other2$button == "extraF" &
                                  other2$response2 == "J", "cons",
                                                                   
                                ifelse(other2$stereotype == "introverted" &
                                  other2$button == "introF" &
                                  other2$response2 == "F", "cons",
                                                                          
                                ifelse(other2$stereotype == "introverted" &
                                  other2$button == "introF" &
                                  other2$response2 == "J", "incs",
                                                                                 
                               "null"))))))))

#Check that there are no null responses
count(other2, 'response3')

#Save the file
save(other2, file="other2.rda")


## SEPARATE THE OTHER-TARGET TEST PHASE DATA BY TRIAL TYPE ## 

#---ingroup target, P1 context
othIng_P1 <- subset(other2, (  (trialType == "P1sam" & target == "ingroup") |
                                    (trialType == "P1walt" & target == "ingroup")   )
                    )
head(othIng_P1)
save(othIng_P1, file = "othIng_P1.rda")         

#---ingroup target, P2 context
othIng_P2 <- subset(other2, (  (trialType == "P2walt" & target == "ingroup") |
                                      (trialType == "P2sam" & target == "ingroup")   )
                    )
head(othIng_P2)
save(othIng_P2, file = "othIng_P2.rda")     

#---ingroup target, shared context
othIng_shared <- subset(other2, (  (trialType == "sharedsam" & target == "ingroup") |
                                      (trialType == "sharedwalt" & target == "ingroup")   )
                        )
head(othIng_shared)
save(othIng_shared, file = "othIng_shared.rda")  

#---ingroup target, mixed context
othIng_mixed <- subset(other2, (  (trialType == "mixedsam" & target == "ingroup") |
                                          (trialType == "mixedwalt" & target == "ingroup")   )
                       )
head(othIng_mixed)
save(othIng_mixed, file = "othIng_mixed.rda")  

#---ingroup target, novel context
othIng_novel <- subset(other2, (  (trialType == "novelsam" & target == "ingroup") |
                                         (trialType == "novelwalt" & target == "ingroup")   )
)
head(othIng_novel)
save(othIng_novel, file = "othIng_novel.rda")  

#---outgroup target, P1 context
othOut_P1 <- subset(other2, (  (trialType == "P1sam" & target == "outgroup") |
                                      (trialType == "P1walt" & target == "outgroup")   )
)
head(othOut_P1)
save(othOut_P1, file = "othOut_P1.rda")         

#---outgroup target, P2 context
othOut_P2 <- subset(other2, (  (trialType == "P2walt" & target == "outgroup") |
                                      (trialType == "P2sam" & target == "outgroup")   )
)
head(othOut_P2)
save(othOut_P2, file = "othOut_P2.rda")     

#---outgroup target, shared context
othOut_shared <- subset(other2, (  (trialType == "sharedsam" & target == "outgroup") |
                                          (trialType == "sharedwalt" & target == "outgroup")   )
)
head(othOut_shared)
save(othOut_shared, file = "othOut_shared.rda")  

#---outgroup target, mixed context
othOut_mixed <- subset(other2, (  (trialType == "mixedsam" & target == "outgroup") |
                                         (trialType == "mixedwalt" & target == "outgroup")   )
)
head(othOut_mixed)
save(othOut_mixed, file = "othOut_mixed.rda")  

#---outgroup target, novel context
othOut_novel <- subset(other2, (  (trialType == "novelsam" & target == "outgroup") |
                                         (trialType == "novelwalt" & target == "outgroup")   )
)
head(othOut_novel)
save(othOut_novel, file = "othOut_novel.rda")  


### OTHER TARGET, INGROUP ANALYSIS ##


#--Ingroup Analysis, Main Comparisons--#

#Ingroup target, Consistent Context
View(othIng_P1)

OI_consCt <- table(othIng_P1$response3)   #Obtain counts of each response
OI_consProp <- prop.table(table(othIng_P1$response3))   #Obtain proportions
rbind(OI_consCt, OI_consProp)

OIcons <- c(345,45)               #Create vector of counts
exp <- c(.5,.5)
chisq.test(OIcons, p=exp)

sum(OIcons)       #To obtain n
cohenW(230.77, 390)      #Obtain effect size


#Ingroup target, Inconsistent Context
View(othIng_P2)

OI_incsCt <- table(othIng_P2$response3)   
OI_incsProp <- prop.table(table(othIng_P2$response3)) 
rbind(OI_incsCt, OI_incsProp)

OIincs <- c(169,221)               
exp <- c(.5,.5)
chisq.test(OIincs, p=exp)

sum(OIincs)     
cohenW(6.9333, 390)           


#Ingroup target, Shared Context
View(othIng_shared)

OI_shrCt <- table(othIng_shared$response3)   
OI_shrProp <- prop.table(table(othIng_shared$response3)) 
rbind(OI_shrCt, OI_shrProp)

OIshr <- c(529,251)               
exp <- c(.5,.5)
chisq.test(OIshr, p=exp)

sum(OIshr)     
cohenW(99.082, 780)  


#Ingroup target, Mixed Context
View(othIng_mixed)
OI_mixCt <- table(othIng_mixed$response3)   
OI_mixProp <- prop.table(table(othIng_mixed$response3)) 
rbind(OI_mixCt, OI_mixProp)    

OImix <- c(510,270)               
exp <- c(.5,.5)
chisq.test(OImix, p=exp)

sum(OImix)     
cohenW(73.846, 780)  


#Ingroup target, Novel Context
View(othIng_novel)
OI_novCt <- table(othIng_novel$response3)   
OI_novProp <- prop.table(table(othIng_novel$response3)) 
rbind(OI_novCt, OI_novProp)         

OInov <- c(558,222)               
exp <- c(.5,.5)
chisq.test(OInov, p=exp)

sum(OInov)     
cohenW(144.74, 780) 


#--Ingroup Analysis, Stereotype Moderation--#

library(gmodels)


#Ingroup target, Consistent context
View(othIng_P1)
CrossTable(othIng_P1$stereotype, othIng_P1$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(6.325862, 390)


#Ingroup target, Inconsistent context
View(othIng_P2)
CrossTable(othIng_P2$stereotype, othIng_P2$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(4.214526, 390)


#Ingroup target, Shared context
View(othIng_shared)
CrossTable(othIng_shared$stereotype, othIng_shared$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(26.42864, 780)


#Ingroup target, Mixed context
View(othIng_mixed)
CrossTable(othIng_mixed$stereotype, othIng_mixed$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(0.85566, 780)


#Ingroup target, novel context
View(othIng_novel)
CrossTable(othIng_novel$stereotype, othIng_novel$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(8.822366, 780)



### OTHER TARGET, OUTGROUP ANALYSIS ##


#Outgroup target, Consistent Context
View(othOut_P1)

OO_consCt <- table(othOut_P1$response3) 
OO_consProp <- prop.table(table(othOut_P1$response3)) 
rbind(OO_consCt, OO_consProp)

OOcons <- c(178,212)               
exp <- c(.5,.5)
chisq.test(OOcons, p=exp)

sum(OOcons)       
cohenW(2.9641, 390)    


#Outroup target, Inconsistent Context
View(othOut_P2)

OO_incsCt <- table(othOut_P2$response3)   
OO_incsProp <- prop.table(table(othOut_P2$response3)) 
rbind(OO_incsCt, OO_incsProp)  

OOincs <- c(38,352)               
exp <- c(.5,.5)
chisq.test(OOincs, p=exp)

sum(OOincs)     
cohenW(252.81, 390)           


#Outroup target, Shared Context
View(othOut_shared)

OO_shrCt <- table(othOut_shared$response3)   
OO_shrProp <- prop.table(table(othOut_shared$response3)) 
rbind(OO_shrCt, OO_shrProp)     

OOshr <- c(192,588)               
exp <- c(.5,.5)
chisq.test(OOshr, p=exp)

sum(OOshr)     
cohenW(201.05,780)  


#Outroup target, Mixed Context
View(othOut_mixed)

OO_mixCt <- table(othOut_mixed$response3)   
OO_mixProp <- prop.table(table(othOut_mixed$response3)) 
rbind(OO_mixCt, OO_mixProp)   

OOmix <- c(214,566)               
exp <- c(.5,.5)
chisq.test(OOmix, p=exp)

sum(OOmix)     
cohenW(158.85, 780)  


#Outroup target, Novel Context
View(othOut_novel)

OO_novCt <- table(othOut_novel$response3)   
OO_novProp <- prop.table(table(othOut_novel$response3)) 
rbind(OO_novCt, OO_novProp)        

OOnov <- c(196,584)               
exp <- c(.5,.5)
chisq.test(OOnov, p=exp)

sum(OOnov)     
cohenW(193.01, 780)  



#--Outgroup Analysis, Stereotype Moderation--#

library(gmodels)


#Outgroup target, Consistent context
View(othOut_P1)
CrossTable(othOut_P1$stereotype, othOut_P1$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(7.406084,390)


#Outgroup target, Inconsistent context
View(othOut_P2)
CrossTable(othOut_P2$stereotype, othOut_P2$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(0.9522378, 390)


#Outgroup target, Shared context
View(othOut_shared)
CrossTable(othOut_shared$stereotype, othOut_shared$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(0.01477604, 780)


#Outgroup target, Mixed context
View(othOut_mixed)
CrossTable(othOut_mixed$stereotype, othOut_mixed$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(15.06257, 780)


#Outgroup target, novel context
View(othOut_novel)
CrossTable(othOut_novel$stereotype, othOut_novel$response3, fisher = T, chisq = T, expected = T, 
           prop.c = F, prop.t = F, sresid = T, format = "SPSS")

cohenW(0.8661411, 780)



## SCALE RATINGS OF OTHER TARGETS ##


View(otherScale)

#Add a column to indicate whether the target is the ingroup or the outgroup
otherScale$target <- ifelse(otherScale$stereotype == "extraverted" & otherScale$trialcode == "scaleSamOv", "ingroup",
                            ifelse(otherScale$stereotype == "extraverted" & otherScale$trialcode == "scaleWaltOv", "outgroup",
                            ifelse(otherScale$stereotype == "introverted" & otherScale$trialcode == "scaleWaltOv", "ingroup",
                            ifelse(otherScale$stereotype == "introverted" & otherScale$trialcode == "scaleSamOv", "outgroup",
                                  "null"))))
#Check for null responses
library(plyr)
count(otherScale$target)


#Check that each variable is the correct data type.
sapply(otherScale, class)

#Change condition, stereotype, people, button, and target to factor
otherScale$condition <- factor(otherScale$condition)
otherScale$stereotype <- factor(otherScale$stereotype)
otherScale$people <- factor(otherScale$people)
otherScale$button <- factor(otherScale$button)
otherScale$target <- factor(otherScale$target)

#Change response from factor to numeric
otherScale$response <- as.numeric(as.character(otherScale$response))


#Convert to wide format and export to a csv file

library(reshape2)

otherScaleW <- dcast(otherScale, subject + condition + stereotype + people + button
                     ~ trialcode, value.var = "response")

View(otherScaleW)

save(otherScaleW, file="otherScaleW.rda")
write.csv(otherScaleW, "otherScaleW.csv")



#Descriptive statistics of scale ratings (wide format data)

library(psych)

#Collapsed across stereotype conditions
describe(otherScaleW$scaleSamOv)
describe(otherScaleW$scaleWaltOv)

#Separated by stereotype condition 
describeBy(otherScaleW$scaleSamOv, otherScaleW$stereotype)
describeBy(otherScaleW$scaleWaltOv, otherScaleW$stereotype)



#Run a repeated measures t-test to compare the mean rating of Sam (extraverted) 
#to the mean rating of Walt (introverted) in the wide format data

ttest <- t.test(otherScaleW$scaleSamOv, otherScaleW$scaleWaltOv, paired = T)  
ttest

library(lsr)
sam <- otherScaleW$scaleSamOv
walt <- otherScaleW$scaleWaltOv
cohensD(sam, walt, method = "paired")


#Run an ANOVA to check for moderation of stereotype

library(ez)   #For the ANOVA
library(lsr)  #For the effect sizes

View(otherScale)    #use the long format data frame

sapply(otherScale, class) 

otherAOV <- ezANOVA(data = otherScale,    
                    dv = .(response), 
                    wid = .(subject), 
                    within = .(trialcode), 
                    between = .(stereotype), 
                    type = 3, 
                    detailed = TRUE,
                    return_aov = TRUE)

otherAOV


#Calculate partial eta squared effect sizes
#Partial eta squared = SS effect/(SS effect + SS error)
#eff = SS of effect
#err = SS of error
#function -> eta2p(eff,err)

View(otherScale)

#trialcode (Sam vs Walt)
eta2p(685.3564,599.1446)

#stereotype
eta2p(1.8646,221.8021)

#trialcode x stereotype
eta2p(15.9990,599.1446)


### See SPSS output for simple effects


### Create plot of scale ratings

#Bar graph of the scale ratings
ggplot(otherScale, aes(trialcode, response, fill = stereotype)) +
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge') +
  labs(x = 'Target', y = 'extraversion/introversion rating')


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


