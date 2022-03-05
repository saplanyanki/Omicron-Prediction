###FEATURE ENGINEERING

####TRAINING
#Feature Engineering Days23 Interval
train.new$days_dose23interval <- (train.new$days_sinceDose3 - train.new$days_sinceDose2)

#Feature Engineering Dose 1 Column
train.new$days_sinceDose1 <- (train.new$days_sinceDose2 + train.new$days_dose12interval)
train.new = add_column(train.new, days_sinceDose1 = train.new$days_sinceDose1, .before = "days_sinceDose2")
train.new = subset(train.new, select = -c(days_sinceDose1) )

#Feature Engineering for invisible cells
train.new$priorSxAtFirstVisitSeverity[train.new$priorSxAtFirstVisitSeverity==""]<-NA
train.new$priorSxAtFirstVisitSeverity[is.na(train.new$priorSxAtFirstVisitSeverity)] = "None"

#Feature Engineering for 3rd dose
train.new$dose_3[train.new$dose_3==""]<-NA
train.new$dose_3[is.na(train.new$dose_3)] = "None"

#Replacements
train.new$days_dose23interval[is.na(train.new$days_dose23interval)] = 0
train.new$days_sinceDose3[is.na(train.new$days_sinceDose3)] = 0

#If no pos test recorded, then 0
train.new$days_sincePosTest_latest[is.na(train.new$days_sincePosTest_latest)] = 0
train.new$Sx_severity_most_recent[is.na(train.new$Sx_severity_most_recent)] <- mean(train.new$Sx_severity_most_recent, na.rm = TRUE)
train.new$days_sinceSxLatest[is.na(train.new$days_sinceSxLatest)] <- mean(train.new$days_sinceSxLatest, na.rm = TRUE)

#Feature Engineering 3 - Get rid of data where people said they did not have any symptoms and reported a severity in symptoms
train.new$priorSxAtFirstVisitSeverity[train.new$priorSxAtFirstVisit=="N" & train.new$priorSxAtFirstVisitSeverity!='None'] = "None"
#Feature Engineering 3 - Get rid of data where people said they had symptoms and reported none severity in symptoms
train.new$priorSxAtFirstVisitSeverity[train.new$priorSxAtFirstVisit=="Y" & train.new$priorSxAtFirstVisitSeverity=='None'] = "Mild"

#One Hot Encoding
dummy <- dummyVars(" ~ .", data=train.new)
newdata <- data.frame(predict(dummy, newdata = train.new))

#subset low relation columns
newdata = subset(newdata, select = -c(centreuclh,dose_3BNT162b2,priorSxAtFirstVisit,priorSxAtFirstVisitN,priorSxAtFirstVisitY,
                                        priorSxAtFirstVisitSeverityMild,priorSxAtFirstVisitSeveritySevere,days_dose12interval,posTest_beforeVisitYes,
                                      CovVar_1,CovVar_3,CovVar_4,CovVar_6,CovVar_7,CovVar_8,CovVar_9,
                                        CovVar_11,CovVar_14,CovVar_15,CovVar_16,CovVar_18,CovVar_19,CovVar_20,CovVar_21,CovVar_22,CovVar_23,
                                        CovVar_24,CovVar_25,CovVar_26,CovVar_27,CovVar_28,CovVar_29,CovVar_30,CovVar_31,CovVar_32,CovVar_36,CovVar_37,
                                        CovVar_39,CovVar_40,CovVar_41,CovVar_48,CovVar_49,CovVar_50,CovVar_51,CovVar_52,CovVar_55,CovVar_56,CovVar_57,
                                        CovVar_58,CovVar_60,CovVar_61,CovVar_62,CovVar_63,CovVar_65,CovVar_66,CovVar_67,CovVar_68,CovVar_69,
                                        CovVar_72,CovVar_73,CovVar_75,CovVar_76,CovVar_78,CovVar_80,CovVar_81,CovVar_82,CovVar_84,CovVar_85,
                                        CovVar_87,CovVar_88,CovVar_89,CovVar_90,CovVar_92,CovVar_93,CovVar_94,CovVar_96,CovVar_98,CovVar_99))

#New Column - Dose 3 + Dose 1
newdata$days_3and1 <- (newdata$days_sinceDose3 + newdata$days_sinceDose1.1)
newdata$days_2and1 <- (newdata$days_sinceDose2 + newdata$days_sinceDose1.1)
newdata$days_3and2 <- (newdata$days_sinceDose3 + newdata$days_sinceDose2)

#New Column for symptoms
newdata$pos_sympt_no_cov <- (newdata$days_sinceSxLatest + newdata$days_sincePosTest_latest)
newdata$sympt_and3 <- (newdata$days_sinceDose3 + newdata$days_sinceSxLatest)
newdata$sympt_and2 <- (newdata$days_sinceDose2 + newdata$days_sinceSxLatest)
newdata$sympt_and1 <- (newdata$days_sinceDose1.1 + newdata$days_sinceSxLatest)

#Positive Cov - Dynamic Negative Dates
newdata$neg_dynamic_dates <- ifelse(
   ( 
     (newdata$days_sincePosTest_latest < 0) 
   ),
   abs(newdata$days_sincePosTest_latest),  # if condition is met
   0   # else put 0
 )

#Day negative tests - Meaning there is no positive test consecutively
newdata$days_sinceNegTest_latest <- ifelse(
  ( 
    (newdata$days_sincePosTest_latest < 0) 
  ),
  (min(newdata$days_sincePosTest_latest) + newdata$neg_dynamic_dates), # if condition is met
  abs(newdata$days_sincePosTest_latest - 1)  # else put 0
)

newdata$int3and1_negtest <- abs(newdata$days_3and1 - newdata$days_sinceNegTest_latest)


#Correlation
relations <- cor(newdata[-1], newdata$ic50_Omicron) 
relations <- as.data.frame(relations)

####TEST

#Feature Engineering Days23 Interval
test.new$days_dose23interval <- (test.new$days_sinceDose3 - test.new$days_sinceDose2)

#Feature Engineering Dose 1 Column
test.new$days_sinceDose1 <- (test.new$days_sinceDose2 + test.new$days_dose12interval)
test.new = add_column(test.new, days_sinceDose1 = test.new$days_sinceDose1, .before = "days_sinceDose2")
test.new = subset(test.new, select = -c(days_sinceDose1) )

#Feature Engineering for invisible cells
test.new$priorSxAtFirstVisitSeverity[test.new$priorSxAtFirstVisitSeverity==""]<-NA
test.new$priorSxAtFirstVisitSeverity[is.na(test.new$priorSxAtFirstVisitSeverity)] = "None"

#Feature Engineering for 3rd dose
test.new$dose_3[test.new$dose_3==""]<-NA
test.new$dose_3[is.na(test.new$dose_3)] = "None"

#Replacements
test.new$days_dose23interval[is.na(test.new$days_dose23interval)] = 0
test.new$days_sinceDose3[is.na(test.new$days_sinceDose3)] = 0

#If no pos test recorded, then 0
test.new$days_sincePosTest_latest[is.na(test.new$days_sincePosTest_latest)] = 0
test.new$Sx_severity_most_recent[is.na(test.new$Sx_severity_most_recent)] <- mean(test.new$Sx_severity_most_recent, na.rm = TRUE)
test.new$days_sinceSxLatest[is.na(test.new$days_sinceSxLatest)] <- mean(test.new$days_sinceSxLatest, na.rm = TRUE)

#Feature Engineering 3 - Get rid of data where people said they did not have any symptoms and reported a severity in symptoms
test.new$priorSxAtFirstVisitSeverity[test.new$priorSxAtFirstVisit=="N" & test.new$priorSxAtFirstVisitSeverity!='None'] = "None"
#Feature Engineering 3 - Get rid of data where people said they had symptoms and reported none severity in symptoms
test.new$priorSxAtFirstVisitSeverity[test.new$priorSxAtFirstVisit=="Y" & test.new$priorSxAtFirstVisitSeverity=='None'] = "Mild"


#Change test data to our needs before prediction + adds columns from encoding
dummy <- dummyVars(" ~ .", data=test.new)
newdata.test <- data.frame(predict(dummy, newdata = test.new))

#subset low relation columns
newdata.test = subset(newdata.test, select = -c(centreuclh,dose_3BNT162b2,priorSxAtFirstVisit,priorSxAtFirstVisitN,priorSxAtFirstVisitY,
                                      priorSxAtFirstVisitSeverityMild,priorSxAtFirstVisitSeveritySevere,days_dose12interval,posTest_beforeVisitYes,
                                      CovVar_1,CovVar_3,CovVar_4,CovVar_6,CovVar_7,CovVar_8,CovVar_9,
                                      CovVar_11,CovVar_14,CovVar_15,CovVar_16,CovVar_18,CovVar_19,CovVar_20,CovVar_21,CovVar_22,CovVar_23,
                                      CovVar_24,CovVar_25,CovVar_26,CovVar_27,CovVar_28,CovVar_29,CovVar_30,CovVar_31,CovVar_32,CovVar_36,CovVar_37,
                                      CovVar_39,CovVar_40,CovVar_41,CovVar_48,CovVar_49,CovVar_50,CovVar_51,CovVar_52,CovVar_55,CovVar_56,CovVar_57,
                                      CovVar_58,CovVar_60,CovVar_61,CovVar_62,CovVar_63,CovVar_65,CovVar_66,CovVar_67,CovVar_68,CovVar_69,
                                      CovVar_72,CovVar_73,CovVar_75,CovVar_76,CovVar_78,CovVar_80,CovVar_81,CovVar_82,CovVar_84,CovVar_85,
                                      CovVar_87,CovVar_88,CovVar_89,CovVar_90,CovVar_92,CovVar_93,CovVar_94,CovVar_96,CovVar_98,CovVar_99))

#New Column - Dose 3 + Dose 1 TEST
newdata.test$days_3and1 <- (newdata.test$days_sinceDose3 + newdata.test$days_sinceDose1.1)
newdata.test$days_2and1 <- (newdata.test$days_sinceDose2 + newdata.test$days_sinceDose1.1)
newdata.test$days_3and2 <- (newdata.test$days_sinceDose3 + newdata.test$days_sinceDose2)

#New Column for symptoms
newdata.test$pos_sympt_no_cov <- (newdata.test$days_sinceSxLatest - newdata.test$days_sincePosTest_latest)
newdata.test$sympt_and3 <- (newdata.test$days_sinceDose3 + newdata.test$days_sinceSxLatest)
newdata.test$sympt_and2 <- (newdata.test$days_sinceDose2 + newdata.test$days_sinceSxLatest)
newdata.test$sympt_and1 <- (newdata.test$days_sinceDose1.1 + newdata.test$days_sinceSxLatest)

#Positive Cov - Dynamic Negative Dates
newdata.test$neg_dynamic_dates <- ifelse(
  ( 
    (newdata.test$days_sincePosTest_latest < 0) 
  ),
  abs(newdata.test$days_sincePosTest_latest),  # if condition is met
  0   # else put 0
)

#Day negative tests - Meaning there is no positive test consecutively
newdata.test$days_sinceNegTest_latest <- ifelse(
  ( 
    (newdata.test$days_sincePosTest_latest < 0) 
  ),
  (min(newdata.test$days_sincePosTest_latest) + newdata.test$neg_dynamic_dates), # if condition is met
  abs(newdata.test$days_sincePosTest_latest - 1)  # else put 0
)

newdata.test$int3and1_negtest <- abs(newdata.test$days_3and1 - newdata.test$days_sinceNegTest_latest)

