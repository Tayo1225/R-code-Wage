# R-code-Wage
R code on wage and it's determinants
load("C:/Users/Tayo/Downloads/NLSY.Rdata")
NLSY1 = NLSY[NLSY$laborinc07>0,]
NLSY1$loghourlyWage= log((NLSY1$laborinc07/NLSY1$hours07))
NLSY1$potentialExperience= ((NLSY1$age79 + 36 )-NLSY1$educ-5)
NLSY_subset = NLSY1[NLSY1$hours07>=1750,]
NLSY_subset2 = NLSY_subset[!is.na(NLSY_subset$hours07)& !is.na(NLSY_subset$educ) & !is.infinite(NLSY_subset$loghourlyWage),]
summary(NLSY_subset2)
lm(NLSY_subset2$loghourlyWage~NLSY_subset2$educ)
cor(NLSY_subset2$educ, NLSY_subset2$loghourlyWage)
hourlywageReg = lm(NLSY_subset2$loghourlyWage~NLSY_subset2$educ)
summary(hourlywageReg)
lm(NLSY_subset2$loghourlyWage~ NLSY_subset2$educ + NLSY_subset2$potentialExperience + (NLSY_subset2$potentialExperience^2))
MincWageEquation = lm(NLSY_subset2$loghourlyWage~ NLSY_subset2$educ + NLSY_subset2$potentialExperience + (NLSY_subset2$potentialExperience^2))
summary(MincWageEquation)
lm(NLSY_subset2$loghourlyWage~ NLSY_subset2$educ + NLSY_subset2$potentialExperience + (NLSY_subset2$potentialExperience^2) + NLSY_subset2$black + NLSY_subset2$hisp + NLSY_subset2$male)
ExtMincWageEq =lm(NLSY_subset2$loghourlyWage~ NLSY_subset2$educ + NLSY_subset2$potentialExperience + (NLSY_subset2$potentialExperience^2) + NLSY_subset2$black + NLSY_subset2$hisp + NLSY_subset2$male)
summary(ExtMincWageEq)
cor(NLSY_subset2$educ, NLSY_subset2$potentialExperience)
cor(NLSY_subset2$educ, (NLSY_subset2$potentialExperience^2))
ExtMincWageEq2 =lm(NLSY_subset2$loghourlyWage~ NLSY_subset2$educ + NLSY_subset2$potentialExperience + (NLSY_subset2$potentialExperience^2) + NLSY_subset2$black + NLSY_subset2$hisp + NLSY_subset2$male + NLSY_subset2$educ_mom + NLSY_subset2$educ_dad + NLSY_subset2$numsibs + NLSY_subset2$afqt81)
summary(ExtMincWageEq2)
plot(NLSY_subset2$hours07,NLSY_subset2$laborinc07)
hist(NLSY_subset2$hours07)
