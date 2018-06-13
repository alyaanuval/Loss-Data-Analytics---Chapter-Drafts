
#Here are the ``R'' Commands used to import the data and create important variables.

#  "R" Commands to Import Data
Fire2009 =read.csv("FireRisk2009.csv", header=TRUE)#, sep="\t")
#View(Fire2009)
Fire2009$LossRatio <- 100*Fire2009$Claim/Fire2009$Premium
Fire2009$NumClmPol <- Fire2009$NumClaim/Fire2009$NumPol
#summary(Fire2009)
#attach(Fire2009)

plot(Fire2009$Class,Fire2009$NumPol, xlab="Class", ylab="Number of Policies")  #  FEWER POLICIES WITH LARGE SUM INSURED

plot(Fire2009$NumPol,Fire2009$NumClaim, xlab="Number of Policies", ylab="Number of Claims") #  MORE CLAIMS WITH MORE POLICIES

Fire2009$NumClmPol <- Fire2009$NumClaim/Fire2009$NumPol
plot(Fire2009$Class,Fire2009$NumClmPol, xlab="Class", 
                    ylab="Number of Claims per Policy")          
#  MORE CLAIMS PER POLICIES AS SUM INSURED INCREASES

Fire2009$SumIns <- (Fire2009$SumFrom+Fire2009$SumTo)/2
plot(Fire2009$SumIns,Fire2009$NumClmPol,xlab="Sum Insured", 
      ylab="Average Number of Claims per Policy",xlim=c(0,50000)) 
#  MORE CLAIMS PER POLICIES AS SUM INSURED INCREASES

plot(log(Fire2009$SumIns),Fire2009$NumClmPol,xlab="Logarithmic Sum Insured",
                                             ylab="Average Number of Claims per Policy")  
#  MORE CLAIMS PER POLICIES AS SUM INSURED INCREASES

Fire2009$PremPol <- Fire2009$Premium/Fire2009$NumPol
plot(Fire2009$Class,Fire2009$PremPol, ylab="Premium per Policy",
                                      xlab="Class")    # PREMIUM PER POLICY ALSO INCREASES AS SUM INSURED INCREASES

Fire2009$NumClmPol <- Fire2009$NumClaim/Fire2009$NumPol
plot(Fire2009$PremPol,Fire2009$NumClmPol, xlab="Premium per Policy", ylab="Number of Claims per Policy")  
          # INTERESTING THAT THE NUMBER OF CLAIMS PER POLICY INCREASES
           # AS THE PREMIUM PER POLICY INCREASES

#  CLAIMS NUMBER MODELS
#  MODEL 1 - IGNORE SUM INSURED, FIT NUMBER OF CLAIMS USING ONLY NUMBER OF POLICIES.
(ModFreq.1.Estimate <- sum(Fire2009$NumClaim)/sum(Fire2009$NumPol))

ModFreq.1 <- glm(NumClaim ~ 1, offset=log(NumPol),poisson(link=log), data=Fire2009)
summary(ModFreq.1)
exp(ModFreq.1$coefficients) # SAME AS THE MEAN

ModFreq.1.Summary <- cbind (Fire2009$NumClaim,
   ModFreq.1.Estimate*Fire2009$NumPol)
   ModFreq.1.Summary  #  THIS IS A POOR FITTING MODEL
(SM.ModFreq.1 <- sum((Fire2009$NumClaim - ModFreq.1.Estimate*Fire2009$NumPol)^2/(ModFreq.1.Estimate*Fire2009$NumPol)))

#  MAKE THESE STATISTICS ROUTINE TO SAVE WORK
ModelSummary1 <- function(ModEstimate){
   ModFreq.Summary <- cbind (Fire2009$NumClaim,ModEstimate)  
   ModFreq.Summary } 
   ModelSummary2 <- function(ModEstimate){sum((Fire2009$NumClaim - ModEstimate)^2/(ModEstimate))}

ModelSummary1( ModFreq.1.Estimate*Fire2009$NumPol);ModelSummary2( ModFreq.1.Estimate*Fire2009$NumPol)

#  MODEL 1A - FIT NUMBER OF CLAIMS USING NUMBER OF POLICIES AS AN EXPLANATORY VARIABLE IN A POISSON REGRESSION
ModFreq.1A <- glm(NumClaim ~ log(NumPol),poisson(link=log), data=Fire2009)
summary(ModFreq.1A)
ModelSummary1(ModFreq.1A$fitted.values);ModelSummary2(ModFreq.1A$fitted.values) #  NOT GREAT BUT BETTER THAN MODEL 1

#  MODEL 2 - INCLUDE CLASS AND log(NumPol) AS EXPLANATORY VARIABLEs IN A POISSON REGRESSION
ModFreq.2 <- glm(NumClaim ~ log(SumIns)+log(NumPol),poisson(link=log), data=Fire2009)
ModelSummary2(ModFreq.2$fitted.values) #  NOT GREAT BUT BETTER THAN MODEL 1

#  MODEL 2 - INCLUDE CLASS AND log(NumPol) AS EXPLANATORY VARIABLEs IN A POISSON REGRESSION
ModFreq.2A <- glm(NumClaim ~ Class+log(NumPol),poisson(link=log), data=Fire2009)
summary(ModFreq.2A)
ModelSummary2(ModFreq.2A$fitted.values) #  NOT GREAT BUT BETTER THAN MODEL 1

#  MODEL 2B - INCLUDE CLASS AS AN EXPLANATORY VARIABLE, NUMPOL AS AN OFFSET, IN A POISSON REGRESSION
ModFreq.2B <- glm(NumClaim ~ Class, offset=log(NumPol),poisson(link=log), data=Fire2009)
summary(ModFreq.2B)

ModelSummary1(ModFreq.2B$fitted.values);ModelSummary2(ModFreq.2B$fitted.values) #  THIS IS THE BEST
#  AS EITHER THE SUM INSURED OR THE NUMBER OF POLICIES INCREASE, THE EXPECTED NUMBER OF CLAIMS INCREASE

#  A FEW OTHER MODELS TRIED BUT NOT ADOPTED
ModFreq.3 <- glm(NumClaim ~ log(SumIns),offset=log(NumPol),poisson(link=log), data=Fire2009)
ModelSummary2(ModFreq.3$fitted.values)
ModFreq.4 <- glm(NumClaim ~ log(SumIns)+log(PremPol),offset=log(NumPol),poisson(link=log), data=Fire2009)
ModelSummary2(ModFreq.4$fitted.values)
ModFreq.5 <- glm(NumClaim ~ Class+log(PremPol),offset=log(NumPol),poisson(link=log), data=Fire2009)
ModelSummary2(ModFreq.5$fitted.values)

plot(Fire2009$NumPol, Fire2009$Claim,xlab="Number of Policies",ylab="Claim")  # RELATIONSHIP BETWEEN TOTAL CLAIMS AND NUMBER OF POLICIES NOT CLEAR

#Fire2009$SumIns <- (Fire2009$SumFrom+Fire2009$SumTo)/2
#plot(Fire2009$SumIns,Fire2009$Claim,xlab="Sum Insured",ylab="Claim") #  SIZE OF POLICY
#plot(Fire2009$Premium,Fire2009$Claim,xlab="Premium",ylab="Claim")     # SAME WITH PREMIUMS
plot(Fire2009$NumClaim, Fire2009$Claim,xlab="Number of Claims",ylab="Claim")  

Fire2009$AvgClaim <- Fire2009$Claim/Fire2009$NumClaim
hist(Fire2009$AvgClaim, main="", xlab="Distribution of Average Claim")

plot(density(Fire2009$AvgClaim), main="", xlab="Average Claims")#Gaussian kernel

#  CLAIM SEVERITY BY SUM INSURED
Fire2009$SumIns <- (Fire2009$SumFrom+Fire2009$SumTo)/2     
plot(Fire2009$SumIns,Fire2009$AvgClaim,xlab="Sum Insured",ylab="Average Claim",xlim=c(0,80000))   

#  CLAIM SEVERITY BY SUM INSURED     
plot(log(Fire2009$SumIns),Fire2009$AvgClaim,xlab="Logarithmic Sum Insured",ylab="Average Claim")

Fire2009$PremPol <- Fire2009$Premium/Fire2009$NumPol
plot(Fire2009$PremPol,Fire2009$AvgClaim,xlab="Premium",ylab="Average Claim",xlim=c(0,20))     

Fire2009$ClaimPrem <- Fire2009$Claim/Fire2009$Premium
plot(density(Fire2009$ClaimPrem), main="", xlab="Claims Per Premium")

plot(log(Fire2009$SumIns),Fire2009$ClaimPrem, xlab="Class", ylab="Average Claims Per Premium")         # CLAIMS PER PREMIUM ARE HIGH FOR LOW SUM INSURED AND HIGH SUM INSURED ("u SHAPE"???)

Fire2009$Claim[19]/Fire2009$NumClaim[19] #  average claim for band 19
sum(Fire2009$Claim)/sum(Fire2009$NumClaim) #  average claim

summary(lm(AvgClaim ~ log(SumIns),data=Fire2009)) 

Fire2009small <- subset(Fire2009,Class<20)     
summary(lm(AvgClaim ~ log(SumIns),data=Fire2009small))

summary(lm(AvgClaim ~ log(SumIns)+PremPol,data=Fire2009small))

GLM.model <- glm(AvgClaim ~ log(SumIns)+PremPol, data=Fire2009,    
      control = glm.control(maxit = 50), 
    family=Gamma(link="log"))      
summary(GLM.model)

cor(ModFreq.2B$fitted.values,Fire2009$NumClaim, method="spearman")      
plot (ModFreq.2B$fitted.values,Fire2009$NumClaim,
      xlab="Frequency Model Fits",ylab="Number of Claims")
abline(0,1)

cor(GLM.model$fitted.values,Fire2009$AvgClaim, method="spearman")      
plot(GLM.model$fitted.values,Fire2009$AvgClaim,xlab="Fitted Values from the GLM Model",
                                               ylab="Average Claim")
abline(0,1)

FinalModelFit <- ModSev.2A$fitted.values*ModFreq.2B$fitted.values
plot (FinalModelFit,Fire2009$Claim,
     xlab="Final Models Fit",ylab="Claim",xlim=c(0,150000))
abline(0,1)
cor(FinalModelFit,Fire2009$Claim, method="spearman")


Sweave("FireSweave.Rnw")