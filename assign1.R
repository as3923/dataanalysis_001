
setwd("/GitHub/dataanalysis_001")


## Download and load the loans data
setInternet2(T)
url <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv"
dest <- "raw/loansData.csv"
download.file(url, dest)
dateDownloaded <- date()
loans <- read.csv(dest)


## Libraries
library(Hmisc)
library(corrgram)


## Relevant variables (?)
logIncome <- log10(loans$Monthly.Income)
logBal <- log10(loans$Revolving.CREDIT.Balance)
loans$numLen <- as.numeric(sub(" months", "", loans$Loan.Length))
loans$numInt <- as.numeric(sub("%", "", loans$Interest.Rate))
loans$numFICO <- as.integer(sub("-",".",loans$FICO.Range))
loans$numDTI <- as.numeric(sub("%","",loans$Debt.To.Income.Ratio))
# IntCut <- cut2(loans$numInt, g = 5)

loana <- na.omit(loans)

# loanp <- loana[loana$Monthly.Income < 30000,]
# lFo <- loans[!(loans$Loan.Length=="60 months" & loans$numFICO < 700 & loans$numInt < 7),]


## Factor models
# lmPup <- lm(loans$numInt ~ loans$Loan.Purpose)
# lmFICO <- lm(loans$numInt ~ loans$FICO.Range + loans$Loan.Length)
# anova(lmFICO)
# 
# aPup <- aov(loans$numInt ~ loans$Loan.Purpose)
# aFICO <- aov(loans$numInt ~ loans$FICO.Range)
# anFICO <- aov(loans$numInt ~ loans$numFICO)
# par(mfrow=c(2,4))
# plot(aPup, col=loans$Loan.Length, pch=20)
# plot(aFICO, col=loans$Loan.Length, pch=20)
# plot(anFICO, col=loans$Loan.Length, pch=20)
# TukeyHSD(aFICO)


## Quant models
# lmnLen <- lm(loans$numInt ~ loans$numLen)
# lmInt <- lm(loans$numInt ~ loans$Amount.Requested + loans$Amount.Funded.By.Investors)
lm3FICO <- lm(loans$numInt[loans$Loan.Length=="36 months"] ~ loans$numFICO[loans$Loan.Length=="36 months"])
lm6FICO <- lm(loans$numInt[loans$Loan.Length=="60 months"] ~ loans$numFICO[loans$Loan.Length=="60 months"])

# lm6FICOo <- lm(lFo$numInt[lFo$Loan.Length=="36 months"] ~ lFo$numFICO[lFo$Loan.Length=="36 months"])
# lm6FICOo <- lm(lFo$numInt[lFo$Loan.Length=="60 months"] ~ lFo$numFICO[lFo$Loan.Length=="60 months"])
# lm3FICOa <- lm(loans$numInt[loans$Loan.Length=="36 months"] ~ loans$numFICO[loans$Loan.Length=="36 months"] + loans$Amount.Funded.By.Investors[loans$Loan.Length=="36 months"])
# lm6FICOa <- lm(loans$numInt[loans$Loan.Length=="60 months"] ~ loans$numFICO[loans$Loan.Length=="60 months"] + loans$Amount.Funded.By.Investors[loans$Loan.Length=="60 months"])
# lm3Final <- lm(loans$numInt[loans$Loan.Length=="36 months"] ~ loans$numFICO[loans$Loan.Length=="36 months"] + loans$Amount.Funded.By.Investors[loans$Loan.Length=="36 months"] + loans$Amount.Requested[loans$Loan.Length=="36 months"] + loans$numDTI[loans$Loan.Length=="36 months"])
# lm6Final <- lm(loans$numInt[loans$Loan.Length=="60 months"] ~ loans$numFICO[loans$Loan.Length=="60 months"] + loans$Amount.Funded.By.Investors[loans$Loan.Length=="60 months"] + loans$Amount.Requested[loans$Loan.Length=="60 months"] + loans$numDTI[loans$Loan.Length=="60 months"])


## Interesting graphs
# plot(logIncome, loans$Interest.Rate, pch=20, col=loans$Loan.Purpose)
# smoothScatter(logIncome, logBal)
# plot(logIncome, loans$Amount.Funded.By.Investors, pch=20, col=loans$Loan.Length)
# plot(loans$Loan.Length, loans$FICO.Range, pch=20, col=IntCut)
# smoothScatter(loans$numInt ~ as.numeric(loans$FICO.Range))

# par(mfrow=c(1,1))
# plot(jitter(loans$numInt) ~ jitter(as.numeric(loans$FICO.Range)), cex=1.4, col=loans$Loan.Length, pch=20, xaxt="n")
# axis(side=1, at=unique(as.numeric(loans$FICO.Range)), labels=unique(loans$FICO.Range))

## Multi value regression plot with fitted lines
par(mfrow=c(1,1), mar=c(5.6,4.1,4.1,2.1))
# plot(jitter(loans$numInt) ~ jitter(loans$FICO.Range), col=loans$Loan.Length, cex=1.4, pch=20, xaxt="n", xlab="", ylab="Interest Rate (%)", main="FICO Range vs. Interest Rate with Fitted Models")
plot(jitter(loans$numInt) ~ jitter(loans$numFICO), col=loans$Loan.Length, cex=1.4, pch=20, xaxt="n", xlab="", ylab="Interest Rate (%)", main="FICO Range vs. Interest Rate with Fitted Models based on Loan Length")
axis(side=1, las=2, at=unique(loans$numFICO), labels=unique(loans$FICO.Range))
mtext(text="FICO Range", side=1, line=4.2)
legend(785,25, legend=unique(loans$Loan.Length), col=c("black","red"), pch=c(20,20))
lines(loans$numFICO[loans$Loan.Length=="36 months"],lm3FICO$fitted,col="black",lwd=3)
lines(loans$numFICO[loans$Loan.Length=="60 months"],lm6FICO$fitted,col="red",lwd=3)

# lines(as.numeric(loans$numFICO[loans$Loan.Length=="36 months"]),lm3FICOa$fitted,col="black",lwd=3)
# lines(as.numeric(loans$numFICO[loans$Loan.Length=="60 months"]),lm6FICOa$fitted,col="red",lwd=3)



# plot(loans$numInt, loans$numFICO, xlab="Interest Rate (%)", cex=1.4, ylab="FICO Range", pch=20, col=loans$Loan.Length)
# legend(19,835, legend=unique(loans$Loan.Length), col=c("black","red"), pch=c(20,20))


loan00 <- data.frame(loans[,c(1,2,9,11:13,15:18)])
colnames(loan00) <- c("Requested", "Funded", "Income", "Open CREDIT", "Revolving CREDIT", "Inquiries", "Loan Length", "Interest Rate", "FICO", "DTI")
corrgram(loan00, order=T, lower.panel=panel.conf, upper.panel=panel.pie, main="Lending Club Loan Data in PCA Order", cex.labels=.8)


## Close correlations
# cor(data.frame(loana$Amount.Funded.By.Investors, loana$Amount.Requested))
# # 0.97
# cor(data.frame(loana$Amount.Requested, loana$Monthly.Income))
# # 0.39 
# cor(data.frame(loana$Amount.Funded.By.Investors, loana$Monthly.Income))
# # 0.37
# cor(data.frame(loana$Monthly.Income, loana$Revolving.CREDIT.Balance))
# # 0.36
# cor(data.frame(loana$numFICO, loana$numInt))
# # -0.71
# cor(data.frame(loana$numLen, loana$numInt))
# # 0.42
# cor(data.frame(loana$Amount.Funded.By.Investors, loana$numInt))
# # 0.34
# cor(data.frame(loana$Amount.Requested, loana$numInt))
# # 0.33


#############################################
##
##  7 NAs; 2 rows with NAs
##
##  Three outliers in Monthly.Income
##  loanp is data without Income outliers
##
##  Majority of loans is for debt_consolidation
##  Strong link between FICO, Loan Length, and Interest Rate
##
##  Some link between Interest Rate and Amount Funded
##  Slightly less link between Interest Rate and Amount Requested
##
