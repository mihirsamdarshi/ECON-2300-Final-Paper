data <- read.csv("higher ed (30).csv", header=TRUE, sep=",")

happiness <- table(data$q1)
happiness
barplot(happiness, main="Overall happiness these days", names.arg=c("Very happy", "Pretty happy", "Not too happy"))

#hypothesis testing for one proportion and confidence interval 
prop.test(x=12,n=30,p=0.5,correct=F, conf.level=0.95, alternative="two.sided")


******************************************************************
q1 <- table(data$q1)
happiness <- as.data.frame(q1)
happiness_bar <- barplot(happiness$Freq, main="Overall happiness these days", names.arg=c("Very happy", "Pretty happy", "Not too happy"), ylim=c(0,16), col="lightblue", xlim=c(0,9), width=1, space=c(1,2,2))
text(happiness_bar, happiness$Freq + 0.4, happiness$Freq)
******************************************************************


payoff <-table(data$q9)
barplot(payoff, main="Has your education paid off?", names.arg=c("Yes", "No"))


useful <-table(data$q20)
barplot(useful, main="Usefullness of college education for career", names.arg=c("Very useful", "Somewhat useful", "Not at all", "Can?¡¥t say"))

*****************************************************************
install.packages("plotrix")
library(plotrix)
barp(useful, main="Usefullness of college education for career", names.arg=c("Very useful", "Somewhat useful", "Not at all", "Can?¡¥t say"),staxx=T)
******************************************************************

par(mfrow=c(2,2))

better1 <-table(data$q22a)
barplot(better1, main="Better prepared for job \nif choosing a different major", names.arg=c("Yes", "No"))

better2 <-table(data$q22b)
barplot(better2, main="Better prepared for job \nif gaining more work experience", names.arg=c("Yes", "No","Maybe"))

better3 <-table(data$q22c)
barplot(better3, main="Better prepared for job \nif starting to look for work sooner", names.arg=c("Yes", "No"))

better4 <-table(data$q22d)
barplot(better4, main="Better prepared for job \nif studying harder", names.arg=c("Yes", "No"))


*******************************************************
betterprep <- data[c("q22a", "q22b", "q22c", "q22d")]
betterprep <- apply(betterprep, 2, count)
better <- melt(betterprep, id.vars="x")

barplot(as.matrix(betterprep), beside=TRUE)

ggplot(better,aes(x=L1,y=value,fill=factor(x)))+ geom_bar(stat="identity",position="dodge")

*******************************************************



happiness_sex <- table(data$q1, data$sex)
barplot(happiness_sex, beside=T, main="Overall happiness these days", names.arg=c("Male", "Female"), legend.text=T, args.legend=list(x=8, y=-1,legend=c("Very happy", "Pretty happy", "Not too happy"),ncol=3)) 

prop.test(c(4,8),c(13,17),correct=F, conf.level=0.9, alternative="less")

happiness_marital <- table(data$q1, data$marital)
barplot(happiness_marital, beside=T, main="Overall happiness these days", names.arg=c("Married", "Living with a partner", "Divorced", "Widowed", "Never Married", "Don't know"),legend=c("Very happy", "Pretty happy", "Not too happy"))

data$partner <- ifelse(data$marital==1 | data$marital==2, 1, 0)
#create a new variable as partner in the dataset
#partner = 1 when marital==1 or 2
#partner = 0 when marital== other value that is not 1 or 2
#"data$marital==1 | data$marital==2" is the condition, if it's true, we define partner as 1, if not true, partner as 0


happiness_partner <- table(data$q1, data$partner)
barplot(happiness_partner, beside=T, main="Overall happiness these days", names.arg=c("Partnered", "Not Partnered"),legend=c("Very happy", "Pretty happy", "Not too happy"))
prop.test(c(4,8),c(14,16),correct=F, conf.level=0.95, alternative="less")


mean(data$q30a, na.rm=T)
mean(data$q30a[which(data$sex==1)], na.rm=T)
mean(data$q30a[which(data$sex==2)], na.rm=T)


cor(data$q30a, data$age, use="complete.obs")

highpay_income <- lm(data$q30a ~ data$age, na.action=na.omit)


