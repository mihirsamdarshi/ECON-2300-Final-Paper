data <- read.csv("american_jobs_data.csv", header=TRUE, sep=",")

familyLifeSatisfaction <- table(data$q2a)
familyLifeSatisfaction

financialLifeSatisfaction <- table(data$q2b)
workKindSatisfaction <- table(data$q2ef2)

jobAvailability <- table(data$q6f2)

contractWorkerIncrease <- table(data$q7ef2)
outsourcingIncrease <- table(data$q7ff2)
techUsageIncrease <- table(data$q7gf2)
usExportIncrease <- table(data$q7hf2)

governmentResponsibility <- table(data$q8df2)
employerResponsibility <- table(data$q8ef2)
collegeResponsibility <- table(data$q8ff2)

fourYearPreparation <- table(data$q11fa2)
twoYearPreparation <- table(data$q12fb2)
profSchoolPreparation <- table(data$q13fc)

politicalParty <- table(data$partyln)
ideology <- table(data$ideo)

sex <- table(data$sex)

