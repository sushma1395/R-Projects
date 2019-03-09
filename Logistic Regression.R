install.packages("xlsx")

library(readxl)

loan <- read_xlsx("C:/Users/gadep/OneDrive/Desktop/ADS/loan.xlsx" , 1)

model <- glm(as.factor(Decision) ~ as.factor(Res_status) + as.factor(Occupation) + as.factor(Job_status) + as.factor(Liab_ref)+ as.factor(Acc_ref), data=loan, 
             family="binomial")

class(as.factor(loan$Res_status))
class(loan$Occupation)

summary(model)

newdata = data.frame(Res_status = "owner"  , Occupation = "creative_",Job_status= "governmen",Liab_ref= "f",Acc_ref= "given" )
logistics = data.frame(Res_status = "rent",Occupation = "creative_" ,Job_status="governmen" , Liab_ref ="f", Acc_ref= "given")
#Predicting the value for input (owner, creative_, governmen, f, given) and (rent, creative_, governmen, f, given) in Loan dataset.
predict(model, newdata, type="response")

predict(model, logistics, type="response")

