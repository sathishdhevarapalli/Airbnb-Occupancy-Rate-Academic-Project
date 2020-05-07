
data_set = read.csv ("C:\\Users\\Sathish Reddy\\Desktop\\Final QMB project\\Data_with_out-dummies.csv")
str(data_set)
hist(data_set$Occupancy_Rate)
hist(log(data_set$Occupancy_Rate))
names(data_set)
dim(data_set)

lm2 = lm (log(data_set$Occupancy_Rate)~Accommodates+Guests_included+Bathrooms+
            Bedrooms+Beds+Price+Extra_people+Minimum_nights+
            Availability_365+Tv+Wifi+Air_conditioning
          +Free_parking_on_premises+Heating+Room_type+
            Host_is_superhost+Price*Minimum_nights*Room_type,data = data_set)



hist(model_1$res)
plot(model_1$residuals)

install.packages("stargazer")
library("stargazer")

stargazer(model_1,model_2,model_3,model_4,type = "text")

