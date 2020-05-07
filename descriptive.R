
data_viz=read.csv("C:\\Users\\Sathish Reddy\\Desktop\\Final QMB project\\with_out-dummy-linear_regression_model.csv")
str(data_viz)

viz <- c("Accommodates","Guests_included","Bathrooms","Bedrooms","Beds","Price","Extra_people","Minimum_nights","Maximum_nights","Availability_365","Occupancy_Rate")
heat_cols <- data_viz[,viz]
View(heat_cols)
names(heat_cols)

# Heatmap
# install.packages("corrplot")
require(corrplot)
corelations=cor(heat_cols)
corrplot(corelations,order = "AOE",method = "color",addCoef.col = "gray")


# Violin Plots for room type
str(data_viz)
data_viz$Room_type = as.factor(data_viz$Room_type)
library(ggplot2)
# Basic violin plot
vio = ggplot(data_viz, aes(x=Room_type, y=Occupancy_Rate,color=Room_type)) + geom_violin()
vio
vio + geom_boxplot(width=0.1)
# Set trim argument to FALSE
dp = ggplot(data_viz, aes(x=Room_type, y=Occupancy_Rate, fill=Room_type)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="red")+
  labs(title="Plot of Occupancy_Rate  by Room_type",x="Room_type", y = "Occupancy_Rate")
dp + theme_classic()

# Violin plots based on host type
data_viz$Host_is_superhost = as.factor(data_viz$Host_is_superhost)
vio_host = ggplot(data_viz, aes(x=Host_is_superhost, y=Occupancy_Rate,color=Host_is_superhost)) + geom_violin()
vio_host
dp_host = ggplot(data_viz, aes(x=Host_is_superhost, y=Occupancy_Rate, fill=Host_is_superhost)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="red")+
  labs(title="Plot of Occupancy_Rate  by Host_type",x="Host_type", y = "Occupancy_Rate")
dp_host + theme_classic()


# Pair plots occupancy rate vs few columns
options(repr.plot.width=9, repr.plot.height=6)
p1 <- ggplot(heat_cols, aes(x=Price, y=Occupancy_Rate)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Scatter plot of Occupancy_Rate vs Price") +
  theme(plot.title = element_text(hjust = 0.4))

p2 <- ggplot(heat_cols, aes(x=Guests_included, y=Occupancy_Rate)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Scatter plot of Occupancy_Rate vs Guests_included") +
  theme(plot.title = element_text(hjust = 0.4))

p3 <- ggplot(heat_cols, aes(x=Extra_people, y=Occupancy_Rate)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Scatter plot of Occupancy_Rate vs Extra_people") +
  theme(plot.title = element_text(hjust = 0.4))

p4 <- ggplot(heat_cols, aes(x=Minimum_nights, y=Occupancy_Rate)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  ggtitle("Scatter plot of Occupancy_Rate vs Minimum_nights") +
  theme(plot.title = element_text(hjust = 0.4))

# install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2,p3,p4)