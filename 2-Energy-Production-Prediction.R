library(tidyverse)
library(caret)
library(ggplot2)

production <- read.csv("D:\\UM\\Sem 4\\WIH2001 - Data Analytics\\Group Project\\Electricity_Production_By_Source.csv")
production <- subset(production, production$Entity == "World")

#summary
summary(production)

#Plotting the graph
ggplot(production, aes(x=Year, y = Total.Energy))+
  geom_point()+
  labs( 
      x ="Year", 
      y = "Total Enerrgy (TWh)",
      title="World Energy Production",
      subtitle ="1985 - 2020"
      )

#Creating the linear regression model
model <- lm(Total.Energy ~ Year, production)

#Linear model formula: Y = coefficient + (slope * X)
summary(model)$coefficients
#Linear model formula: Total.Energy = -930907.6902 + (473.4178 * Year)

#Plot the linear regression model
ggplot(production, aes(x=Year, y = Total.Energy))+
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs( 
    x ="Year", 
    y = "Total Enerrgy (TWh)",
    title="World Energy Production",
    subtitle ="1985 - 2020"  )
  

#Predict for Year 2030
predict(model, data.frame(Year = 2030))

#Plot the prediction on the graph
ggplot(production, aes(x=Year, y = Total.Energy))+
  geom_point() +
  geom_abline(slope = 473.4178, intercept = -930907.6902 ) + 
  geom_vline(xintercept = 2030) + 
  geom_hline(yintercept = predict(model, data.frame(Year = 2030)) ) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs( 
    x ="Year", 
    y = "Total Enerrgy (TWh)",
    title="World Energy Production",
    subtitle ="Prediction for Year 2030"  )

#Evaluate the model
summary(model)