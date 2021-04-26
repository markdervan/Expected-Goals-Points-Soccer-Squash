library(ggplot2)
library(DynNom)
library(dplyr)
# to print the first five rows of the dataframe we're working with
head(John_Newell_Squash_Data)

# putting values in variables
Shot_Location= John_Newell_Squash_Data$"Shot Location"
Winner = John_Newell_Squash_Data$Winner
Game = John_Newell_Squash_Data$Game
#----------------------------------------------------------------------------------------
#Various Graphs
tbl_location <- with(John_Newell_Squash_Data, table(Winner, Shot_Location))

ggplot(as.data.frame(tbl_location), aes(Shot_Location, Freq, fill = Winner)) + 
  theme(axis.text = element_text(size=10, colour =  'Black')) +
  scale_x_discrete(breaks = seq(0, 16, by = 1)) +
  geom_col(size= 12, position = 'dodge')

tbl_Game <- with(John_Newell_Squash_Data, table(Game, Winner))

ggplot(as.data.frame(tbl_Game), aes(Game, Freq, fill = Winner)) + 
  theme(axis.text = element_text(size=10, colour =  'Black')) +
  geom_col(size= 12, position = 'dodge')

Winners <-data.frame(data = subset(John_Newell_Squash_Data, Winner=="1"))
tbl_Winners <- with(Winners, table(data.Winner, data.Shot.Location))

ggplot(as.data.frame(tbl_Winners), aes(data.Winner, Freq, fill = data.Shot.Location)) + 
  theme(axis.text = element_text(size=10, colour =  'Black')) +
  geom_col(size= 5, position = 'dodge2')
#------------------------------------------------------------------------------------------
# modeling the logistic regression with the
# independent features as Shot Location
# dependent features as Point
mod <- glm(Winner ~ factor(Shot_Location), data=John_Newell_Squash_Data, family=binomial)
summary(mod)

# getting the intercepts of the model
int <- coef(mod)
int_coef <- int[1]
Shot_Location_coef <- int[2]

# giving the xP value to the shots
for (i in seq(1,nrow(John_Newell_Squash_Data))){
  sum = int_coef + Shot_Location_coef*John_Newell_Squash_Data[i,"Shot Location"] 
  John_Newell_Squash_Data[i,"xP"] = exp(sum)/(1+exp(sum))
}

#Predict xP
table(John_Newell_Squash_Data$Winner)
pred.data <- seq(1:16)
pred.data <-data.frame(Shot_Location = seq(1:16))
pred.xP <- predict(mod,pred.data , type ="response")
pred.data$pred.xP <- pred.xP

# HeatMap for Pred.Data
pred.data <- pred.data%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data <- pred.data%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))

ggplot(pred.data, aes(x, y)) +
  geom_tile(aes(fill = pred.xP)) +
  scale_fill_gradient(low="white", high="blue")

#Dynom Package
colnames(John_Newell_Squash_Data)[6] <- "Shot_Location"
DynNom(mod,John_Newell_Squash_Data)
Shot_Location <- as.factor(Shot_Location)
mod <- glm(Point ~ Shot_Location, data=John_Newell_Squash_Data, family=binomial)
summary(mod)

#--------------------------------------------------------------------------------
#Previous Shot Location
Previous_Shot_Location= John_Newell_Squash_Data$"Previous Shot Location"
mod1 <- glm(Winner ~ factor(Shot_Location) + factor(Previous_Shot_Location), data=John_Newell_Squash_Data, family=binomial)
summary(mod1)

#Previous Shot Location 1 HeatMap

pred.data1 <- seq(1:16)
pred.data1 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 1)
pred.xP.1 <- predict(mod1,pred.data1 , type ="response")
pred.data1$pred.xP.1 <- pred.xP.1

pred.data1 <- pred.data1%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data1 <- pred.data1%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data1, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.1)) +
  scale_fill_gradient2(low="white", high="red")

#Previous Shot Location 2 HeatMap

pred.data2 <- seq(1:16)
pred.data2 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 2)
pred.xP.2 <- predict(mod1,pred.data2 , type ="response")
pred.data2$pred.xP.2 <- pred.xP.2

pred.data2 <- pred.data2%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data2 <- pred.data2%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data2, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.2)) +
  scale_fill_gradient(low="white", high="Red")

#Previous Shot Location 3 HeatMap

pred.data3 <- seq(1:16)
pred.data3 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 3)
pred.xP.3 <- predict(mod1,pred.data3 , type ="response")
pred.data3$pred.xP.3 <- pred.xP.3

pred.data3 <- pred.data3%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data3 <- pred.data3%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data3, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.3)) +
  scale_fill_gradient(low="White", high="red")

#Previous Shot Location 4 HeatMap

pred.data4 <- seq(1:16)
pred.data4 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 4)
pred.xP.4 <- predict(mod1,pred.data4 , type ="response")
pred.data4$pred.xP.4 <- pred.xP.4

pred.data4 <- pred.data4%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data4 <- pred.data4%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data4, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.4)) +
  scale_fill_gradient(low="white", high="Red")

#Previous Shot Location 5 HeatMap

pred.data5 <- seq(1:16)
pred.data5 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 5)
pred.xP.5 <- predict(mod1,pred.data5 , type ="response")
pred.data5$pred.xP.5 <- pred.xP.5

pred.data5 <- pred.data5%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data5 <- pred.data5%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data5, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.5)) +
  scale_fill_gradient(low="white", high="Red")

#Previous Shot Location 6 HeatMap

pred.data6 <- seq(1:16)
pred.data6 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 6)
pred.xP.6 <- predict(mod1,pred.data6 , type ="response")
pred.data6$pred.xP.6 <- pred.xP.6

pred.data6 <- pred.data6%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data6 <- pred.data6%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data6, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.6)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 7 HeatMap

pred.data7 <- seq(1:16)
pred.data7 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 7)
pred.xP.7 <- predict(mod1,pred.data7 , type ="response")
pred.data7$pred.xP.7 <- pred.xP.7

pred.data7 <- pred.data7%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data7 <- pred.data7%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data7, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.7)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 8 HeatMap

pred.data8 <- seq(1:16)
pred.data8 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 8)
pred.xP.8 <- predict(mod1,pred.data8 , type ="response")
pred.data8$pred.xP.8 <- pred.xP.8

pred.data8 <- pred.data8%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data8 <- pred.data8%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data8, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.8)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 9 HeatMap

pred.data9 <- seq(1:16)
pred.data9 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 9)
pred.xP.9 <- predict(mod1,pred.data9 , type ="response")
pred.data9$pred.xP.9 <- pred.xP.9

pred.data9 <- pred.data9%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data9 <- pred.data9%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data9, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.9)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 1 HeatMap

pred.data10 <- seq(1:16)
pred.data10 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 10)
pred.xP.10 <- predict(mod1,pred.data10 , type ="response")
pred.data10$pred.xP.10 <- pred.xP.10

pred.data10 <- pred.data10%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data10 <- pred.data10%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data10, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.10)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 11 HeatMap

pred.data11 <- seq(1:16)
pred.data11 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 11)
pred.xP.11 <- predict(mod1,pred.data11 , type ="response")
pred.data11$pred.xP.11 <- pred.xP.11

pred.data11 <- pred.data11%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data11 <- pred.data11%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data11, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.11)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 12 HeatMap

pred.data12 <- seq(1:16)
pred.data12 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 12)
pred.xP.12 <- predict(mod1,pred.data12 , type ="response")
pred.data12$pred.xP.12 <- pred.xP.12

pred.data12 <- pred.data12%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data12 <- pred.data12%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data12, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.12)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 13 HeatMap

pred.data13 <- seq(1:16)
pred.data13 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 13)
pred.xP.13 <- predict(mod1,pred.data13 , type ="response")
pred.data13$pred.xP.13 <- pred.xP.13

pred.data13 <- pred.data13%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data13 <- pred.data13%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data13, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.13)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 14 HeatMap

pred.data14 <- seq(1:16)
pred.data14 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 14)
pred.xP.14 <- predict(mod1,pred.data14 , type ="response")
pred.data14$pred.xP.14 <- pred.xP.14

pred.data14 <- pred.data14%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data14 <- pred.data14%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data14, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.14)) +
  scale_fill_gradient(low="white", high="red")

#Previous Shot Location 15 HeatMap

pred.data15 <- seq(1:16)
pred.data15 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 15)
pred.xP.15 <- predict(mod1,pred.data15 , type ="response")
pred.data15$pred.xP.15 <- pred.xP.15

pred.data15 <- pred.data15%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data15 <- pred.data15%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data15, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.15)) +
  scale_fill_gradient( low="white", high="red")

#Previous Shot Location 16 HeatMap

pred.data16 <- seq(1:16)
pred.data16 <-data.frame(Shot_Location = seq(1:16), Previous_Shot_Location = 16)
pred.xP.16 <- predict(mod1,pred.data16 , type ="response")
pred.data16$pred.xP.16 <- pred.xP.16

pred.data16 <- pred.data16%>%
  mutate(y = case_when(`Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 8 ~ 3,
                       `Shot_Location` <= 12 ~ 2,
                       `Shot_Location` <= 16 ~ 1))

pred.data16 <- pred.data16%>%
  mutate(x = case_when(`Shot_Location` <= 1 ~ 1,
                       `Shot_Location` <= 2 ~ 2,
                       `Shot_Location` <= 3 ~ 3,
                       `Shot_Location` <= 4 ~ 4,
                       `Shot_Location` <= 5 ~ 1,
                       `Shot_Location` <= 6 ~ 2,
                       `Shot_Location` <= 7 ~ 3,
                       `Shot_Location` <= 8 ~ 4,
                       `Shot_Location` <= 9 ~ 1,
                       `Shot_Location` <= 10 ~ 2,
                       `Shot_Location` <= 11 ~ 3,
                       `Shot_Location` <= 12 ~ 4,
                       `Shot_Location` <= 13 ~ 1, 
                       `Shot_Location` <= 14 ~ 2,
                       `Shot_Location` <= 15 ~ 3,
                       `Shot_Location` <= 16 ~ 4,
  ))
ggplot(pred.data16, aes(x, y)) +
  geom_tile(aes(fill = pred.xP.16)) +
  scale_fill_gradient(low="White", high="red")

