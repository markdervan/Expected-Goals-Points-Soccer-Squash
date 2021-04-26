#xG Metric
x = xGData$X*105/100
y = xGData$Y*65/100

#Calculating Distance from X Y coordinates 
if (y < 32.5) {
  dist1 = sqrt(x^2 + (32.5-y)^2)
} else {
  dist1 = sqrt(x^2 + (32.5-(65-y)^2))
}
xGData$Distance1 <- dist1

#Calculating angle from X Y coordinates
c = sqrt(x^2 + y^2)
d = 90-(atan(y/x)*57.296)
post2_dist = sqrt(36.16^2 + c^2 - 2*36.16*c*cos(d))
post2_angle = (acos((post2_dist^2+36.16^2-c^2)/(2*post2_dist*36.16)))*57.296
post1_dist = sqrt(post2_dist^2 + 7.32^2 - 2*post2_dist*7.32*cos(post2_angle))
Shooting_Angle = (acos((post1_dist^2 + post2_dist^2 - 7.32^2)/(2*post1_dist*post2_dist)))*57.296
xGData$Shooting_Angle <- Shooting_Angle

# plotting histograms of goals
barplot(table(goal), xlab="Goal Scored", ylab="Number of Goals scored",main="Goal Data")

# modeling the logistic regression with the
# independent features as Angle and Distance
# dependent features as Goal
mod <- glm(Goal ~ Shooting_Angle + dist1, data=xGData, family=binomial)
summary(mod)

# getting the intercepts of the model
int <- coef(mod)
int_coef <- int[1]
ang_coef <- int[2]
dist_coef <- int[3]

# giving the xG value to the shots
for (i in seq(1,nrow(xGData))){
  sum = int_coef + ang_coef*xGData[i,"Shooting_Angle"] + (dist_coef*xGData[i,"Distance1"])
  xGData[i,"xG"] = exp(sum)/(1+exp(sum))
}

# plotting a few plots to show dependency of xG on Distance and Angle
plot(dist1,xGData$xG,main="Expected Goals vs Distance",xlab="Distance",ylab="xG",ylim=c(0,1))
plot(Shooting_Angle,xGData$xG,main="Expected Goals vs Angle",xlab="Angle",ylab="xG",xlim=c(0,180),ylim=c(0,0.6))

#All shot graphs (add measure to x axis)
tbl_Shots <- with(xGData, table(Goal, Distance1))
ggplot(as.data.frame(tbl_Shots), aes(Distance1, Freq, fill = Goal)) + 
  scale_x_discrete(breaks = seq(0, 80, by = 5)) +
  geom_col(size=5, position = 'dodge2')

#Rounding angle and distance for easier to interpret diagrams
xGData$Distance_Rounded2 <- round(xGData$Distance1)
xGData$Angle_Rounded2 <- round(xGData$Shooting_Angle)

#Goal vs No Goal Distance & Angle
xGData %>% 
  ggplot(aes(Distance_Rounded2, group = Goal, color = Goal)) +
  geom_bar(size = 2) 

xGData %>% 
  ggplot(aes(Angle_Rounded2, group = Goal, color = Goal)) +
  geom_bar(size = 2) 

#Data Visualisation making the pitch
library(ggsoccer)
library(ggplot2)
x2 = xGData$X
y2 = xGData$Y

ggplot(xGData) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(aes(x = x2, y = y2),
             fill = "yellow", 
             shape = 22,
             size = 2) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  ggtitle("Shotmap",
          "PL 2008-2018")

#Goals ShotMap
Goals <-data.frame(data = subset(xGData, Goal=="1"))
x1 = Goals$data.X
y1 = Goals$data.Y
ggplot(Goals) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(aes(x = x1, y = y1),
             fill = "Pink", 
             shape = 22,
             size = 2) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  ggtitle("Goal Shotmap",
          "PL 2008-2018")

#Heatmap of xG
ggplot(xGData, aes(x2, y2)) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_tile(aes(fill = xG)) +
  scale_fill_gradient(low="white", high="red")+
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  ggtitle("Shot xG Heatmap",
          "PL 2008-2018")
#Bar Chart of goals based on Distance & Angle
Distance_Goals <- Goals$data.Distance1
Angle_Goals <- Goals$data.Shooting_Angle
Goals$Distance_Rounded_Goals <- round(Distance_Goals)
Goals$Angle_Rounded_Goals <- round(Angle_Goals)
tbl_Angle <- with(Goals, table(data.Goal, Angle_Rounded_Goals))
tbl_Distance <- with(Goals, table(data.Goal, Distance_Rounded_Goals))

ggplot(as.data.frame(tbl_Distance), aes(Distance_Rounded_Goals, Freq, fill = data.Goal)) + 
  theme(axis.text = element_text(size=8, colour =  'Orange')) +
  geom_col(position = 'dodge')

ggplot(as.data.frame(tbl_Angle), aes(Angle_Rounded_Goals, Freq, fill = data.Goal)) + 
  theme(axis.text = element_text(size=8, colour =  'Black')) +
  scale_x_discrete(breaks = seq(0, 360, by = 10)) +
  geom_col(position = 'dodge')

#shot heatmap
#Heatmap of xG
ggplot(xGData, aes(x2, y2)) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_tile(aes(fill = Goal)) +
  scale_fill_gradient(low="Black", high="Blue")+
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  ggtitle("Shot Heatmap",
          "PL 2008-2018")


