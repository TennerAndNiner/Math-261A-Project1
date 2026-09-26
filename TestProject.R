library(ggplot2)
my_data <- read.csv("/Users/vinhemduong/R_Code/Project/openpl_sample_male.csv")

my_data <- na.omit(my_data)

fortyGroup <- my_data[my_data$AgeClass == as.character("45-49"),]
squat <- fortyGroup$Squat
bench <- fortyGroup$Bench
deadlift <- fortyGroup$Deadlift
weight <- fortyGroup$BodyweightKg
#wilks <- fortyGroup$Wilks
#dots <- fortyGroup$Dots
total <- fortyGroup$TotalKg


mod <- lm(formula = Bench ~ BodyweightKg, data = fortyGroup, na.action = na.exclude)

intercept <- mod$coefficients[1]
slope <- mod$coefficients[2]

#ggp <- ggplot(data = fortyGroup, aes(x = weight, y = bench)) +
  #geom_point() + labs(title = "Male Lifters Bodyweight Kg vs Bench Press")

#ggp + geom_abline(intercept = intercept, slope = slope, color = "red")

#plot(weight, squat)

#abline(a = mod$coefficients[1], b = mod$coefficients[2])

fortyGroup$predicted <- fitted.values(mod)
fortyGroup$residuals <- residuals(mod)
#qplot(fortyGroup$predicted, fortyGroup$residuals) + geom_smooth(method = "lm", se = F)

qqp <- ggplot(data = fortyGroup, aes(sample = residuals)) + stat_qq(color = "steelblue") + stat_qq_line(color = "red", linewidth = 1) + 
            theme_minimal() + ggtitle("Q-QPlot")
qqp


#qqplot(my_data$predicted, my_data$residuals) + geom_smooth(method = "lm", se = F)
#qqline(x = my_data$predicted, my_data$residuals)
