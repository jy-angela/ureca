library(dplyr)
library(ggplot2)

#####################1. SEMA data
#use the data to illustrate which question has the highest anxiety level for respondents
data<-SEMA_data1[,c(3:22)]

# Calculate the column means
means <- colMeans(data)

# Round the means to 2 decimal places
means <- round(means, 2)

# Create a new data frame with the means
mean_table <- data.frame(variable = names(means), mean = means)

# Print the mean table
mean_table

# Set the title, horizontal axis name, and vertical axis name
chart_title <- "Distribution of Anxiety Levels of Different Questions: mean_table"
x_axis_name <- "Question Type"
y_axis_name <- "Anxiety Level"

bar_chart <- ggplot(mean_table, aes(x = variable, y = mean)) +
  geom_bar(stat = "identity") +
  labs(title = chart_title, x = x_axis_name, y = y_axis_name)

# Print the bar chart
print(bar_chart)

ggplot(mean_table,aes(x=variable,y=mean))+geom_point()
#for the scatter plot, there is no such obvious relationship
ggplot(mean_table,aes(x=variable,y=mean))+geom_bar(stat="identity", color="black")
#q4 has the highest anxiety level

##########then, I focus on the question 4 which has the highest anxiety level to investigate the correlation between anxiety level
#and the academic performance
str(ureca_data2)
data2<-ureca_data2

cor_test <- cor.test(data2$SEMA, data2$processing_speed_index, method = "pearson")
print(cor_test)

str(data2)
data2%>%filter(verbal_comprehension_index > 80 & verbal_comprehension_index < 100)%>%ggplot(aes(x=SEMA, y=verbal_comprehension_index))+geom_point()+geom_smooth()

######################the correlation between verbal comprehension index and anxiety level
data3<-ureca_data3
v1<-data3%>%filter(verbal_comprehension_index > 80 & verbal_comprehension_index < 100, SEMA >1)

# Correlation test
cor_test <- cor.test(v1$SEMA, v1$verbal_comprehension_index, method = "pearson")
print(cor_test)

# Access the p-value
p_value <- cor_test$p.value

# Print the p-value
print(p_value)


# Scatter plot with correlation test results
scatter_plot <- ggplot(v1, aes(x = SEMA, y = verbal_comprehension_index)) +
  geom_point() +
  geom_smooth() +
  geom_text(aes(x = max(v1$SEMA), y = max(v1$verbal_comprehension_index),
                label = paste0("Correlation: ", round(cor_test, 2))),
            hjust = 1, vjust = -0.5, color = "blue") +
  labs(x = "SEMA", y = "Verbal Comprehension Index", title = "Scatter Plot with Correlation Test")


# Print scatter plot
print(scatter_plot)


##################the correlation between working memory index and anxiety level
data4<-ureca_data41
v2<-data4%>%filter(working_memory_index > 90)

# Correlation test
cor_test2 <- cor.test(v2$SEMA, v2$working_memory_index, method = "pearson")
print(cor_test2)

# Access the p-value
p_value2 <- cor_test2$p.value

# Print the p-value
print(p_value2)


# Scatter plot with correlation test results
scatter_plot2 <- ggplot(v2, aes(x = SEMA, y = working_memory_index)) +
  geom_point() +
  geom_smooth() +
  geom_text(aes(x = max(v2$SEMA), y = max(v2working_memory_index),
                label = paste0("Correlation: ", round(cor_test, 2))),
            hjust = 1, vjust = -0.5, color = "blue") +
  labs(x = "SEMA", y = "Verbal Comprehension Index", title = "Scatter Plot with Correlation Test")


# Print scatter plot
print(scatter_plot)









ggplot(data2, aes(x=SEMA, y=verbal_comprehension_index, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=SEMA, y=visual_spatial_index, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=SEMA, y=fluid_reasoning_index, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=SEMA, y=working_memory_index, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=SEMA, y=processing_speed_index, col=class))+geom_point()+geom_smooth()

ggplot(data2, aes(x=verbal_comprehension_index, y=SEMA, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=visual_spatial_index, y=SEMA, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=fluid_reasoning_index, y=SEMA, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=working_memory_index, y=SEMA, col=class))+geom_point()+geom_smooth()
ggplot(data2, aes(x=processing_speed_index, y=SEMA, col=class))+geom_point()+geom_smooth()

library(psych) 
pairs.panels(data2,method = "pearson", hist.col =  "steelblue",
             pch = 21, density = TRUE, ellipses = FALSE )

#对于所有的index，和SEMA有最显著的关系的是working memory index, 其他的来说的话就是index和index之间的关系
#the relationship between verbal-comprehension & fluid-reasoning index








