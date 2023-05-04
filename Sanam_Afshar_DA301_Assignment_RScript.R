## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# Determine your working directory
getwd()


# Change your current directory.
setwd(dir='C:/Users/sanama/OneDrive - Autodesk/LSE/Course-3/LSE_DA301_assignment_files') 

# Install and import Tidyverse.
# Import the tidyverse library.
library(tidyverse)

# Install and import Tidyverse.
# Import the tidyverse library.
library(tidyverse)

par(mar = c(1, 1, 1, 1))        # Changing area margins

# Import the data set.
# Import the CSV files.

turtle_reviews <- read.csv('turtle_reviews.csv', header = TRUE)
turtle_sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frames.
#turtle_reviews
#view(turtle_reviews)

turtle_sales
view(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
game <- turtle_sales[,c('Product', 'Platform', 'Year', 'Genre', 'Publisher', 
                        'NA_Sales', 'EU_Sales', 'Global_Sales')]

game <- na.omit(game) # remove NA

game$Year <- as.factor(as.character(game$Year))
game$NA_Sales <- game$NA_Sales * 1000000 + 1
game$EU_Sales <- game$EU_Sales * 1000000 + 1
game$Global_Sales <- game$Global_Sales * 1000000 + 1
# Format column names 
colnames(game) <- c("Product", "Platform", "Year", "Genre", "Publisher", 
                    "NA.Sales", "EU.Sales", "Global.Sales")
str(game)

# View the data frame.
view(game)

# View the descriptive statistics.
summary(game)

# Calculate standard deviation for each variable
sapply(turtle_sales, sd, na.rm=TRUE)

# Calculate range for each variable
#sapply(turtle_sales, function(turtle_sales) max(turtle_sales, na.rm=TRUE)-min(turtle_sales, na.rm=TRUE))

#define function that calculates mode
#find_mode <- function(x) {
#  u <- unique(x)
#  tab <- tabulate(match(x, u))
#  u[tab == max(tab)]
#}

#calculate mode for each variable
#sapply(turtle_sales, find_mode)


################################################################################

# 2. Review plots to determine insights into the data set.

Product.Log <- log(game$Product)
NA.Sales.Log <- log(game$NA.Sales)   
EU.Sales.Log <- log(game$EU.Sales)  
Global.Sales.Log <- log(game$Global.Sales)

game.log <- cbind.data.frame(Product.Log, NA.Sales.Log, EU.Sales.Log,Global.Sales.Log)
game <- cbind.data.frame(game, game.log)  # The data we use for analysis

## 2a) Scatterplots
# Create scatterplots.

qplot(Product, NA.Sales, data=game)
qplot(Product, EU.Sales, data=game)
qplot(Product, Global.Sales, data=game)

## 2b) Histograms
# Create histograms.
qplot(Product, bins=5, data=game)

## 2c) Boxplots
# Create boxplots.

qplot(Product, Platform, data=game, colour=I('red'), geom='boxplot')

qplot(Product, Global.Sales, colour=Genre, data=game, geom='col')

qplot(Product, Global.Sales, colour=Publisher, data=game, geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################
# Import and read CSV file.
library(readr)
# Data wrangling.
library(dplyr)
# Data wrangling.
library(tidyr)
# Create statistical summaries.
library(skimr)
# Create a report as an HTML file.
library(DataExplorer)

library (moments)

# 1. Load and explore the data

as_tibble(game)
View(game)

# View data frame created in Week 4.

head(game)
dim(game)

# Check output: Determine the min, max, and mean values.
summary(game)


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
game_sales <- game %>% group_by(Product) %>% summarise(game = sum(Product))

# View the data frame.
view(game_sales)

# Explore the data frame.
as_tibble(game_sales)
head(game_sales)
dim(game_sales)

## 2b) Determine which plot is the best to compare game sales.
library(ggplot2)

# Create scatterplots.
# 1. Global Sales
ggplot(game, aes(x = Product, y = Global.Sales)) + # aesthetic mapping
  geom_point() + # add data points
  stat_smooth(method = lm)  # add trend line

# 2. NA Sales
ggplot(game, aes(x = Product, y = NA.Sales)) + # aesthetic mapping
  geom_point() + # add data points
  stat_smooth(method = lm)  # add trend line

# 3. EU Sales
ggplot(game, aes(x = Product, y = EU.Sales)) + # aesthetic mapping
  geom_point() + # add data points
  stat_smooth(method = lm)  # add trend line

# Create histograms.
#ggplot(game, aes(x = Global.Sales)) + geom_histogram()

ggplot(game, aes(x = Global.Sales, y = ..density..)) + 
  geom_histogram(fill = "cornsilk", 
                 colour = "grey60", size = .2) +
  geom_density()

ggplot(game, aes(x = NA.Sales, y = ..density..)) + 
geom_histogram(fill = "blue", 
               colour = "grey60", size = .2) +
  geom_density()

ggplot(game, aes(x = EU.Sales, y = ..density..)) + 
geom_histogram(fill = "green", 
               colour = "grey60", size = .2) +
  geom_density()


#### Analysis of Sale By Year

Year <- game$Year 
counts <- data.frame(table(Year))
p <- game %>%
  select(Year, Global.Sales) %>%
  group_by(Year) %>%
  summarise(Total.Sales = sum(Global.Sales))
q <- cbind.data.frame(p, counts[2])  # Add counts to data frame
names(q)[3] <- "count"
q$count <- as.numeric(q$count)

ggplot(q, aes(x = Year, y = Total.Sales, label = q$count)) +
  geom_col(fill = "blue") +
  geom_point(y = q$count * 500000, size = 3, shape = 21, fill = "green" ) +
  geom_text(y = (q$count + 50) * 500000) +  # Position of the text: count of games each year
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "cornsilk"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete("Year", labels = as.character(Year), breaks = Year)


# Regroup platform as Platform.type 
pc <- c("PC")
xbox <- c("X360", "XB", "XOne")
nintendo <- c("Wii", "WiiU", "N64", "GC", "NES", "3DS", "DS") 
playstation <- c("PS", "PS2", "PS3", "PS4", "PSP", "PSV")
game <- game %>%
  mutate(Platform.type = ifelse(Platform %in% pc, "PC",
                                ifelse(Platform %in% xbox, "Xbox",
                                       ifelse(Platform %in% nintendo, "Nintendo", 
                                              ifelse(Platform %in% playstation, "Playstation", "Others")))))

ggplot(game, aes(x = Platform.type)) + geom_bar(fill = "orange")

#####################
# Create boxplots.
library(ggplot2)
game$Platform.type <- as.factor(game$Platform.type)
ggplot(game, aes(x = Platform.type, y = Global.Sales.Log)) +
  geom_boxplot()

############################
# Ring plot for Genre
dat <- data.frame(table(game$Genre))
dat$fraction <- dat$Freq / sum(dat$Freq)
dat <- dat[order(dat$fraction), ]
dat$ymax <- cumsum(dat$fraction)
dat$ymin <- c(0, head(dat$ymax, n = -1))
names(dat)[1] <- "Genre"
library(ggplot2)
ggplot(dat, aes(fill = Genre, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(colour = "grey30") +  # Background color
  coord_polar(theta = "y") +  # Coordinate system to polar
  xlim(c(0, 4)) +  
  labs(title = "Ring plot for Genre", fill = "Genre") +
  theme(plot.title = element_text(hjust = 0.5))


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

name <- colnames(game)[c(6, 7, 8, 9, 10, 11, 12, 12)]  # Pick up the numeric columns according to the names 
par(mfrow = c(2, 4))  # Layout in 2 rows and 4 columns
for (i in 1:length(name)){
  sub <- sample(game[name[i]][, 1], 350)
  submean <- mean(sub)
  hist(sub, main = paste("Hist. of", name[i], sep = " "), xlab = name[i])
  abline(v = submean, col = "blue", lwd = 1)
  qqnorm(sub, main = paste("Q-Q Plot of", name[i], sep = " "))
  qqline(sub) 
  if (i == 1) {s.t <- shapiro.test(sub)
  } else {s.t <- rbind(s.t, shapiro.test(sub))
  }
}
s.t <- s.t[, 1:2]  # Take first two columns of shapiro.test result
s.t <- cbind(name, s.t) # Add variable name for the result
s.t


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# 1. Check for normal distribution
# Specify the qqnorm function.
# Draw a qqplot using the Global.Sales data.

qqnorm(game$Global.Sales,
       col='blue',
       xlab="z Value",
       ylab="Global.Sales",
       main = "Global Sales Normal Distribution")

# Perform Shapiro-Wilk test.

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(game$Global.Sales,
       col='red',
       lwd=2) 

# Run a Shapiro-Wilk test:
shapiro.test(game$Global.Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# Specify the skewness and kurtosis functions.
skewness(game$Global.Sales) 
kurtosis(game$Global.Sales)

# 4. Perform a t-test
# Specify the t.test function.
# Set the data source, the confidence interval (95%),
# and the theoretical mean:  

t.test(game$Global.Sales,
       conf.level=0.95,
       mu=5349087)

# 5. Prepare data
# Run a Shapiro-Wilk test:

# manipulate data
game2 <- select(game, -Platform, -Genre, -Publisher, -Platform.type, 
                -Product.Log, -Product, -Year) 
view(game2)
#shapiro.test(game2$Year)
shapiro.test(game2$NA.Sales)
shapiro.test(game2$EU.Sales)
shapiro.test(game2$Global.Sales)

## 3d) Determine correlation
# Determine correlation.
# Specify the cor function.
# Set the first and second variables

cor(game2$NA.Sales, game2$Global.Sales)
cor(game2$EU.Sales, game2$Global.Sales) 

# Determine the correlation for the whole data frame.
round (cor(game2),
       digits=2)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
library(ellipse)  
library(corrplot)

st <- game2[, c(1, 2, 3, 4, 5, 6)]  # Take numeric variables as goal matrix
st <- na.omit(st)

corMatrix <- cor(as.matrix(st))  # Correlation matrix

col <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                          "cyan", "#007FFF", "blue", "#00007F"))

png(filename = "corrplot.png", width = 1200, height = 800)
corrplot.mixed(corMatrix, order = "AOE", lower = "number", lower.col = "black", 
               number.cex = .8, upper = "ellipse",  upper.col = col(10), 
               diag = "u", tl.pos = "lt", tl.col = "black")
dev.off()

png(filename = "cluster_dendogram.png", width = 1200, height = 800)
plot(hclust(as.dist(1 - cor(as.matrix(st)))))  # Hierarchical clustering
dev.off()

#### BY REGION
library(reshape2)
game %>%
  select(Year, NA.Sales.Log, EU.Sales.Log, Global.Sales.Log) %>%
  melt(id.vars = "Year") %>%   # Stacks other columns into "Year"
  group_by(Year, variable) %>% 
  summarise(total.sales = sum(value)) %>%
  ggplot(aes(x = Year, y = total.sales, color = variable, group = variable)) +
  geom_point() + 
  geom_line() + 
  labs(x = "Year", y = "Total Sales Log Value", color = "Region") +
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill="pink"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

### By Genre (Year wise log global sales by Genre)

game %>% 
  select(Year, Global.Sales.Log, Genre) %>%
  group_by(Year, Genre) %>%
  summarise(Total.Sales.Log = sum(Global.Sales.Log)) %>%
  ggplot(aes(x = Year, y = Total.Sales.Log, group = Genre, fill = Genre)) +
  geom_area() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "blue"),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank()) +
  labs(title = "Year-wise log Global Sales by Genre")

### Yearly market share by platform type

library(viridis)
library(scales)
p <- game %>%
  group_by(Platform.type, Year) %>%
  summarise(total = sum(Global.Sales))
p$Year <- as.numeric(as.character(p$Year))
ggplot(p, aes(x = Year, fill = Platform.type)) +
  geom_density(position = "fill") +
  labs(y = "Market Share") +
  labs(title = "Yearly Market Share by Platform Type") +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = percent_format()) 


####################
# Compute 1-way ANOVA test for log value of global sales by Platform Type
model <- aov(Global.Sales.Log ~ Platform.type, data = game)
summary(model)

tukey <- TukeyHSD(model)

dev.new(noRStudioGD = TRUE)
par(mar = c(4, 10, 2, 1))
plot(tukey, las = 1)


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
summary(game2)
head(game2)

# Determine a summary of the data frame.

summary.data.frame(game2)
###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

cor(game2)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
dev.new(noRStudioGD = TRUE)
plot(game$Year, game$Global.Sales.Log, xlab="Year", ylab = "Global Sales")

# Create a model with only one x variable.
model1 <- lm(Global.Sales.Log ~ Year)


# View the model.
model1


# View more outputs for the model - the full regression table.
summary(model1)

# Plot the model.
# View residuals on a plot.
dev.new(noRStudioGD = TRUE)
qqnorm(residuals(model1), main = "Global Sales Residuals")
qqline(residuals(model1), col='red', main = "Global Sales Residuals")

#abline(coefficients(model1), col='red')
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
dev.new(noRStudioGD = TRUE)
plot(game$Year, game$NA.Sales.Log, xlab="Year", ylab = "North America Sales")

# Create a model with only one x variable.
model2 <- lm(NA.Sales.Log ~ Year)


# View the model.
model2


# View more outputs for the model - the full regression table.
summary(model2)

# Plot the model.
# View residuals on a plot.
dev.new(noRStudioGD = TRUE)
qqnorm(residuals(model2), main = "North America Sales Residuals")
qqline(residuals(model2), col='blue')


dev.new(noRStudioGD = TRUE)
plot(game$Year, game$EU.Sales.Log, xlab="Year", ylab = "European Sales")

# Create a model with only one x variable.
model3 <- lm(EU.Sales.Log ~ Year)


# View the model.
model3


# View more outputs for the model - the full regression table.
summary(model3)

# Plot the model.
# View residuals on a plot.
dev.new(noRStudioGD = TRUE)
qqnorm(residuals(model3), main = "European Sales Residuals")
qqline(residuals(model3), col='green')

# Multiple linear regression model.
# 4. Compare the two models

# Arrange plot with the par(mfrow) function.
dev.new(noRStudioGD = TRUE)
par(mfrow=c(3, 1))


# Compare both graphs (model1 and model2).
plot(game$Year, game$Global.Sales.Log, xlab = "Year", ylab = "Global Sales")
#abline(coefficients(model1), col='red')

plot(game$Year, game$NA.Sales.Log, xlab = "Year", ylab = "North America Sales")
#abline(coefficients(model2), col='blue') 

plot(game$Year, game$EU.Sales.Log, xlab = "Year", ylab = "European Sales")
#abline(coefficients(model3), col='green') 


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

df <- game
# Creates a linear model
my_linear_model <- lm(Global.Sales.Log~NA.Sales.Log, data = df)

# Prints the model results
my_linear_model


##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

# Creating a data frame
variable_NAsales <- data.frame(sales = c(34.02, 3.93, 2.73, 2.26, 22.08))

# Fiting the linear model
#linear_model <- lm(Global.Sales.Log~NA.Sales.Log, data = df)

# Predicts the future values
#predict(linear_model, newdata = variable_NAsales)

# Predicts the values with confidence interval
#predict(linear_model, newdata = variable_NAsales, interval = 'confidence')


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




