
            #######Libraries used####
library(openxlsx)
library(readxl)
library(DescTools)
library(e1071)
                    
                    #####Reading the data ######
my_data<-read.xlsx("dataset04.xlsx")

                    ###### Checking for the missing values ######
complete.cases(my_data) #No FALSEs returned so we conclude that the data set has no missing values

                    #Exercise one :Analyzing  the  variable STARTING_PRICE#


# Histogram plot for visualizing frequency of starting price

hist(my_data$STARTING_PRICE,
     breaks = 50,
     col = "lightblue",
     main = "Distribution of Starting Prices",
     xlab = "STARTING_PRICE",
     ylab = "Frequency")

#### Measures of Location

mean(my_data$STARTING_PRICE)
median(my_data$STARTING_PRICE)
Mode(my_data$STARTING_PRICE)

# First quantile
Q1 <- quantile(my_data$STARTING_PRICE, 0.25)
Q1

# Second quantile = Median
Q2 <- quantile(my_data$STARTING_PRICE, 0.50)
Q2

# Third Quantile
Q3 <- quantile(my_data$STARTING_PRICE, 0.75)
Q3

# Measures of Variability 
range(my_data$STARTING_PRICE)
IQR_value <- IQR(my_data$STARTING_PRICE)
IQR_value
var(my_data$STARTING_PRICE)
sd(my_data$STARTING_PRICE)
sqrt(mean((my_data$STARTING_PRICE - mean(my_data$STARTING_PRICE))^2))

summary(my_data$STARTING_PRICE)
# plotting a dot-plot to show the variability better 
stripchart(my_data$STARTING_PRICE, 
           pch = 19, 
           col = "blue",
           main = "Dotplot of Starting Prices",
           xlab = "Starting Price (SEK)")

# Measures of shape 
skewness(my_data$STARTING_PRICE)

hist(my_data$STARTING_PRICE,
     breaks = 50,
     col = "lightblue",
     main = "Distribution of Starting Prices",
     xlab = "STARTING_PRICE",
     ylab = "Frequency")


#Outliers are in this range: below Q1−1.5×IQR or above Q3+1.5×IQR.
outliers1 <- Q1 - 1.5 * IQR_value
outliers2 <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- my_data$STARTING_PRICE[my_data$STARTING_PRICE < outliers1 | my_data$STARTING_PRICE > outliers2]

# Number of outliers
length(outliers)

#Box plot for visualizing the location and variability parameters and outliers in starting price 
boxplot(my_data$STARTING_PRICE,
        main = "Boxplot of Starting Prices",
        xlab = "STARTING_PRICE",
        ylab = "Starting Price (SEK)",
        outcol = "red",
        col = "lightblue")




                    #Exercise two: Analyzing simultaneously the variables REGION and TYPE#

# Describing two categorical variables with Contingency table and Mosaic plot

#preparing the contingency table 
contingency_table= table(my_data$REGION, my_data$TYPE)
Total_Count_in_contingency_table = addmargins(contingency_table)
contingency_table
print(Total_Count_in_contingency_table)
# Joint Relative distribution table
proportions <- prop.table(contingency_table, margin = 2) # Column based
print("Proportions Table (Column-wise):")
print(proportions)

# Graphical Analysis by  Mosaic plot
mosaicplot(contingency_table,
           main = "Mosaic Plot: REGION vs TYPE",
           xlab = "Region",
           ylab = "Housing Types",
           color= c("skyblue", "lightgreen", "coral"),)

# Plot a bar plot to show he type of housing units differ between regions 

barplot(
  contingency_table,
  col = c("skyblue", "lightgreen", "coral","yellow","purple"), 
  legend.text = TRUE,
  args.legend = list(title = "Housing Type", x = "topright"),
  main = "Distribution of Housing Types by Region",
  xlab = "Region",
  ylab = "Count"
)


              # Exercise three: Analyzing  the variables REGION and AREA #

# Gathering information of unit sizes across regions 
summary_statistics<-by(my_data$AREA, my_data$REGION, summary)
summary_table <- do.call(rbind, summary_statistics) # This converts the by output to a data frame
View(summary_table)

# Creating a box plot for AREA within each REGION
boxplot(AREA ~ REGION, 
        data = my_data, 
        main = "Boxplot of AREA by REGION", 
        col =  "skyblue",
        xlab = "Region", 
        ylab = "Area")


                    # Exercise 4 : Analyzing  the  relationship  between  the  variables STARTING_PRICE and AREA#

#plotting the two numerical variables AREA(independent) and STARTING_PRICE (dependent)
plot(my_data$AREA, my_data$STARTING_PRICE,
     main = "Scatter Plot: Starting Price vs Area",
     xlab = "Area",
     ylab = "Starting Price",
     col= "blue")


#  Pearson correlation coefficient: 0.6348584
cor(my_data$STARTING_PRICE , my_data$AREA )


              #Exercise 5: fitting a linear regression 
#.Simple linear regression 
simple_reg <- lm(STARTING_PRICE ~ AREA , data= my_data)
abline(simple_reg)

summary(simple_reg) # Multiple R-squared:  0.403 meaning that 40% of the variation in starting price is explained by area


              #Exercise 6 : Fitting simple_reg that explains STARTING_PRICE in terms of REGION,TYPE,BALCONY,ROOM & AREA.#

# Fitting Multiple Linear Regression

multi_reg <- lm(STARTING_PRICE~ REGION + TYPE+ BALCONY + ROOMS + AREA  , data= my_data)
multi_reg$coefficients

# report of the estimated regression in equation form
summary(multi_reg)

# Getting coefficients
coefficients <- coef(multi_reg)
coefficients

# for interpreting the coefficients of the regression.
# Through this line we changed the variables on x-axis from horizontal to vertical
par(las = 2)
barplot( multi_reg$coefficients, )
multi_reg$fitted.values
multi_reg$residuals

                       ###Exercise seven :Predicting ####

# reading the test data
test_data <- read.xlsx("test.xlsx")
test_data


# Converting categorical variables to factors for both datasets
my_data$REGION <- as.factor(my_data$REGION)
my_data$TYPE <- as.factor(my_data$TYPE)
my_data$BALCONY <- as.factor(my_data$BALCONY)

test_data$REGION <- as.factor(test_data$REGION)
test_data$TYPE <- as.factor(test_data$TYPE)
test_data$BALCONY <- as.factor(test_data$BALCONY)

# Predicting starting prices for the test dataset
test_data$PREDICTED_PRICE <- predict(multi_reg, newdata = test_data)

print(test_data)

# Plot the predicted prices against AREA

plot(test_data$AREA, test_data$PREDICTED_PRICE, col = "blue", 
     main = "Predicted Prices vs. Area", 
     xlab = "Area (sqm)", 
     ylab = "Predicted Starting Price in SEK")


# Add a regression line for AREA vs. PREDICTED_PRICE
x.grid <- seq(min(test_data$AREA), max(test_data$AREA), length = 6)
predicted_line <- predict(multi_reg, newdata = data.frame(AREA = x.grid, 
                                                      REGION = levels(test_data$REGION)[1], 
                                                      TYPE = levels(test_data$TYPE)[1], 
                                                      BALCONY = levels(test_data$BALCONY)[1], 
                                                      ROOMS = mean(test_data$ROOMS)))

lines(x.grid, predicted_line, col = "red", lwd = 2)

# Minimum predicted starting price
min_price <- min(test_data$PREDICTED_PRICE)
cat("Minimum Predicted Starting Price:", min_price, "\n")

