#Electronic Sales data set imported in global environment
#Shortening data set's name
final <- Electronic_sales_Sep2023.Sep2024

#Checking dimensions of data set
dim(final)

#Generate six number statistic summary
summary(final)

#Load tidyverse
library(tidyverse)
#Load reshape2
library(reshape2)
#Load ggplot2
library(ggplot2)
#Load sqldf
library(sqldf)
#Load dplyr
library(dplyr)

###########################################################
##Cleaning data; filter out one record where gender = N/A;#
##Transform purchase data column to time format############
###########################################################
cleaned <- final %>% filter(Gender != "#N/A")

#Cleaning data; transform purchase data column in time format 
library(lubridate)

# Ensure the column is of type character
cleaned$Purchase.Date <- as.character(cleaned$Purchase.Date)

# Convert the Purchase Date column to Date format
cleaned$Purchase.Date <- ymd(cleaned$Purchase.Date)

# Extract month and year from Purchase Date
cleaned$Month <- floor_date(cleaned$Purchase.Date, "month")


###########################################################
##Cleaning data; combining "PayPal" and "Paypal"###########
###########################################################
# Combine the two PayPal columns together
cleaned$Payment.Method <- gsub("Paypal", "PayPal", cleaned$Payment.Method)

# Verify the changes
table(cleaned$Payment.Method)


###########################################################
##Cleaning data; combining the three add on types##########
###########################################################
# Combine the Add.ons.Purchased column into three groups of add-on types and remove extra spaces
transaction_table <- transaction_table %>%
  separate_rows(Add.ons.Purchased, sep = ",") %>%
  mutate(Add.ons.Purchased = trimws(Add.ons.Purchased)) %>%
  filter(Add.ons.Purchased != "")


####################################
##Separate dataset into two tables##
####################################
# Create the Customer Table
customer_table <- cleaned %>%
  select(Customer.ID, Age, Gender, Loyalty.Member)


# Create the Transaction Table
transaction_table <- cleaned %>%
  select(Customer.ID, Product.Type, SKU, Rating, Order.Status, Payment.Method, 
         Total.Price, Unit.Price, Quantity, Purchase.Date, Shipping.Type, 
         Add.ons.Purchased, Add.on.Total)

# Save the tables to CSV files
write.csv(customer_table, "Customer_Table.csv", row.names = FALSE)
write.csv(transaction_table, "Transaction_Table.csv", row.names = FALSE)


##########
##Query1##
##########
#Count number of males and females from Gender in the dataset
Query1 <- "SELECT COUNT(*) AS Male_Count
          FROM cleaned
          WHERE Gender = 'Male';"
sqldf(Query1)
#Male_Count 10,164

Query1.1 <- "SELECT COUNT(*) AS Female_Count
          FROM cleaned
          WHERE Gender = 'Female';"
sqldf(Query1.1)
#Female_Count 9,835

Query1.2 <- "SELECT COUNT(*) AS Loyalty_Member
            FROM cleaned
            WHERE `Loyalty.Member` = 'Yes';"
sqldf(Query1.2)
#Loyalty_Member 4,342

Query1.3 <- "SELECT COUNT(*) AS Nonloyalty_Member
            FROM cleaned
            WHERE `Loyalty.Member` = 'No';"
sqldf(Query1.3)
#Nonloyalty_Member 15,657


##########
##Query2##
##########
#Total monthly sales sorted from highest to lowest
Query2 <- "SELECT Month, SUM(`Total.Price`) AS Total_Sales
          FROM cleaned
          GROUP BY Month
          ORDER BY Total_Sales DESC;"

sqldf(Query2)


##########
##Query3## 
##########
# Find top 5 transactions for loyalty members only
Query3 <- "SELECT c.`Customer.ID`, c.Age, c.Gender, SUM(t.`Total.Price`) AS Total_Spending
FROM customer_table c
JOIN transaction_table t
ON c.`Customer.ID` = t.`Customer.ID`
WHERE c.`Loyalty.Member` = 'Yes'
GROUP BY c.`Customer.ID`, c.Age, c.Gender
ORDER BY Total_Spending DESC
LIMIT 5;"

sqldf(Query3)


##########
##Query4## 
##########
Query4 <- "SELECT t.`Product.Type`, SUM(t.'Quantity') AS Total_Quantity
          FROM customer_table c
          JOIN transaction_table t
          ON c.`Customer.ID` = t.`Customer.ID`
          WHERE c.`Loyalty.Member` = 'Yes'
          GROUP BY t.`Product.Type`
          ORDER BY Total_Quantity;"

sqldf(Query4)

Query4.1 <- "SELECT t.`Product.Type`, SUM(t.'Quantity') AS Total_Quantity
          FROM customer_table c
          JOIN transaction_table t
          ON c.`Customer.ID` = t.`Customer.ID`
          WHERE c.`Loyalty.Member` = 'No'
          GROUP BY t.`Product.Type`
          ORDER BY Total_Quantity;"
sqldf(Query4.1)


##########
##Query5## 
##########
Query5 <- "SELECT t.`Product.Type`, AVG(t.`Rating`) AS Average_Rating
          FROM customer_table c
          JOIN transaction_table t
          ON c.`Customer.ID` = t.`Customer.ID`
          WHERE c.`Loyalty.Member` = 'Yes'
          GROUP BY t.`Product.Type`
          ORDER BY Average_Rating DESC;"
sqldf(Query5)

Query5.1 <- "SELECT t.`Product.Type`, AVG(t.`Rating`) AS Average_Rating
          FROM customer_table c
          JOIN transaction_table t
          ON c.`Customer.ID` = t.`Customer.ID`
          WHERE c.`Loyalty.Member` = 'No'
          GROUP BY t.`Product.Type`
          ORDER BY Average_Rating DESC;"

sqldf(Query5.1)

##########
##Query6## 
##########
# Split the Add.ons.Purchased column into individual add-ons and remove extra spaces
#Cleaning from above
transaction_table <- transaction_table %>%
  separate_rows(Add.ons.Purchased, sep = ",") %>%
  mutate(Add.ons.Purchased = trimws(Add.ons.Purchased)) %>%
  filter(Add.ons.Purchased != "")

# Use SQL to count the number of times each add-on was purchased
Query6 <- "SELECT `Add.ons.Purchased` as Add_on, COUNT(*) as Purchase_Count 
          FROM transaction_table 
          GROUP BY Add_on 
          ORDER BY Purchase_Count DESC;"

sqldf(Query6)


##########
##Query7## 
##########
Query7 <- "SELECT t.`Shipping.Type`, COUNT(*) AS Shipping_Count
          FROM transaction_table t
          JOIN customer_table c
          ON t.`Customer.ID` = c.`Customer.ID`
          WHERE c.`Loyalty.Member` = 'Yes'
          GROUP BY t.`Shipping.Type`
          ORDER BY Shipping_Count DESC;"
sqldf(Query7)

Query7.1 <- "SELECT t.`Shipping.Type`, COUNT(*) AS Shipping_Count
          FROM transaction_table t
          JOIN customer_table c
          ON t.`Customer.ID` = c.`Customer.ID`
          WHERE c.`Loyalty.Member` = 'No'
          GROUP BY t.`Shipping.Type`
          ORDER BY Shipping_Count DESC;"

sqldf(Query7.1)


##########
##Query8## 
##########
Query8 <- "SELECT `Product.Type`, COUNT(*) AS Number_of_Cancelled_Orders, SUM(`Total.Price`) AS Total_Cancelled_Value
          FROM transaction_table
          WHERE `Order.Status` = 'Cancelled'
          GROUP BY `Product.Type`
          ORDER BY Total_Cancelled_Value DESC;"

sqldf(Query8)


##########
##Query9## 
##########

Query9 <- "SELECT CASE WHEN Age < 20 THEN 'Under 20'
          WHEN Age BETWEEN 20 AND 29 THEN '20-29'
          WHEN Age BETWEEN 30 AND 39 THEN '30-39'
          WHEN Age BETWEEN 40 AND 49 THEN '40-49'
          WHEN Age BETWEEN 50 AND 59 THEN '50-59'
          WHEN Age BETWEEN 60 AND 69 THEN '60-69'
          WHEN Age BETWEEN 70 AND 79 THEN '70-79'
          ELSE '80 and above'
          END AS Age_Group, SUM(t.`Total.Price`) AS Total_Sales
          FROM customer_table c
          JOIN transaction_table t 
          ON c.`Customer.ID` = t.`Customer.ID`
          GROUP BY Age_Group
          ORDER BY Total_Sales DESC;"

sqldf(Query9)


##########
##Query10# 
##########
Query10 <- "SELECT c.`Loyalty.Member`, SUM(t.`Total.Price`) AS Total_Sales
            FROM customer_table c
            JOIN transaction_table t 
            ON c.`Customer.ID` = t.`Customer.ID`
            GROUP BY c.`Loyalty.Member`;"

sqldf(Query10)

##########
##Query11# 
##########
Query11 <- "SELECT c.`Gender`, t.`Product.Type`, SUM(t.`Total.Price`) AS Total_Sales
            FROM customer_table c
            JOIN transaction_table t 
            ON c.`Customer.ID` = t.`Customer.ID`
            GROUP BY c.`Gender`, t.`Product.Type`
            ORDER BY Total_Sales DESC;"

sqldf(Query11)



##########
##Query12# 
##########
Query12 <- "SELECT `Customer.ID`, COUNT(SKU) AS Number_of_Purchases, 
            SUM(`Total.Price`) AS Total_Spending
            FROM cleaned
            GROUP BY `Customer.ID`
            HAVING Number_of_Purchases > 1
            ORDER BY Total_Spending DESC
            ;"

sqldf(Query12)


###############################################################
##Visualization 1; Create a pie chart for gender distribution##
###############################################################
gender_counts <- table(cleaned$Gender)
gender_df <- as.data.frame(gender_counts)

ggplot(gender_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Gender Distribution",
       x = "",
       y = "") +
  theme_void() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(title = "Gender"))

# Create a bar chart for loyalty member distribution
ggplot(cleaned, aes(x = Loyalty.Member)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Loyalty Member Membership",
       x = "Loyalty Member",
       y = "Count") +
  theme_minimal()

# Create a box plot for age distribution and facet wrap by gender
ggplot(cleaned, aes(x = "", y = Age, fill = Gender)) +
  geom_boxplot(color = "black") +
  labs(title = "Age Distribution by Gender",
       x = "",
       y = "Age") +
  theme_minimal() +
  facet_wrap(~ Gender) +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink"))

#Histogram of customer age distribution
ggplot(cleaned, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "light blue", color = "black") +
  labs(title = "Age Distribution of Customers", x = "Age", y = "Frequency") +
  theme_minimal()

#Histogram of customer age distribution by gender
ggplot(cleaned, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "white") +
  labs(title = "Age Distribution of Customers", x = "Age", y = "Frequency") +
  facet_wrap(~ Gender) +
  theme_minimal()

#####################################################################
##Visualization 2; Create a violin plot of total spending by gender##
#####################################################################
# Create a box plot to show the spending difference by gender
ggplot(cleaned, aes(x = Gender, y = `Total.Price`, fill = Gender)) +
  geom_violin() +
  labs(title = "Spending Difference by Gender",
       x = "Gender",
       y = "Total Price") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink"))

##########################################################################
##Visualization 3; Create a bar chart of average spending distribution by 
##loyalty membership######################################################
##########################################################################
# Calculate the average spending amount by loyalty membership
avg_spending_by_loyalty <- aggregate(Total.Price ~ Loyalty.Member, cleaned, mean)

# Create a bar plot to show the average spending difference by loyalty membership
ggplot(avg_spending_by_loyalty, aes(x = Loyalty.Member, y = Total.Price, fill = Loyalty.Member)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average Spending by Loyalty Membership",
       x = "Loyalty Member",
       y = "Average Total Price") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "lightgreen"))

#Calculate average amount spent grouped by loyalty member or not
cleaned %>%
  select(Loyalty.Member, Total.Price) %>%
  filter(Loyalty.Member == "Yes") %>%
  summarize(member_average_purchases = mean(Total.Price))
#3138.011

cleaned %>%
  select(Loyalty.Member, Total.Price) %>%
  filter(Loyalty.Member == "No") %>%
  summarize(nonmember_average_purchases = mean(Total.Price))
#3191.975


##########################################################################
##Visualization 4; Create a line for total/average spending distribution##
##########################################################################
#Load scales and lubridate package
library(scales)
library(lubridate)

# Convert Purchase Date to Date type (cleaning part 3)
cleaned$Purchase.Date <- ymd(cleaned$Purchase.Date)

# Extract the month and year from the Purchase Date
cleaned$Month <- format(cleaned$Purchase.Date, "%b %Y")

# Group by month and calculate average sales
monthly_sales <- aggregate(Total.Price ~ Month, cleaned, mean)

# Convert Month to a factor with the correct levels
monthly_sales$Month <- factor(monthly_sales$Month, levels = c("Sep 2023", "Oct 2023", "Nov 2023", "Dec 2023", 
                                                              "Jan 2024", "Feb 2024", "Mar 2024", "Apr 2024", 
                                                              "May 2024", "Jun 2024", "Jul 2024", "Aug 2024", 
                                                              "Sep 2024"))

# Create a line chart of average monthly sales
ggplot(monthly_sales, aes(x = Month, y = Total.Price, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Monthly Sales",
       x = "Month",
       y = "Average Sales") +
  scale_y_continuous(labels = comma) +  # Format y-axis with commas
  scale_x_discrete(drop = FALSE) +  # Ensure all months are listed on the x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Group by month and calculate total sales
monthly_total_sales <- aggregate(Total.Price ~ Month, cleaned, sum)

# Convert Month to a factor with the correct levels
monthly_total_sales$Month <- factor(monthly_total_sales$Month, levels = c("Sep 2023", "Oct 2023", "Nov 2023", "Dec 2023", 
                                                                          "Jan 2024", "Feb 2024", "Mar 2024", "Apr 2024", 
                                                                          "May 2024", "Jun 2024", "Jul 2024", "Aug 2024", 
                                                                          "Sep 2024"))

# Create a line plot of total monthly sales
ggplot(monthly_total_sales, aes(x = Month, y = Total.Price, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Total Monthly Sales",
       x = "Month",
       y = "Total Sales") +
  scale_y_continuous(labels = comma) +  # Format y-axis with commas
  scale_x_discrete(drop = FALSE) +  # Ensure all months are listed on the x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#############################################################################
##Visualization 5; Create a bar chart of average spending by payment method##
#############################################################################
# Calculate the average spending amount by payment method
avg_spending_by_payment <- aggregate(Total.Price ~ Payment.Method, cleaned, mean)

# Create a bar plot to show the average spending difference by payment method
ggplot(avg_spending_by_payment, aes(x = Payment.Method, y = Total.Price, fill = Payment.Method)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average Spending by Payment Method",
       x = "Payment Method",
       y = "Average Total Price") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#############################################################################
##Visualization 6; Create a bar chart of the most popular product types######
#############################################################################
# Group by Product Type and sum the Quantity sold
cleaned %>%
  select(Product.Type) %>%
  count(Product.Type)

product_sales <- cleaned %>%
  group_by(Product.Type) %>%
  summarise(Total.Sales = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(Total.Sales))  # Sort by Total Sales in descending order

# Create a bar chart of product sales by type
ggplot(product_sales, aes(x = Product.Type, y = Total.Sales, fill = Product.Type)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total Sales by Product Type", x = "Product Type", y = "Total Quantity Sold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################################################################
##Visualization 7; Create a bar chart about product type revenue distribution##
###############################################################################
# Create a lookup table for SKUs and product types
sku_lookup <- data.frame(
  SKU = c("SKU1001", "SKU1002", "SKU1003", "SKU1004", "SKU1005", "SMP234", "TBL345", "SWT567", "LTP123", "HDP456"),
  Product_Type = c("Smartphone", "Tablet", "Smartwatch", "Smartphone", "Laptop", "Smartphone", "Tablet", "Smartwatch", "Laptop", "Headphones"))

# Merge the dataset with the lookup table
data_with_product_type <- cleaned %>%
  left_join(sku_lookup, by = "SKU")

# Group by Product_Type and sum total revenue
revenue_by_product_type <- data_with_product_type %>%
  group_by(Product_Type) %>%
  summarise(Total_Revenue = sum(Total.Price, na.rm = TRUE)) %>%
  arrange(desc(Total_Revenue))

# Create a bar chart with products on the x-axis and whole numbers on the y-axis
ggplot(revenue_by_product_type, aes(x = reorder(Product_Type, Total_Revenue), y = Total_Revenue, fill = Product_Type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  labs(title = "Revenue by Product Type", x = "Product Type", y = "Total Revenue") +
  theme_minimal() +
  theme(legend.position = "none")



###############################################################################
##Visualization 8; Average rating of products #################################
###############################################################################
# Calculate the average rating for each product
avg_rating_product <- cleaned %>%
  group_by(Product.Type) %>%
  summarise(Average.Rating = mean(Rating, na.rm = TRUE))

# Create a ggplot visualization (dot chart)
ggplot(avg_rating_product, aes(x = Average.Rating, y = Product.Type)) +
  geom_point(size = 3) +
  ggtitle("Average Rating of Each Product") +
  xlab("Average Rating") +
  ylab("Product Type") +
  theme_minimal()


###############################################################################
##Visualization 9; Distribution of add-ons purchased ##########################
###############################################################################
# Split the Add.ons.Purchased column into individual add-ons and remove extra spaces
#Same code as Query6
transaction_table <- transaction_table %>%
  separate_rows(Add.ons.Purchased, sep = ",") %>%
  mutate(Add.ons.Purchased = trimws(Add.ons.Purchased)) %>%
  filter(Add.ons.Purchased != "")

# Count the number of times each add-on was purchased
add_on_counts <- transaction_table %>%
  group_by(Add.ons.Purchased) %>%
  summarise(Purchase_Count = n()) %>%
  arrange(desc(Purchase_Count))

# Create a histogram
ggplot(add_on_counts, aes(x = reorder(Add.ons.Purchased, Purchase_Count), y = Purchase_Count)) +
  geom_histogram(stat = "identity", fill = "skyblue", color = "black") +
  ggtitle("Number of Times Each Add-on Was Purchased") +
  xlab("Add-on") +
  ylab("Purchase Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################################################################
##Visualization 10; Heat map of Shipping Types and Purchase Date ##############
###############################################################################
# Extract year and month from 'Purchase Date'
visual10 <- cleaned %>%
  mutate(Year = year(Purchase.Date),
         Month = month(Purchase.Date, label = TRUE))

# Filter out data for the year 2023
cleaned_visual10 <- visual10 %>%
  filter(Year != 2023)

# Create a pivot table to count the number of purchases for each shipping type by month
pivot_table <- cleaned_visual10 %>%
  group_by(Shipping.Type, Month) %>%
  summarise(Purchase_Count = n()) %>%
  spread(Month, Purchase_Count, fill = 0)

# Convert the pivot table to long format for ggplot2
pivot_long <- pivot_table %>%
  gather(key = "Month", value = "Purchase_Count", -Shipping.Type)

# Ensure the months are in the correct order of Jan 2024 - Sept 2024
pivot_long$Month <- factor(pivot_long$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))

# Create the heat map
ggplot(pivot_long, aes(x = Month, y = Shipping.Type, fill = Purchase_Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Shipping Types and Purchase Date",
       x = "Month",
       y = "Shipping Type",
       fill = "Purchase Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################################################################
##Visualization 11; Pie chart distribution of completed and cancelled orders ##
###############################################################################
# Group by order status and count occurrences
order_status_counts <- transaction_table %>%
  group_by(Order.Status) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Calculate percentages
order_status_counts <- order_status_counts %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a pie chart of order status distribution with percentages
pie(order_status_counts$Count, labels = paste(order_status_counts$Order.Status, 
                                              round(order_status_counts$Percentage, 1), "%"), 
    main = "Order Status Distribution", col = rainbow(length(order_status_counts$Order.Status)))


###############################################################################
##Visualization 12; Total Sales By Age Group Bar Graph ########################
###############################################################################

# Categorize customers into age groups
merged_data <- cleaned %>%
  mutate(Age.Group = cut(Age, breaks = c(0, 19, 29, 39, 49, 59, 69, 79, Inf),
                         labels = c("Under 20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80 and above")))

# Calculate total sales for each age group
age_group_sales <- merged_data %>%
  group_by(Age.Group) %>%
  summarise(Total.Sales = sum(Total.Price, na.rm = TRUE))

# Plot the data using ggplot
ggplot(age_group_sales, aes(x = Age.Group, y = Total.Sales, fill = Age.Group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Sales by Age Group",
       x = "Age Group",
       y = "Total Sales") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################
##Visualization 13; Scatter Plot of total number of purchases and total spending#
###############################################################################
# Summarize data to get total number of purchases and total amount spent per customer
customer_summary <- cleaned %>%
  group_by(Customer.ID) %>%
  summarize(TotalPurchases = n(),
            TotalSpent = sum(Total.Price))

# Scatter plot of Total Purchases vs. Total Spent
ggplot(customer_summary, aes(x = TotalPurchases, y = TotalSpent)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Total Purchases vs. Total Spent",
       x = "Total Number of Purchases",
       y = "Total Amount Spent") +
  theme_minimal()


#Correlation for conclusion
cor(cleaned$Age, cleaned$Total.Price)

