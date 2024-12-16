#Electronic Sales data set imported in global environment
#Shortening data set's name
final <- Electronic_sales_Sep2023.Sep2024

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
