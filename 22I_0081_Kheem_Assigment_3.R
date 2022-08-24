
# Read a text file 
df <- read.delim(file.choose(), sep =",", header = TRUE, dec =".")

#view head of data
head(df)

#view tail of data
tail(df)

# checking the class of date and convert it into class date
class(df$Visit_Date)

Date <- as.Date(df$Visit_Date, format = "%m/%d/%y")
class(Date)

class(df$Total_Purchases_In_USD)
class(df$CustomerID)

#load plyr 
library(plyr)
library(lubridate)


#sort Date column
srtDate =  sort(Date)

# create dataframe of weeks from dates

wk <- lubridate::week(ymd(srtDate))
wk <- sort(wk)


# Reference date i.e last date of 5th week (20/10/2020) 
reference_Date = "20-10-2020"
reference_Date = as.Date(reference_Date)
class(reference_Date)

# indexing the data before reference date
bfr = df$Customer_ID[1:904389]

#calculating total revenue 
total_revenue <- sum((df$Total_Purchases_In_USD[1:904389]))
total_revenue


#calculating max purchase in a day
newdata <- subset(df, Total_Purchases_In_USD != 0) # new data is without missing values
max_purchase = max(newdata$Total_Purchases_In_USD)
max_purchase = df %>% 
  group_by(Total_Purchases_In_USD) %>% 
  summarise(uniqueid = n_distinct(srtDate))
max_purchase

#calculating min purchase in a day
min_purchase =  min(newdata$Total_Purchases_In_USD)
min_purchase

#total visit days
library(dplyr)
visitDays = df %>% 
  group_by(df$Visit_Date) %>% 
  summarise(uniqueid = n_distinct(df$CustomerID))

total_visit = sum(df$Total_Purchase_In_USD == 0)
total_visit = 1058198 - total_visit 

# standard deviation in sales
sd_sales = sd(newdata$Total_Purchases_In_USD, na.rm = TRUE)

#Week one total sales
#wk_1 = (df$Week_Number == '38')
#wk_1 = df$Total_Purchase_In_USD[1:153390]
#wk_1 = sum(wk_1)
#wk_1

#Week two total sales
#wk_2 = (df$Week_Number == '39')
#wk_2 = df$Total_Purchase_In_USD[153390:333168]
#wk_2 = sum(wk_2)
#wk_2

#Week three total sales
#wk_3 = (df$Week_Number == '40')
#wk_3 = df$Total_Purchase_In_USD[333168:540209]
#wk_3 = sum(wk_3)
#wk_3


#Week four total sales
#wk_4 = (df$Week_Number == '41')
#wk_4 = df$Total_Purchase_In_USD[540209:723856]
#wk_4 = sum(wk_4)
#wk_4

#Week five total sales
#wk_5 = (df$Week_Number == '42')
#wk_5 = df$Total_Purchase_In_USD[723856:904390]
#wk_5 = sum(wk_5)
#wk_5



#sum(df$Total_Purchase_In_USD)
#create Detailed DataSet 
data = data.frame(CUSTOMER_ID = 1:1058198,
                  Visit_Date = srtDate,
                  Customer_ID = df$CustomerID,
                  Total_Purchase_In_USD = df$Total_Purchases_In_USD,
                  Week_Number = wk,
                  Total_Revenue = total_revenue,
                  Max_Purchase_In_Day = max_purchase,
                  Min_Purchase_In_Day = min_purchase,
                  Total_Visit_Days = total_visit,
                  Standard_Deviation_In_Sales = sd_sales
                  #W1_Total_Sales = wk_1,
                  #W2_Total_Sales = wk_2,
                  #W3_Total_Sales = wk_3,
                  #W4_Total_Sales = wk_4,
                  #W5_Total_Sales = wk_5
                  )


#Task 1 Calculate the week with highest earning
print("Week three is with highest earning")
wk_3

#Task 2 Identifying the most valued customer who purchased most
value_Customer = max(data$Total_Purchase_In_USD)
value_Customer
print("The most valued person is from week 5 and Customer ID is 813141")


#Task 3 categorize the customer in three groups i.e Poor, Medicore and Rich
Categories <- cut(data$Total_Purchase_In_USD, breaks = 3, labels = c("Poor","Medicore","Rich"))
table(Categories)



write.csv(data, "Detailed_DataSet.csv")
newda = read.csv("Detailed_DataSet.csv")
