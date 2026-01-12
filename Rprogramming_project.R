# loading the libraries

library(tidyverse)
library(lubridate)

# loading in the dataset
retail <- read.csv("online_retail.csv")

#cleaning the dataset
retail <- retail %>% drop_na(CustomerID, Description)
retail <- retail %>% filter(Quantity > 0)

retail <- read.csv("online_retail.csv", stringsAsFactors = FALSE)
retail$InvoiceDate <- dmy_hms(retail$InvoiceDate)

str(retail$InvoiceDate)
head(retail$InvoiceDate, 20)
retail$InvoiceDate <- parse_date_time(retail$InvoiceDate,
                                      orders = c("mdy HM", "mdy HMS",
                                                 "dmy HM", "dmy HMS",
                                                 "ymd HM", "ymd HMS"),
                                      exact = FALSE)
sum(is.na(retail$InvoiceDate))
head(retail$InvoiceDate)
head(retail$InvoiceDate, 20)

# removing extra spaces from description column
retail$Description <- str_trim(retail$Description)

# Adding total sales column
retail <- retail %>% mutate(Total_Sales = Quantity * UnitPrice)

# extracting month and year for analysis
retail <- retail %>% mutate(Month = month(InvoiceDate,label=TRUE),
                            Year = year(InvoiceDate))
head(retail)

#Exploratory Data Analysis
## Total sales
sum(retail$Total_Sales)

## Top 10 products
top_products <- retail %>%
  group_by(Description) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)
top_products

retail <- retail %>%
  mutate(Total_Sales = Quantity * UnitPrice)
colnames(retail)

## sales by country
sales_by_country <- retail %>%
  group_by(Country) %>%
  summarise(Total_Sales = sum(Total_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))
head(sales_by_country)

##sales over time
sales_by_date <- retail %>%
  group_by(InvoiceDate) %>%
  summarise(Daily_Sales = sum(Total_Sales))

ggplot(sales_by_date, aes(x = InvoiceDate, y=Daily_Sales)) +
  geom_line(col = "blue") +
  labs(title = "Daily Sales Over Time", x="Date", y="Sales")

## customer segmentation
library(arules)
transactions <- as(split(retail$StockCode, retail$InvoiceNo),
                   "transactions")
rules <- apriori(transactions, parameter = list(supp = 0.01,
                                                conf = 0.5))
inspect(head(sort(rules, by="lift"), 10))

## monthly/yearly sales trends
monthly_sales <- retail %>%
  group_by(Year, Month) %>%
  summarise(monthly_sales = sum(Total_Sales, na.rm = TRUE))

#visualization
ggplot(monthly_sales, aes(x=Month, y=monthly_sales, group = Year,
                          col=factor(Year))) +
  geom_line() +
  labs(title = "Monthly Sales Trend", x="Month", y="Sales")


# key insights
From the analysis that was done, it was found that the 
top 3 best-selling products were Dotcom postage, Regency 
cakestand 3 tier, and Paper craft, little birdie. While the top 3
most performing countries were the UK, Netherlands, and Eire. 
Furthermore, the peak selling months were found to be October & January.  
Lastly, sales trend sky rocketed during the months of October and November,
but fell during the month of December.