
library(AzureML)
library(dplyr)
library(ggplot2)
library(scales)

install.packages("xts") # You also need to install the xts package
library(xts)

install.packages("broom") # And the broom package
library(broom)

library("AzureML")
ws <- workspace()
ordersDataSet <- download.datasets(ws, "Wide World Importers")
head(ordersDataSet)

ordersDataSet %>%
  select(StockItemID, Description) %>%
  distinct() -> stockItemsDataSet

head(stockItemsDataSet)

ordersDataSet %>%
  select(StockItemID, Quantity) %>%
  group_by(StockItemID) %>%
  summarize(numOrdered = sum(Quantity)) %>% 
  arrange(desc(numOrdered)) %>%
  filter(row_number() <= 20) -> numOrderedDataSet

head(numOrderedDataSet)

results <- inner_join(numOrderedDataSet, stockItemsDataSet, by = "StockItemID")

head(results)

options(repr.plot.width=10, repr.plot.height=8)  
ggplot(results) +
  geom_point(mapping = aes(x = reorder(Description, -numOrdered), y = numOrdered), color = "red", size = 5) +
  labs(x = "Product", y = "Total Ordered") +
  scale_x_discrete(labels = function(x) { lapply(strwrap(x, width = 25, simplify = FALSE), paste, collapse = "\n")}) +
  theme(axis.text.x = element_text(angle = 90, size = 10))

ordersDataSet %>%
  filter(StockItemID == 191) -> productSalesDataSet

head(productSalesDataSet)
str(productSalesDataSet)

timeSeriesOrders <- xts(productSalesDataSet[c("StockItemID", "Quantity")], 
                        order.by = as.POSIXct(productSalesDataSet$PickingCompletedWhen, format = "%m/%d/%Y"))
head(timeSeriesOrders)

dailyTotals <- apply.daily(timeSeriesOrders, function(x){ apply(x$Quantity, 2, sum)})

head(dailyTotals)

graphData <- tidy(dailyTotals)
head(graphData)

graphData$index <- as.POSIXct(graphData$index, origin="1970-01-01")
head(graphData)

options(repr.plot.width=15, repr.plot.height=5)
ggplot(graphData, aes(x = index, y = value)) +
  ggtitle("Product Sales Over Time for Product 191") +
  xlab("Date") +
  ylab("Units Sold") +
  scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(color = "blue")

firstDay <- as.POSIXct("2013-01-01")
lastDay <- as.POSIXct("2013-12-31")
day1 <- as.numeric(firstDay)
dayN <- as.numeric(lastDay)
productSalesFor2013 <- filter(graphData, (index >= day1) & (index <= dayN))

options(repr.plot.width=15, repr.plot.height=5)
ggplot(productSalesFor2013, aes(x = index, y = value)) +
  ggtitle("Product Sales for Product 191 in 2013") +
  xlab("Date") +
  ylab("Units Sold") +
  scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(color = "blue")

ggplot(productSalesFor2013, aes(x = index, y = value)) +
  ggtitle("Product Sales for Product 191 in 2013") +
  xlab("Date") +
  ylab("Units Sold") +
  scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(color = "black", alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 9))

firstDay <- as.POSIXct("2014-01-01")
lastDay <- as.POSIXct("2014-12-31")
day1 <- as.numeric(firstDay)
dayN <- as.numeric(lastDay)
productSalesFor2014 <- filter(graphData, (index >= day1) & (index <= dayN))

ggplot(productSalesFor2014, aes(x = index, y = value)) +
  ggtitle("Product Sales for Product 191 in 2014") +
  xlab("Date") +
  ylab("Units Sold") +
  scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(color = "black", alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 9))

firstDay <- as.POSIXct("2015-01-01")
lastDay <- as.POSIXct("2015-12-31")
day1 <- as.numeric(firstDay)
dayN <- as.numeric(lastDay)
productSalesFor2015 <- filter(graphData, (index >= day1) & (index <= dayN))

ggplot(productSalesFor2015, aes(x = index, y = value)) +
  ggtitle("Product Sales for Product 191 in 2015") +
  xlab("Date") +
  ylab("Units Sold") +
  scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(color = "black", alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 9))
