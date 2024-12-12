# Data Cleaning
colnames(supermarket_sales)=supermarket_sales[1,]

supermarket_sales= supermarket_sales[-1,]

supermarket_sales$`Unit price` <- as.numeric(supermarket_sales$`Unit price`)

supermarket_sales$Quantity=as.numeric(supermarket_sales$Quantity)

supermarket_sales$total_sales_revenue=
  supermarket_sales$`Unit price`* supermarket_sales$Quantity
# Question 1 What is the total sales revenue per city ?
total_sales_per_city=
  aggregate(
    x=supermarket_sales$total_sales_revenue,
    by=list(supermarket_sales$City),
    FUN=sum
  )

library(ggplot2)
ggplot(data=total_sales_per_city,
       aes(x=Group.1,y=x))+
  geom_bar(stat="identity",fill='orange',color='skyblue')+
labs(title = 'city wise sales',
     xlab= 'city',
     ylab= 'sales'
     )
# Question 2 Which product line generates the most revenue?

Product_line_most_revenue= aggregate(
  x= supermarket_sales$total_sales_revenue,
  by=list(supermarket_sales$`Product line`),
  FUN= sum
)

library(ggplot2)
ggplot(data = Product_line_most_revenue, 
       aes(x=Group.1,y=x))+
  geom_bar(stat= 'identity', 
           fill= c('red','black','orange','yellow','purple','skyblue'))+
  labs(title = 'sales per product line',
       x='Product line',
       y='Total Revenue' )

#Question 3 How does sales revenue vary by customer type?
  
Sales_revenue_variation= aggregate(
  x=supermarket_sales$total_sales_revenue,
  by=list(supermarket_sales$`Customer type`),
  FUN= sum
)     

ggplot(data=Sales_revenue_variation,
       aes(x=Group.1,y=x))+
  geom_bar(stat = 'identity',fill=c('red','blue'))+
  labs(
    title = 'Member vs Normal'
  )
  
## Question 4  What is the average spending per customer for each gender?

avg_sales_gender = aggregate(
  x=supermarket_sales$total_sales_revenue,
  by=list(supermarket_sales$Gender),
  FUN=mean
)
colnames(avg_sales_gender)=c('Gender','Sales') 

ggplot(data=avg_sales_gender,
         aes(x=Gender,y=Sales))+
    geom_bar(stat = 'identity',width= 0.4,fill = c('pink','skyblue'))+
  labs(title = "Sales as per Gender")

## Question 5 Are there peak sales hours during the day?

supermarket_sales$Hour= format(strptime
  (supermarket_sales$Time,format="%H:%M"), "%H")

Sales_per_Hour=aggregate(
  x=supermarket_sales$total_sales_revenue,
  by=list(supermarket_sales$Hour),
  FUN=sum
)

colnames(Sales_per_Hour)=c("Hour","Revenue")
library(ggplot2)

ggplot(data=Sales_per_Hour,
       aes(x=Hour,y=Revenue))+
  geom_bar(stat='identity',width = 0.4)+
  labs(titie="Peak Hour of Sales")

# Question 6  How do sales trends vary across months?

supermarket_sales$Date <- as.Date(supermarket_sales$Date, format = "%m/%d/%Y")

supermarket_sales$Month= format(supermarket_sales$Date,"%B")

Sales_per_month=aggregate(
  x=supermarket_sales$total_sales_revenue,
  by=list(supermarket_sales$Month),
  FUN=sum
)
colnames(Sales_per_month)=c("Month","Revenue")

ggplot(data = Sales_per_month,
       aes(x=Month,y=Revenue))+
  geom_bar(stat="identity",width=0.4,fill=c('red','black','blue'))
                               


