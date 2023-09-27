### load in the required libraries 

```{r}
library(readr)
library(tidyverse)
library(here)
library(dplyr)
library(tidyr)
library(RColorBrewer)
```

### read in the data using here() function

```{r}
state_milk_production <- read_csv(here("data/state_milk_production.csv"))
clean_cheese <- read_csv(here("data/clean_cheese.csv"))
fluid_milk_sales <- read_csv(here("data/fluid_milk_sales.csv"))
milk_products_facts <- read_csv(here("data/milk_products_facts.csv"))
milkcow_facts <- read_csv(here("data/milkcow_facts.csv"))
```

### disable scientific notation

```{r}
options(scipen = 999)
```

## coord_flip x,y flip

## Figure 1
Make a scatter plot showing the milk production in pounds versus the average number of milk cows using milkcow_facts. Write a caption for this plot.

```{r}
## 
milkcow_facts %>%
  ggplot(aes(x = avg_milk_cow_number/1e6, y = milk_production_lbs/1e6,color= year)) + #The x-axis will be the number of milk cows (in millions) and the y-axis will be milk production (in millions of pounds)
  geom_point() + theme_classic() + #plot the data 
  ggtitle("milk production vs. milk cow numbers") + #label the axes and title the plot
  xlab("average milk production (in million pounds)") 
ylab("average number of milk cows (in million)")

```

## Figure 2
Examine the milk total milk production in each region over time using state_milk_production. We will plot line plots for the milk production versus the year faceted by region. Write a caption for this plot.
```{r}
new_state_milk_production <- 
  state_milk_production %>%
  group_by(region, year) %>% #takes the existing table and converts it into a grouped table
  summarize(totalp = sum(milk_produced)) #total production of milk each year by region 

ggplot(new_state_milk_production, aes(x = year, y = totalp/1e6, color = region)) +
  geom_line() +  
  labs(x = "Year", y = "Total Milk Production (millions of pounds)", title = "Total Milk Production by Region Over Time") + #lable the axes and title the plot
  facet_wrap(~ region, ncol = 2, scales = "free") +   #Facet the plot by region
  theme(legend.position = "none")  #remove the legend
```

## Figure 3
Make a line plot showing the amount (in pounds) of each milk products sold over time using fluid_milk_sales. Write a caption for this plot.

```{r}
new_fluid_milk_sales <- 
  fluid_milk_sales %>%
  data.frame(x=log(fluid_milk_sales$year), y=log(fluid_milk_sales$pounds)) #log scale the data
ggplot(new_fluid_milk_sales, aes(x = x, y = y, color = milk_type)) +
  geom_line() +  # Add lines
  labs(x = "Time", y = "Pounds of Product", title = "Milk Product Sales Over Time")  #label the axes & add the title for the plots
```

## Figure 4
Make a stacked bar plot of the different type of cheeses consumed over time using a long format version of clean_cheese that you create. Write a caption for this plot.

```{r}
clean_cheese_long <- 
  clean_cheese%>%
  select(-c(`Total American Chese`,`Total Italian Cheese`,`Total Natural Cheese`,`Total Processed Cheese Products`)) #remove all the total variable

new_clean_cheese_long <- 
  pivot_longer(clean_cheese_long, cols=-Year, names_to = "CheeseType", values_to = "Pounds") #change the data to long format and store the data

new_clean_cheese_long%>%
  ggplot(aes(x = Year, y = Pounds, fill = CheeseType)) + #plot the data, x is the time and y is the pounds of cheese, color the plot by different types of cheese
  geom_bar(stat = "identity") +
  labs(x = "Time", y = "Pounds of Cheese", title = "cheese consumption Over Time") +
  scale_fill_brewer(palette = "Paired")  # Change color by using RColorBrewer

```

## Figure 5
Time to be creative! Make an original figure using the data. Join two of the dataframes together and use variables from both sides of the join to make this figure. This figure should tell a story and be easy to understand (just as the figures above are!). Change the colors of this plot from the default ggplot2 colors. Write a caption for this plot.

```{r}

```
