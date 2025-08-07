## -----------------------------------------------------------------------------------------------------------------
# Load readr, dplyr, forcats, ggplot2
library("readr")
library("dplyr")
library("forcats")
library("ggplot2")


## -----------------------------------------------------------------------------------------------------------------
# Set the default figure font size to 20 (and use the gray theme for plot colors)
theme_set(theme_gray(20))

# Display plots in the workspace with a width of 10 inches and a height of 7 inches
opts <- options(repr.plot.width = 10, repr.plot.height = 8)


## ----setup--------------------------------------------------------------------------------------------------------
# Read the CSV file "data/vgsales.csv"
vgsales <- read.csv("data/vgsales.csv")

# Glimpse the result
glimpse(vgsales)


## -----------------------------------------------------------------------------------------------------------------
# Using vgsales, filter for rows where Name equals "Grand Theft Auto V"
df <- vgsales %>%
	filter(Name == "Grand Theft Auto V")


## -----------------------------------------------------------------------------------------------------------------
# Using vgsales, group by Name, then summarize to calculate Total_Global_Sales as the sum of Global_Sales
global_sales_by_name <- vgsales %>%
	group_by(Name) %>%
	summarize(Total_Global_Sales = sum(Global_Sales))


# Slice global_sales_by_name for top 10 rows by maximum Total_Global_Sales
top_global_sales_by_name <- global_sales_by_name %>%
	slice_max(order_by = Total_Global_Sales, n=10)


# See the result
top_global_sales_by_name


## -----------------------------------------------------------------------------------------------------------------
# Using top_global_sales_by_name, plot Total_Global_Sales versus Name
# Add a column geom
ggplot(top_global_sales_by_name, aes(x= Name, y= Total_Global_Sales))+
	geom_col() + 
	theme_minimal()


## -----------------------------------------------------------------------------------------------------------------
# Redraw the previous plot
# Use flipped coordinates
ggplot(top_global_sales_by_name, aes(x= Total_Global_Sales, y= Name))+
	geom_col(fill="lightblue") + 
	theme_minimal()


## -----------------------------------------------------------------------------------------------------------------
# Mutate top_global_sales_by_name so Name is reordered by Total_Global_Sales
top_global_sales_by_name_ordered <- top_global_sales_by_name %>%
	mutate(Name = fct_reorder(Name, Total_Global_Sales))
	
head(top_global_sales_by_name_ordered)
# Redraw the previous plot
ggplot(top_global_sales_by_name_ordered, aes(x= Total_Global_Sales, y= Name))+
	geom_col(fill="lightblue") + 
	theme_minimal()


## -----------------------------------------------------------------------------------------------------------------
# Using vgsales, filter for rows where Platform_Generation is equal to "7th"
seventh_generation <- vgsales %>%
	filter(Platform_Generation ==  "7th")



# See the result
seventh_generation


## -----------------------------------------------------------------------------------------------------------------
# Using seventh_generation,  
# group by Year,  
# then summarize to calculate Total_Global_Sales as the sum of Global_Sales
total_7th_gen_global_sales_by_year <- seventh_generation %>%
	group_by(Year) %>%
	summarize(Total_Global_Sales = sum(Global_Sales)) %>%
	arrange(desc(Year))


# See the result
total_7th_gen_global_sales_by_year


## -----------------------------------------------------------------------------------------------------------------
# Using total_7th_gen_global_sales_by_year, plot Total_Global_Sales versus Year
# Add a line geom with size 2
ggplot(total_7th_gen_global_sales_by_year, aes(x= Year, y= Total_Global_Sales)) +
	geom_line(linewidth = 2) +
	labs(x = "Year", y= "Total Global Sales") 



## -----------------------------------------------------------------------------------------------------------------
# Using seventh_generation,  
# group by Year and Platform,  
# then summarize to calculate Total_Global_Sales as the sum of Global_Sales
total_7th_gen_global_sales_by_year_platform <- seventh_generation %>%
	group_by(Year, Platform) %>%
	summarize(Total_Global_Sales = sum(Global_Sales), .groups="drop")

head(total_7th_gen_global_sales_by_year_platform)

# Using total_7th_gen_global_sales_by_year_platform, plot Total_Global_Sales versus Year, colored by Platform.
# Add a line geom with size 2
ggplot(total_7th_gen_global_sales_by_year_platform, aes(x= Year, y= Total_Global_Sales, color=Platform)) +
	geom_line(linewidth = 2) +
	labs(x = "Year", y= "Total Global Sales") 



## -----------------------------------------------------------------------------------------------------------------
# Using vgsales, 
# group by Year and Platform,  
# then summarize to calculate Total_Global_Sales as the sum of Global_Sales
total_global_sales_by_year_platform <- vgsales %>%
	group_by(Year, Platform) %>%
	summarise(Total_Global_Sales = sum(Global_Sales), .groups="drop")


# Using total_global_sales_by_year_platform, plot Total_Global_Sales versus Year, colored by Platform.
# Add a line geom with size 2

ggplot(total_global_sales_by_year_platform, aes(x= Year, y= Total_Global_Sales, color=Platform)) +
	geom_line(linewidth = 2) +
	labs(x = "Year", y= "Total Global Sales") 



## -----------------------------------------------------------------------------------------------------------------
# Using vgsales, 
# group by Year, Platform_Company and Platform_Generation, 
# then summarize to calculate Total_Global_Sales as the sum of Global_Sales
total_global_sales_by_year_platform <- vgsales %>%
	group_by(Year, Platform_Company, Platform_Generation) %>%
	summarize(Total_Global_Sales = sum(Global_Sales), .ungroup = "drop")


# Using total_global_sales_by_year_platform, plot Total_Global_Sales versus Year, colored by Platform_Company
# Add a line geom with size 2
# Facet the plot, wrapping by Platform_Generation.
ggplot(total_global_sales_by_year_platform, aes(x= Year, y= Total_Global_Sales, color=Platform_Company)) +
	geom_line(linewidth = 2) +
	labs(x = "Year", y= "Total Global Sales") +
	facet_wrap(~ Platform_Generation)




## -----------------------------------------------------------------------------------------------------------------
# Redraw the same plot, with 1 column in the facetting
ggplot(total_global_sales_by_year_platform, aes(x= Year, y= Total_Global_Sales, color=Platform_Company)) +
	geom_line(linewidth = 2) +
	labs(x = "Year", y= "Total Global Sales") +
	facet_wrap(~ Platform_Generation, ncol = 1)


