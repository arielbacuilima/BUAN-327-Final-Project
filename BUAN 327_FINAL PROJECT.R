# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
CostReport_2021= read.csv("2021_CostReport.csv")

colnames(CostReport_2021)

#### Calculations:

#1. Calculate average revenue per nursing home
average_revenue <- CostReport_2021 %>%
  group_by(Facility.Name) %>%
  summarise(average_revenue = mean(Gross.Revenue, na.rm = TRUE))
average_revenue

#2. Calculate average net income per nursing home
average_net_income <- CostReport_2021 %>%
  group_by(Facility.Name) %>%
  summarise(avg_net_income = mean(Net.Income, na.rm = TRUE))
average_net_income

#3, Calculate profit margin per county
profit_margin <- CostReport_2021 %>%
  group_by(County) %>%
  summarise(profit_margin = (sum(Net.Income) / sum(Gross.Revenue)) * 100, .groups = "drop")
profit_margin

#4. Find the top 10 counties with the highest profit margin
top_profit_counties <- profit_margin %>%
  group_by(County) %>%
  summarise(avg_profit_margin = mean(profit_margin)) %>%
  arrange(desc(avg_profit_margin)) %>%
  slice_head(n = 10)

#5. Find the counties with the lowest number of beds available
low_bed_counties <- CostReport_2021 %>%
  group_by(County) %>%
  summarise(total_beds = sum(Number.of.Beds)) %>%
  arrange(total_beds) %>%
  slice_head(n = 10) 

#### Create 2 maps to show which counties could be better for investing in nursing homes
###6. Map 1: Profit Margin by county
library(tidyverse)
library(sf)
library(maps)

CostReport_2021 <- merge(CostReport_2021, profit_margin, by = "County", all.x = TRUE)

colnames(CostReport_2021)

# Load county map data
counties <- map_data("county")

# Prepare the data: clean and aggregate
CostReport_2021 <- CostReport_2021 %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Profit_Margin = sum(profit_margin, na.rm = TRUE))

# Prepare the map data: merge and clean
counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(CostReport_2021, by = c("subregion" = "County"))

# Plot the map
ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Profit_Margin)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Profit Margin") +
  labs(title = "Profit Margin by County") +
  theme_minimal()

###7. Map 2: Average length of stay by County
CostReport_2021= read.csv("2021_CostReport.csv")

# Load county map data
counties <- map_data("county")

# Prepare the data: clean and aggregate
CostReport_2021 <- CostReport_2021 %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Avg_length_Stay = sum(SNF.Average.Length.of.Stay.Total, na.rm = TRUE))

# Prepare the map data: merge and clean
counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(CostReport_2021, by = c("subregion" = "County"))

# Plot the map
ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Avg_length_Stay)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Avg length of Stay") +
  labs(title = "Average length of stay for patients") +
  theme_minimal()   

###8. Create a graph to show the relationship between length of stay and ratings

# Read datasets
CostReport_2021 <- read.csv("2021_CostReport.csv")
ratings <- read.csv("ProviderInfo_2021.csv")

# Merge datasets based on common identifier
merged_data <- merge(CostReport_2021, ratings, by.x = "Provider.CCN", by.y = "Federal.Provider.Number", all = TRUE)

# Extract required columns
data <- merged_data[, c("Facility.Name", "SNF.Average.Length.of.Stay.Total", "Overall.Rating")]

# Remove rows with NAs in any column
cleaned_data <- na.omit(data)

# Calculate average length of stay for each rating
rating_averages <- cleaned_data %>%
  filter(!is.na(Overall.Rating)) %>%  # Remove rows with NA in Overall.Rating
  group_by(Overall.Rating) %>%
  summarise(AverageLengthStay = mean(SNF.Average.Length.of.Stay.Total))

# Convert rating to factor to control order in the plot
rating_averages$Overall.Rating <- factor(rating_averages$Overall.Rating, levels = unique(rating_averages$Overall.Rating))

# Create a horizontal bar chart
ggplot(rating_averages, aes(x = Overall.Rating, y = AverageLengthStay, fill = Overall.Rating)) +
  geom_col(width = 0.5) +  # Bar chart
  geom_line(aes(group = 1), color = "red", size = 1.5) +  # Line plot
  labs(
    x = "Overall Rating",
    y = "Average Length of Stay (Total)",
    title = "Average Length of Stay by Overall Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Make text slanted for readability
  )

###9. Create a pie chart: Share of Nursing Homes by Type of Control

#To do this create the pie chart, we first wanted to change the "Type of Control" column from 
# purely numerical and needing a key, to simply stating what type of nursing home each of them 
# was, elimating the need for the key in the data dictionary. 

convert_control <- function(value) {
  case_when(
    value == 1 ~ "Voluntary Nonprofit-Church",
    value == 2 ~ "Voluntary Nonprofit-Other",
    value == 3 ~ "Proprietary-Individual",
    value == 4 ~ "Proprietary-Corporation",
    value == 5 ~ "Proprietary-Partnership",
    value == 6 ~ "Proprietary-Other",
    value == 7 ~ "Governmental-Federal",
    value == 8 ~ "Governmental-City-County",
    value == 9 ~ "Governmental-County",
    value == 10 ~ "Governmental-State",
    value == 11 ~ "Governmental-Facility District",
    value == 12 ~ "Governmental-City",
    value == 13 ~ "Governmental-Other",
    TRUE ~ as.character(value)
  )
}

#Establishes the changes in the CostReport_2021 data frame
CostReport_2021$Type.of.Control <- convert_control(CostReport_2021$Type.of.Control)
head(CostReport_2021$Type.of.Control)

#Creates a new table to show the counts of each type of control. 
# This makes creating the pie chart simpler
control_counts <- table(CostReport_2021$Type.of.Control)
control_df <- data.frame(Control_Type = names(control_counts),
                         Count = as.numeric(control_counts))

#Plot pie chart
ggplot(control_df, aes(x = "", y = Count, fill = Control_Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Share of Nursing Homes by Type of Control",
       fill = "Type of Control") +
  theme_void() +
  scale_fill_discrete(name = "Type of Control")

###10. Run a Hypothesis test ANOVA to compare the revenues in three different states

# We choose three states: California (CA), Texas (TX), and Virginia (VA)
# Filter data for the three chosen states

# Ho: The mean revenue is the same across all three states (uCA = uTX = uVA)
# Ha: At least one is different

selected_states <- c("CA", "TX", "VA")
revenues <- CostReport_2021 %>%
  filter(State.Code %in% selected_states) %>%
  select(State.Code, Gross.Revenue)
na.omit(revenues)

# ANOVA test
anova_result <- aov(Gross.Revenue ~ State.Code, revenues)
summary(anova_result)

#Findings from AVOVA test:
#Since p-value = 2e-16 < 0.05, so we reject the Null
# At least one pair of means is significantly different from each other


###11. Create a regression analysis 

reg_analyis <- lm(Net.Income ~ Total.Discharges.Total + NF.Admissions.Total + 
                    +                     Total.Charges, CostReport_2021)
summary(reg_analyis)

#Ho: No Relation
#Ha: Yes Relation

#Findings from regression analysis:

#Since p-value= 0.01222 < 0.05, so we reject the Null, so there is a relation
# We conclude that Total Discharges, Total Admissions, and Total Charges are 
# significant influential factors impacting nursing homes performance


############## Overall Findings ##################
# 1. SHOULD you invest in nursing homes? Overall, we would not recommend investing for several reasons,
  #1. Our map of profit margin by County found that Nursing homes in most U.S states had very low profitability 
  #2. According to nationwide staffing data for Q1 2022, the average nursing home reported a nursing staff turnover 
    #rate of 53.3%, which means that constantly hiring and training new nursing staff incurs a lot of expenses,
    #decreasing profit margins and doesn't look appealing to investors 
  #3. Also, nursing homes are not the best investment compared to other industries like Software as a Service (SaaS) 
    # with a 40% profit margin, financial with 30% or Pharmaceutical with 20%.

# 2. If you choose to invest, where should you invest? 
  # If you chose to invest then, then I would look for nursing homes in states like Minnesota, 
  # Virginia, Nebraska, Arizona as they are the most profitable according to our findings from the top_profit_counties.   

# 3.If you were to invest in a nursing home, what attributes should it have? 
  # I would recommend looking at location by state because certain states are more profitable than others like Virginia.
  # Total discharges and total admissions are also important attributes to consider as they were found to be influential 
  # factors impacting nursing homes net income.
  # Rating is also an important attribute b/c nursing homes with high ratings typically have lower patient turnover 
  # rates, although our graph says otherwise. 




