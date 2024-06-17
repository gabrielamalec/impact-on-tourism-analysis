# libraries
library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(ggcorrplot)
library(stargazer)
library(scatterplot)

# upload data
tourism_data <- read_excel("tourism_data2.xlsx")
View(tourism_data2)

# simplify names of columns
names(tourism_data2) <- c('country', 'regions', 'subregions', 'arrivals', 'population','GDP', 'HDI',
                          'Happiness',
                          'TTCI', 'Enabling_environment', 'TT_policy_conditions', 'Infrastructure',
                          'Natural_cultural_resources',
                          'Business environment', 'Safety and security', 'Health and hygiene','Human resources and
labour market', 'ICT readiness',
                          'Prioritization of Travel & Tourism', 'International Openness','Price competitiveness',
                          'Environmental sustainability',
                          'Air transport infrastructure', 'Ground and port infrastructure', 'Tourist service
infrastructure',
                          'Natural resources', 'Cultural resources and business travel')

# check for missing values
data_num <- tourism_data2[,-c(1,2,3)]
missing_values <- function(x){
  data_num %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                  labels = c("Present","Missing")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables",
         y = "Observations")
}
missing_values(df)

# remove rows with missing values
data <- tourism_data2 %>%
  drop_na()
summary(data)

#numbers to represent subgroups (regions)
data[data$regions == 'East Asia and the Pacific', 'region'] = 1
data[data$regions == 'Eastern Europe and Central Asia', 'region'] = 2
data[data$regions == 'Europe', 'region'] = 3
data[data$regions == 'Latin America and Caribbean', 'region'] = 4
data[data$regions == 'Middle East and North Africa', 'region'] = 5
data[data$regions == 'North America', 'region'] = 6
data[data$regions == 'Sub-Saharan Africa', 'region'] = 7
data[data$regions == 'South Asia', 'region'] = 8

#boxplot
ggplot(data, aes(x = factor(region), y = arrivals)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Region", y = "Tourist Arrivals") +
  ggtitle("Boxplot of tourist arrivals by region") + 
  theme_minimal()

# correlation
data %>%
  dplyr::select('arrivals', 'region', 'population', 'Happiness', 'GDP','HDI',
                'Business environment', 'Safety and security', 'Health and hygiene',
                'Prioritization of Travel & Tourism', 'International Openness','Price competitiveness',
                'Environmental sustainability',
                'Air transport infrastructure', 'Ground and port infrastructure',
                'Natural resources', 'Cultural resources and business travel') %>%
  cor(.) %>%
  ggcorrplot(., type = "lower", lab = TRUE)
data %>%
  dplyr::select('arrivals', 'population', 'GDP', 'Happiness','Enabling_environment',
                'TT_policy_conditions', 'Infrastructure', 'Natural_cultural_resources') %>%
  cor(.) %>%
  ggcorrplot(., type = "lower", lab = TRUE)

# correlation 2
data3 <- data[,c(4,6,12,13)]
chart.Correlation(data3, histogram=TRUE, pch=19)

# basic models
reg1 <- lm(arrivals ~ GDP, data=data)
summary(reg1)
reg2 <- lm(arrivals ~ GDP + region + Infrastructure, data=data)
summary(reg2)
reg3 <- lm(arrivals ~ GDP + region + Infrastructure + Natural_cultural_resources , data=data)
summary(reg3)
reg4 <- lm(arrivals ~ GDP + region + Infrastructure + Natural_cultural_resources +
             TT_policy_conditions + Enabling_environment, data=data)
summary(reg4)

# Improved
reg5 <- lm(arrivals ~ GDP + region + Infrastructure + Natural_cultural_resources + population,
           data=data)
summary(reg5)
reg6 <- lm(arrivals ~ GDP + region + Infrastructure + Natural_cultural_resources +
             Happiness*population, data=data)
summary(reg6)

# McFadden’s Pseudo R2
list(reg1 = pR2(reg1)["McFadden"],
     reg2 = pR2(reg2)["McFadden"],
     reg3 = pR2(reg3)["McFadden"],
     reg4 = pR2(reg4)["McFadden"],
     reg5 = pR2(reg5)["McFadden"],
     reg6 = pR2(reg6)["McFadden"])

# Print table with model info in html code
stargazer(reg1, reg2, reg3, reg4, title="Results part 1", type='html',align=TRUE)
stargazer(reg5, reg6, title="Results part 2", type='html',align=TRUE)

#plot
lm_fit <- lm(arrivals ~ GDP + region + Infrastructure + Natural_cultural_resources +
               Happiness*population, data=data)
summary(lm_fit)
predicted_df <- data.frame(mpg_pred = predict(lm_fit, data), arrivals=data$arrivals)
ggplot(data = data, aes(x = GDP, y = arrivals)) +
  geom_point(color='blue') +
  geom_line(color='red',data = predicted_df, aes(x=mpg_pred, y=data$arrivals))

