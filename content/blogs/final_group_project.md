---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2021-09-30"
description: Credit Card Fraud # the title that will show up once someone gets to this page
draft: false
image: spices.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: final_group_project # slug is the shorthand URL address... no spaces plz
title: Credit Card Fraud
---




# The problem

The goal of the project is to predict fraudulent credit card transactions.

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no? 

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.

## Obtain the data

The dataset is too large to be hosted on Canvas or Github, so please download it from dropbox https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0 and save it in your `dsb` repo, under the `data` folder.

As we will be building a classifier model using tidymodels, there's two things we need to do:

1. Define the outcome variable `is_fraud` as a factor, or categorical, variable, instead of the numerical 0-1 varaibles.
2. In tidymodels, the first level is the event of interest. If we leave our data as is, `0` is the first level, but we want to find out when we actually did (`1`) have a fraudulent transaction


```
## Rows: 671,028
## Columns: 14
## $ trans_date_trans_time <dttm> 2019-02-22 07:32:58, 2019-02-16 15:07:20, 2019-…
## $ trans_year            <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, …
## $ category              <chr> "entertainment", "kids_pets", "personal_care", "…
## $ amt                   <dbl> 7.79, 3.89, 8.43, 40.00, 54.04, 95.61, 64.95, 3.…
## $ city                  <chr> "Veedersburg", "Holloway", "Arnold", "Apison", "…
## $ state                 <chr> "IN", "OH", "MO", "TN", "CO", "GA", "MN", "AL", …
## $ lat                   <dbl> 40.1186, 40.0113, 38.4305, 35.0149, 39.4584, 32.…
## $ long                  <dbl> -87.2602, -80.9701, -90.3870, -85.0164, -106.385…
## $ city_pop              <dbl> 4049, 128, 35439, 3730, 277, 1841, 136, 190178, …
## $ job                   <chr> "Development worker, community", "Child psychoth…
## $ dob                   <date> 1959-10-19, 1946-04-03, 1985-03-31, 1991-01-28,…
## $ merch_lat             <dbl> 39.41679, 39.74585, 37.73078, 34.53277, 39.95244…
## $ merch_long            <dbl> -87.52619, -81.52477, -91.36875, -84.10676, -106…
## $ is_fraud              <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

The data dictionary is as follows

| column(variable)      | description                                 |
|-----------------------|---------------------------------------------|
| trans_date_trans_time | Transaction DateTime                        |
| trans_year            | Transaction year                            |
| category              | category of merchant                        |
| amt                   | amount of transaction                       |
| city                  | City of card holder                         |
| state                 | State of card holder                        |
| lat                   | Latitude location of purchase               |
| long                  | Longitude location of purchase              |
| city_pop              | card holder's city population               |
| job                   | job of card holder                          |
| dob                   | date of birth of card holder                |
| merch_lat             | Latitude Location of Merchant               |
| merch_long            | Longitude Location of Merchant              |
| is_fraud              | Whether Transaction is Fraud (1) or Not (0) |

We also add some of the variables we considered in our EDA for this dataset during homework 2.


```r
card_fraud <- card_fraud %>% 
  mutate( hour = hour(trans_date_trans_time),
          wday = wday(trans_date_trans_time, label = TRUE),
          month_name = month(trans_date_trans_time, label = TRUE),
          age = interval(dob, trans_date_trans_time) / years(1)
) %>% 
  rename(year = trans_year) %>% 
  
  mutate(
    
    # convert latitude/longitude to radians
    lat1_radians = lat / 57.29577951,
    lat2_radians = merch_lat / 57.29577951,
    long1_radians = long / 57.29577951,
    long2_radians = merch_long / 57.29577951,
    
    # calculate distance in miles
    distance_miles = 3963.0 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians)),

    # calculate distance in km
    distance_km = 6377.830272 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians))

  )
```

## Exploratory Data Analysis (EDA) 

You have done some EDA and you can pool together your group's expertise in which variables to use as features.
You can reuse your EDA from earlier, but we expect at least a few visualisations and/or tables to explore teh dataset and identify any useful features.

Group all variables by type and examine each variable class by class. The dataset has the following types of variables:

1.  Strings
2.  Geospatial Data
3.  Dates
4.  Date/Times
5.  Numerical

Strings are usually not a useful format for classification problems. The strings should be converted to factors, dropped, or otherwise transformed.

***Strings to Factors*** 

-   `category`, Category of Merchant
-   `job`, Job of Credit Card Holder

***Strings to Geospatial Data*** 

We have plenty of geospatial data as lat/long pairs, so I want to convert city/state to lat/long so I can compare to the other geospatial variables. This will also make it easier to compute new variables like the distance the transaction is from the home location. 

-   `city`, City of Credit Card Holder
-   `state`, State of Credit Card Holder



##  Exploring factors: how is the compactness of categories?

-   Do we have excessive number of categories? Do we want to combine some?


```r
card_fraud %>% 
  count(category, sort=TRUE)%>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 14 × 3
##    category           n   perc
##    <chr>          <int>  <dbl>
##  1 gas_transport  68046 0.101 
##  2 grocery_pos    63791 0.0951
##  3 home           63597 0.0948
##  4 shopping_pos   60416 0.0900
##  5 kids_pets      58772 0.0876
##  6 shopping_net   50743 0.0756
##  7 entertainment  48521 0.0723
##  8 food_dining    47527 0.0708
##  9 personal_care  46843 0.0698
## 10 health_fitness 44341 0.0661
## 11 misc_pos       41244 0.0615
## 12 misc_net       32829 0.0489
## 13 grocery_net    23485 0.0350
## 14 travel         20873 0.0311
```

```r
card_fraud %>% 
  count(job, sort=TRUE) %>% 
  mutate(perc = n/sum(n))
```

```
## # A tibble: 494 × 3
##    job                            n    perc
##    <chr>                      <int>   <dbl>
##  1 Film/video editor           5106 0.00761
##  2 Exhibition designer         4728 0.00705
##  3 Naval architect             4546 0.00677
##  4 Surveyor, land/geomatics    4448 0.00663
##  5 Materials engineer          4292 0.00640
##  6 Designer, ceramics/pottery  4262 0.00635
##  7 IT trainer                  4014 0.00598
##  8 Financial adviser           3959 0.00590
##  9 Systems developer           3948 0.00588
## 10 Environmental consultant    3831 0.00571
## # ℹ 484 more rows
```

```r
# The number of categories (14 for category and 494 for job) looks fine considering the large (~671K) no of records. However, we can look at combining a few jobs if necessary (e.g. bring a few IT jobs under one IT umbrella)
```


The predictors `category` and `job` are transformed into factors.


```r
card_fraud <- card_fraud %>% 
  mutate(category = factor(category),
         job = factor(job))


glimpse(card_fraud)
```

```
## Rows: 671,028
## Columns: 24
## $ trans_date_trans_time <dttm> 2019-02-22 07:32:58, 2019-02-16 15:07:20, 2019-…
## $ year                  <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, …
## $ category              <fct> entertainment, kids_pets, personal_care, grocery…
## $ amt                   <dbl> 7.79, 3.89, 8.43, 40.00, 54.04, 95.61, 64.95, 3.…
## $ city                  <chr> "Veedersburg", "Holloway", "Arnold", "Apison", "…
## $ state                 <chr> "IN", "OH", "MO", "TN", "CO", "GA", "MN", "AL", …
## $ lat                   <dbl> 40.1186, 40.0113, 38.4305, 35.0149, 39.4584, 32.…
## $ long                  <dbl> -87.2602, -80.9701, -90.3870, -85.0164, -106.385…
## $ city_pop              <dbl> 4049, 128, 35439, 3730, 277, 1841, 136, 190178, …
## $ job                   <fct> "Development worker, community", "Child psychoth…
## $ dob                   <date> 1959-10-19, 1946-04-03, 1985-03-31, 1991-01-28,…
## $ merch_lat             <dbl> 39.41679, 39.74585, 37.73078, 34.53277, 39.95244…
## $ merch_long            <dbl> -87.52619, -81.52477, -91.36875, -84.10676, -106…
## $ is_fraud              <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ hour                  <int> 7, 15, 22, 10, 17, 1, 9, 17, 16, 12, 10, 16, 23,…
## $ wday                  <ord> Fri, Sat, Fri, Sun, Sat, Mon, Sun, Tue, Sat, Sun…
## $ month_name            <ord> Feb, Feb, Dec, Mar, Feb, Sep, Dec, Jun, Dec, Dec…
## $ age                   <dbl> 59.34607, 72.87570, 34.74299, 28.09431, 33.85676…
## $ lat1_radians          <dbl> 0.7002017, 0.6983289, 0.6707388, 0.6111253, 0.68…
## $ lat2_radians          <dbl> 0.6879527, 0.6936959, 0.6585264, 0.6027105, 0.69…
## $ long1_radians         <dbl> -1.522978, -1.413195, -1.577551, -1.483816, -1.8…
## $ long2_radians         <dbl> -1.527620, -1.422876, -1.594686, -1.467940, -1.8…
## $ distance_miles        <dbl> 50.56023, 34.69750, 72.10554, 61.50534, 43.21622…
## $ distance_km           <dbl> 81.36880, 55.84021, 116.04262, 98.98324, 69.5497…
```

```r
# Any outliers?
card_fraud %>%
  select(category, amt, lat, long) %>%
  pivot_longer(cols = 2:4,
               names_to = "feature",
               values_to = "value") %>%
  ggplot() +
  aes(x = category, y = value, fill = category) +
  coord_flip() +
  geom_boxplot() +
  facet_wrap(~feature, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-1.png" width="672" />

```r
# Distribution of transaction amounts
ggplot(card_fraud, aes(x = amt)) +
  geom_histogram(binwidth = 50) +
  labs(x = "Transaction Amount", y = "Count") +
  theme_minimal() # shows skewed distribution
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-2.png" width="672" />

```r
# Distribution of log transaction amounts
ggplot(card_fraud, aes(x = log(amt))) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Log of Transaction Amount", y = "Count") +
  theme_minimal() # this shows that using log in the model would be more appropritae
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-3.png" width="672" />

```r
# Boxplots of numerical variables grouped by the target variable (is_fraud)
ggplot(card_fraud, aes(x = is_fraud, y = amt)) +
  geom_boxplot(color = "#5989e3", fill = "#5989e3", alpha = 0.7) +
  labs(x = "Is Fraud", y = "Amount", title = "Boxplot of Amount by Fraud Status")
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-4.png" width="672" />

```r
# Boxplots of numerical variables grouped by is_fraud
ggplot(card_fraud, aes(x = is_fraud, y = amt)) +
  geom_boxplot(color = "#5989e3", fill = "#5989e3", alpha = 0.7) +
  labs(x = "Is Fraud", y = "Amount", title = "Boxplot of Amount by Fraud Status")
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-5.png" width="672" />

```r
# Time series analysis of the transaction amounts
card_fraud %>%
  ggplot(aes(x = trans_date_trans_time, y = amt)) +
  geom_line(color = "#5989e3") +
  labs(x = "Time", y = "Amount", title = "Transaction Amount over Time")
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-6.png" width="672" />

```r
ggplot(card_fraud, aes(x = amt)) +
  geom_histogram(binwidth = 20, fill = "blue") +
  labs(title = "Histogram of Transaction Amount", x = "Transaction Amount")
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-7.png" width="672" />

```r
# Number and percentage of fraud transactions by year- to see whether there are any trends
fraud_by_year <- card_fraud %>%
  group_by(year) %>%
  summarize(no_of_transactions = n(),
            no_of_fraud_transactions = sum(is_fraud == 1),
            percentage_of_fraud = (no_of_fraud_transactions / no_of_transactions) * 100)

# Number of fraud transactions by category (each year separately)- again to find trends
fraud_transactions_by_category <- card_fraud %>%
  group_by(year, category) %>%
  summarize(fraud_count = sum(is_fraud == 1)) %>%
  ggplot(aes(x = fraud_count, y = fct_reorder(category, fraud_count))) +
  geom_col(fill = "blue", color = "black") +
  facet_wrap(~year, scales = "free", ncol = 1) +
  labs(x = "Number of Fraud Transactions", y = "Category", title = "Number of Fraud Transactions by Category (Year-wise)") +
  theme_minimal()
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
# Number of fraud transactions by job (each year separately, top 10 jobs)
fraud_transactions_by_job <- card_fraud %>%
  group_by(year, job) %>%
  summarize(fraud_count = sum(is_fraud == 1)) %>%
  group_by(year) %>%
  top_n(n = 10, wt = fraud_count) %>%
  ungroup() %>%
  ggplot(aes(x = fraud_count, y = fct_reorder(job, fraud_count))) +
  geom_col(fill = "blue", color = "black") +
  facet_wrap(~year, scales = "free", ncol = 1) +
  labs(x = "Number of Fraud Transactions", y = "Job", title = "Number of Fraud Transactions by Job (Year-wise)") +
  theme_minimal()
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
# Number of fraud transactions by state (each year separately, top 10 states)
fraud_transactions_by_state <- card_fraud %>%
  group_by(year, state) %>%
  summarize(fraud_count = sum(is_fraud == 1)) %>%
  top_n(n = 10, wt = fraud_count) %>%
  arrange(year, desc(fraud_count)) %>%
  ggplot(aes(x = state, y = fraud_count, fill = "blue")) +
  geom_col(color = "black") +
  facet_wrap(~year, scales = "free", ncol = 1) +
  labs(x = "State", y = "Number of Fraud Transactions", title = "Number of Fraud Transactions by State (Year-wise)") +
  theme_minimal() +
  theme(legend.position = "none")
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
# Number of fraud transactions by city population (each year separately, defined ranges)
fraud_transactions_by_city_population <- card_fraud %>%
  mutate(city_pop_range = cut(city_pop, breaks = c(0, 5000, 10000, 15000, Inf))) %>%
  group_by(year, city_pop_range) %>%
  summarize(fraud_count = sum(is_fraud == 1)) %>%
  ggplot(aes(x = city_pop_range, y = fraud_count, fill = year)) +
  geom_col(color = "black") +
  facet_wrap(~year, scales = "free", ncol = 1) +
  labs(x = "City Population Range", y = "Number of Fraud Transactions", title = "Number of Fraud Transactions by City Population (Year-wise)") +
  theme_minimal() +
  theme(legend.title = element_blank())
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
# Number of fraud transactions by age (small ranges of 5)
fraud_transactions_by_age <- card_fraud %>%
  mutate(age_range = cut(age, breaks = seq(0, 100, by = 5))) %>%
  group_by(age_range) %>%
  summarize(fraud_count = sum(is_fraud == 1)) %>%
  ggplot(aes(x = age_range, y = fraud_count, fill = "blue")) +
  geom_col(color = "black") +
  labs(x = "Age Range", y = "Number of Fraud Transactions", title = "Number of Fraud Transactions by Age") +
  theme_minimal() +
  theme(legend.position = "none")

# Number of fraud transactions by month (each year separately, arranged in descending order)
fraud_transactions_by_month <- card_fraud %>%
  group_by(year, month_name) %>%
  summarize(fraud_count = sum(is_fraud == 1)) %>%
  arrange(year, desc(fraud_count)) %>%
  ggplot(aes(x = fraud_count, y = fct_reorder(month_name, fraud_count, .desc = TRUE), fill = "blue")) +
  geom_col(color = "black") +
  facet_wrap(~year, scales = "free", ncol = 1) +
  labs(x = "Number of Fraud Transactions", y = "Month", title = "Number of Fraud Transactions by Month (Year-wise)") +
  theme_minimal() +
  theme(legend.position = "none")
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
# Number of fraud transactions by hour (each year separately)
fraud_transactions_by_hour <- card_fraud %>%
  group_by(year, hour) %>%
  summarize(fraud_count = sum(is_fraud == 1)) %>%
  ggplot(aes(x = hour, y = fraud_count, fill = "blue")) +
  geom_col(color = "black") +
  facet_wrap(~year, scales = "free", ncol = 1) +
  labs(x = "Hour", y = "Number of Fraud Transactions", title = "Number of Fraud Transactions by Hour (Year-wise)") +
  theme_minimal() +
  theme(legend.position = "none")
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```r
# Printing the visualizations
print(fraud_by_year)
```

```
## # A tibble: 2 × 4
##    year no_of_transactions no_of_fraud_transactions percentage_of_fraud
##   <dbl>              <int>                    <int>               <dbl>
## 1  2019             478646                     2721               0.568
## 2  2020             192382                     1215               0.632
```

```r
print(fraud_transactions_by_category)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-8.png" width="672" />

```r
print(fraud_transactions_by_job)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-9.png" width="672" />

```r
print(fraud_transactions_by_state)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-10.png" width="672" />

```r
print(fraud_transactions_by_city_population)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-11.png" width="672" />

```r
print(fraud_transactions_by_age)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-12.png" width="672" />

```r
print(fraud_transactions_by_month)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-13.png" width="672" />

```r
print(fraud_transactions_by_hour)
```

<img src="/blogs/final_group_project_files/figure-html/convert-strings-to-factors-14.png" width="672" />

`category` has 14 unique values, and `job` has 494 unique values. The dataset is quite large, with over 670K records, so these variables don't have an excessive number of levels at first glance. However, it is worth seeing if we can compact the levels to a smaller number.

### Why do we care about the number of categories and whether they are "excessive"?

Consider the extreme case where a dataset had categories that only contained one record each. There is simply insufficient data to make correct predictions using category as a predictor on new data with that category label. Additionally, if your modeling uses dummy variables, having an extremely large number of categories will lead to the production of a huge number of predictors, which can slow down the fitting. This is fine if all the predictors are useful, but if they aren't useful (as in the case of having only one record for a category), trimming them will improve the speed and quality of the data fitting.

If I had subject matter expertise, I could manually combine categories. If you don't have subject matter expertise, or if performing this task would be too labor intensive, then you can use cutoffs based on the amount of data in a category. If the majority of the data exists in only a few categories, then it might be reasonable to keep those categories and lump everything else in an "other" category or perhaps even drop the data points in smaller categories. 


## Do all variables have sensible types?

Consider each variable and decide whether to keep, transform, or drop it. This is a mixture of Exploratory Data Analysis and Feature Engineering, but it's helpful to do some simple feature engineering as you explore the data. In this project, we have all data to begin with, so any transformations will be performed on the entire dataset. Ideally, do the transformations as a `recipe_step()` in the tidymodels framework. Then the transformations would be applied to any data the recipe was used on as part of the modeling workflow. There is less chance of data leakage or missing a step when you perform the feature engineering in the recipe.

## Which variables to keep in your model?

You have a number of variables and you have to decide which ones to use in your model. For instance, you have the latitude/lognitude of the customer, that of the merchant, the same data in radians, as well as the `distance_km` and `distance_miles`. Do you need them all? 


## Fit your workflows in smaller sample

You will be running a series of different models, along the lines of the California housing example we have seen in class. However, this dataset has 670K rows and if you try various models and run cross validation on them, your computer may slow down or crash.

Thus, we will work with a smaller sample of 10% of the values the original dataset to identify the best model, and once we have the best model we can use the full dataset to train- test our best model.



```r
# select a smaller subset
my_card_fraud <- card_fraud %>% 
  # select a smaller subset, 10% of the entire dataframe 
  slice_sample(prop = 0.10) %>% 
  select(is_fraud,amt,category, distance_km,age,hour) # selected only those that are necessary- it takes too long to process if you include more so job has not been used to train the model
glimpse(my_card_fraud)
```

```
## Rows: 67,102
## Columns: 6
## $ is_fraud    <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ amt         <dbl> 45.42, 6.24, 56.96, 110.87, 90.58, 40.57, 5.60, 61.24, 111…
## $ category    <fct> food_dining, misc_pos, kids_pets, grocery_pos, misc_pos, f…
## $ distance_km <dbl> 130.16109, 36.27143, 104.90190, 53.63529, 41.89036, 27.922…
## $ age         <dbl> 64.95279, 61.88286, 32.19421, 74.42646, 25.65104, 55.20775…
## $ hour        <int> 17, 5, 21, 1, 6, 19, 16, 12, 12, 22, 14, 17, 6, 5, 1, 12, …
```


## Split the data in training - testing


```r
# **Split the data**

set.seed(123)

data_split <- initial_split(my_card_fraud, # updated data
                           prop = 0.8, 
                           strata = is_fraud)

card_fraud_train <- training(data_split) 
card_fraud_test <- testing(data_split)
glimpse(card_fraud_train)
```

```
## Rows: 53,681
## Columns: 6
## $ is_fraud    <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ amt         <dbl> 6.24, 56.96, 110.87, 90.58, 40.57, 5.60, 111.61, 15.23, 1.…
## $ category    <fct> misc_pos, kids_pets, grocery_pos, misc_pos, food_dining, s…
## $ distance_km <dbl> 36.27143, 104.90190, 53.63529, 41.89036, 27.92261, 107.976…
## $ age         <dbl> 61.88286, 32.19421, 74.42646, 25.65104, 55.20775, 21.91423…
## $ hour        <int> 5, 21, 1, 6, 19, 16, 12, 22, 14, 17, 6, 5, 1, 12, 16, 21, …
```


## Cross Validation

Start with 3 CV folds to quickly get an estimate for the best model and you can increase the number of folds to 5 or 10 later.


```r
set.seed(123)
cv_folds <- vfold_cv(data = card_fraud_train, 
                          v = 3, 
                          strata = is_fraud)
cv_folds 
```

```
## #  3-fold cross-validation using stratification 
## # A tibble: 3 × 2
##   splits                id   
##   <list>                <chr>
## 1 <split [35787/17894]> Fold1
## 2 <split [35787/17894]> Fold2
## 3 <split [35788/17893]> Fold3
```


## Define a tidymodels `recipe`

What steps are you going to add to your recipe? Do you need to do any log transformations?


```r
fraud_rec <- recipe(is_fraud ~ ., data = card_fraud_train) %>%
  step_log(amt) %>% # apply log transformation to amount variable considering better distribution in log
  step_novel(all_nominal(), -all_outcomes()) %>% # convert all nominal variables, excluding the outcome variable. This ensures that these are treated as discrete categories 
  step_normalize(all_numeric()) %>% #ensures all variables at a similar scale
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_numeric(), -all_outcomes())  #eliminate redundancies
  glimpse(card_fraud_train)
```

```
## Rows: 53,681
## Columns: 6
## $ is_fraud    <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ amt         <dbl> 6.24, 56.96, 110.87, 90.58, 40.57, 5.60, 111.61, 15.23, 1.…
## $ category    <fct> misc_pos, kids_pets, grocery_pos, misc_pos, food_dining, s…
## $ distance_km <dbl> 36.27143, 104.90190, 53.63529, 41.89036, 27.92261, 107.976…
## $ age         <dbl> 61.88286, 32.19421, 74.42646, 25.65104, 55.20775, 21.91423…
## $ hour        <int> 5, 21, 1, 6, 19, 16, 12, 22, 14, 17, 6, 5, 1, 12, 16, 21, …
```

Once you have your recipe, you can check the pre-processed dataframe 


```r
prepped_data <- 
  fraud_rec %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

glimpse(prepped_data)
```

```
## Rows: 53,681
## Columns: 18
## $ amt                     <dbl> -1.17470029, 0.42214080, 0.90306941, 0.7571139…
## $ distance_km             <dbl> -1.37824582, 0.97759240, -0.78220674, -1.18536…
## $ age                     <dbl> 0.92404098, -0.78985626, 1.64817028, -1.167587…
## $ hour                    <dbl> -1.1444432, 1.1983476, -1.7301409, -0.9980188,…
## $ is_fraud                <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_food_dining    <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_gas_transport  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_grocery_net    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0…
## $ category_grocery_pos    <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_health_fitness <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0…
## $ category_home           <dbl> 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_kids_pets      <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_misc_net       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_misc_pos       <dbl> 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1…
## $ category_personal_care  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ category_shopping_net   <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0…
## $ category_shopping_pos   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
## $ category_travel         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```


## Define various models

You should define the following classification models:

1. Logistic regression, using the `glm` engine
2. Decision tree, using the `C5.0` engine
3. Random Forest, using  the `ranger` engine and setting `importance = "impurity"`)  
4. A boosted tree using Extreme Gradient Boosting, and the `xgboost` engine
5. A k-nearest neighbours,  using 4 nearest_neighbors and the `kknn` engine  


```r
## Model Building 

# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`:  classification
# 5 different models used as mentioned above

log_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

tree_spec <- decision_tree() %>%
  set_engine("C5.0") %>%
  set_mode("classification")

rf_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification") %>%
  set_args(importance = "impurity")

xgb_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = 4) %>%
  set_engine("kknn") %>%
  set_mode("classification")
```

## Bundle recipe and model with `workflows`


```r
## Bundle recipe and model with `workflows`


log_wflow <- # new workflow object
 workflow() %>% # use workflow function
 add_recipe(fraud_rec) %>%   # use the new recipe
 add_model(log_spec)   # add your model spec

tree_wflow <- workflow() %>%
  add_recipe(fraud_rec) %>%
  add_model(tree_spec)

rf_wflow <- workflow() %>%
  add_recipe(fraud_rec) %>%
  add_model(rf_spec)

xgb_wflow <- workflow() %>%
  add_recipe(fraud_rec) %>%
  add_model(xgb_spec)

knn_wflow <- workflow() %>%
  add_recipe(fraud_rec) %>%
  add_model(knn_spec)
```


## Fit models

You may want to compare the time it takes to fit each model. `tic()` starts a simple timer and `toc()` stops it


```r
tic()
log_res <- log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
```

```
## 1.74 sec elapsed
```

```r
log_time <- time[[4]]

tic()
tree_res <- tree_wflow %>% # we calculate the time taken for each model fit similar to above
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
```

```
## 7 sec elapsed
```

```r
tree_time <- time[[4]]

tic()
rf_res <- rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
```

```
## 50.5 sec elapsed
```

```r
rf_time <- time[[4]]

tic()
xgb_res <- xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
```

```
## 3.2 sec elapsed
```

```r
xgb_time <- time[[4]]

tic()
knn_res <- knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 
time <- toc()
```

```
## 122.11 sec elapsed
```

```r
knn_time <- time[[4]]

log_res %>%  collect_metrics(summarize = TRUE) # show average performance over all folds
```

```
## # A tibble: 8 × 6
##   .metric   .estimator    mean     n   std_err .config             
##   <chr>     <chr>        <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.995       3 0.000458  Preprocessor1_Model1
## 2 f_meas    binary     0.00766     3 0.00766   Preprocessor1_Model1
## 3 kap       binary     0.00738     3 0.00760   Preprocessor1_Model1
## 4 precision binary     0.0833      3 0.0833    Preprocessor1_Model1
## 5 recall    binary     0.00402     3 0.00402   Preprocessor1_Model1
## 6 roc_auc   binary     0.810       3 0.00793   Preprocessor1_Model1
## 7 sens      binary     0.00402     3 0.00402   Preprocessor1_Model1
## 8 spec      binary     1.00        3 0.0000375 Preprocessor1_Model1
```

```r
log_res %>%  collect_metrics(summarize = FALSE) # show performance for every single fold
```

```
## # A tibble: 24 × 5
##    id    .metric   .estimator .estimate .config             
##    <chr> <chr>     <chr>          <dbl> <chr>               
##  1 Fold1 recall    binary        0.0120 Preprocessor1_Model1
##  2 Fold1 precision binary        0.25   Preprocessor1_Model1
##  3 Fold1 f_meas    binary        0.0230 Preprocessor1_Model1
##  4 Fold1 accuracy  binary        0.995  Preprocessor1_Model1
##  5 Fold1 kap       binary        0.0226 Preprocessor1_Model1
##  6 Fold1 sens      binary        0.0120 Preprocessor1_Model1
##  7 Fold1 spec      binary        1.00   Preprocessor1_Model1
##  8 Fold1 roc_auc   binary        0.808  Preprocessor1_Model1
##  9 Fold2 recall    binary        0      Preprocessor1_Model1
## 10 Fold2 precision binary        0      Preprocessor1_Model1
## # ℹ 14 more rows
```

```r
log_pred <- log_res %>% collect_predictions()
log_pred %>%  conf_mat(is_fraud, .pred_class) 
```

```
##           Truth
## Prediction     1     0
##          1     1     7
##          0   286 53387
```

```r
log_pred %>% 
  conf_mat(is_fraud, .pred_class) %>% 
  autoplot(type = "mosaic") +
  geom_label(aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TP", "FN", "FP", "TN")))
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-10-1.png" width="672" />

```r
log_pred %>% 
  conf_mat(is_fraud, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-10-2.png" width="672" />

```r
log_pred %>% #roc curver
  group_by(id) %>% 
  roc_curve(is_fraud, .pred_1) %>% 
  autoplot()
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-10-3.png" width="672" />

```r
rf_res %>%  collect_metrics(summarize = TRUE) # decision tree results for others
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.997     3 0.000412  Preprocessor1_Model1
## 2 f_meas    binary     0.644     3 0.0320    Preprocessor1_Model1
## 3 kap       binary     0.642     3 0.0322    Preprocessor1_Model1
## 4 precision binary     0.946     3 0.0185    Preprocessor1_Model1
## 5 recall    binary     0.489     3 0.0328    Preprocessor1_Model1
## 6 roc_auc   binary     0.971     3 0.0130    Preprocessor1_Model1
## 7 sens      binary     0.489     3 0.0328    Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.0000496 Preprocessor1_Model1
```

```r
xgb_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.998     3 0.000278  Preprocessor1_Model1
## 2 f_meas    binary     0.764     3 0.0117    Preprocessor1_Model1
## 3 kap       binary     0.763     3 0.0118    Preprocessor1_Model1
## 4 precision binary     0.894     3 0.0102    Preprocessor1_Model1
## 5 recall    binary     0.668     3 0.0160    Preprocessor1_Model1
## 6 roc_auc   binary     0.970     3 0.0119    Preprocessor1_Model1
## 7 sens      binary     0.668     3 0.0160    Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.0000677 Preprocessor1_Model1
```

```r
knn_res %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n   std_err .config             
##   <chr>     <chr>      <dbl> <int>     <dbl> <chr>               
## 1 accuracy  binary     0.997     3 0.000310  Preprocessor1_Model1
## 2 f_meas    binary     0.585     3 0.00249   Preprocessor1_Model1
## 3 kap       binary     0.584     3 0.00254   Preprocessor1_Model1
## 4 precision binary     0.910     3 0.0211    Preprocessor1_Model1
## 5 recall    binary     0.431     3 0.00486   Preprocessor1_Model1
## 6 roc_auc   binary     0.838     3 0.00587   Preprocessor1_Model1
## 7 sens      binary     0.431     3 0.00486   Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.0000818 Preprocessor1_Model1
```

```r
tree_res %>%  collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 6
##   .metric   .estimator  mean     n  std_err .config             
##   <chr>     <chr>      <dbl> <int>    <dbl> <chr>               
## 1 accuracy  binary     0.997     3 0.000685 Preprocessor1_Model1
## 2 f_meas    binary     0.704     3 0.0584   Preprocessor1_Model1
## 3 kap       binary     0.702     3 0.0587   Preprocessor1_Model1
## 4 precision binary     0.871     3 0.0418   Preprocessor1_Model1
## 5 recall    binary     0.592     3 0.0630   Preprocessor1_Model1
## 6 roc_auc   binary     0.877     3 0.0193   Preprocessor1_Model1
## 7 sens      binary     0.592     3 0.0630   Preprocessor1_Model1
## 8 spec      binary     1.00      3 0.000160 Preprocessor1_Model1
```

## Compare models


```r
## Model Comparison

log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Logistic Regression",
         time = log_time)

tree_metrics <- 
  tree_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Decision Tree",
         time = tree_time)

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest",
         time = rf_time)

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost",
         time = xgb_time)

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn",
         time = knn_time)


# add mode models here

# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                            tree_metrics,
                            rf_metrics,
                           xgb_metrics,
                           knn_metrics
                      ) %>% 
  # get rid of 'sec elapsed' and turn it into a number
  mutate(time = str_sub(time, end = -13) %>% 
           as.double()
         )
#Pivot wider to create barplot
  model_comp <- model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

  # show mean are under the curve (ROC-AUC) for every model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
   geom_text(
     size = 3,
     aes(label = round(mean_roc_auc, 2), 
         y = mean_roc_auc + 0.08),
     vjust = 1
  )+
  theme_light()+
  theme(legend.position = "none")+
  labs(y = NULL)
```

<img src="/blogs/final_group_project_files/figure-html/compare_models-1.png" width="672" />

## Which metric to use

This is a highly imbalanced data set, as roughly 99.5% of all transactions are ok, and it's only 0.5% of transactions that are fraudulent. A `naive` model, which classifies everything as ok and not-fraud, would have an accuracy of 99.5%, but what about the sensitivity, specificity, the AUC, etc?

## `last_fit()`

```r
## `last_fit()` on test set

# - `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 

# - provide the workflow object of the best model as well as the data split object (not the training data). 
 last_fit_rf <- last_fit(rf_wflow, 
                        split = data_split,
                        metrics = metric_set(
                          accuracy, f_meas, kap, precision,
                          recall, roc_auc, sens, spec)) #random forest is the best performing model (keeps fluctuating between random forest and xgb- have used rf as that is the result in the latest iteration)
last_fit_rf %>% collect_metrics(summarize = TRUE)
```

```
## # A tibble: 8 × 4
##   .metric   .estimator .estimate .config             
##   <chr>     <chr>          <dbl> <chr>               
## 1 accuracy  binary         0.997 Preprocessor1_Model1
## 2 f_meas    binary         0.711 Preprocessor1_Model1
## 3 kap       binary         0.709 Preprocessor1_Model1
## 4 precision binary         0.931 Preprocessor1_Model1
## 5 recall    binary         0.574 Preprocessor1_Model1
## 6 sens      binary         0.574 Preprocessor1_Model1
## 7 spec      binary         1.00  Preprocessor1_Model1
## 8 roc_auc   binary         0.991 Preprocessor1_Model1
```



## Get variable importance using `vip` package



```r
last_fit_rf %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()
```

```
## Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
## ℹ Please use `extract_fit_parsnip()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-12-1.png" width="672" />

## Plot Final Confusion matrix and ROC curve



```r
## Final Confusion Matrix

last_fit_rf %>%
  collect_predictions() %>% 
  conf_mat(is_fraud, .pred_class) %>% 
  autoplot(type = "heatmap")
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-13-1.png" width="672" />

```r
## Final ROC curve
last_fit_rf %>% 
  collect_predictions() %>% 
  roc_curve(is_fraud, .pred_1) %>% 
  autoplot()
```

<img src="/blogs/final_group_project_files/figure-html/unnamed-chunk-13-2.png" width="672" />


##  Calculating the cost of fraud to the company 


- How much money (in US\$ terms) are fraudulent transactions costing the company? Generate a table that summarizes the total amount of legitimate and fraudulent transactions per year and calculate the % of fraudulent transactions, in US\$ terms. Compare your model vs the naive classification that we do not have any fraudulent transactions. 


```r
# couldnt solve so eliminated sample code
```
