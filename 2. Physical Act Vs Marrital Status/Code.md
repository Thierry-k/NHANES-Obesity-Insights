Does Physical Activity Differ by Marital Status?
================

## Overview

This project investigates whether physical activity patterns differ
across marital status groups using data from the NHANES dataset. The
primary goal is to determine if individuals’ levels of physical activity
— such as vigorous work, moderate exercise, or transportation by walking
or biking — vary depending on their marital status.

## Research Question

**Is there a statistically significant difference in physical activity
levels across different marital statuses?**

## Variables of Interest

- **Marital Status**  
  Coded into six categories:
  - Married  
  - Widowed  
  - Divorced  
  - Separated  
  - Never Married  
  - Living Together
- **Physical Activity Types**  
  Binary (Yes/No) indicators of whether the respondent engaged in:
  - Vigorous Work Activity  
  - Moderate Work Activity  
  - Walking or Biking  
  - Vigorous Recreational Activity  
  - Moderate Recreational Activity

## Methodological Approach

The methodology uses **contingency tables** to examine the relationship
between marital status and each type of physical activity. For each
table:

- We observe the **raw counts** of individuals participating (or not) in
  a given activity across marital groups.
- We compute **row-wise proportions** to understand how physical
  activity behaviors differ within each marital group.
- We apply a **Chi-squared test of independence** to assess whether the
  differences are statistically significant.

``` r
# Clean and prepare the dataset
df <- nhanes %>%
  drop_na(marstat, vigwrk, modwrk, wlkbik, vigrecexr, modrecexr, sedmin) # Drops all the missing values

str(df)
```

    ## 'data.frame':    5781 obs. of  21 variables:
    ##  $ id       : int  1 3 4 5 6 7 9 11 12 14 ...
    ##  $ gender   : Factor w/ 2 levels "Male","Female": 1 2 1 2 1 1 2 2 1 1 ...
    ##  $ age      : int  34 60 26 49 80 80 42 45 28 44 ...
    ##  $ marstat  : Factor w/ 6 levels "Married","Widowed",..: 1 2 1 6 1 2 1 1 5 3 ...
    ##  $ samplewt : num  80101 20090 22538 74212 11998 ...
    ##  $ psu      : int  1 2 1 2 1 1 2 2 1 2 ...
    ##  $ strata   : int  9 1 14 11 3 5 1 4 2 5 ...
    ##  $ tchol    : int  135 202 160 259 182 148 170 225 299 197 ...
    ##  $ hdl      : int  50 45 45 45 75 49 35 82 51 37 ...
    ##  $ sysbp    : int  114 154 102 118 142 126 NA 106 108 118 ...
    ##  $ dbp      : int  88 70 50 82 62 62 NA 62 62 90 ...
    ##  $ wt       : num  87.4 116.8 97.6 86.7 79.1 ...
    ##  $ ht       : num  165 166 173 168 174 ...
    ##  $ bmi      : num  32.2 42.4 32.6 30.6 26 ...
    ##  $ vigwrk   : Factor w/ 2 levels "Yes","No": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ modwrk   : Factor w/ 2 levels "Yes","No": 2 2 2 2 1 1 2 1 2 2 ...
    ##  $ wlkbik   : Factor w/ 2 levels "Yes","No": 2 2 1 2 1 2 2 2 1 2 ...
    ##  $ vigrecexr: Factor w/ 2 levels "Yes","No": 2 2 1 2 2 2 2 1 1 2 ...
    ##  $ modrecexr: Factor w/ 2 levels "Yes","No": 2 2 2 2 1 2 2 1 1 2 ...
    ##  $ sedmin   : int  480 240 720 240 60 540 30 720 30 600 ...
    ##  $ obese    : Factor w/ 2 levels "No","Yes": 1 2 1 1 1 1 2 1 1 1 ...

``` r
# Template function
analyze_by_marstat <- function(activity_var, label) {
  tab <- table(Activity = df[[activity_var]], MaritalStatus = df$marstat)
  row_prop <- round(addmargins(prop.table(tab, margin = 1), margin = 2), 3)
  
  print(kable(addmargins(tab, margin = 2), caption = paste(label, "vs Marital Status")))
  
  cat("\n Row Proportions:")
  print(kable(row_prop))

  print(chisq.test(tab))
  
}

# Run for all physical activity types
analyze_by_marstat("vigwrk", "Vigorous Work Activity")
```

    ## 
    ## 
    ## Table: Vigorous Work Activity vs Marital Status
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together|  Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|----:|
    ## |Yes |     555|      38|      113|        33|           199|             105| 1043|
    ## |No  |    2437|     458|      519|       159|           817|             348| 4738|
    ## 
    ##  Row Proportions:
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together| Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|---:|
    ## |Yes |   0.532|   0.036|    0.108|     0.032|         0.191|           0.101|   1|
    ## |No  |   0.514|   0.097|    0.110|     0.034|         0.172|           0.073|   1|
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tab
    ## X-squared = 46.496, df = 5, p-value = 7.196e-09

``` r
analyze_by_marstat("modwrk", "Moderate Work Activity")
```

    ## 
    ## 
    ## Table: Moderate Work Activity vs Marital Status
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together|  Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|----:|
    ## |Yes |    1056|      99|      224|        58|           379|             192| 2008|
    ## |No  |    1936|     397|      408|       134|           637|             261| 3773|
    ## 
    ##  Row Proportions:
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together| Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|---:|
    ## |Yes |   0.526|   0.049|    0.112|     0.029|         0.189|           0.096|   1|
    ## |No  |   0.513|   0.105|    0.108|     0.036|         0.169|           0.069|   1|
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tab
    ## X-squared = 64.7, df = 5, p-value = 1.293e-12

``` r
analyze_by_marstat("wlkbik", "Walking or Biking")
```

    ## 
    ## 
    ## Table: Walking or Biking vs Marital Status
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together|  Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|----:|
    ## |Yes |     639|      89|      165|        60|           396|             158| 1507|
    ## |No  |    2353|     407|      467|       132|           620|             295| 4274|
    ## 
    ##  Row Proportions:
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together| Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|---:|
    ## |Yes |   0.424|   0.059|    0.109|     0.040|         0.263|           0.105|   1|
    ## |No  |   0.551|   0.095|    0.109|     0.031|         0.145|           0.069|   1|
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tab
    ## X-squared = 160.2, df = 5, p-value < 2.2e-16

``` r
analyze_by_marstat("vigrecexr", "Vigorous Recreational Activity")
```

    ## 
    ## 
    ## Table: Vigorous Recreational Activity vs Marital Status
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together|  Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|----:|
    ## |Yes |     486|      17|      101|        35|           321|             107| 1067|
    ## |No  |    2506|     479|      531|       157|           695|             346| 4714|
    ## 
    ##  Row Proportions:
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together| Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|---:|
    ## |Yes |   0.455|   0.016|    0.095|     0.033|         0.301|           0.100|   1|
    ## |No  |   0.532|   0.102|    0.113|     0.033|         0.147|           0.073|   1|
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tab
    ## X-squared = 211.3, df = 5, p-value < 2.2e-16

``` r
analyze_by_marstat("modrecexr", "Moderate Recreational Activity")
```

    ## 
    ## 
    ## Table: Moderate Recreational Activity vs Marital Status
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together|  Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|----:|
    ## |Yes |    1165|     130|      257|        61|           448|             159| 2220|
    ## |No  |    1827|     366|      375|       131|           568|             294| 3561|
    ## 
    ##  Row Proportions:
    ## 
    ## |    | Married| Widowed| Divorced| Separated| Never Married| Living Together| Sum|
    ## |:---|-------:|-------:|--------:|---------:|-------------:|---------------:|---:|
    ## |Yes |   0.525|   0.059|    0.116|     0.027|         0.202|           0.072|   1|
    ## |No  |   0.513|   0.103|    0.105|     0.037|         0.160|           0.083|   1|
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tab
    ## X-squared = 52.476, df = 5, p-value = 4.309e-10

### Interpretation Guide

For each type of physical activity, a significant Chi-squared result (p
\< 0.05) suggests a relationship between that activity and marital
status - there’s statistical evidence that physical activity is not
independent of marital status.

Row proportions help us see which groups are more or less active,
providing insight into behavioral differences that may be shaped by
lifestyle, responsibilities, or social structures tied to marital
status.
