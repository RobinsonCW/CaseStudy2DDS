Naive Bayes
================
Chance Robinson
12/04/2019

  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Library Imports](#library-imports)
      - [Load the CSV Data](#load-the-csv-data)
          - [Convert Integers to Strings](#convert-integers-to-strings)
          - [Convert Integers to Factors](#convert-integers-to-factors)
      - [Prepare Dataframe](#prepare-dataframe)
      - [Logistic Regression](#logistic-regression)
      - [Train / Test Split](#train-test-split)
          - [Test](#test)
          - [Area Under the Curve](#area-under-the-curve)

# Exploratory Data Analysis

## Library Imports

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# Naive Bayes
library(naivebayes)
```

    ## naivebayes 0.9.6 loaded

``` r
# downSample
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
# ROC Curves
library(ROCR)
```

    ## Loading required package: gplots

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

``` r
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

## Load the CSV Data

``` r
data <- read.csv("../../data/CaseStudy2-data.csv", stringsAsFactors=TRUE, header = TRUE)
```

``` r
head(data)
```

    ##   ID Age Attrition    BusinessTravel DailyRate             Department
    ## 1  1  32        No     Travel_Rarely       117                  Sales
    ## 2  2  40        No     Travel_Rarely      1308 Research & Development
    ## 3  3  35        No Travel_Frequently       200 Research & Development
    ## 4  4  32        No     Travel_Rarely       801                  Sales
    ## 5  5  24        No Travel_Frequently       567 Research & Development
    ## 6  6  27        No Travel_Frequently       294 Research & Development
    ##   DistanceFromHome Education   EducationField EmployeeCount EmployeeNumber
    ## 1               13         4    Life Sciences             1            859
    ## 2               14         3          Medical             1           1128
    ## 3               18         2    Life Sciences             1           1412
    ## 4                1         4        Marketing             1           2016
    ## 5                2         1 Technical Degree             1           1646
    ## 6               10         2    Life Sciences             1            733
    ##   EnvironmentSatisfaction Gender HourlyRate JobInvolvement JobLevel
    ## 1                       2   Male         73              3        2
    ## 2                       3   Male         44              2        5
    ## 3                       3   Male         60              3        3
    ## 4                       3 Female         48              3        3
    ## 5                       1 Female         32              3        1
    ## 6                       4   Male         32              3        3
    ##                  JobRole JobSatisfaction MaritalStatus MonthlyIncome
    ## 1        Sales Executive               4      Divorced          4403
    ## 2      Research Director               3        Single         19626
    ## 3 Manufacturing Director               4        Single          9362
    ## 4        Sales Executive               4       Married         10422
    ## 5     Research Scientist               4        Single          3760
    ## 6 Manufacturing Director               1      Divorced          8793
    ##   MonthlyRate NumCompaniesWorked Over18 OverTime PercentSalaryHike
    ## 1        9250                  2      Y       No                11
    ## 2       17544                  1      Y       No                14
    ## 3       19944                  2      Y       No                11
    ## 4       24032                  1      Y       No                19
    ## 5       17218                  1      Y      Yes                13
    ## 6        4809                  1      Y       No                21
    ##   PerformanceRating RelationshipSatisfaction StandardHours StockOptionLevel
    ## 1                 3                        3            80                1
    ## 2                 3                        1            80                0
    ## 3                 3                        3            80                0
    ## 4                 3                        3            80                2
    ## 5                 3                        3            80                0
    ## 6                 4                        3            80                2
    ##   TotalWorkingYears TrainingTimesLastYear WorkLifeBalance YearsAtCompany
    ## 1                 8                     3               2              5
    ## 2                21                     2               4             20
    ## 3                10                     2               3              2
    ## 4                14                     3               3             14
    ## 5                 6                     2               3              6
    ## 6                 9                     4               2              9
    ##   YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager
    ## 1                  2                       0                    3
    ## 2                  7                       4                    9
    ## 3                  2                       2                    2
    ## 4                 10                       5                    7
    ## 5                  3                       1                    3
    ## 6                  7                       1                    7

### Convert Integers to Strings

  - These numeric columns will be converted into strings

<!-- end list -->

``` r
data$ID <- as.character(data$ID)
data$EmployeeNumber <- as.character(data$EmployeeNumber)
data$EmployeeCount <- as.character(data$EmployeeCount)
data$StandardHours <- as.character(data$StandardHours)
data$Over18 <- as.character(data$Over18)
```

### Convert Integers to Factors

``` r
data$JobInvolvement <- factor(data$JobInvolvement, ordered = TRUE,
                              levels = c(1, 2, 3, 4),
                              labels = c("Low", "Medium", "High", "Very High"))

data$JobSatisfaction <- factor(data$JobSatisfaction, ordered = TRUE,
                              levels = c(1, 2, 3, 4),
                              labels = c("Low", "Medium", "High", "Very High"))

data$PerformanceRating <- factor(data$PerformanceRating, ordered = TRUE,
                              levels = c(1, 2, 3, 4),
                              labels = c("Low", "Good", "Excellent", "Outstanding"))

data$RelationshipSatisfaction <- factor(data$RelationshipSatisfaction, ordered = TRUE,
                              levels = c(1, 2, 3, 4),
                              labels = c("Low", "Medium", "High", "Very High"))

data$WorkLifeBalance <- factor(data$WorkLifeBalance, ordered = TRUE,
                              levels = c(1, 2, 3, 4),
                              labels = c("Bad", "Better", "Good", "Best"))

### THIS WAS NOT ACTUALLY PROVIDED ON THE WALL
data$EnvironmentSatisfaction <- factor(data$EnvironmentSatisfaction, ordered = TRUE,
                              levels = c(1, 2, 3, 4),
                              labels = c("Low", "Medium", "High", "Very High"))

data$StockOptionLevel <- factor(data$StockOptionLevel, ordered = TRUE,
                              levels = c(0, 1, 2, 3),
                              labels = c("Zero", "One", "Two", "Three"))

data$JobLevel <- factor(data$JobLevel, ordered = TRUE,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("One", "Two", "Three", "Four", "Five"))

data$Education <- factor(data$Education, ordered = FALSE,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("One", "Two", "Three", "Four", "Five"))
```

## Prepare Dataframe

``` r
cols_to_remove <- c("ID", "EmployeeNumber", "EmployeeCount", "StandardHours", "Over18")


data.mod <- data %>%
  select(-cols_to_remove) %>%
  mutate(Attrition = factor(Attrition, labels = c("No", "Yes"))) 

summary(data.mod)
```

    ##       Age        Attrition           BusinessTravel   DailyRate     
    ##  Min.   :18.00   No :730   Non-Travel       : 94    Min.   : 103.0  
    ##  1st Qu.:30.00   Yes:140   Travel_Frequently:158    1st Qu.: 472.5  
    ##  Median :35.00             Travel_Rarely    :618    Median : 817.5  
    ##  Mean   :36.83                                      Mean   : 815.2  
    ##  3rd Qu.:43.00                                      3rd Qu.:1165.8  
    ##  Max.   :60.00                                      Max.   :1499.0  
    ##                                                                     
    ##                   Department  DistanceFromHome Education  
    ##  Human Resources       : 35   Min.   : 1.000   One  : 98  
    ##  Research & Development:562   1st Qu.: 2.000   Two  :182  
    ##  Sales                 :273   Median : 7.000   Three:324  
    ##                               Mean   : 9.339   Four :240  
    ##                               3rd Qu.:14.000   Five : 26  
    ##                               Max.   :29.000              
    ##                                                           
    ##           EducationField EnvironmentSatisfaction    Gender      HourlyRate    
    ##  Human Resources : 15    Low      :172           Female:354   Min.   : 30.00  
    ##  Life Sciences   :358    Medium   :178           Male  :516   1st Qu.: 48.00  
    ##  Marketing       :100    High     :258                        Median : 66.00  
    ##  Medical         :270    Very High:262                        Mean   : 65.61  
    ##  Other           : 52                                         3rd Qu.: 83.00  
    ##  Technical Degree: 75                                         Max.   :100.00  
    ##                                                                               
    ##    JobInvolvement  JobLevel                        JobRole     JobSatisfaction
    ##  Low      : 47    One  :329   Sales Executive          :200   Low      :179   
    ##  Medium   :228    Two  :312   Research Scientist       :172   Medium   :166   
    ##  High     :514    Three:132   Laboratory Technician    :153   High     :254   
    ##  Very High: 81    Four : 60   Manufacturing Director   : 87   Very High:271   
    ##                   Five : 37   Healthcare Representative: 76                   
    ##                               Sales Representative     : 53                   
    ##                               (Other)                  :129                   
    ##   MaritalStatus MonthlyIncome    MonthlyRate    NumCompaniesWorked OverTime 
    ##  Divorced:191   Min.   : 1081   Min.   : 2094   Min.   :0.000      No :618  
    ##  Married :410   1st Qu.: 2840   1st Qu.: 8092   1st Qu.:1.000      Yes:252  
    ##  Single  :269   Median : 4946   Median :14074   Median :2.000               
    ##                 Mean   : 6390   Mean   :14326   Mean   :2.728               
    ##                 3rd Qu.: 8182   3rd Qu.:20456   3rd Qu.:4.000               
    ##                 Max.   :19999   Max.   :26997   Max.   :9.000               
    ##                                                                             
    ##  PercentSalaryHike   PerformanceRating RelationshipSatisfaction
    ##  Min.   :11.0      Low        :  0     Low      :174           
    ##  1st Qu.:12.0      Good       :  0     Medium   :171           
    ##  Median :14.0      Excellent  :738     High     :261           
    ##  Mean   :15.2      Outstanding:132     Very High:264           
    ##  3rd Qu.:18.0                                                  
    ##  Max.   :25.0                                                  
    ##                                                                
    ##  StockOptionLevel TotalWorkingYears TrainingTimesLastYear WorkLifeBalance
    ##  Zero :379        Min.   : 0.00     Min.   :0.000         Bad   : 48     
    ##  One  :355        1st Qu.: 6.00     1st Qu.:2.000         Better:192     
    ##  Two  : 81        Median :10.00     Median :3.000         Good  :532     
    ##  Three: 55        Mean   :11.05     Mean   :2.832         Best  : 98     
    ##                   3rd Qu.:15.00     3rd Qu.:3.000                        
    ##                   Max.   :40.00     Max.   :6.000                        
    ##                                                                          
    ##  YearsAtCompany   YearsInCurrentRole YearsSinceLastPromotion
    ##  Min.   : 0.000   Min.   : 0.000     Min.   : 0.000         
    ##  1st Qu.: 3.000   1st Qu.: 2.000     1st Qu.: 0.000         
    ##  Median : 5.000   Median : 3.000     Median : 1.000         
    ##  Mean   : 6.962   Mean   : 4.205     Mean   : 2.169         
    ##  3rd Qu.:10.000   3rd Qu.: 7.000     3rd Qu.: 3.000         
    ##  Max.   :40.000   Max.   :18.000     Max.   :15.000         
    ##                                                             
    ##  YearsWithCurrManager
    ##  Min.   : 0.00       
    ##  1st Qu.: 2.00       
    ##  Median : 3.00       
    ##  Mean   : 4.14       
    ##  3rd Qu.: 7.00       
    ##  Max.   :17.00       
    ## 

``` r
# str(data.mod)
```

## Logistic Regression

## Train / Test Split

``` r
set.seed(1234)

sample.data <- sample_frac(data.mod, 1)

split.perc = .70

train.indices = sample(1:dim(sample.data)[1],round(split.perc * dim(sample.data)[1]))

train = sample.data[train.indices,]
test = sample.data[-train.indices,]

train <- upSample(train, as.factor(train$Attrition), list = FALSE)
train$Class <- NULL

# library(DMwR)
# train <- SMOTE(Attrition ~ ., train, perc.over = 100, perc.under=200)
```

``` r
lg.model <- glm(Attrition~DistanceFromHome+JobInvolvement+OverTime+BusinessTravel+NumCompaniesWorked+WorkLifeBalance+EnvironmentSatisfaction+YearsSinceLastPromotion+YearsWithCurrManager+TrainingTimesLastYear, data = train, family = binomial("logit"))
summary(lg.model)
```

    ## 
    ## Call:
    ## glm(formula = Attrition ~ DistanceFromHome + JobInvolvement + 
    ##     OverTime + BusinessTravel + NumCompaniesWorked + WorkLifeBalance + 
    ##     EnvironmentSatisfaction + YearsSinceLastPromotion + YearsWithCurrManager + 
    ##     TrainingTimesLastYear, family = binomial("logit"), data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1330  -0.7515  -0.0037   0.7400   2.4545  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     -1.306889   0.429522  -3.043 0.002345 ** 
    ## DistanceFromHome                 0.060319   0.009357   6.446 1.15e-10 ***
    ## JobInvolvement.L                -1.501131   0.279874  -5.364 8.16e-08 ***
    ## JobInvolvement.Q                 0.763350   0.231932   3.291 0.000997 ***
    ## JobInvolvement.C                 0.043293   0.154838   0.280 0.779784    
    ## OverTimeYes                      1.699470   0.173361   9.803  < 2e-16 ***
    ## BusinessTravelTravel_Frequently  1.733107   0.370370   4.679 2.88e-06 ***
    ## BusinessTravelTravel_Rarely      1.096741   0.336320   3.261 0.001110 ** 
    ## NumCompaniesWorked               0.054291   0.032471   1.672 0.094526 .  
    ## WorkLifeBalance.L               -1.703038   0.269222  -6.326 2.52e-10 ***
    ## WorkLifeBalance.Q                0.595273   0.226205   2.632 0.008499 ** 
    ## WorkLifeBalance.C               -0.248960   0.163147  -1.526 0.127014    
    ## EnvironmentSatisfaction.L       -0.287499   0.160339  -1.793 0.072962 .  
    ## EnvironmentSatisfaction.Q        1.054188   0.172954   6.095 1.09e-09 ***
    ## EnvironmentSatisfaction.C       -0.478558   0.176596  -2.710 0.006730 ** 
    ## YearsSinceLastPromotion          0.158845   0.033640   4.722 2.34e-06 ***
    ## YearsWithCurrManager            -0.284032   0.033917  -8.374  < 2e-16 ***
    ## TrainingTimesLastYear           -0.054926   0.066043  -0.832 0.405597    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1408.48  on 1015  degrees of freedom
    ## Residual deviance:  969.75  on  998  degrees of freedom
    ## AIC: 1005.8
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
test$predict <- predict(lg.model, test, type="response")
test$predict <- as.factor(ifelse(test$predict >0.45, "Yes", "No"))
```

### Test

``` r
confusionMatrix(data=test$predict,  
                reference=test$Attrition, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  No Yes
    ##        No  153  12
    ##        Yes  69  27
    ##                                           
    ##                Accuracy : 0.6897          
    ##                  95% CI : (0.6297, 0.7453)
    ##     No Information Rate : 0.8506          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.2381          
    ##                                           
    ##  Mcnemar's Test P-Value : 4.902e-10       
    ##                                           
    ##             Sensitivity : 0.6923          
    ##             Specificity : 0.6892          
    ##          Pos Pred Value : 0.2813          
    ##          Neg Pred Value : 0.9273          
    ##              Prevalence : 0.1494          
    ##          Detection Rate : 0.1034          
    ##    Detection Prevalence : 0.3678          
    ##       Balanced Accuracy : 0.6907          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

### Area Under the Curve

``` r
# ?pROC

auc <- roc(as.integer(test$Attrition), as.integer(test$predict))


g <- ggroc(auc, alpha = 0.5, colour = "red", linetype = 2, size = 2) +
  theme_minimal() + 
  ggtitle(paste('AUC of Test Set:', round(auc$auc[[1]],4))) + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")
  

plot(g)
```

<img src="logisticregression_files/figure-gfm/logistic-regression-auc-plot-test-1.png" style="display: block; margin: auto;" />
