very frustating way to make a spine.chart
================
Jason Grahn
1/25/2019

``` r
# Traditional Conjoint Analysis (R)

print.digits <- 2  # set number of digits on print and spine chart
library(support.CEs)  # package for survey construction 

# generate a balanced set of product profiles for survey
provider.survey <- Lma.design(attribute.names = 
  list(brand = c("AT&T","T-Mobile","US Cellular","Verizon"), 
  startup = c("$100","$200","$300","$400"), 
  monthly = c("$100","$200","$300","$400"),
  service = c("4G NO","4G YES"), 
  retail = c("Retail NO","Retail YES"),
  apple = c("Apple NO","Apple YES"), 
  samsung = c("Samsung NO","Samsung YES"), 
  google = c("Nexus NO","Nexus YES")), nalternatives = 1, nblocks=1, seed=9999)

#sink(here::here("project3/questions_for_survey.txt")  # send survey to external text file
#questionnaire(provider.survey)
#sink() # send output back to the screen

# user-defined function for plotting descriptive attribute names 
effect.name.map <- function(effect.name) { 
  if(effect.name=="brand") return("Mobile Service Provider")
  if(effect.name=="startup") return("Start-up Cost")
  if(effect.name=="monthly") return("Monthly Cost")
  if(effect.name=="service") return("Offers 4G Service")
  if(effect.name=="retail") return("Has Nearby Retail Store")
  if(effect.name=="apple") return("Sells Apple Products")
  if(effect.name=="samsung") return("Sells Samsung Products")
  if(effect.name=="google") return("Sells Google/Nexus Products")
  } 

# read in conjoint survey profiles with respondent ranks
conjoint.data.frame <- readr::read_csv(here::here("project3/mobile.csv"))

#building a randomizer to change the rankings in the mobile data.
set.seed(42)
ranklist <- c(1:16) #build a vector of 1 through 16
samplist <- sample(ranklist, #sample that vector and make it a list
                   size=16)
samplist <- tibble::tibble(samplist) #turn that list into it's own table

#using that table to redo the ranking list. 
conjoint.data.frame <- add_column(conjoint.data.frame, sample(ranklist,
                                                              size=16)) %>% 
  rename(ranking2 = "sample(ranklist, size = 16)") 
conjoint.data.frame
```

    ## # A tibble: 16 x 10
    ##    brand startup monthly service retail apple samsung google ranking
    ##    <chr> <chr>   <chr>   <chr>   <chr>  <chr> <chr>   <chr>    <dbl>
    ##  1 AT&T  $100    $100    4G NO   Retai… APPL… Samsun… Nexus…      11
    ##  2 Veri… $300    $100    4G NO   Retai… APPL… Samsun… Nexus…      12
    ##  3 US C… $400    $200    4G NO   Retai… APPL… Samsun… Nexus…       9
    ##  4 Veri… $400    $400    4G YES  Retai… APPL… Samsun… Nexus…       2
    ##  5 Veri… $200    $300    4G NO   Retai… APPL… Samsun… Nexus…       8
    ##  6 Veri… $100    $200    4G YES  Retai… APPL… Samsun… Nexus…      13
    ##  7 US C… $300    $300    4G YES  Retai… APPL… Samsun… Nexus…       7
    ##  8 AT&T  $400    $300    4G NO   Retai… APPL… Samsun… Nexus…       4
    ##  9 AT&T  $200    $400    4G YES  Retai… APPL… Samsun… Nexus…       5
    ## 10 T-mo… $400    $100    4G YES  Retai… APPL… Samsun… Nexus…      16
    ## 11 US C… $100    $400    4G NO   Retai… APPL… Samsun… Nexus…       3
    ## 12 T-mo… $200    $200    4G NO   Retai… APPL… Samsun… Nexus…       6
    ## 13 T-mo… $100    $300    4G YES  Retai… APPL… Samsun… Nexus…      10
    ## 14 US C… $200    $100    4G YES  Retai… APPL… Samsun… Nexus…      15
    ## 15 T-mo… $300    $400    4G NO   Retai… APPL… Samsun… Nexus…       1
    ## 16 AT&T  $300    $200    4G YES  Retai… APPL… Samsun… Nexus…      14
    ## # … with 1 more variable: ranking2 <int>

``` r
# set up sum contrasts for effects coding as needed for conjoint analysis
options(contrasts=c("contr.sum","contr.poly"))

# main effects model specification
main.effects.model <- {ranking2 ~ brand + startup + monthly + service + 
  retail + apple + samsung + google}

# fit linear regression model using main effects only (no interaction terms)
main.effects.model.fit <- lm(main.effects.model, data=conjoint.data.frame)
print(summary(main.effects.model.fit)) 
```

    ## 
    ## Call:
    ## lm.default(formula = main.effects.model, data = conjoint.data.frame)
    ## 
    ## Residuals:
    ##     1     2     3     4     5     6     7     8     9    10    11    12 
    ##  0.25 -0.25 -0.25  0.25  0.25 -0.25  0.25 -0.25 -0.25  0.25  0.25  0.25 
    ##    13    14    15    16 
    ## -0.25 -0.25 -0.25  0.25 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)    8.500      0.250  34.000   0.0187 *
    ## brand1         1.250      0.433   2.887   0.2123  
    ## brand2         0.250      0.433   0.577   0.6667  
    ## brand3        -2.000      0.433  -4.619   0.1357  
    ## startup1       3.000      0.433   6.928   0.0913 .
    ## startup2      -2.750      0.433  -6.351   0.0994 .
    ## startup3       1.250      0.433   2.887   0.2123  
    ## monthly1      -1.500      0.433  -3.464   0.1789  
    ## monthly2       1.500      0.433   3.464   0.1789  
    ## monthly3       2.000      0.433   4.619   0.1357  
    ## service1      -0.125      0.250  -0.500   0.7048  
    ## retail1        1.250      0.250   5.000   0.1257  
    ## apple1         2.375      0.250   9.500   0.0668 .
    ## samsung1       1.875      0.250   7.500   0.0844 .
    ## google1       -0.875      0.250  -3.500   0.1772  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1 on 1 degrees of freedom
    ## Multiple R-squared:  0.9971, Adjusted R-squared:  0.9559 
    ## F-statistic: 24.21 on 14 and 1 DF,  p-value: 0.1581

``` r
# save key list elements of the fitted model as needed for conjoint measures
conjoint.results <- 
  main.effects.model.fit[c("contrasts","xlevels","coefficients")]

conjoint.results$attributes <- names(conjoint.results$contrasts)

# compute and store part-worths in the conjoint.results list structure
part.worths <- conjoint.results$xlevels  # list of same structure as xlevels
end.index.for.coefficient <- 1  # intitialize skipping the intercept
part.worth.vector <- NULL # used for accumulation of part worths
for(index.for.attribute in seq(along=conjoint.results$contrasts)) {
  nlevels <- length(unlist(conjoint.results$xlevels[index.for.attribute]))
  begin.index.for.coefficient <- end.index.for.coefficient + 1
  end.index.for.coefficient <- begin.index.for.coefficient + nlevels -2
  last.part.worth <- -sum(conjoint.results$coefficients[
    begin.index.for.coefficient:end.index.for.coefficient])
  part.worths[index.for.attribute] <- 
    list(as.numeric(c(conjoint.results$coefficients[
      begin.index.for.coefficient:end.index.for.coefficient],
      last.part.worth)))
  part.worth.vector <- 
    c(part.worth.vector,unlist(part.worths[index.for.attribute]))    
  } 
conjoint.results$part.worths <- part.worths

# compute standardized part-worths
standardize <- function(x) {(x - mean(x)) / sd(x)}
conjoint.results$standardized.part.worths <- 
  lapply(conjoint.results$part.worths,standardize)
 
# compute and store part-worth ranges for each attribute 
part.worth.ranges <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts)) 
  part.worth.ranges[index.for.attribute] <- 
  dist(range(conjoint.results$part.worths[index.for.attribute]))
conjoint.results$part.worth.ranges <- part.worth.ranges

sum.part.worth.ranges <- sum(as.numeric(conjoint.results$part.worth.ranges))

# compute and store importance values for each attribute 
attribute.importance <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts)) 
  attribute.importance[index.for.attribute] <- 
  (dist(range(conjoint.results$part.worths[index.for.attribute]))/
  sum.part.worth.ranges) * 100
conjoint.results$attribute.importance <- attribute.importance
 
# data frame for ordering attribute names
attribute.name <- names(conjoint.results$contrasts)
attribute.importance <- as.numeric(attribute.importance)
temp.frame <- data.frame(attribute.name,attribute.importance)
conjoint.results$ordered.attributes <- 
  as.character(temp.frame[sort.list(
  temp.frame$attribute.importance,decreasing = TRUE),"attribute.name"])

# respondent internal consistency added to list structure
conjoint.results$internal.consistency <- summary(main.effects.model.fit)$r.squared 
 
# user-defined function for printing conjoint measures
if (print.digits == 2) 
  pretty.print <- function(x) {sprintf("%1.2f",round(x,digits = 2))} 
if (print.digits == 3) 
  pretty.print <- function(x) {sprintf("%1.3f",round(x,digits = 3))} 
 
# report conjoint measures to console 
# use pretty.print to provide nicely formated output
for(k in seq(along=conjoint.results$ordered.attributes)) {
  cat("\n","\n")
  cat(conjoint.results$ordered.attributes[k],"Levels: ",
  unlist(conjoint.results$xlevels[conjoint.results$ordered.attributes[k]]))
  
  cat("\n"," Part-Worths:  ")
  cat(pretty.print(unlist(conjoint.results$part.worths
    [conjoint.results$ordered.attributes[k]])))
    
  cat("\n"," Standardized Part-Worths:  ")
  cat(pretty.print(unlist(conjoint.results$standardized.part.worths
    [conjoint.results$ordered.attributes[k]])))  
    
  cat("\n"," Attribute Importance:  ")
  cat(pretty.print(unlist(conjoint.results$attribute.importance
    [conjoint.results$ordered.attributes[k]])))
  }
```

    ## 
    ##  
    ## startup Levels:  $100 $200 $300 $400
    ##   Part-Worths:  3.00 -2.75 1.25 -1.50
    ##   Standardized Part-Worths:  1.15 -1.06 0.48 -0.58
    ##   Attribute Importance:  22.12
    ##  
    ## apple Levels:  APPLE NO APPLE YES
    ##   Part-Worths:  2.37 -2.37
    ##   Standardized Part-Worths:  0.71 -0.71
    ##   Attribute Importance:  18.27
    ##  
    ## monthly Levels:  $100 $200 $300 $400
    ##   Part-Worths:  -1.50 1.50 2.00 -2.00
    ##   Standardized Part-Worths:  -0.73 0.73 0.98 -0.98
    ##   Attribute Importance:  15.38
    ##  
    ## samsung Levels:  Samsung NO Samsung YES
    ##   Part-Worths:  1.87 -1.87
    ##   Standardized Part-Worths:  0.71 -0.71
    ##   Attribute Importance:  14.42
    ##  
    ## brand Levels:  AT&T T-mobile US Cellular Verizon
    ##   Part-Worths:  1.25 0.25 -2.00 0.50
    ##   Standardized Part-Worths:  0.89 0.18 -1.43 0.36
    ##   Attribute Importance:  12.50
    ##  
    ## retail Levels:  Retail NO Retail YES
    ##   Part-Worths:  1.25 -1.25
    ##   Standardized Part-Worths:  0.71 -0.71
    ##   Attribute Importance:  9.62
    ##  
    ## google Levels:  Nexus NO Nexus YES
    ##   Part-Worths:  -0.87 0.87
    ##   Standardized Part-Worths:  -0.71 0.71
    ##   Attribute Importance:  6.73
    ##  
    ## service Levels:  4G NO 4G YES
    ##   Part-Worths:  -0.12 0.12
    ##   Standardized Part-Worths:  -0.71 0.71
    ##   Attribute Importance:  0.96

``` r
# plotting of spine chart begins here
# all graphical output is routed to external pdf file
pdf(file = here::here("project3/fig_preference_mobile_services_results.pdf"), width=8.5, height=11)
spine.chart(conjoint.results)
dev.off()  # close the graphics output device
```

    ## png 
    ##   2

``` r
# Suggestions for the student:
# Enter your own rankings for the product profiles and generate
# conjoint measures of attribute importance and level part-worths.
# Note that the model fit to the data is a linear main-effects model.
# See if you can build a model with interaction effects for service
# provider attributes.
```
