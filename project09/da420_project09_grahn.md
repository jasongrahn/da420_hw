project09
================
jason grahn
3/8/2019

Use TWO of the supervised learning predictive modeling methods (e.g., neural network, support vector machine, and naïve Bayes) you choose to do the prediction on the Iris Data Set.

-   Compare the performance (i.e., accuracy) of the two methods.
-   Please describe each method you used,
-   the R/SAS package you used,
-   the performance (i.e., accuracy) comparison of the two methods.

Make sure you include the commands and outputs, as well as the interpretations of each output.

``` r
#load data
data <- iris 

# i dont like Camel case headers, so I'll correct those too
data <- data.table::setnames(data, tolower(names(iris[1:5])))

#show head of the data
head(data,5) %>% 
  knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align:right;">
sepal.length
</th>
<th style="text-align:right;">
sepal.width
</th>
<th style="text-align:right;">
petal.length
</th>
<th style="text-align:right;">
petal.width
</th>
<th style="text-align:left;">
species
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
5.1
</td>
<td style="text-align:right;">
3.5
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
4.9
</td>
<td style="text-align:right;">
3.0
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
4.7
</td>
<td style="text-align:right;">
3.2
</td>
<td style="text-align:right;">
1.3
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
4.6
</td>
<td style="text-align:right;">
3.1
</td>
<td style="text-align:right;">
1.5
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
<tr>
<td style="text-align:right;">
5.0
</td>
<td style="text-align:right;">
3.6
</td>
<td style="text-align:right;">
1.4
</td>
<td style="text-align:right;">
0.2
</td>
<td style="text-align:left;">
setosa
</td>
</tr>
</tbody>
</table>
``` r
summary(iris)
```

    ##   sepal.length    sepal.width     petal.length    petal.width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

A short exploration of the data shows sepal width has the the smallest variation of means to it's median, while petal length appears to have the largest differences. We have the same number of species in the dataset which will make it ideal for sampling.

Visualization
=============

``` r
# I like a common theme to my plots
common_theme <- theme_light() +
  theme(legend.position = "bottom")

# histograms for each 
histogram <- iris %>% 
  ggplot() +
  geom_histogram(aes(x = petal.length, fill=species)) +
  labs(title = "Histogram") +
  common_theme

# density plots for each
density <- iris %>% 
  ggplot() + 
  geom_density(aes(x=petal.length, fill=species), alpha = 0.5) +
  labs(title = "Density") +
  common_theme

#put these together into a pretty picture
cowplot::plot_grid(histogram, density)
```

![](da420_project09_grahn_files/figure-markdown_github/histogram%20and%20density%20plotting-1.png)

The histogram shows setosa clearly sits on it's own with very short petal lengths; but virginica and veriscolor have at least some degree of overlap.

Density plotting reinforces the histogram. It does a better job showing the overlap between virginica and versicolor.

``` r
sepal.scatter <- iris %>% ggplot() + 
  geom_point(aes(x = sepal.length, y = sepal.width, color = species)) +
  labs(title = "Sepal vars") +
  common_theme

petal.scatter <- iris %>% ggplot() + 
  geom_point(aes(x = petal.length, y = petal.width, color = species)) +
  labs(title = "Petal vars") +
  common_theme

cowplot::plot_grid(sepal.scatter, petal.scatter)
```

![](da420_project09_grahn_files/figure-markdown_github/scatter%20plotting-1.png)

Scatterplotting for sepal variables shows similar behaviors to what we saw in the histogram and density plots. Clustering of the setosa lengths and widths and mixtures of versicolor and viginica points. The petal variables are a bit more defined. Setosa certainly sits on it's own, and now we can see better definitions between versicolor and virginica species; with virginica having (generally) longer and wider petals than versicolor.

Building sample vs training data
--------------------------------

### Use two thirds of data of each class (i.e., Setosa, Versicolour and Virginica) as training set and do the prediction on the rest.

The first step in making our train v test dataset would be to assign random values to each row, then pull those out. I dont know how to do that in one full swoop, so I'll subset each species, build some 1's and 2's, assign *those* to each subset, put them all back together, then filter out train vs test with the 1's and 2's. I'm 100% positive there's a better way to do this, probably with one simple package; but this is what I *know*, so that's what I'll run with. We **should** be able to use the train vs test data for both methods.

``` r
set.seed(666)

setosa.sub <- data %>% filter(species == "setosa")
versicolor.sub <- data %>% filter(species == "versicolor") 
virginica.sub <- data %>% filter(species == "virginica") 

set.seed(666)

ind <- sample(2, 
              nrow(setosa.sub), 
              replace=TRUE,
              prob=c(0.67, 0.33))

#tibble(ind) %>% group_by(ind) %>% summarize(n = n())
setosa.sub <- setosa.sub %>% mutate(ind = ind) 
versicolor.sub <- versicolor.sub %>% mutate(ind = ind) 
virginica.sub <- virginica.sub %>% mutate(ind = ind)

data.joined <- setosa.sub %>% 
  full_join(versicolor.sub) %>% 
  full_join(virginica.sub)
```

    ## Joining, by = c("sepal.length", "sepal.width", "petal.length", "petal.width", "species", "ind")
    ## Joining, by = c("sepal.length", "sepal.width", "petal.length", "petal.width", "species", "ind")

``` r
# make training set and labels
data.training <- data.joined %>% 
  filter(ind == 1) %>% 
  dplyr::select(sepal.length, sepal.width, petal.length, petal.width)

data.trainLabels <- data.joined[ind==1,5]
# make testing set and labels
data.test <- data.joined %>% 
  filter(ind == 2) %>% 
  dplyr::select(sepal.length, sepal.width, petal.length, petal.width)

data.testLabels <- data.joined[ind==2, 5]
```

K-Nearest Neighbors
===================

*K*-Nearest Neighbors uses the neighbors of a given datapoint in order to classify a new datapoint. It's called *k*-nearest because the value *k* determines how many neighbor points are used in the classification. If too little neighbors are used, we may over-fit the data; while using too many neighbors overly "smooths" out the data and leads to more generalization, resulting in less classifications.

To execute KNN, we can use the `class` package. It's good to reinforce the education we've received so we use this this package because it requires minimal inputs and we've been trained on it already. `Class` contains the `knn` function which requires, quite simply, your training dataset, your testing dataset, what your selected *k-value* is, and how to label the output.

``` r
data.knn <- knn(train = data.training,
                test = data.test, 
                k = 5,
                cl = data.trainLabels)
table(data.knn)
```

    ## data.knn
    ##     setosa versicolor  virginica 
    ##         17         18         16

``` r
table(data.testLabels, data.knn)
```

    ##                data.knn
    ## data.testLabels setosa versicolor virginica
    ##      setosa         17          0         0
    ##      versicolor      0         17         0
    ##      virginica       0          1        16

``` r
accuracy.table <- data.joined %>% 
  filter(ind == 2) %>% 
  mutate(data.knn = data.knn,
         predict.true = if_else(species == data.knn, TRUE, FALSE)) 

accuracy.rate <- accuracy.table %>% 
  group_by(predict.true) %>% 
  summarize(count = n(),
            rate = count / nrow(accuracy.table) * 100,
            rate = round(rate,2))

accuracy.rate %>% 
  knitr::kable()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
predict.true
</th>
<th style="text-align:right;">
count
</th>
<th style="text-align:right;">
rate
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.96
</td>
</tr>
<tr>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
98.04
</td>
</tr>
</tbody>
</table>
Nice! We have 98.04 percent accuracy!
