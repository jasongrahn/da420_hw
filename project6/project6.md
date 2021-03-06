Project 06
================
Jason Grahn
2/15/2019

``` r
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## A Tidytext example from https://www.tidytextmining.com/ would be 
## more modern and applicable for student learning. 

## Instead, let's install a bunch of dead packages because this code 
## is old and out of date. 

require(devtools)

# install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz") 
## this is a dead URL

# note the original URL is dead too. replace the .org with .net and the download works correctly. 
# install_url("http://www.omegahat.net/Rstem/Rstem_0.4-1.tar.gz")
# install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
```

``` r
##  It's incredibly bad practice to ship code that asks for packages to be installed. This would be better as a docker image or a use_this:: package.

# install.packages("plyr") 
# install.packages("ggplot2") 
# install.packages("wordcloud") 
# install.packages("RColorBrewer") 
# install.packages("tm") 
# install.packages("SnowballC")

library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(sentiment)
```

``` r
set.seed(42)
data <- readLines("http://www.r-bloggers.com/wp-content/uploads/2016/01/vent.txt") 
 # from: http://www.wvgazettemail.com/ 
df <- data.frame(data) 
textdata <- df[df$data, ] 
textdata = gsub("[[:punct:]]", "", textdata)
```

``` r
textdata = gsub("[[:punct:]]", "", textdata) 
textdata = gsub("[[:digit:]]", "", textdata) 
textdata = gsub("http\\w+", "", textdata) 
textdata = gsub("[ \t]{2,}", "", textdata) 
textdata = gsub("^\\s+|\\s+$", "", textdata) 

try.error = function(x) { 
  y = NA 
  try_error = tryCatch(tolower(x), error=function(e) e) 
  if (!inherits(try_error, "error")) 
    y = tolower(x) 
  return(y) 
  } 

textdata = sapply(textdata, try.error) 
textdata = textdata[!is.na(textdata)] 
names(textdata) = NULL
```

``` r
class_emo = classify_emotion(textdata, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(textdata, algorithm="bayes")
polarity = class_pol[,4]

sent_df = data.frame(text=textdata, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)

sent_df = within(sent_df, emotion <- factor(emotion, 
                                            levels=names(sort(table(emotion), 
                                                                       decreasing=TRUE))))
```

``` r
emotion_plot <- ggplot(sent_df, aes(x=emotion)) + 
  geom_bar(aes(y=..count.., fill=emotion)) + 
  scale_fill_brewer(palette="Dark2") + 
  labs(title = "Emotion Categories",
       x="", 
       y="") +
  theme(legend.position = "none") +
  ylim(0,55)

polarity_plot <- ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(title="Polarity Categories", 
       x = "",
       y="") +
  theme(legend.position = "none") +
  ylim(0,55)

cowplot::plot_grid(emotion_plot,polarity_plot)
```

<img src="project6_files/figure-markdown_github/unnamed-chunk-3-1.png" width="90%" />

``` r
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)

for (i in 1:nemo){ 
  tmp = textdata[emotion == emos[i]] 
  emo.docs[i] = paste(tmp, collapse=" ") 
}

emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
```

``` r
comparison.cloud(tdm, 
                 colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), 
                 random.order = FALSE,
                 title.size = 1.5)
```

<img src="project6_files/figure-markdown_github/unnamed-chunk-5-1.png" width="90%" />
