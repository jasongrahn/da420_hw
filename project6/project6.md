project6
================
Jason Grahn
2/11/2019

``` r
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

sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
```

``` r
emotion_plot <- ggplot(sent_df, aes(x=emotion)) + 
  geom_bar(aes(y=..count.., fill=emotion)) + 
  scale_fill_brewer(palette="Dark2") + 
  labs(title = "Emotion Categories",
       x="", 
       y="") +
  theme(legend.position = "none")

polarity_plot <- ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(title="Polarity Categories", 
       x = "",
       y="") +
  theme(legend.position = "none")

cowplot::plot_grid(emotion_plot,polarity_plot)
```

![](project6_files/figure-markdown_github/unnamed-chunk-3-1.png)

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
colnames(tdm) = emos

## unsure why this doesn't work: 
#comparison.cloud(tdm, 
#                 colors = brewer.pal(nemo, "Dark2"),
#                 scale = c(3,.5), 
#                 random.order = FALSE,
#                 title.size = 1.5)
```
