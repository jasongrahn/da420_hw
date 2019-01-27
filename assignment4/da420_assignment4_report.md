da420\_homework4
================
Jason Grahn
1/26/2019

``` r
# download from: http://ie.technion.ac.il/serveng/callcenterdata/index.html, 
#   http://ie.technion.ac.il/serveng/callcenterdata/FebruaryTxt.ZIP

#make our data frame
callcenterdata <- read.table(here::here("assignment4/february.txt"), 
                      header = TRUE,
                      sep = "") %>% 
  #filter out unnecessary bits
  filter(server != "PHANTOM",  #ghost calls dont count
         vru_time >= 0,  #vru time can't be negative
         date < 990208) %>%  #just use the one week
  #make some new variables
  mutate(wait_total = vru_time + q_time, 
         long_wait_binary = if_else(wait_total>120, 1, 0), #if it's a long wait, score it 1
         date = ymd(19000000 + date), #make crappy date format into full date number, then make it ymd
         day_of_week = factor(wday(date), #use date to make day of week factors
                              levels = c(1:7),
                              labels = c("Sunday","Monday","Tuesday",
                                         "Wednesday","Thursday","Friday","Saturday"))) 
#let's summarize it
long_call_summary <- callcenterdata %>% 
  group_by(day_of_week) %>% 
  summarize(count = n(),
            long_count = sum(long_wait_binary),
            percentage_long = round(long_count / count * 100, 2)) 

#let's plot it
long_call_summary %>% 
  select(day_of_week, percentage_long) %>% 
  ggplot(aes(x = day_of_week, y = percentage_long, label = percentage_long)) +
  geom_col(aes(fill = desc(percentage_long))) +
  geom_label() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title= "Days of the week and % of calls with wait above 120 seconds",
       subtitle = "Sunday and Monday need staffing attention to bring rate lower")
```

![](da420_assignment4_report_files/figure-markdown_github/unnamed-chunk-1-1.png)
