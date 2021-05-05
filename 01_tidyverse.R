# Suggestive solutions
library(tidyverse)
library(ggplot2)
library(lubridate)
# Q2. 
StockPrice = readr::read_csv("data/StockPrice.csv")
StockPrice %>% nrow()
StockPrice %>% 
  distinct(permno) %>% 
  nrow()

StockPrice %>% 
  count(permno)

min(StockPrice$prc)
max(StockPrice$prc)

# Q3. 
summary(StockPrice$prc)

# Q4. 
StockPrice = StockPrice %>% 
  group_by(permno) %>% 
  mutate(ret = log(prc/lag(prc))) %>% 
  ungroup()

# Q5. 
StockPrice %>% 
  group_by(permno) %>% 
  summarize(
    mean(ret, na.rm=TRUE),
    median(ret, na.rm=TRUE),
    min(ret, na.rm=TRUE),
    max(ret, na.rm=TRUE),
    sd(ret, na.rm=TRUE),
  ) %>% 
  ungroup()
  
# Q6. 
StockPrice %>% 
  group_by(permno) %>% 
  slice_tail() %>% 
  ungroup()

# Q7. 
StockPrice %>% 
  group_by(permno) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  mutate(
    f_prc = prc*exp(0.05*0.5)
  )

# Q8. 
# e.g. permno = 10001
StockPrice %>% 
  filter(permno==10001, lubridate::month(date) == 12) %>% 
  na.omit() %>% 
  ggplot(aes(x=date, y=prc)) + geom_line()
StockPrice %>% 
  filter(permno==10001, lubridate::month(date) == 12) %>% 
  na.omit() %>% 
  ggplot(aes(x=date, y=ret)) + geom_line()

# Q9.
Ff=readr::read_csv("data/ff4_monthly_202104042231.csv")
Ff = Ff %>% 
  select(year, month, mktrf, rf)
StockPrice = StockPrice %>% 
  mutate(
    year = year(date),
    month = month(date)
  )
StockPrice = StockPrice %>% 
  left_join(Ff, by=c("year", "month"))

# scatter plot
StockPrice %>% 
  filter(permno==10001, lubridate::month(date) == 12) %>% 
  na.omit() %>% 
  mutate(retrf = ret-rf) %>% 
  ggplot(aes(x=retrf, y=mktrf)) + geom_point()
# repeat Q7 by real rf
StockPrice %>% 
  group_by(permno) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  mutate(
    f_prc = prc*exp(rf*12*0.5)
  ) %>% View()

# Q10.
StockPrice %>% 
  group_by(permno) %>% 
  summarize(
    meanret = mean(ret, na.rm=TRUE),
    meanmktret = mean(mktrf+rf,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    large_than_mktret = ifelse(meanret>=meanmktret,1,0)
  ) %>% 
  count(large_than_mktret)
