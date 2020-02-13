#brexit analysis
update.packages("dslabs")
library(tidyverse)
options(digits = 3)
# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500 #number of voters in samples
ev_remain <- N*p #expected number voting remain
se_remain <- sqrt(p*(1-p)*N) 
se_remain
se_xbar <- sqrt(p*(1-p)/N) #xbar is expected proportion voting remain
se_xbar
d #spread between proportion voting remain and proportion voting leave
se_d <- 2*se_xbar
se_d

head(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)


brexit_polls[1,] #yougov poll
xhat1 <- brexit_polls$x_hat[1]
xhat1
#lower 95% CI for remain
lower <- xhat1 - qnorm(.975)*sqrt(xhat1*(1-xhat1)/brexit_polls$samplesize[1])
#upper CI for remain
upper <- xhat1 + qnorm(.975)*sqrt(xhat1*(1-xhat1)/brexit_polls$samplesize[1])
lower
upper

#june polls only
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize)) %>%
  mutate(se_d_2 = 2*se_x_hat) %>%
  mutate(lower = spread - qnorm(.975)*se_d_2) %>%
  mutate(upper = spread + qnorm(.975)*se_d_2) %>%
  mutate(hit = d >= lower & d <= upper)
head(june_polls)
nrow(june_polls)
length(which(0 >= june_polls$lower & 0 <= june_polls$upper))/nrow(june_polls)
#above is the proportion of polls that contain 0
length(which(june_polls$lower > 0))/nrow(june_polls) 
#proportion that predict remain (CI totally above zero)
mean(june_polls$hit)

#hit rate by pollster
june_polls %>% group_by(pollster) %>%
  summarize(npolls = n(), hit_rate = mean(hit), bias = mean(spread) - d) %>% 
  #all summarizes must be in 1 summarize or it won't work
  arrange(desc(hit_rate))

#boxplot
june_polls %>% ggplot(aes(x = poll_type, y = spread)) +
  geom_boxplot()

#calculate CIs by spread for june polls
#grouped by poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = ((spread + 1)/2), 
            se_p_hat = sqrt(p_hat*(1-p_hat)/N), 
            se_spread = 2*se_p_hat, 
            lower = spread - qnorm(.975)*se_spread, 
            upper = spread + qnorm(.975)*se_spread)
combined_by_type

#2by2 table of poll type and status, perform chi square test
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit) 
head(brexit_hit)
brexit_hit_2by2 <- tibble(poll_type = c("Online","Telephone"), 
                          hit = c(length(which(brexit_hit$poll_type == "Online" & 
                                                 brexit_hit$hit == TRUE)), 
                                  length(which(brexit_hit$poll_type == "Telephone" & 
                                                 brexit_hit$hit == TRUE))),
                          no_hit = c(length(which(brexit_hit$poll_type == "Online" & 
                                                    brexit_hit$hit == FALSE)),
                                     length(which(brexit_hit$poll_type == "Telephone" & 
                                                    brexit_hit$hit == FALSE))))
brexit_hit_2by2
chisqtest_brexit <- brexit_hit_2by2 %>%
  select(-poll_type) %>%
  chisq.test()
chisqtest_brexit$p.value
#q10
odds_online <- (brexit_hit_2by2$hit[1]/
                  (brexit_hit_2by2$hit[1] + brexit_hit_2by2$no_hit[1])) / 
  (brexit_hit_2by2$no_hit[1]/
     (brexit_hit_2by2$hit[1] + brexit_hit_2by2$no_hit[1]))
odds_online
odds_telephone <- (brexit_hit_2by2$hit[2]/
                     (brexit_hit_2by2$hit[2] + brexit_hit_2by2$no_hit[2])) / 
  (brexit_hit_2by2$no_hit[2]/
     (brexit_hit_2by2$hit[2] + brexit_hit_2by2$no_hit[2]))
odds_telephone
odds_online/odds_telephone
#plot spread over time, colored by poll type
#plot smooth curves with span of 0.4, with individual data points. line for final d
spreadbytime <- brexit_polls %>% 
  ggplot(aes(x = enddate, y = spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -0.038))
spreadbytime
#graph proportion over time colored by vote, raw proportions of votes on givenpoll
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
voteproportion_bytime <- brexit_long %>%
  ggplot(aes(x = enddate, y = proportion, col = vote)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) #add trendline
voteproportion_bytime
