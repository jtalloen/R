
##########################################################################################
############################### Set up and Loading data ##################################
##########################################################################################


###  Markdown set up

---
  title: "12 Period AP Small Pilot"
author: "Joachim Talloen"
date: "`r format(Sys.time(), '%d %B %Y')`"
output: 
  pdf_document: 
  keep_tex: true
header-includes:
  \usepackage{float}
  \floatplacement{figure}{H}
  \newcommand{\bcenter}{\begin{center}}
  \newcommand{\ecenter}{\end{center}}
---

### Latex Title Things 
  
  \newpage
  \bcenter
  ## Within Subject Period Level Analysis
  \ecenter
  \hfill\break
  ### Summary Statistics at Period Level


### Loading packages and data
pacman::p_load(plyr, dplyr, lfe, tidyverse, stargazer, ggplot2, gmodels, pwr, tidyr, magrittr, car, scales, ggpubr, cowplot, ggthemes)
knitr::opts_chunk$set(comment = NA, echo = F, results = 'asis', out.width = "70%", fig.align = "center",
                      warning = F, message = F)
# set wd if you want
knitr::opts_knit$set(root.dir = "C:/Users/joach/Box/Joe/Research/CMU/Dissertation/Cheating with ER/")
df <- read.csv("190417__Cheating_with_ER_Pilot_6_Encryptions.csv")


##########################################################################################
############################### Variable Manipulation ####################################
##########################################################################################

### Creating new variables using mutate and using subset to delete rows and columns
### subset filters based on criteria you want data points to have

df1 <- df %>%
  mutate(X1.same = ifelse(df1[, 'X1.noStart'] == df1[, 'X1.noBreaks'], 1, 0),
         completeall = select(., complete1, complete2, complete3) %>% rowSums(na.rm = T)) %>%
  subset(., mtwid != "" & mtwid != "mtwid") 

agreementdata2 = agreementdata1 %>%
  group_by(participant_id) %>%
  arrange(p_agreement_sign_date, desc(p_termination_date), .by_group = TRUE) %>%
  subset(., p_agreement_type_id == 17)

dfFinal <- dfFinal %>%
  mutate(ageCat = cut(age, 
                      breaks = c(-Inf, 29, 36, 46, Inf),
                      labels = c("0 - 28 Years Old", "29 - 35 Years Old", "36 - 45 Years Old", "46 or older"),
                      right = T))


## Lag function - if you want to subtract values from previous row, within group:
dfFees <- dfFees%>%
  arrange(workerId, Month) %>%
  group_by(workerId) %>%
  mutate(Fees = Cumulative_Fees - lag(Cumulative_Fees),
         Fees = ifelse(Month == 1, Cumulative_Fees, Fees)) %>%
  ungroup()


### can also use base R to create variables in loops
### and to get rid of na's

for(k in 1:40){
  df1[, paste0('X', k, '.same')] <- ifelse(df1[, paste0('X', k, '.noStart')] == df1[, paste0('X', k, '.noBreaks')], 1, 0)
}

for(l in 1:40){
  df1[, paste0('X', l, '.noBreaks')] <- as.numeric(as.character(df1[,paste0('X', l, '.noBreaks')]))
  df1[, paste0('X', l, '.noBreaks')][is.na(df1[, paste0('X', l, '.noBreaks')])] <- 0
}


##########################################################################################
################################### Summary Stats ########################################
##########################################################################################

### Stargazer

stargazer(df[which(df$cond == 2), c("bonus", "sessions", "completeall", "complete1", "complete2", "complete3", "complete4", "complete5",
                                    "complete6", "complete7")], type = "latex", iqr=FALSE, mean.sd=F,  summary = T, header = F)


# alternatively using tidyverse
df1 %>% 
  filter(., cond == 1) %>%
  select(totalSame, notSame) %>%
  stargazer(., type = "text", header = F, iqr = F)


### Table structure

# Mode function

Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

data <- banking_manager %>%
  group_by(layer) %>%
  dplyr::select(direct_reports, ideal_span) %>%
  summarise_all(funs(N = n(),
                     Mean = mean, 
                     SD = sd,
                     Min = min,
                     q25 = quantile(., 0.25),
                     Median = median, 
                     q75 = quantile(., 075),
                     Max = max,
                     Mode = Mode,
                     SE = std.error
  )) %>%
  mutate(layer = as.factor(layer)) %>%
  pivot_longer(-layer,
               names_to = c('Variable','.value'),
               names_pattern = '(.+)_(.+)$') %>%
  mutate(
    ci_mult = qt(0.975, N - 1),
    ci = SE*ci_mult
  )



### Tabulations using CrossTable

df %>%
  with(., CrossTable(bonus, cond, chisq = T))


##########################################################################################
#################################### Regressions #########################################
##########################################################################################

### Running basic OLS regressions
lmses <- lm(sessions ~ cond, data = df)
lmall <- lm(completeall ~ cond, data = df) 

# displaying in stargazer
stargazer(lmses, lmall, type="latex", header = F)

### Running binary DV regressions.  Automatically assumes logistic

df %>%
  glm(bonus ~ cond, family = binomial, data = .) %>%
  summary(.)


### comparing beta's 

linearHypothesis(lmw5, "indirect = direct", test = "F")
linearHypothesis(lmw6, "indirect = direct", test = "F")
linearHypothesis(lmw7, "indirect = direct", test = "F")
linearHypothesis(lmw8, "indirect = direct", test = "F")

stargazer(lmw5, lmw6, lmw7, lmw8, header = F,
          title = "Main Effects by Week",
          column.labels = c("Week 5", "Week 6", "Week 7", "Week 8"),
          omit.stat = c("f", "ser"),
          add.lines = list("", "F-test p-value:", c("\\hspace{2em} indirect = direct", "0.091$^{*}$", "0.066$^{*}$",  "0.080$^{*}$", "0.194"), "", "\\hline"),
          digits = 3,
          table.placement = "H")

### Running fixed effects models (first slot, 0 here bc no FE) and clustering

lmb <- felm(complete ~ cond | 0 | 0 | mtwid, data = dfl)
lmbr <- felm(complete ~ cond + round | 0 | 0 | mtwid, data = dfl)
lmbint <- felm(complete ~ cond*round | 0 | 0 | mtwid, data = dfl)

stargazer(lmb, lmbr, lmbint, type = "latex", header = F)

stargazer(lmpost, lmposta, type = "latex", header = F, 
          title = "Looking at Week by Week (Weeks -9 to 8)",
          omit.stat = "ser",
          add.lines = list("", c("Participant FE:", "Y", "Y", "Y", "Y"), c("Week FE:", "Y", "Y", "Y", "Y"), "", "\\hline"),
          column.labels = c("\\shortstack{\\Summary Statistics \\ Condition Only}", "\\shortstack{\\Price Level Graph \\ Condition Only}", "Whole Sample"),
          digits = 3,
          column.separate = c(1, 1, 3))


##########################################################################################
###################################### ggplot2 ###########################################
##########################################################################################

# two conditions on the same graph
ggplot(df, aes(x=sessions, color = factor(cond))) + 
  geom_freqpoly() + 
  scale_color_discrete(name = "Condition", labels = c("No ER", "ER")) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2)) 
  

dfl %>% 
  aggregate(complete ~ round + cond, data = ., mean) %>%
  ggplot(., aes(x=as.numeric(round), y=as.numeric(complete), color = factor(cond))) + geom_point() +
  geom_smooth(method = lm, se = T)

# by condition on two different graphs:
df %>%
  subset(., cond == 2) %>% 
  ggplot(., aes(x=sessions)) + geom_freqpoly(color = "navy") + ggtitle("No ER Condition")

df %>%
  subset(., cond == 3) %>% 
  ggplot(., aes(x=sessions)) + geom_freqpoly( color = "navy") + ggtitle("ER Condition") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5))


dfbla %>%
  ggplot(., aes(x = `Problem Set 3 (152264)`)) +
  geom_histogram(color = "black", fill = "salmon", bins = 100) +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.25, 10.25), breaks=seq(0, 10, 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.25)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab("Problem Set 3 Grade") +
  scale_color_discrete(name = "Condition", labels = c("Direct", "Indirect", "Control")) 
  

### Creating ggplot economist theme + geom_text and errorbar
  
# dark blue = rgb(1, 77, 100, maxColorValue = 255)
# lighter =  rgb(1, 162, 217, maxColorValue = 255)
# dark green = rgb(0, 136, 125, maxColorValue = 255)
# gray = rgb(103, 148, 167, maxColorValue = 255)


data %>%
  ggplot(., aes(y = Mean, x = as.factor(layer), fill = as.factor(Variable))) + 
  geom_col(width = 0.6, position = position_dodge()) +
  theme_economist() + 
  scale_fill_economist() + 
  scale_color_economist() +
  # geom_errorbar(aes(ymin = Mean-ci, ymax = Mean+ci),
  #               width = 0.2,
  #               position = position_dodge(width = 0.9)) +
  # geom_text(aes(label = paste0(round(Mean, digits = 0), '\n(', Median, ')'), group = layer),
  #           position = position_dodge(width = 0.9)) +
  labs(y = "Average (\u00b1 95%CI) Span",
       x = "Layer") +
  theme(legend.position = "bottom",
        axis.text = element_text(colour="black"),
        axis.text.y = element_text(hjust = 0.75))


##########################################################################################
#################################### power tests #########################################
##########################################################################################

### Power tests of t-test and prop test
# Let's do some power analysis. Using the fact that effect sizes are B/SD, where SD = sqrt(N)*SE

d2 = 6.910/(sqrt(98)*4.610)
pwr.t.test(d = d2, power = 0.8, sig.level = 0.05)

# we can just use a function for proportions to calculate ES
pwr.2p.test(h = ES.h(p1 = 0.442, p2 = 0.250), power = 0.8, sig.level = 0.05)

prop.test(x = c(22, 31), n = c(73, 73))
prop.test(x = c(73/3, 31), n = c(73, 73))



### Pulling your data structure easily so you can post online or share

dput(droplevels(head(df1[,c("X1.startAll", "X2.startAll", "X3.startAll", "X4.startAll", "X5.startAll", "X6.startAll", "X7.startAll", "X8.startAll")])))


##########################################################################################
################################# transforming data ######################################
##########################################################################################

# new pivot functions are great
# wide to long

dfStats <- df %>%
  select(workerId, condition, caucasian, study, gender, age, education, M1A1_Shares:M12A6_Shares, M1A1_Min:M12A6_Avg)%>%
  pivot_longer(-c(workerId, condition, caucasian, study, gender, age, education),
               names_to = c("Month", "Asset", ".value"),
               names_pattern = "M(\\d+)A(\\d+)_(.*)"
               )



