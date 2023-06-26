library(tidyverse)
library(lubridate)
library(lmtest)
library(sandwich)
library(clusterSEs)
library(npregfast)
library(extrafont)
library(readxl)
library(gridExtra)
library(stargazer)

"%!in%" <- Negate("%in%")

Election_State <- c("New Jersey", "Virginia")

df <- read_excel("/Users/ziyuhe/Documents/GitHub/gubernatorial_election_option_pricing/dataset/Utility Companies Elections.xlsx", 
                 sheet = "Option Prices")

df_wide <- df
df_wide <- df_wide[, 1:3]
df_wide$before <- df_wide$after1 <- df_wide$after2 <- NA

df <- df |> 
  pivot_longer(c(4:ncol(df)), names_to = "Day")
df$Day <- as.Date(as.numeric(df$Day), origin = "1899-12-30")

names(df) <- c("Ticker", "State", "Sector", "Day", "IV")

df$Dayaway <- as.numeric(difftime(df$Day, as.Date("2021-11-02"), units = "days"))

df <- df |> 
  filter(Dayaway < 10)

virginia <- df |> 
  filter(State == "Virginia")

rest <- df |> 
  filter(State != "Virginia" & State != "California" & State != "New Jersey")

california <- df |> 
  filter(State == "California")

ggplot(df |> filter(Ticker == "AES")) +
  geom_line(aes(x = Day, y = IV)) +
  geom_vline(xintercept = as.Date("2021-11-02"), lty = 2, col = "red") +
  geom_smooth(aes(x = Day, y = IV), data = df |> filter(Ticker == "AES") |> filter(Day <= as.Date("2021-11-02")), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = df |> filter(Ticker == "AES") |> filter(Day >= as.Date("2021-11-02")), method = "lm", se = F) +
  labs(y = "Implied Volatility") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        title = element_text(size = 16),
        axis.text = element_text(size = 14))

ggsave("../figures/AES.png")

ggplot(df |> filter(Ticker == "XEL")) +
  geom_line(aes(x = Day, y = IV)) +
  geom_vline(xintercept = as.Date("2021-11-02"), lty = 2, col = "red") +
  geom_smooth(aes(x = Day, y = IV), data = df |> filter(Ticker == "XEL") |> filter(Day <= as.Date("2021-11-02")), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = df |> filter(Ticker == "XEL") |> filter(Day >= as.Date("2021-11-02")), method = "lm", se = F) +
  labs(y = "Implied Volatility") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        title = element_text(size = 16),
        axis.text = element_text(size = 14))
ggsave("../figures/XEL.png")

company_lst <- unique(df$Ticker)
for (i in 1:length(company_lst)) {
  company <- df |> 
    filter(Ticker == company_lst[i])
  lm_before <- lm(IV ~ Dayaway, data = company |> filter(Dayaway <= 0))
  lm_after1 <- lm(IV ~ Dayaway, data = company |> filter(Dayaway > 0))
  lm_after2 <- lm(IV ~ Dayaway, data = company |> filter(Dayaway > 1))
  df_wide[i, "before"] <- lm_before$fitted.values[length(lm_before$fitted.values)] * 100
  df_wide[i, "after1"] <- lm_after1$fitted.values[1] * 100
  df_wide[i, "after2"] <- lm_after2$fitted.values[1] * 100
}

df_wide$onedaydiff <- df_wide$after1 - df_wide$before
df_wide$twodaydiff <- df_wide$after2 - df_wide$before
df_wide$onedayratio <- df_wide$onedaydiff / df_wide$before * 100
df_wide$twodayratio <- df_wide$twodaydiff / df_wide$before * 100

names(df_wide) <- c("Ticker", "State", "Sector", "After2", "After1", 
                    "Before", "OnedayDiff", "TwodayDiff", "OnedayRatio", "TwodayRatio")

df_wide$Election <- as.numeric(df_wide$State == "Virginia" | df_wide$State == "New Jersey")



summary(lm(OnedayDiff ~ Election + Sector + State, data = df_wide))
summary(lm(TwodayDiff ~ Election + Sector + State, data = df_wide))

summary(lm(OnedayRatio ~ Election + Sector + State, data = df_wide))
summary(lm(TwodayRatio ~ Election + Sector + State, data = df_wide))

utility <- df_wide |> 
  filter(Sector == "Utilities")

lm_util_onedaydiff <- lm(OnedayDiff/100 ~ Election + State, data = utility)
lm_util_twodaydiff <- lm(TwodayDiff ~ Election + State, data = utility)

lm_util_onedayratio <- lm(OnedayRatio ~ Election + State, data = utility)
lm_util_twodayratio <- lm(TwodayRatio ~ Election + State, data = utility)

coeftest(lm_util_onedaydiff, vcovHC(lm_util_onedaydiff, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_twodaydiff, vcovHC(lm_util_twodaydiff, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_onedayratio, vcovHC(lm_util_onedayratio, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_twodayratio, vcovHC(lm_util_twodayratio, type = 'HC0', cluster = 'Ticker'))

utility_large <- utility[sample(nrow(utility), 200, replace = T), ]

lm_util_onedaydiff <- lm(OnedayDiff ~ Election + State, data = utility)
lm_util_twodaydiff <- lm(TwodayDiff ~ Election + State, data = utility)
coeftest(lm_util_onedaydiff, vcovHC(lm_util_onedaydiff, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_twodaydiff, vcovHC(lm_util_twodaydiff, type = 'HC0', cluster = 'Ticker'))

utility_long <- df |> 
  filter(Sector == "Utilities")
utility_long$Treat <- as.numeric(utility_long$State %in% Election_State & utility_long$Dayaway <= 0)
#utility_long$Treat[utility_long$Dayaway == 1] = NA
#utility_long <- utility_long |> 
#  drop_na()
utility_long$Treat_State <- as.numeric(utility_long$State %in% Election_State)

utility_did <- lm(IV ~ Treat + factor(Dayaway) + Ticker, data = utility_long)
coeftest(utility_did, vcovHC(utility_did, type = 'HC0', cluster = 'Ticker'))

fit0 <- frfast(IV ~ Dayaway, data = utility_long |> filter(State %!in% Election_State), 
               nboot = 500, kbin = 44, na.action = "na.omit", 
               model = "np", smooth = "kernel", kernel = "triang",)
dataplot0 <- as.data.frame(fit0$pl)[1]
names(dataplot0)[1] <- "pl"
pu <- as.data.frame(fit0$pu)[1]
names(pu) <- "pu"
p <-  as.data.frame(fit0$p)[1]
names(p) <- "p"
x <- as.data.frame(fit0$x)
names(x) <- "x"
dataplot0 <- cbind(dataplot0, pu, p, x)
dataplot0$treatment = 0

fit1 <- frfast(IV ~ Dayaway, data = utility_long |> filter(State %in% Election_State), 
               nboot = 500, kbin = 44, na.action = "na.omit", 
               model = "np", smooth = "kernel", kernel = "triang",)
dataplot1 <- as.data.frame(fit1$pl)[1]
names(dataplot1)[1] <- "pl"
pu <- as.data.frame(fit1$pu)[1]
names(pu) <- "pu"
p <-  as.data.frame(fit1$p)[1]
names(p) <- "p"
x <- as.data.frame(fit1$x)
names(x) <- "x"
dataplot1 <- cbind(dataplot1, pu, p, x)
dataplot1$treatment = 1
dataplot <- rbind(dataplot0, dataplot1)

RDD1 <- ggplot() + 
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %in% Election_State & Dayaway <= 0), method = "loess", se = F) +
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %in% Election_State & Dayaway >= 1), method = "loess", se = F) +
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %!in% Election_State & Dayaway <= 0), method = "loess", se = F) +
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %!in% Election_State & Dayaway >= 1), method = "loess", se = F) +
  labs(y = "Implied Volatility") +
  scale_color_discrete(name  = "Election", breaks=c("0", "1"), labels=c("No", "Yes")) +
  theme_minimal()+
  theme(text = element_text(family = "serif", size = 14),
        title = element_text(size = 16),
        legend.position = "bottom")

RDD2 <- ggplot() + 
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %in% Election_State & Dayaway <= 0), method = "loess", se = F) +
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %in% Election_State & Dayaway > 1), method = "loess", se = F) +
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %!in% Election_State & Dayaway <= 0), method = "loess", se = F) +
  geom_smooth(aes(x = Day, y = IV, color = as.factor(Treat_State)), 
              data = utility_long |> filter(State %!in% Election_State & Dayaway > 1), method = "loess", se = F) +
  labs(y = "Implied Volatility") +
  scale_color_discrete(name  = "Election", breaks=c("0", "1"), labels=c("No", "Yes")) +
  theme_minimal()+
  theme(text = element_text(family = "serif", size = 14),
        title = element_text(size = 16),
        legend.position = "bottom")
  
g <- arrangeGrob(RDD1, RDD2, ncol = 2)
ggsave("../figures/RDD.png", g)

ggplot(dataplot, aes(x = x, y = p)) +
  geom_line(aes(y = p, color = as.factor(treatment))) +
  geom_vline(aes(xintercept = 0), lty = 2) + 
  scale_color_discrete(name  = "Gubernatorial Election", breaks=c("0", "1"), labels=c("No Election", "Election")) +
  theme_classic() + 
  theme(legend.position = "bottom",
        text = element_text(family = "serif")) +
  labs(x = "Days from Election", y = "Implied Volatility")

ggsave("../figures/parallel.png")

utility_long_treated <- utility_long |> 
  filter(State %in% Election_State)
utility_long_treated$predicted <- predict(utility_did, newdata = utility_long_treated)
utility_treat_mean <- utility_long_treated |> 
  group_by(Dayaway) |> 
  summarise(avgiv = mean(predicted))

utility_long_control <- utility_long |> 
  filter(State %!in% Election_State)
utility_long_control$predicted <- predict(utility_did, newdata = utility_long_control)
utility_control_mean <- utility_long_control |> 
  group_by(Dayaway) |> 
  summarise(avgiv = mean(predicted))

virginia <- read_excel("/Users/ziyuhe/Documents/GitHub/gubernatorial_election_option_pricing/dataset/Gubernatorial2021.xlsx", 
                       sheet = "Virginia")
newjersey <- read_excel("/Users/ziyuhe/Documents/GitHub/gubernatorial_election_option_pricing/dataset/Gubernatorial2021.xlsx", 
                       sheet = "New Jersey")

VA_poll <- NJ_poll <- data.frame(matrix(ncol = 2, nrow = 16))
names(VA_poll) <- names(NJ_poll) <- c("Day", "Uncertainty")
VA_poll$Day <- NJ_poll$Day <- as.Date(unique(df$Day))
VA_poll <- VA_poll[1:9, ]
NJ_poll <- NJ_poll[1:9, ]

virginia$Date <- as.Date(virginia$Date)
newjersey$Date <- as.Date(newjersey$Date)
virginia$McAuliffe <- virginia$McAuliffe / virginia$Total * 100
virginia$Youngkin <- virginia$Youngkin / virginia$Total * 100
newjersey$Murphy <- newjersey$Murphy / newjersey$Total * 100
newjersey$Ciattarelli <- newjersey$Ciattarelli / newjersey$Total * 100
for (i in 1:9) {
  df_VA <- virginia |> 
    filter(Date <= VA_poll$Day[i])
  VA_mean <- mean(df_VA$McAuliffe)
  VA_sd <- sd(df_VA$McAuliffe)
  VA_poll$Uncertainty[i] <- min(dnorm(50, VA_mean, VA_sd), 1 - dnorm(50, VA_mean, VA_sd))
  
  df_NJ <- newjersey |> 
    filter(Date <= NJ_poll$Day[i])
  NJ_mean <- mean(df_NJ$Murphy)
  NJ_sd <- sd(df_NJ$Murphy)
  NJ_poll$Uncertainty[i] <- min(dnorm(50, NJ_mean, NJ_sd), 1 - dnorm(50, NJ_mean, NJ_sd))
}

utility_long$Treat_cont <- 0
utility_long$Treat_cont[utility_long$State == "Virginia" & utility_long$Treat == 1] = VA_poll$Uncertainty
utility_long$Treat_cont[utility_long$State == "New Jersey" & utility_long$Treat == 1] = NJ_poll$Uncertainty

utility_did_cont <- lm(IV ~ Treat_cont + factor(Dayaway) + Ticker, data = utility_long)
coeftest(utility_did_cont, vcovHC(utility_did_cont, type = 'HC0', cluster = 'Ticker'))

utility$Treat_cont <- 0
utility$Treat_cont[utility$State == "Virginia" & utility$Election == 1] = VA_poll$Uncertainty[nrow(VA_poll)]
utility$Treat_cont[utility$State == "New Jersey" & utility$Election == 1] = NJ_poll$Uncertainty[nrow(NJ_poll)]

lm_util_onedaydiff_cont <- lm(OnedayDiff ~ Treat_cont + State, data = utility)
lm_util_twodaydiff_cont <- lm(TwodayDiff ~ Treat_cont + State, data = utility)
coeftest(lm_util_onedaydiff_cont, vcovHC(lm_util_onedaydiff_cont, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_twodaydiff_cont, vcovHC(lm_util_twodaydiff_cont, type = 'HC0', cluster = 'Ticker'))

utility_pred <- cbind(utility_treat_mean, utility_control_mean)[, c(1, 2, 4)]
names(utility_pred) <- c("Dayaway", "Treated", "Control")
ggplot(utility_pred) +
  geom_line(aes(x = Dayaway, y = Treated, col = "Yes")) +
  geom_line(aes(x = Dayaway, y = Control, col = "No")) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  scale_color_manual(values = c("Yes" = "red", "No" = "blue")) +
  labs(x = "Days from Election", y = "Predicted Implied Volatility", colour = "Election") +
  theme_minimal()+
  theme(text = element_text(family = "serif", size = 14),
        title = element_text(size = 16),
        legend.position = "bottom")
ggsave("../figures/predicted_IV.png")


stargazer(lm_util_onedaydiff, lm_util_twodaydiff, lm_util_onedaydiff_cont, lm_util_twodaydiff_cont,
          type = "latex", header = F,
          keep = c("Election","Treat"),
          title = "RDD Model",
          column.separate = c(2, 2),
          column.labels = c("Binary", "Continuous"),
          omit.stat = c("f", "ser", "adj.rsq"), omit = c("Constant"), 
          add.lines = list(c("State FE", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Number of Firms","29", "29", "29", "29", "29")),
          table.placement = "H")

stargazer(utility_did, utility_did_cont,
          type = "latex", header = F,
          keep = "Treat",
          title = "TWFE",
          column.separate = c(1, 1),
          column.labels = c("Binary", "Continuous"),
          omit.stat = c("f", "ser", "adj.rsq"), omit = c("Constant"), 
          add.lines = list(c("Time FE", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("State FE", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Number of Firms","29", "29", "29", "29", "29")),
          table.placement = "H")
