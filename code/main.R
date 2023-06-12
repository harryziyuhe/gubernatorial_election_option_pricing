library(tidyverse)
library(lubridate)
library(lmtest)
library(sandwich)
library(clusterSEs)
library(npregfast)
library(extrafont)

"%!in%" <- Negate("%in%")

Election_State <- c("California", "New Jersey", "Virginia")

df <- read_excel("Downloads/Utility Companies Elections.xlsx", 
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

ggplot(california) +
  geom_point(aes(x = Day, y = IV)) +
  geom_vline(xintercept = as.Date("2021-11-02")) +
  geom_smooth(aes(x = Day, y = IV), data = california |> filter(Day <= as.Date("2021-11-02")), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = california |> filter(Day > as.Date("2021-11-02")), method = "lm", se = F)

ggplot(virginia) +
  geom_point(aes(x = Day, y = IV)) +
  geom_vline(xintercept = as.Date("2021-11-02")) +
  geom_smooth(aes(x = Day, y = IV), data = virginia |> filter(Day <= as.Date("2021-11-02")), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = virginia |> filter(Day > as.Date("2021-11-03")), method = "lm", se = F)

ggplot(rest) +
  geom_point(aes(x = Day, y = IV)) +
  geom_vline(xintercept = as.Date("2021-11-02")) +
  geom_smooth(aes(x = Day, y = IV), data = rest |> filter(Day <= as.Date("2021-11-02")), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = rest |> filter(Day > as.Date("2021-11-02")), method = "lm", se = F)

ggplot(df |> filter(Ticker == "CNP")) +
  geom_point(aes(x = Day, y = IV)) +
  geom_vline(xintercept = as.Date("2021-11-02")) +
  geom_smooth(aes(x = Day, y = IV), data = df |> filter(Ticker == "CNP") |> filter(Day <= as.Date("2021-11-02")), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = df |> filter(Ticker == "CNP") |> filter(Day > as.Date("2021-11-02")), method = "lm", se = F)

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

df_wide$Election <- as.numeric(df_wide$State == "Virginia" | df_wide$State == "California" | df_wide$State == "New Jersey")
df_wide$CloseElection <- as.numeric(df_wide$State == "Virginia")



summary(lm(OnedayDiff ~ Election + Sector + State, data = df_wide))
summary(lm(TwodayDiff ~ Election + Sector + State, data = df_wide))

summary(lm(OnedayRatio ~ Election + Sector + State, data = df_wide))
summary(lm(TwodayRatio ~ Election + Sector + State, data = df_wide))

utility <- df_wide |> 
  filter(Sector == "Utilities")

lm_util_onedaydiff <- lm(OnedayDiff ~ Election + CloseElection + State, data = utility)
lm_util_twodaydiff <- lm(TwodayDiff ~ Election + CloseElection + State, data = utility)

lm_util_onedayratio <- lm(OnedayRatio ~ Election + CloseElection + State, data = utility)
lm_util_twodayratio <- lm(TwodayRatio ~ Election + CloseElection + State, data = utility)

coeftest(lm_util_onedaydiff, vcovHC(lm_util_onedaydiff, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_twodaydiff, vcovHC(lm_util_twodaydiff, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_onedayratio, vcovHC(lm_util_onedayratio, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_twodayratio, vcovHC(lm_util_twodayratio, type = 'HC0', cluster = 'Ticker'))

utility_large <- utility[sample(nrow(utility), 200, replace = T), ]

lm_util_onedaydiff <- lm(OnedayDiff ~ Election + CloseElection + State, data = utility)
lm_util_twodaydiff <- lm(TwodayDiff ~ Election + CloseElection + State, data = utility)
coeftest(lm_util_onedaydiff, vcovHC(lm_util_onedaydiff, type = 'HC0', cluster = 'Ticker'))
coeftest(lm_util_twodaydiff, vcovHC(lm_util_twodaydiff, type = 'HC0', cluster = 'Ticker'))

utility_long <- df |> 
  filter(Sector == "Utilities")
utility_long$Treat <- as.numeric(utility_long$State %in% Election_State & utility_long$Dayaway <= 0)

utility_did <- lm(IV ~ Treat + Ticker + factor(Dayaway), data = utility_long)
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

ggplot(utility_long) + 
  geom_point(aes(x = Day, y = IV), data = utility_long |> filter(State %in% Election_State), color = "black", alpha = 0.3) +
  geom_smooth(aes(x = Day, y = IV), data = utility_long |> filter(State %in% Election_State & Dayaway <= 0), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = utility_long |> filter(State %in% Election_State & Dayaway >= 0), method = "lm", se = F) +
  geom_smooth(aes(x = Day, y = IV), data = utility_long |> filter(State %!in% Election_State & Dayaway <= 0), method = "lm", se = F, color = "red") +
  geom_smooth(aes(x = Day, y = IV), data = utility_long |> filter(State %!in% Election_State & Dayaway >= 0), method = "lm", se = F, color = "red")
  
ggplot(dataplot, aes(x = x, y = p)) +
  geom_line(aes(y = p, color = as.factor(treatment))) +
  geom_vline(aes(xintercept = 2)) + 
  scale_color_discrete(name  ="Gubernatorial Election", breaks=c("0", "1"), labels=c("No Election", "Election")) +
  theme_classic() + 
  theme(legend.position="bottom",
        text = element_text(family = "serif")) +
  labs(x = "Year", y = "State Taxes (% Income)", title = "(A)")

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

