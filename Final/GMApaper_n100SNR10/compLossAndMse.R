rm(list=ls())
library(ggplot2)

data <- read.csv("sls_results.csv")

ggplot(data, aes(y=SLSmc)) +
  geom_boxplot() +
  labs(title = "Integral matching error of SLS", subtitle = expression("Setup: 50 simulations GMA, n=100, SNR=10%")) +
  ylab("Integral matching loss function") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size=15, face="bold"),
    plot.subtitle = element_text(hjust = 0.5, size=12, face="bold"),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"))
ggsave(filename="GMApaper_SLS_loss.pdf", device="pdf")

library(tidyr)

data_wide <- gather(data[-c(1,2),], key="Parameter", value="Estimate")
data_wide$Parameter <- substring(data_wide$Parameter, 8)
data_wide <- data_wide[data_wide$Parameter > "",]

data_lin <- data_wide[data_wide$Parameter<"b",]
data_nlin <- data_wide[data_wide$Parameter>"b",]

ggplot(data_lin, aes(x=Estimate)) +
  geom_histogram(binwidth=0.05) +
  labs(title = "Linear Parameter estimate distributions", subtitle = expression("Setup: 50 simulations GMA, n=100, SNR=10%")) +
  facet_wrap(~Parameter) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size=15, face="bold"),
    plot.subtitle = element_text(hjust = 0.5, size=12, face="bold"),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"))
ggsave(filename="GMApaper_SLS_lin-params.pdf", device="pdf")

ggplot(data_nlin, aes(x=Estimate)) +
  geom_histogram(binwidth=0.1) +
  labs(title = "Nonlinear Parameter estimate distributions", subtitle = expression("Setup: 50 simulations GMA, n=100, SNR=10%")) +
  facet_wrap(~Parameter) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size=15, face="bold"),
    plot.subtitle = element_text(hjust = 0.5, size=12, face="bold"),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"))
ggsave(filename="GMApaper_SLS_nlin-params.pdf", device="pdf")

