library(ggplot2)
library(ggthemes)
library(tidyverse) # data manipulation
library(readr)

theme_set(theme_tufte(ticks = FALSE, base_family = "serif", base_size = 17)) # base_size affects font sizes of axis labels etc
theme_update(axis.text = element_text(size = 15),
             axis.title = element_text(size = 16),
             legend.text = element_text(size = 15),
             legend.title = element_text(size = 16),
             plot.caption = element_text(size = 15),
             legend.key.size = unit(1.5, "line")) # set size for axis marks and breaks

rm(list=ls())

cbbPalette <- c("#000000", "#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#CC79A7")

# Line plots

# data <- read_csv("boxplots_loss.csv") %>%
#  group_by(dataset, Method) %>%
#  summarise("MSE" = mean(Loss)) %>%
#  ungroup() %>%
#  mutate_at(vars(dataset, Method), factor)

NLS_Lin_mseGMA_n200SNR5 <- c(0.6966493, 2.0684950, 2.8060566)
SLS_Lin_mseGMA_n200SNR5 <- c(1.288322, 1.288322, 1.288322)
NLS_Lin_mseGMA_n100SNR5 <- c(1.019141, 2.925760, 3.720261)
SLS_Lin_mseGMA_n100SNR5 <- c(1.671625, 1.671625, 1.671625)

data <- data.frame(dataset=factor(c(1,1,2,2,3,3,1,1,2,2,3,3)),
                   Method = c('NLS','SLS','NLS','SLS','NLS','SLS',
                            'NLS','SLS','NLS','SLS','NLS','SLS'),
                   N = c(rep("100",6), rep("200",6)),
                   MSE = log(c(NLS_Lin_mseGMA_n100SNR5[1],SLS_Lin_mseGMA_n100SNR5[1],
                          NLS_Lin_mseGMA_n100SNR5[2],SLS_Lin_mseGMA_n100SNR5[2], 
                          NLS_Lin_mseGMA_n100SNR5[3],SLS_Lin_mseGMA_n100SNR5[3],
                          NLS_Lin_mseGMA_n200SNR5[1],SLS_Lin_mseGMA_n200SNR5[1],
                          NLS_Lin_mseGMA_n200SNR5[2],SLS_Lin_mseGMA_n200SNR5[2], 
                          NLS_Lin_mseGMA_n200SNR5[3],SLS_Lin_mseGMA_n200SNR5[3]))
                   )

ggplot(data = data, aes(colour = N)) +
  scale_x_discrete(limits = c(3,2,1), labels = c("high", "medium", "low"), expand = expand_scale(mult = 0, add = 0)) +
  scale_colour_manual(values = cbbPalette) +
  geom_line(aes(x = dataset, y = MSE, group = interaction(Method, N), linetype = Method), size = 0.71) +
  geom_point(show_guide = FALSE, aes(x = dataset, y = MSE, group = interaction(Method, N)), size = 2) +
  xlab("Prior information") +
  ylab("MSE") +
  labs(title = "MSEs for linear parameters", caption = expression("Setup: GMA, SNR=20%")) +
  theme(legend.position="top", legend.direction="horizontal")
ggsave("figure_1b.pdf")

