# Loading Required Librairs 
library(patchwork)

# Examining Impact of Meff on Relationship Between Mobility and Transmission 
mobility <- seq(1, -1, -0.1)
meff_values <- c(-10, -8, -6, -5, -4, -3, -2, -1, 0)
R0 <- 3
for (i in 1:9) {
  if (i == 1) {
    x <- R0 * 2 * plogis(-mobility * meff_values[i])
    df <- data.frame(Meff = rep(meff_values[i], length(x)),
                     Rt = x, 
                     mobility = mobility)
  } else {
    x <- R0 * 2 * plogis(-mobility * meff_values[i])
    df <- rbind(df, 
                data.frame(Meff = rep(meff_values[i], length(x)),
                           Rt = x, 
                           mobility = mobility))
  }
}

df_2 <- df %>%
  mutate(Meff = factor(Meff))
a <- ggplot(df_2, aes(x = mobility, y = Rt, col = Meff)) +
  geom_path() +
  lims(y = c(0, 6), x = c(1, -1)) +
  scale_colour_manual(values = rev(blues9)) +
  theme(legend.position = "none")
  
# Examining the Impact of Meff_pl Given a Particule Value of Meff
down_indices <- 1:10
up_indices <- 11:19
overall_mobility <- c(seq(1, 0.1, -0.1), rev(seq(1, 0.1, -0.1))[-1])
upwards_mobility <- c(rep(0, length(down_indices)), rev(seq(1, 0.1, -0.1))[-1])

one_Meff <- 3
one_Meff_pl <- 0
one_Rt <- R0 * 2 * plogis((-one_Meff * (1 - overall_mobility))  -  (one_Meff_pl * (upwards_mobility)))

two_Meff <- 3
two_Meff_pl <- 1
two_Rt <- R0 * 2 * plogis((-two_Meff * (1 - overall_mobility))  -  (two_Meff_pl * (upwards_mobility)))

three_Meff <- 3
three_Meff_pl <- 3
three_Rt <- R0 * 2 * plogis((-three_Meff * (1 - overall_mobility))  -  (three_Meff_pl * (upwards_mobility)))

df <- data.frame(Rt = c(one_Rt, two_Rt, three_Rt),
                 time = rep(1:19, 3),
                 Meff = c(rep(one_Meff, length(one_Rt)), rep(two_Meff, length(two_Rt)), rep(three_Meff, length(three_Rt))),
                 Meff_pl = c(rep(one_Meff_pl, length(one_Rt)), rep(two_Meff_pl, length(two_Rt)), rep(three_Meff_pl, length(three_Rt))),
                 Mobility = rep(overall_mobility, 3))
df <- df %>%
  mutate(Meff_pl = factor(Meff_pl))

scaling_factor <- 3
scale <- 1/scaling_factor
b <- ggplot(df, aes(x = time, y = Rt)) +
  geom_path() +
  geom_line(aes(x = time, y = Mobility * scaling_factor), size = 1, linetype = 2) +
  scale_y_continuous(name = "Deaths", sec.axis = sec_axis(~.*scale, name = "Mobility Relative to Baseline")) +
  facet_grid(Meff_pl~.) +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

a + b
df_test <- data.frame(time = 1:22, mobility = overall_mobility)

ggplot(df_test, aes(x = time, y = mobility)) +
  geom_line()




# df <- data.frame(Rt = c(one_Rt, two_Rt, three_Rt, overall_mobility),
#                  time = rep(1:22, 4),
#                  Meff = c(rep(one_Meff, length(one_Rt)), rep(two_Meff, length(two_Rt)), rep(three_Meff, length(three_Rt)), rep (0, length(three_Rt))),
#                  Meff_pl = c(rep(one_Meff_pl, length(one_Rt)), rep(two_Meff_pl, length(two_Rt)), rep(three_Meff_pl, length(three_Rt)), rep(-1, length(three_Rt))))


