
gw_rate <- function(site) {
  
  if(!site %in% c("mountain","prairie", "desert", "beach")) {
    warning("site not included")
  }
  
gw_depth <- data.frame(sitename = c("mountain",
                                    "prairie",
                                    "desert",
                                    "beach"),
                       depth = c(32, 41, 63, 2),
                       slope = c(11.2, 0.4, 0.8, 2.6))

site_select <- filter(gw_depth, sitename == site)

transport_rate <- 1.4 * site_select$slope + 3.6 * site_select$depth  

return(transport_rate)
}

gw_rate(site = "beach")


# calculate logistic growth

logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K - N0)/N0) * exp(-r * time))
  return(Nt)
}

logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

time_vec <- seq(from = 0, to = 50, by = 0.1)

pop_1 <- logistic_growth(100, 6000, 0.27, time = time_vec)

# can do with for loop
pop_1_vec <- vector("numeric", length = length(time_vec))

for (i in seq_along(time_vec)) {
  population <- logistic_growth(100, 6000, 0.27, time = time_vec[i])
  pop_1_vec[i] <- population
}

pop_time_1 <- data.frame(time_vec, pop_1)

ggplot(data = pop_time_1, aes(x = time_vec, y = pop_1)) +
  geom_line()

r_sequence <- seq(0.2, 0.4, 0.01)

out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_sequence))

for(i in seq_along(r_sequence)) { #outer loop of growth rates
  for (j in seq_along(time_vec)) { #inner loop of time steps
    population <- logistic_growth(100, 6000, r = r_sequence[i], time = time_vec[j])
    out_matrix[j, i] <- population
  }
  

out_df <- data.frame(out_matrix, time = time_vec)

colnames(out_df) <- c(paste0("growth_rate_", r_sequence), "time")

out_df_long<- out_df |>
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "population size")

ggplot(data = out_df_long, aes(x = time, y = `population size`)) +
  geom_line(aes(colour = growth_rate), show.legend = FALSE) +
  theme_minimal()


                