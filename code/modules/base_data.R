base_data <- function(s, x_max){
  data.frame(s) %>%
    group_by(s) %>%
    expand(mile = 0:x_max) %>%
    mutate(prob_1 = 0.5, prob_0 = 1 - prob_1) %>%
    ungroup() %>%
    arrange(mile)
}