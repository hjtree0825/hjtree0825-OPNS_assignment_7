simulate_wo_par <- function(num_rep){
  data.frame(index = 1:num_rep) %>%
    mutate(U = runif(n(),0,1),
           type = ifelse(U > pi_s, s[2], s[1]) #pi_s= 0.25
    ) %>%
    select(index, type) %>%
    group_by(index, type) %>%
    expand(mile = 1:x_max) %>%
    ungroup() %>%
    
    left_join(x = ., y = ccp, by = c("type" = "s", "mile" = "mile")) %>%
    select(index, type, mile, prob_1) %>%
    mutate(U = runif(n(),0,1),
           replaced = ifelse(U < prob_1, 1, 0)) %>%
    filter(replaced == 1) %>%
    select(index, type, mile) %>%
    group_by(index, type) %>%
    arrange(mile) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(index)
}