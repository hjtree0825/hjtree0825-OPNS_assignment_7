simulate <- function(ccp){
  runif(1,0,1) %>% {
    type <- ifelse(. > pi_s, s[2], s[1])
  } %>% 
    expand.grid(type=., mile = 1:x_max) %>%
    
    left_join(x = ., y = ccp, by = c("type" = "s", "mile" = "mile")) %>%
    select(type, mile, prob_1) %>%
    mutate(U = runif(n(),0,1),
           replaced = ifelse(U < prob_1, 1, 0)) %>%
    filter(replaced == 1) %>%
    select(type, mile) %>%
    group_by(type) %>%
    arrange(mile) %>%
    filter(row_number() == 1) %>%
    ungroup()
}