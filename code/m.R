source('header.R')

# parameters
theta <- data.frame(theta_1 = 10, theta_2 = 1)
s <- c(1, 4)
pi_s <- 0.25
beta <- 0.99
eps <- 10^(-3)
val_tol <- 0.01

num_rep <- 10^6

# from the last homework assignment
x_max <- ceiling(1/theta$theta_2*(theta$theta_1*max(s) - log(1/(1-eps)-1)))

list(s= s, x_max= x_max) %>% 
  base_data() %T>% {
	  . ->> ccp #save ccp
  } %>% 
  val_iter(., val_tol= 10^-6, theta, beta) %>% 
  group_by(s)  %T>% {
    group_split(.) ->> output
  } %>% 
    group_walk(~ saveRDS(.x, paste0(var_save, 'output', .y$s, '.rds')))

png("histograms_parallel.png", width=4, height=8, units="in", res=300)
par(mfrow=c(length(s),1))

seq(num_rep) %>% 
  plyr::llply(
    function(l)
      simulate(ccp),
    .parallel=TRUE
  ) %>%
  melt() %>% 
  dcast(., L1~variable) %>% 
  group_by(type) %T>% {
    group_split(.) ->> sim_output
  } %>% 
  group_walk(~ hist(.$mile))

dev.off()

png("histograms_wo_parallel.png", width=4, height=8, units="in", res=300)
par(mfrow=c(length(s),1))

simulate_wo_par(num_rep) %>% 
  group_by(type) %T>% 
    group_walk(~ saveRDS(.x, paste0(var_save, 'sim_type', .y$type, '.rds'))) %>% 
    group_walk(~ hist(.$mile))

dev.off()
