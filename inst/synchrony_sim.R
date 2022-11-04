library(tidyverse)
library(synchrony)
library(compositions)
library(patchwork)
library(gganimate)
library(magick)
library(ggtext)

synchrony <- function(df){
  species.sd = apply(df, MARGIN = 2, FUN = sd)
  community.var = var(rowSums(df))
  return(
    tibble(denom = round((sum(species.sd)^2)),
           num = round(community.var),
           sync = community.var/(sum(species.sd, na.rm = TRUE)^2))
  )
}


sim_sync <- function(n = 15, cov = 0.8, return.df = F,
                     m1 = 30, m2 = 30, m3 = 30, m4 = 30){
  rho1 <- 0.1
  rho2 <- 0.1
  rho3 <- 0.1
  
  S <- matrix(c(1.0,  cov,  cov, cov,
                cov,  1.0,  cov, cov,
                cov,  cov,  1.0, cov,
                cov,  cov,  cov, 1.0)
              ,nrow=4, ncol=4)
  mu <- c(m1, m2, m3, m4)
  eps <- rlnorm.rplus(n,log(mu),S)
  
  
  df <- tibble(
    time = 1:n,
    w = as.vector(arima.sim(list(ar=rho1),
                            n,innov=eps[,1],
                            start.innov=eps[,1])),
    
    x = as.vector(arima.sim(list(ar=rho1),
                            n,innov=eps[,2],
                            start.innov=eps[,2])),
    
    y = as.vector(arima.sim(list(ar=rho2),
                            n,innov=eps[,3],
                            start.innov=eps[,3])),
    
    z = as.vector(arima.sim(list(ar=rho3),
                            n,innov=eps[,4],
                            start.innov=eps[,4]))
  ) %>%
    {. ->> df1.m} %>%
    tidyr::gather(var, value, -time)
  
  high_sync <- as.matrix(df1.m[,-1])
  
  # estimate portfolio effect
  sync1 <- synchrony(df = high_sync)
  if (!return.df){
    return(sync1)
  } else {
    return(list(sync1, df))
  }
  
}


n <- 2000
window_width = n/100  # How much of the whole data to show at once
frames = 100   # Increase to make smoother animation & bigger file
shift_per_frame = 1

# synchronous biomass----
df_sync <- sim_sync(cov = 0.5,n = n, return.df = T,
                    m1 = 250, m2 = 300, m3 = 300, m4 = 350)[[2]]
df_copied_sync <- map_df(seq_len(frames), ~df_sync, .id = "id") %>%
  mutate(id = as.integer(id)) %>%
  filter(time >= id * shift_per_frame,
         time <= id * shift_per_frame + window_width)

df_cs_sp <-  df_copied_sync %>% 

  mutate(s = spline(df_copied_sync$time, 
                    df_copied_sync$value))

sync_biomass <-
  ggplot(df_copied_sync) + 
  geom_line(aes(x = time, 
                y = value, 
                color = factor(var)),
            show.legend = F,
            alpha = 0.75) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  labs(y = "Biomass",
       x = "Time",
       color = "Species") +
  ggsci::scale_color_d3() +
  theme_void() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  transition_manual(id) +
  view_follow(fixed_y = TRUE)


anim_save(animation = sync_biomass, 
          height = 500, width =800,
          detail = 5,
          filename = here::here("inst/app/www/sync_biomass.gif"),
          fps = 5)
