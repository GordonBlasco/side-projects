mutate_jitter_sp <- function(data, lat, long, decimal = 3){
  
  lat <- enquo(lat)
  long <- enquo(long)
  
  
  need_change <- count(data, !!lat, !!long) %>%
    filter(n > 1) %>%
    mutate(change = 10^(-decimal))
  
  i <- nrow(data)
  j <- nrow(need_change)           
  
  while (j >= 1) {
    
    data <- data %>%
      full_join(., need_change) %>%
      select(-n) %>%
      mutate(change = replace_na(change, 0)) %>%
      mutate(
        jitter_lat = !!lat + (change * runif(i, min = -10, max = 10)),
        jitter_long = !!long + (change * runif(i, min = -10, max = 10))
      ) %>% 
      select(-change)
    
    need_change_check <- count(data, jitter_lat, jitter_long) %>%
      filter(n > 1) 
    
    j <- nrow(need_change_check) 
    
  }
  return(data)  
}