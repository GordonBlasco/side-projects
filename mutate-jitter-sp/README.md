# mutate_jitter_sp()
This function adds a custom amount of jitter to spatial points data. This can be used to anonymize sensetive information, or spread overlapping points for better plotting. 

```R
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
```

### Example: 
```R
sp_dataframe %>%
  mutate_jitter_sp(data = ., lat = lat_column, long = long_column)
```

https://gordonblasco.github.io/mutate-jitter-sp/example_markdown

