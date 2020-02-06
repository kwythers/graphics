library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


by_cyl <- mtcars %>%  
  group_by(cyl, gear) %>%  
  nest() %>%  
  rename(cyl_data = data)

by_gear <- mtcars %>% 
  group_by(gear) %>%  
  nest() %>%  
  rename(gear_data = data)

mtcars_nest <- left_join(by_cyl, by_gear, by = 'gear')

mtcars_nest <- mtcars_nest %>% 
  mutate(
    map(cyl_data, ~ ggplot(., aes(x = wt, y = mpg)) + 
                             geom_point() +
                             geom_smooth(se = TRUE, color = 'blue')
    )
  ) %>% 
  rename(plot_cyl = `map(...)`)

mtcars_nest <- mtcars_nest %>% 
  mutate(
    map(gear_data, ~ ggplot(., aes(x = wt, y = mpg)) + 
          geom_point() +
          geom_smooth(se = TRUE, color = 'red')
    )
  ) %>% 
  rename(plot_gear = `map(...)`)

mtcars_nest$plot_cyl[1]
mtcars_nest$plot_gear[1]

#linear_model <- lm(mpg ~ hp + wt, data = mtcars)

#How to get these two plots on one figure?
mtcars_nest <- mtcars_nest %>% 
  mutate( 
    plot_cyl_gear = map2(cyl_data, gear_data,
                         ~ ggplot(.x, aes(x = wt, y = mpg)) +
                           geom_smooth(se = TRUE, color = 'blue') +
                           geom_smooth(data = .y, se = TRUE, color = 'red')
    ) )

mtcars_nest$plot_cyl_gear[1]
  
  
