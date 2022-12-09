library(tidyverse)
library(maps)
# The functions might be useful for A4
source("../source/a4-helpers.R")


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#
highest_jail_pop_black <- incarceration_df %>% 
  group_by(state) %>% 
  summarize(m = sum(black_jail_pop, na.rm = T)) %>% 
  filter(m > 0) %>%
  select(state, m)

highest_jail_pop_white <- incarceration_df %>% 
  group_by(state) %>% 
  summarize(k = sum(white_jail_pop, na.rm = T)) %>% 
  filter(k > 0) %>% 
  select(state, k)

highest_jail_pop <- incarceration_df %>% 
  group_by(state) %>% 
  summarize(h = sum(total_jail_pop, na.rm = T)) %>% 
  filter(h > 0) %>% 
  select(state, h)
highest_jail_pop <- highest_jail_pop[-10, ]
highest_jail_pop <- highest_jail_pop[-43, ]
highest_pops <- cbind(highest_jail_pop, highest_jail_pop_black, highest_jail_pop_white)

pop_prop <- data.frame(proportion_black = highest_pops$m / highest_pops$h, proportion_white = highest_pops$k / highest_jail_pop$h)
each_state_proportion <- cbind(pop_prop, highest_jail_pop) 
mean(each_state_proportion$proportion_black)
mean(each_state_proportion$proportion_white)
#
top_5_jail_pop <- highest_jail_pop %>% arrange(-h) %>% head()
bottom_5_jail_pop <- highest_jail_pop %>% arrange(h) %>% head()

#
past_dates <- incarceration_df %>% 
  group_by(county_name) %>% 
  filter(year == min(year)) %>% 
  select(total_jail_pop)

current_dates <- incarceration_df %>% 
  group_by(county_name) %>% 
  filter(year == max(year)) %>% 
  select(total_jail_pop)

pop_diff <- current_dates$total_jail_pop - past_dates$total_jail_pop
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  incarceration_df_new <- incarceration_df
  jail_pop_by_year <- aggregate(
    x = incarceration_df_new$total_jail_pop,
    by = list(incarceration_df_new$year),
    FUN = sum,
    na.rm = T
  )
  colnames(jail_pop_by_year)[1] = "year"
  colnames(jail_pop_by_year)[2] = "total_jail_pop"
return(jail_pop_by_year)   
}
get_year_jail_pop()
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  bar_graph <- ggplot(data = get_year_jail_pop(),
    aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat = "identity")    
  return(bar_graph)   
} 
plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_jail_pop_by_states <- function(states) {
  states_vector <- incarceration_df[incarceration_df$state %in% states,]
  filtered_state <- states_vector %>% 
    group_by(state, year) %>% 
    summarize(p = sum(total_jail_pop, na.rm = T)) %>% 
    select(state, year, p)
  return(filtered_state)
}
get_jail_pop_by_states("WA")

plot_jail_pop_by_states <- function(states) {
  bargraph_2 <- ggplot(data = get_jail_pop_by_states(states),
    aes(x = year, y = p)) + 
    geom_bar(stat = "identity")
  return(bargraph_2)
}
plot_jail_pop_by_states("WA")
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_black_jail_pop <- function() {
  incarceration_df_new <- incarceration_df
  black_jail_pop <- aggregate(
    x = incarceration_df_new$black_jail_pop,
    by = list(incarceration_df_new$year),
    FUN = sum,
    na.rm = T
  )
  colnames(black_jail_pop)[1] = "year"
  colnames(black_jail_pop)[2] = "total_black_jail_pop"
  return(black_jail_pop)
}
get_black_jail_pop()

plot_black_jail_pop <- function() {
  bar_graph <- ggplot(data = get_black_jail_pop(),
                      aes(x = year, y = total_black_jail_pop)) + 
    geom_bar(stat = "identity")    
  return(bar_graph)
}
plot_black_jail_pop()
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
library(maps)
states <- data.frame(
  states = c("CA", "TX"), 
  lat = c(34.0522, 32.5539),
  long = c(-118.2437, -94.3154)
  )
state_shape <- map_data("state")

plotting_cities <- function(){
  ggplot(state_shape) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) + 
  geom_point(
    data = states,
    mapping = aes(x = long, y = lat), 
    color = "red"
  ) + 
  coord_map()
}
plotting_cities()
