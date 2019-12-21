#Precious Stowers
#analysis.R

#------Summary Information-------

df <- read.csv("shootings-2018.csv",
               header = TRUE,
               stringsAsFactors = FALSE
)
View(df)

library(dplyr)
library(ggplot2)

#How many shootings events occured?
num_of_shootings <- nrow(df) - 1
  View(num_of_shootings)
  
#How many lives were lost?
lives_lost <- df %>%
  select(num_killed) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE)
View(lives_lost)

#which city was the most impacted by shootings?
most_impact <- df %>%
  filter(num_killed == max(num_killed)) %>%
  select(city)
View(most_impact)

#Which city had the most number of injuries?
max_injured <- df %>%
  filter(num_injured == max(num_injured)) %>%
  select(city)
View(max_injured)

#What was the average number of injuries?
average_num_injured <- df %>%
  summarise(mean = mean(num_injured)) %>%
  round()
View(average_num_injured)

#What was the average number of deaths?
average_num_killed <- df %>%
  summarise(mean = mean(num_killed)) %>%
  round()
View(average_num_killed)

#-------Summary Table----------------

# Putting the summary table together based in Washington State
summary_table <- df %>%
  filter(state == "Washington") %>%
  group_by(date) %>%
  summarise(state = "Washington",
            num_injured = sum(num_injured),
            num_killed = sum(num_killed))

#--------Description of a particular incident-------

# Views whole row of particular incident
particular_incident <- df %>%
  filter(city == "Trenton",
         num_killed) %>%
  group_by(city)

View(particular_incident)

# Views date of particular incident
particular_date <- particular_incident %>%
  pull(date)
View(particular_date)

# Views kills of particular incident
particular_kills <- particular_incident %>%
  pull(num_killed)
View(particular_kills)

# Views injuries of particular incident
particular_injuries <- particular_incident %>%
  pull(num_injured)
View(particular_injuries)

# View address of particular incident
particular_address <- particular_incident %>%
  pull(address)
View(particular_address)

# View state of particular incident
particular_state <- particular_incident %>%
  pull(state)
View(particular_state)

# --------- Interactive map ---------
library(leaflet)

map <- leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(radius = ~num_killed,
                    opacity = .5,
                    stroke = FALSE,
                    fillOpacity = .5,
                    lat = ~lat,
                    lng = ~long,
                    popup = ~paste("City:", city, "<br>",
                                   "# Killed:", num_killed, "<br>",
                                   "# Injured:", num_injured)
                         )
print(map)

# ------- plot of choice ---------

library(ggplot2)

plot_table <- df %>%
  filter(state == "Florida") %>%
  group_by(date) %>%
  summarise(state = "Florida",
            num_injured = sum(num_injured),
            num_killed = sum(num_killed))

# Question - What is the rate of change between the number killed in each event
#                                 over time?

ggplot(data = plot_table, aes(x = date, y = num_killed, group = 1)) +
  geom_line(color = "BLACK") +
  geom_point() +
  ggtitle("Amount killed in Florida events over time") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
