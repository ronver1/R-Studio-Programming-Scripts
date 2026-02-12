# Include libraries #
library(mosaic)
library(tidyverse)



# risk.csv Risky Business ggplot #
ggplot(risk) + 
  geom_histogram(aes(x=income, y=..density..), binwidth=30000,fill = "darkblue", 
                 color = "darkblue") +
  facet_wrap(~cheated, labeller=label_both) +
  labs(x="Annual Income",
       y="Relative Density",
       title="Does Cheating in Relationships affect one's Annual Income?") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))


# risk.csv Blue Skies ggplot #
ggplot(risk) + 
  geom_histogram(aes(x=age, y=..density..), binwidth=7,fill = "darkblue", 
                 color = "darkblue") +
  facet_wrap(~skydiving) +
  labs(x="Age",
       y="Relative Density",
       title="Does Age affect one's likelihood to Skydive?") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))
tapply(risk$income, risk$cheated, median, na.rm = TRUE)

xtabs(~skydiving, data=risk) %>%
  prop.table %>%
  round(3)


# risk.csv Education and Gambling probablities and ggplot #
# Use the following tables to calculate Joint and Conditional Probablities #
# in order to determine independence between gamble and education #
xtabs(~gamble, data=risk) %>%
  prop.table %>%
  round(3)
xtabs(~education, data=risk) %>%
  prop.table %>%
  round(3)
xtabs(~gamble + education, data=risk) %>%
  addmargins()


ggplot(risk) + 
  geom_bar(aes(x=education), binwidth=7,fill = "darkblue", 
           color = "darkblue") +
  facet_wrap(~gamble) +
  labs(x="Maximum Education Level",
      y="Relative Density",
      title="Does Education Level affect Individual Propensity to Gamble?") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))


# candies.csv The Golden Ticket ggplot #
ggplot(candies) + 
  geom_point(aes(x=Price, y=Win), color = "darkblue") + 
  labs(x="Price",
       y="Win Percentage",
       title="Does Higher-Priced Candy Win more \nFrequently than Lower-Priced Candy?") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5))


# candies.csv Caramel and Nougat #
# Use the table to calculate the probablities #
xtabs(~Caramel + Nougat, data=candies) %>%
  addmargins()

# candies.csv Choclate and Fruit #
ggplot(candies) + 
  geom_bar(aes(x=Chocolate), fill = "darkblue", 
           color = "darkblue") +
labs(x="Chocolate",
     title="The Relationship between Chocolate and Fruit Candies") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  facet_wrap(~Fruit) +
  theme(plot.title = element_text(hjust = 0.5))


#superbowl.csv Sling Baby #
# Use the summary statistics below to calculate Sling Baby's Z-scores #
superbowl %>%
  summarize(avg_likes = mean(likes),
            sd_likes = sd(likes),
            sd_dislikes = sd(dislikes),
            avg_dislikes = mean(dislikes))

# superbowl.csv Animals in Ads #
ggplot(superbowl) + 
  geom_bar(aes(x=brand, fill=animals), 
           color = "darkblue") +
  labs(x="Brands",
       title="Proportion of Superbowl Ads that Feature Animals") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# superbowl.csv Commercials that are Funny #
ggplot(superbowl) + 
  geom_bar(aes(x=brand, fill=funny), 
           color = "darkblue") +
  labs(x="Brands",
       title="Proportion of Superbowl Ads that are Funny") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# superbowl.csv Largest Average Viewership after Superbowl #
superbowl %>%
  group_by(brand) %>%
  summarize(avg_views = mean(views)) %>%
  arrange(desc(avg_views)) %>%
  head(4)
brands = superbowl %>%
  filter(brand=='Budweiser' | brand=='Coca-Cola' | brand=='Doritos' | brand=='NFL') %>%
  group_by(year, brand) %>%
  summarize(median = median(likes))

ggplot(brands) + 
  geom_line(aes(x=year, y=median), color = "darkblue") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  labs(x="Year",
       y="Median YouTube Likes",
       title="How does the number of Youtube Likes change \nover time for the most-viewed Superbowl Ads?") +
  facet_wrap(~brand) +
  theme(plot.title = element_text(hjust = 0.5))


# MSFT.csv Time vs High Price #
ggplot(MSFT) + 
  geom_point(aes(x=Year, y=Close.t.), color = "darkblue") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  labs(x="Year",
       y="Closing Price",
       title="Microsoft's Closing Price Over the Years?") +
  theme(plot.title = element_text(hjust = 0.5))

  
           
           