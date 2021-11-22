
library(VIM)
library(mice)

library(MixAll)
library(rtkore)

library(arm)

library(jtools)
library(ggstance)
library(ggplot2)

library(interactions)

library(corrplot)

library(maps)
library(RColorBrewer)
library(tidyverse)

library(klaR)
library(naivebayes)
library(dplyr)
library(psych)

library(lme4)
library(lattice)

library(plotly)

library(readr)
library(dplyr)

library(leaflet)
library(htmltools)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(VIM)
library(usmap)

library(gganimate)
library(ggrepel)
library(gapminder)

data <- read.csv("incarceration_trends.csv")
head(data)

columns <- c("year","fips","state","county_name", "total_pop",
             "total_jail_pop","total_jail_pop_rate","total_prison_pop","total_prison_pop_rate")
selected_data = data[columns]
colnames(selected_data) <- c("year","fips","state","county_name", "total_pop",
             "total_jail_pop","total_jail_pop_rate","total_prison_pop","total_prison_pop_rate")
str(selected_data)

selected_data[is.na(selected_data)] = 0
str(selected_data)

selected_data_year <- selected_data %>% 
  group_by(year) %>%
  summarize(sum_total_pop= sum(as.numeric(total_pop)),
            sum_total_jail_pop= sum(as.numeric(total_jail_pop)),
            sum_total_jail_pop_rate = sum_total_jail_pop/sum_total_pop,
            sum_total_prison_pop= sum(as.numeric(total_prison_pop)),
            sum_total_prison_pop_rate = sum_total_prison_pop/sum_total_pop
            )
head(selected_data_year)

selected_data_year_type <- pivot_longer(
    selected_data_year,
    cols = c(sum_total_prison_pop_rate,sum_total_jail_pop_rate),
    names_to = "type"
)
head(selected_data_year_type)

selected_data_county <- selected_data %>% 
  group_by(fips) %>%
  summarize(sum_total_pop= sum(as.numeric(total_pop)),
            sum_total_jail_pop= sum(as.numeric(total_jail_pop)),
            sum_total_jail_pop_rate = sum_total_jail_pop/sum_total_pop,
            sum_total_prison_pop= sum(as.numeric(total_prison_pop)),
            sum_total_prison_pop_rate = sum_total_prison_pop/sum_total_pop
            )
head(selected_data_county)

selected_data_state <- selected_data %>% 
# filter(year == 2000) %>%
  group_by(state) %>%
  summarize(sum_total_pop= sum(as.numeric(total_pop)),
            sum_total_jail_pop= sum(as.numeric(total_jail_pop)),
            sum_total_jail_pop_rate = sum_total_jail_pop/sum_total_pop,
            sum_total_prison_pop= sum(as.numeric(total_prison_pop)),
            sum_total_prison_pop_rate = sum_total_prison_pop/sum_total_pop
            )
head(selected_data_state)

ggplot(selected_data_year, aes(x = year)) +
  geom_area(aes(y=sum_total_jail_pop_rate+sum_total_prison_pop_rate,
                fill="sum_total_prison_pop_rate")) +
  geom_area(aes(y=sum_total_jail_pop_rate,
                fill="sum_total_jail_pop_rate")) +
  theme_bw()+
  theme(
    legend.title = element_blank()
  )+
  scale_fill_brewer(palette = "Paired")+
  labs(title='Nationwide Jail and Prison Population Rate through Time',
       subtitle='Calculated from the sum of related population by counties ÷ the sum of population by counties.')+
  ylab('')+
  xlab('Year')

cors <- cor(selected_data_year)
png(height=2800, width=3200, pointsize=32, file="cors.png")
corrplot(cors, tl.col = 'black')
corrplot(cors, add=TRUE, type="lower", method="number",
         diag=FALSE, tl.pos="n", cl.pos="n")
corrplot(cors, add=TRUE, type="upper", tl.pos="n", tl.col = 'black')
dev.off()
corrplot(cors,tl.col = 'black')

qplot(sum_total_prison_pop, sum_total_jail_pop,
      data=selected_data_year, 
      size=sum_total_pop, col=year, main="Jail-Prison") + theme_bw()

qplot(sum_total_jail_pop_rate, sum_total_prison_pop_rate,
      data=selected_data_state, 
      size=sum_total_pop, col=sum_total_pop, main="Jail-Prison") + theme_bw()

plot_usmap (
  data = selected_data_county, values = "sum_total_jail_pop_rate", color = "red"
) +
scale_fill_continuous(
  low = "white", high = "red", name = "Population Rate", label = scales::comma
) +
labs(
  title = "Total Historical Jail Population Rate by Counties",
  subtitle = "Calculated by sum of total jail population overtime ÷ sum of total population overtime."
) +
theme(legend.position = "right")

plot_usmap (
  data = selected_data_county, values = "sum_total_prison_pop_rate", color = "red"
) +
scale_fill_continuous(
  low = "white", high = "red", name = "Population Rate", label = scales::comma
) +
labs(
  title = "Total Historical Prison Population Rate by Counties",
  subtitle = "Calculated by sum of total prison population overtime ÷ sum of total population overtime."
) +
theme(legend.position = "right")

plot_usmap(
  data = selected_data_state, values = "sum_total_jail_pop_rate", color = "red"
) + 
scale_fill_continuous(
  low = "white", high = "red", name = "Population Rate", label = scales::comma
) +
labs(
  title = "Total Historical Jail Population Rate by States",
  subtitle = "Calculated by sum of total jail population by counties ÷ sum of total population by counties."
) +
theme(legend.position = "right")

plot_usmap(
  data = selected_data_state, values = "sum_total_prison_pop_rate", color = "red"
) + 
scale_fill_continuous(
  low = "white", high = "red", name = "Population Rate", label = scales::comma
) +
labs(
  title = "Total Historical Prison Population Rate by States",
  subtitle = "Calculated by sum of total Prison population by counties ÷ sum of total population by counties."
) +
theme(legend.position = "right")
