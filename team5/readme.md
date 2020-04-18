# Folder team 5

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)

#We based our code on : https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker
# link to the data - this link is an extract from our Github, it would be better if we collect the daily actualized data from oxford : https://github.com/OxCGRT/covid-policy-tracker
data <- read_csv(file = "https://raw.githubusercontent.com/warint/covid-19/master/team5/data.csv")

# Explanation of the variables in the dataset : https://www.bsg.ox.ac.uk/sites/default/files/2020-04/BSG-WP-2020-031-v4.0_0.pdf
dataCovid <- data %>%
  mutate (Date = ymd(Date)) %>% 
  select(-CountryCode, -`S8_Fiscal measures`,-`S9_Monetary measures`,-`S11_Investment in Vaccines`,-`S10_Emergency investment in health care`, -X27)

#We defined our thresholds for each strategy
dataCovid <- mutate(dataCovid, 
                    School_closing = case_when(`S1_School closing` == 2 & S1_IsGeneral == 1 ~ 1,TRUE ~ 0),
                    Workplace_closing = case_when(`S2_Workplace closing` == 2 & S2_IsGeneral == 1 ~ 1,TRUE ~ 0),
                    Cancel_Public_Events = case_when(`S3_Cancel public events` == 2 & S3_IsGeneral == 1 ~ 1,TRUE ~ 0),
                    Close_Public_Transport = case_when(`S4_Close public transport` == 2 & S4_IsGeneral == 1 ~ 1,TRUE ~ 0),
                    Public_Information_Campaigns = case_when(`S5_Public information campaigns` == 1 & S5_IsGeneral == 1 ~ 1,TRUE ~ 0),
                    Restrictions_On_Internal_Movement = case_when(`S6_Restrictions on internal movement` == 2 & S6_IsGeneral == 1 ~ 1,TRUE ~ 0),
                    International_travel_controls = case_when(`S7_International travel controls` >= 2 ~ 1,TRUE ~ 0),
                    Limited_Testing_Policy = case_when(`S12_Testing framework` == 1 | `S12_Testing framework` == 2 ~ 1,TRUE ~ 0), 
                    Open_Testing_Policy = case_when(`S12_Testing framework` == 3 ~ 1,TRUE ~ 0),
                    Contact_tracing = case_when(`S13_Contact tracing` >= 1 ~ 1,TRUE ~ 0))

#Selection of a country, France,  among 193 countries
dataCovidFrance <- dataCovid %>%
  filter (CountryName == "France")
View(dataCovidFrance)

#Visualisation of French Confirmed Cases over Time
ggplot(dataCovidFrance,aes(x=Date, y=ConfirmedCases, col=StringencyIndex))+
  geom_line()+
  ggtitle("French Confirmed Cases over Time")+
  scale_y_log10() #we put y at a log10 scale

#Pivot the CovidFrance dataset to gather the observations regarding the public policies
dataCovidFrance_td <- dataCovidFrance %>% 
  select(CountryName, Date, ConfirmedCases, ConfirmedDeaths, StringencyIndex, School_closing,
         Workplace_closing,
         Cancel_Public_Events ,
         Close_Public_Transport ,
         Public_Information_Campaigns,
         Restrictions_On_Internal_Movement,
         International_travel_controls ,
         Limited_Testing_Policy , 
         Open_Testing_Policy,
         Contact_tracing) %>%
  pivot_longer(-c(CountryName,Date, ConfirmedCases,ConfirmedDeaths, StringencyIndex), names_to = "policy", values_to = "value")

#select the observation when the policies are effective based on our threshold
test4 <- dataCovidFrance_td %>% 
  filter(value == 1) %>%
  select(CountryName, Date, policy, value)

#dataframe with the dates of the implementation of each policies
start <- test4 %>%
  group_by(policy, value) %>%
  slice(1)

#dataframe with the dates of the end of each policies
end <- test4 %>%
  group_by(policy, value) %>%
  arrange(value) %>%  
  slice(n())

#dataset gathering implementation and end dates for each policies
startend <- left_join(start, end, by = c("CountryName", "policy", "value"))

startend <- startend %>% 
  rename(
    start = Date.x,
    end = Date.y
  )

##Datavisualization of the implementation of the French public policies regarding the number of confirmed case over time
library(ggrepel)

p<- ggplot(dataCovidFrance_td, aes(x=Date, y=ConfirmedCases)) + 
  geom_line(
    size = 1) +
  geom_vline(
    data = startend,
    aes(xintercept = jitter(as.numeric(start)), col=policy),
    alpha = 0.5,
    size = 1)+
  geom_label_repel(
    aes(x = start , y = 50 , label = policy, fill = policy),
    data = startend,
    position = "jitter",
    colour = "white", 
    fontface = "bold",
    segment.color = "grey50")+
  ggtitle("French Public Policies and Confirmed Cases over Time")+
  scale_y_log10("Confirmed Cases") + 
  scale_x_date('Time', date_labels = "%e %b", date_breaks = "1 week")+
  theme_classic()+
  theme(
    plot.title=element_text(family='', face='bold', colour='black', size=21, hjust=0.5, vjust=0.5)
  )
p


#We we want to do an animation:
library(gganimate)
theme_set(theme_bw())
p + transition_reveal(Date)

'''
What still need to be done :
1- Add a nice animation 
2- Plug to daily updated database
'''


##if we want to compare Public policies & ConfirmedDeaths
  geom_line(
    data = dataCovidFrance_td,
    aes(x=Date, y=ConfirmedDeaths),
    colour = "red")


