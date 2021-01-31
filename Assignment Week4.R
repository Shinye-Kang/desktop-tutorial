library(dplyr, ggplot2) #데이터 가공, 데이터 시각화
install.packages('janitor') #row_to_names 메서드 사용하기 위해 인스톨
library(janitor)
install.packages('ggmap')
library(ggmap)

Sui_rate =read.csv('SDGSUICIDE.csv', stringsAsFactors = F)
Fac = read.csv('Facilities.csv')
HR = read.csv('Human_resources.csv')
Continent = read.csv('countryContinent.csv')
Cont = Continent[,c('country', 'continent')]
Cont <- rename(Cont, Country = country)

#left_join을 위해 2행을 column name으로 만듦
Sui_rate <- Sui_rate %>% row_to_names(row_number = 2) 
Total <- left_join(Sui_rate, Fac, by = 'Country')
Total <- left_join(Total, HR, by = 'Country')
Total <- left_join(Total, Cont, by = 'Country')

write.csv(Total, file='Total.csv')
Total = read.csv('Total.csv')
View(Total)

#정신보건인력과 자살률 평균(연령층 비율 고려 X)의 상관관계 그래프
#20~24세 자살률 상위 25% 국가 중, 자살률의 남여비가 가장 큰 국가 7개 그래프
#대륙별 자살률 그래프(성별)
#주어진 데이터와 관련있을 것 같은 데이터를 스스로 찾아 그래프 그려보기. 

#Question 1-1
Total_HR1<-
  Total %>%
  mutate(Sui_rate = (over25+over20+over15+over10)/4) %>%
  dplyr::select(Country, Sex, Sui_rate, Psychiatrist, Nurse, Socialworkers,Psychologists) %>%
  filter(Sex == ' Both sexes')
Total_HR2<- na.omit(Total_HR1)
Total_HR <-
  Total_HR2 %>% 
  mutate(total = Psychiatrist + Nurse + Socialworkers + Psychologists)

p = ggplot(data = Total_HR, aes(x = total, y = Sui_rate, fill = Country)) + geom_point()

library(plotly)
ggplotly(p)

#Question 1-2

ggplot(data = Total, aes(x=Sex, y=over20)) + geom_violin(color='pink')

Total_Both <-
  Total%>%
  filter(Sex == ' Both sexes')

quantile(Total_Both$over20)

Total_top25 <-
  Total %>%
  dplyr::select(Country, Sex, over20) %>%
  filter(Sex == ' Both sexes' & over20 > 13.5)
Total_top25$Country

Sui_over20_male <-
  Total %>%
  dplyr::select(Country, Sex, over20) %>%
  filter(Sex == ' Male', Country %in% Total_top25$Country)
Sui_over20_female <-
  Total %>%
  dplyr::select(Country, Sex, over20) %>%
  filter(Sex == ' Female', Country %in% Total_top25$Country)

Sui_over20_male$sex_ratio <-
  Sui_over20_male$over20/Sui_over20_female$over20

ratio_7 <-
  Sui_over20_male %>%
  arrange(desc(sex_ratio)) %>%
  head(7)

ggplot(data = ratio_7, aes(x= reorder(Country, sex_ratio), y= sex_ratio)) +
  geom_col() +
  coord_flip() +
  labs( x= 'Country', y= 'Suicide sex ratio', title = '20-24 years old Suicide Sex Ratio, Top 7 Countries', subtitle = 'Analyzed by K') +
  theme_minimal()


#Q3. 대륙별 20-24세 자살률 그래프

Sui_Cont <- 
  Total %>%
  filter(!is.na(continent)) %>%
  group_by(continent, Sex) %>%
  summarise(mean_sui = mean(over20))
Sui_Cont

ggplot(data = Sui_Cont, aes(x=reorder(continent, mean_sui), y=mean_sui, fill = Sex)) +
  geom_col(position='dodge') +
  labs( x= 'Continent', y= 'Crude suicide rates (per 100 000 population)', title = 'Age 20-24 Suicide Rate by Continent', subtitle = 'Analyzed by K') +
  theme_minimal()

#Q4. 
