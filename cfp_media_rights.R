## ----include=FALSE------------------------------------------------------------------------------------------

#import libraries
library(readr)
library(mosaic)
library(ggthemes)
library(treemapify)
library(dplyr)
library(gt)
library(gtExtras)
library(tidyr)
library(truncnorm)

#import general viewership data for CFP
path = "/Users/lorenzo_dube/Documents/GitHub/CFP_Media_Rights/CFB_Bowl_Viewership.csv"
viewership = read_csv(path)

social_path = "/Users/lorenzo_dube/Documents/GitHub/CFP_Media_Rights/Social_Data.csv"
social = read_csv(social_path)
social_long = gather(social, "Platform", "Audience", 2:5)

last3BCS_firstCFP = viewership[viewership$SEASON %in% c('2010', '2011', '2012', '2013', '2014') & viewership$ROUND == 'NC', ]
last5BCS_CFP = viewership[viewership$SEASON %in% c('2010', '2011', '2012', '2013', '2015', '2016', '2017', '2018', '2009', '2008', '2007', '2006',  '2019', '2020', '2021', '2022', '2023') & viewership$ROUND == 'NC', ]
BCS_CFP = viewership[viewership$SEASON %in% c('2010', '2011', '2012', '2013', '2015', '2016', '2017', '2018', '2009', '2008', '2007', '2006', '2019', '2020', '2021', '2022', '2023', '2014') & viewership$ROUND == 'NC', ]
# create a separate dataframe with just the data from the CFP era
cfp_only = viewership[viewership$Format == 'CFP' ,]
#treat the game type as a categorical factor and Viewers as a numeric factor
cfp_only$Viewers = as.numeric(cfp_only$Viewers)
cfp_only$ROUND = factor(cfp_only$ROUND)
#format to all lowercase for round for easy calls
cfp_only<- cfp_only %>%
  mutate(ROUND = tolower(ROUND))
#create a column for whether game is a playoff game or not
cfp_only['playoff'] <- NA
cfp_only$playoff[cfp_only$ROUND == 'ny6'] <- 'Non-Playoff'
cfp_only$playoff[cfp_only$ROUND == 'nc' | cfp_only$ROUND == 'semi'] <-'Playoff'

#create separate data where national championship is excluded
cfp_no_nc = cfp_only[cfp_only$ROUND != "nc",]
cfp_nc_only = cfp_only[cfp_only$ROUND == "nc" & cfp_only$SEASON != '2014',]
cfp_no_nc['ROUND_NAME'] <- NA
cfp_no_nc$ROUND_NAME[cfp_no_nc$ROUND == 'ny6'] <- 'Non-Playoff'
cfp_no_nc$ROUND_NAME[cfp_no_nc$ROUND == 'semi'] <- 'Playoff Semifinal'

#create separate data where non-playoff games are excluded
cfp_semi_nc = cfp_only[cfp_only$ROUND != "ny6",]
cfp_semi_nc["ROUND_NAME"] <- NA
cfp_semi_nc$ROUND_NAME[cfp_semi_nc$ROUND == 'semi'] <- 'Semifinal'
cfp_semi_nc$ROUND_NAME[cfp_semi_nc$ROUND == 'nc'] <- 'Championship'

#create dataset that has average playoff vs. non-playoff viewership year to year
avg_viewers <- cfp_only %>%
  group_by(SEASON, playoff) %>%
  summarise(avg_value = mean(Viewers))
no_first_cfp_avg_viewers <-avg_viewers[avg_viewers$SEASON != '2014',]

#individaual datasets for semis, championship, and non-playoff NY6 bowls
semis = cfp_no_nc[cfp_no_nc$ROUND == 'semi',]
nysix = cfp_no_nc[cfp_no_nc$ROUND == 'ny6',]
champ = cfp_semi_nc[cfp_semi_nc$ROUND == 'nc',]

#Pull in add analysis data
path2 <-"/Users/lorenzo_dube/Documents/GitHub/CFP_Media_Rights/CFP_Ad_Analysis.csv"
ad_type_analysis = read_csv(path2)




############
#analysis to compare playoff vs. non-playoff games comparing semis and non-playoff NY6 bowls

options(scipen = 999)
ggplot(cfp_no_nc, aes(x = ROUND_NAME, y = Viewers)) +
  geom_boxplot(outlier.colour='#cba762', fill ='#cba762' ) +
  xlab(label = "Non-Playoff vs. Playoff New Year's Six Bowls") +
  ylab(label = "Viewers") +
  ggtitle("The Viewership Effect of the CFP") +
  theme_fivethirtyeight()+
  theme(axis.title.x = element_text(colour = '#E52534', face="bold", margin = margin(t = 10)),
        axis.title.y = element_text(colour = '#E52534', margin = margin(r = 20), face = "bold"))

viewer_fit = lm(Viewers~ROUND, data = cfp_no_nc)
summary(viewer_fit)
confint(viewer_fit)
##########
#Analysis comparing CFP NC Viewership in both eras with the first CFP removed
ggplot(BCS_CFP, aes(x = Format, y = Viewers)) +
  geom_boxplot(outlier.colour='#cba762', fill ='#cba762' ) +
  xlab(label = "Format") +
  ylab(label = "Viewers") +
  ggtitle("BCS vs. CFP National Championship Viewership\n(w/o Inaugural CFP)") +
  theme_fivethirtyeight()+
  theme(axis.title.x = element_text(colour = '#E52534', face="bold", margin = margin(t = 10)),
        axis.title.y = element_text(colour = '#E52534', margin = margin(r = 20), face = "bold"), 
        plot.title = element_text(hjust = 0.5))

viewer2_fit = lm(Viewers~Format, data = last5BCS_CFP)
summary(viewer2_fit)
confint(viewer2_fit)

hist(last5BCS_CFP$Viewers)

###########
#analysis to compare playoff semis to national championship to see round to round difference

ggplot(cfp_semi_nc, aes(x = ROUND_NAME, y = Viewers)) +
  geom_boxplot(outlier.colour='#cba762', fill ='#cba762' ) +
  xlab(label = "Championship vs. Semifinal Playoff Games") +
  ylab(label = "Viewers") +
  ggtitle("The Viewership Effect of Playoff Round") +
  theme_fivethirtyeight()+
  theme(axis.title.x = element_text(colour = '#E52534', face="bold", margin = margin(t = 10)),
        axis.title.y = element_text(colour = '#E52534', margin = margin(r = 20), face = "bold"))

semi_nc_fit = lm(Viewers~ROUND, data = cfp_semi_nc)
summary(semi_nc_fit)
confint(semi_nc_fit)
#######################
#Analysis of Viewership Changes over time for CPF vs non-playoff NY6 Bowls

ggplot(no_first_cfp_avg_viewers, aes(x = SEASON, y = avg_value, colour = playoff)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method ='lm', se=FALSE)+
  scale_color_manual(values=c('#E52534','#cba762'), name="") +
  ggtitle("Historical Viewership: CFP and New Year's Six\n(w/o Inaugural CFP)") +
  theme_fivethirtyeight()+
  theme(plot.title = element_text(hjust = 0.5))

cfp_viewer_year_model = lm(Viewers~SEASON, cfp_semi_nc[cfp_semi_nc$SEASON != '2014',])
summary(cfp_viewer_year_model)
confint(cfp_viewer_year_model)

ny6_viewer_year_model = lm(Viewers~SEASON, nysix)
summary(ny6_viewer_year_model)
confint(ny6_viewer_year_model)
#####################
#BCS CFP
ggplot(BCS_CFP, aes(x = SEASON, y = Viewers)) +
  geom_line(alpha = 1, color = '#cba762') +
  ggtitle("Historical Viewership: CFP and New Year's Six\nw/o Inaugural CFP") +
  theme_fivethirtyeight()+
  theme(plot.title = element_text(hjust = 0.5))

#######################
#Treeplot for types of adds based on minutes of add time
ggplot(na.omit(ad_type_analysis), aes(area = `Ad Time Per Category`, fill = Category, label = paste(`Ad Time Per Category`))) +
  geom_treemap()+
  scale_fill_manual(values = c('#E52534','#cba762', '#1e1e1e', '#e5e2dc', '#b5a68c', '#e3cf9f', '#900606', '#54646c', '#ac3c3c', '#6f6b66', '#7e7e7d', '#c6a05f', '#f0eada', '#d9a899'))+
  ggtitle("Ad Type Share By Minutes") +
  theme(plot.title = element_text(family = "sans", size=12),
        legend.text = element_text(family = "sans"))+
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) 
######################
#check for first year of new format bump
percent_increase = mean(last3BCS_firstCFP[last3BCS_firstCFP$Format == 'CFP',]$Viewers)/mean(last3BCS_firstCFP[last3BCS_firstCFP$Format == 'BCS',]$Viewers)
print(percent_increase)
###########
#check for comparison without first year increase
percent_increase = mean(last5BCS_CFP[last5BCS_CFP$Format == 'CFP',]$Viewers)/mean(last5BCS_CFP[last5BCS_CFP$Format == 'BCS',]$Viewers)
print(percent_increase)

###################
#Calculations for best/worst case future viewer analysis
mean(semis$Viewers)
confint(semis$Viewers)
######################3
####################
#AVG Viewers/Nielsen Table Setup


path3 = "/Users/lorenzo_dube/Documents/GitHub/CFP_Media_Rights/Avg_CFP_Broadcast_Viewers_Nielsen.csv"
view_niels_table = read_csv(path3)
view_niels_table %>%  
  gt() %>% 
  gt_theme_538() %>% 
  tab_header(title = "Average Viewership and Nielsen Rating by Season")


########################
#Social Media Audience Increase
#Analysis of Viewership Changes over time for CPF vs non-playoff NY6 Bowls

ggplot(social_long, aes(x = Year, y = Audience, colour=Platform)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method ='lm', se=FALSE)+
  scale_color_manual(values=c('#E52534', '#900606', '#1e1e1e','#cba762'), name="") +
  theme(legend.position = "none")+
  ggtitle("CFP Social Audience Increase Since 2016") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5))



twitter_model = lm(Twitter~Year, social)
summary(twitter_model)
confint(twitter_model)

instagram_model = lm(Instagram~Year, social)
summary(instagram_model)
confint(instagram_model)

facebook_model = lm(Facebook~Year, social)
summary(facebook_model)
confint(facebook_model)

total_social_model = lm(Total~Year, social)
summary(total_social_model)
confint(total_social_model)


#######
#Variables for Analysis
rd_inc_mean = 1.214
rd_inc_high = 1.389 
rd_inc_low = 1.038
yoy_dec_low = -1509657
yoy_dec_high = -229065.6
yoy_dec_mean = -869396
year1_inc = 1.311934
exp_year1_nc_viewers_mean = (20423879)*year1_inc
exp_year1_nc_viewers_high = (20551931)*year1_inc
exp_year1_nc_viewers_low = (20295813)*year1_inc

mean_estimate = list()
mean_viewers = exp_year1_nc_viewers_mean + (2*exp_year1_nc_viewers_mean)/rd_inc_mean + (4*exp_year1_nc_viewers_mean)/rd_inc_mean/rd_inc_mean + (4*exp_year1_nc_viewers_mean)/rd_inc_mean/rd_inc_mean/rd_inc_mean
mean_estimate = append(mean_estimate, mean_viewers)
for (x in 1:(12-1)){
  mean_viewers = mean_viewers-(yoy_dec_mean*11/3)
  mean_estimate = append(mean_estimate, mean_viewers)
}

################
#Deal Viewership Model

rtruncnorm(n=11, mean = mean(cfp_nc_only$Viewers), sd = sd(cfp_nc_only$Viewers), a=confint(cfp_nc_only$Viewers)[1], b=confint(cfp_nc_only$Viewers)[2])
boot1 = Do(1000) * {
  single_year = rtruncnorm(n=12, mean = mean(cfp_nc_only$Viewers), sd = sd(cfp_nc_only$Viewers)/sqrt(length(cfp_nc_only)), a=confint(cfp_nc_only$Viewers)[1], b=confint(cfp_nc_only$Viewers)[2])
  single_year[1] = single_year[1]*1.311934
  res <- mean(single_year)
}
confint(boot1)





