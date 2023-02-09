## ----include=FALSE------------------------------------------------------------------------------------------

#import libraries
library(readr)
library(mosaic)
library(dplyr)
library(ggthemes)
library(treemapify)

#import general viewership data for CFP
path = "/Users/lorenzo_dube/Documents/GitHub/CFP_Media_Rights/CFB_Bowl_Viewership.csv"
viewership = read_csv(path)
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
summarize(viewer_fit)
confint(viewerfit)
##########
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
summarize(semi_nc_fit)
confint(semi_nc_fit)
#######################
#Analysis of Viewership Changes over time for CPF vs non-playoff NY6 Bowls

ggplot(avg_viewers, aes(x = SEASON, y = avg_value, colour = playoff)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method ='lm', se=FALSE)+
  scale_color_manual(values=c('#E52534','#cba762'), name="") +
  ggtitle("Historical Viewership: CFP and New Year's Six") +
  theme_fivethirtyeight()

cfp_viewer_year_model = lm(Viewers~SEASON, cfp_semi_nc)
summary(cfp_viewer_year_model)
confint(cfp_viewer_year_model)
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












nc_viewer_year_model = lm(Viewers~SEASON, champ)
summary(nc_viewer_year_model)
confint(nc_viewer_year_model)

semi_viewer_year_model = lm(Viewers~SEASON, semis)
summary(semi_viewer_year_model)
confint(semi_viewer_year_model)

