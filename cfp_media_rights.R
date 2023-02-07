## ----include=FALSE------------------------------------------------------------------------------------------
library(readr)
library(mosaic)
library(dplyr)
library(ggthemes)

path = "/Users/lorenzo_dube/Documents/GitHub/CFP_Media_Rights/CFB_Bowl_Viewership.csv"
viewership = read_csv(path)
# create a separate dataframe with just the data from the CFP era
cfp_only = viewership[viewership$Format == 'CFP' ,]
#treat the game type as a categorical factor
cfp_only$Viewers = as.numeric(cfp_only$Viewers)
cfp_only$ROUND = factor(cfp_only$ROUND)
cfp_no_nc = cfp_only[cfp_only$ROUND != "NC",]

cfp_no_nc <- cfp_no_nc %>%
  mutate(ROUND = tolower(ROUND))

semis = cfp_no_nc[cfp_no_nc$ROUND == 'semi',]
nysix = cfp_no_nc[cfp_no_nc$ROUND == 'ny6',]
  

viewer_fit = lm(Viewers~ROUND+SEASON, data = cfp_no_nc)

plotModel(viewer_fit, Viewers~SEASON)
summary(viewer_fit)

predicted_df <- data.frame(mpg_pred = predict(viewer_fit, cfp_no_nc), hp=df$hp)

ggplot(viewer_fit, aes(SEASON, Viewers)) +
  geom_point() + 
  geom_line(aes(SEASON,viewer_fit)) +
  scale_color_fivethirtyeight("ROUND") +
  theme_fivethirtyeight()

ggplot(cfp_no_nc,aes(y = Viewers, x = SEASON, shape = ROUND, colour = ROUND)) +
  geom_point() + geom_smooth(method = "lm", fill = NA) + scale_color_fivethirtyeight("ROUND") + theme_fivethirtyeight()

ggplot() + 
  geom_smooth(method = 'lm', data=nysix, aes(x=SEASON, y=Viewers)) + 
  
  geom_smooth(method = 'lm', data=semis, aes(x=SEASON, y=Viewers)) +
  scale_color_fivethirtyeight("ROUND") +
  theme_fivethirtyeight()
  
  


  
  





# The model from last time
salaryfit_exp = lm(Salary~Gender+Exp, data=salary)

plotModel(salaryfit_exp, Salary~Exp)

salaryfit_int = lm(Salary~Gender*Exp, data=salary)



summary(salaryfit_int)

plotModel(salaryfit_int, Salary~Exp)

gap1 = do(1000) * {
  fit = lm(Salary ~ Gender * Exp, data = resample(salary))
  betas = coef(fit)
  exper = 5 # 25th percentile of experience
  betas[2] + betas[4]*exper
}

confint(gap1)

gap2 = do(1000) * {
  fit = lm(Salary ~ Gender * Exp, data = resample(salary))
  betas = coef(fit)
  exper = 10 
  betas[2] + betas[4]*exper
}

confint(gap2)

## ----include=FALSE------------------------------------------------------------------------------------------
gpa = read_csv(paste0(path, 'grades.csv'))

fit1 = lm(MBAGPA ~BachGPA+Age, data=gpa)
coef(fit1)

# The model with interactions
lm(MBAGPA ~ BachGPA*Age, data=gpa)


