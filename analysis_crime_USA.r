#Source: https://www.kaggle.com/marshallproject/crime-rates
crime_marshall = read.csv("report_Kaggle_marshall_project.csv", header = TRUE)
class(crime_marshall)

#Statistical analysis source: https://www.methodenberatung.uzh.ch/de/datenanalyse_spss/

#Data prep and visualization###########################################################

#Substitute NA values per means of the column - seemed to work but produced a very weird hist
#crime_marshall$violent_crimes[is.na(crime_marshall$violent_crimes)] <- round(mean(crime_marshall$violent_crimes, na.rm = TRUE))
#crime_marshall$population[is.na(crime_marshall$population)] <- round(mean(crime_marshall$population, na.rm = TRUE))

#Easy solution below also works, but throws away data
crime_marshall <- na.omit(crime_marshall)

#Set years to factors, instead of double numbers
crime_marshall$report_year <- as.factor(crime_marshall$report_year)

#creating a string vector of all years
years <- levels(crime_marshall$report_year)
#years

#For loop that analyses each year, plotting and calculating their correlation between 
#population
#size and violent crimes per capita
for (year in years) {
  #print (year)
  tmp_df <- crime_marshall[crime_marshall$report_year == year,]
  #Data prep (normalization of crime number per city population)
  tmp_df$viol_crimes_capita = (tmp_df$violent_crimes/tmp_df$population)
  
  m=mean(tmp_df$viol_crimes_capita)
  std=sqrt(var(tmp_df$viol_crimes_capita))
  
  filename_tmp <- paste("/home/bia/Documents/Courses/Statistik/histogram_violent_crimes_capita_", year, ".pdf", sep = "")
  #print(filename_tmp)
  
  pdf(file=filename_tmp)
    #Histogram of the dependent variable to check distribution shape
    hist(tmp_df$viol_crimes_capita, breaks=20, freq=F, col="lightgray", 
            xlab=paste("Violent crimes per capita (USA: ", year, ")", sep = ""), main ="")
    #Add to hist: density line for data
    lines(density(tmp_df$viol_crimes_capita), lwd = 2, col = "red")
    #Add to hist: normal curve for the means and std deviation
    curve(dnorm(x, mean=m, sd=std), 
             col="darkblue", lwd = 2, add=TRUE)
  dev.off()
  rm(tmp_df)
}

#Korrelation###############################################################################
#H1: Es gibt einen Zusammenhang zwischen "Number of violent crimes" und "Population" in der USA

#Voraussetzungen: beide variablen sind metrisch, normalverteilt ist NICHT angenommen und der
#untersuchte Zusammenhang zwischen die beide Variablen ist linear

#Streu diagram - einfach
options(scipen=5)

#for year in years 

plot(x=marshall_2014$population, y=marshall_2014$violent_crimes,
     xlab="Population (USA: 2014)",
     ylab="Number of violent crimes (USA: 2014)") 

plot(x=marshall_1975$population, y=marshall_1975$violent_crimes,
     xlab="Population (USA: 1975)",
     ylab="Number of violent crimes (USA: 1975)") 

#Following tutorial: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
#Streu diagram - more features and with the Pearson correlation line
library("ggplot2")
library("magrittr")
library("ggpubr")

ggscatter(marshall_2014, x = "population", y = "viol_crimes_per_capita2014", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Population (USA: 2014)", 
          ylab = "Number of violent crimes (USA: 2014)")

ggscatter(marshall_1975, x = "population", y = "viol_crimes_per_capita1975", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Population (USA: 1975)", 
          ylab = "Number of violent crimes (USA: 1975)")

#From the plots above, we see that the 5 biggest cities bias the correlation
#It would be better to exclude these 5 biggest cities, and then run the correlation test
#To get a better idea

#Test for normailty in both variables - Apparently not normal!
shapiro.test(crime_marshall$violent_crimes) # W = 0.40371, p-value < 2.2e-16
shapiro.test(crime_marshall$population) #W = 0.45905, p-value < 2.2e-16

#Visual inspection of the data normality using Q-Q plots (quantile-quantile plots)
#Q-Q plot draws the correlation between a given sample and the normal distribution
#Again, not normally distributed
ggqqplot(crime_marshall$violent_crimes, ylab = "Number of violent crimes (USA: 1975 - 2015)")
ggqqplot(crime_marshall$population, ylab = "Population (USA: 1975 - 2015)")

#Preparation: remove the 5 biggest cities
marshall_2014_without5biggest <- marshall_2014[!rownames(marshall_2014) %in% c(2733,2721,2702,2714,2739), ]
marshall_1975_without5biggest <- marshall_1975[!rownames(marshall_1975) %in% c(42,11,30,48,17), ]

#Result: The distributions are not normal, so I will use the non-parametric test 
#Spearman rank correlation to calculate the correlation coefficient

#Voraussetzung: die zwei Variablen sind metrisch (also mindestens ordinalskaliert)

cor.test(crime_marshall$violent_crimes, crime_marshall$population,  method = "spearman")
#p-value < 2.2e-16
#rho = 0.5177028 
0.5177028**2
#rho_quadrat: 0.2680162

#Ergebnis: Die "Number of violent crimes" in der USA (1975 - 2015) ist positiv korreliert 
#und signifikant mit der Population, rho = 0.518, p-value < 2.2e-16, n = 2688. 
#Dabei handelt es sich nach Cohen (1992) um einen starken Effekt (rho = 0.518).

#Wir lehnen H0 ab und nehmen H1 für dieses Dataset

#Jetzt machen wir das gleiche, aber mit den Dataset ohne die 5 größte Städte
cor.test(marshall_2014_without5biggest$viol_crimes_per_capita2014, marshall_2014_without5biggest$population, method = "spearman")
cor.test(marshall_1975_without5biggest$viol_crimes_per_capita1975, marshall_1975_without5biggest$population, method = "spearman")

cor.test(marshall_2014$rapes_percapita, marshall_2014$population, method = "spearman")

ggscatter(marshall_2014_without5biggest, x = "population", y = "viol_crimes_per_capita2014", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Population (USA: 2014) - without 5 biggest cities", 
          ylab = "Number of violent crimes per capita (USA: 2014)")

ggscatter(marshall_1975_without5biggest, x = "population", y = "viol_crimes_per_capita1975", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Population (USA: 1975) - without 5 biggest cities", 
          ylab = "Number of violent crimes per capita (USA: 1975)")

#Linear regression############################################################################


#Dependent variable: violent crimes per capita
#Independent variable: time
violent_crimes_capita2014.lm <- lm(formula=viol_crimes_per_capita2014~population, data=marshall_2014)
summary(violent_crimes_capita2014.lm)

#Visualization of the linear model
plot(y=viol_crimes_per_capita2014, x=marshall_2014$population)
abline(reg=violent_crimes_capita2014.lm,lty=1)

#Same analysis for rapes per capita
rapes_capita2014.lm <- lm(formula=rapes_percapita ~ population, data=marshall_2014)
summary(rapes_capita2014.lm)

plot(y=marshall_2014$rapes_percapita, x=marshall_2014$population)
abline(reg=rapes_capita2014.lm,lty=1)

#t-test#######################################################################################

#Wir testen ob die zentrale tendezen von "violent crimes" von 1975 und 2014 sich unterscheiden.
#Our normality tests show that our distributions are NOT normal (p-value < 0.05) 
#So, I will run the parametric t-test as well as the non-parametric version Wilcoxon test 
#for comparing two means

#Check normality
hist(marshall_1975$crimes_percapita)
hist(marshall_2014$crimes_percapita)
shapiro.test(marshall_1975$crimes_percapita)
shapiro.test(marshall_2014$crimes_percapita)

#H1: The means of violent crimes per capita in the US cities is lower in 2014 than it was in 1975 
#     mu_2014 < mu_1975
#H0: The means of violent crimes per capita in the US cities is equal or higher in 2014 than in 1975
#     mu_2014 >= mu_1975

#t-test for comparing the means - parametric test
t.test(x=marshall_2014$crimes_percapita, y=marshall_1975$crimes_percapita, var.equal = FALSE)
#Result: the difference between the means is NOT significant (p-value = 0.1482)

#Wilcoxon test for comparing the central values - non-parametric test
wilcox.test(x=marshall_2014$crimes_percapita, y=marshall_1975$crimes_percapita)
#Result: the difference between the central ranks is NOT significant (p-value = 0.1905)
#In the case of violent crimes per capita, and also to rapes per capita

#We take the H0 hypothesis, because there is no statistical difference between the central
#measurements of violent crimes between 2014 and 1975

#Now, let's take a look at homicides (1975 x 2014)
t.test(x=marshall_2014$homicides_percapita, y=marshall_1975$homicides_percapita)
#t = -3.4015, df = 132.98, p-value = 0.0008856, means 2014 = 10.73, means 1975 = 16.72
wilcox.test(x=marshall_2014$homicides_percapita, y=marshall_1975$homicides_percapita)
#W = 1322.5, p-value <<< 0.05

#We see that there IS a significant difference between homicides per capita from 1975 and 2014
#with the homicides being smaller in 2014

#ANOVA#######################################################################################
#Now that we know more about the data, let's take a look at it in more broad terms
#What have we learned so far? We see that:

#(i) there is a tendency in 2014 for bigger cities to be safer than smaller cities - 
#correlation experiments about violent crimes;
#(ii) in 1975 there was no difference between violent crimes in small or big cities

#(iii) New York is notably changed in regards to violent crimes, leaving the 6th place of most
#dangerous city in 1975 to 38th most dangerous in 2014 (violent crimes per capita)
#(iv)Detroit remains the most dangerous city - violent crimes per capita (1975 and 2014)

#(iv) there is no difference between violent crimes per capita or rapes per capita 
#in 1975 and 2014
#(v) there is a difference between homicides per capita in 1975 and 2014, with the number
#being smaller in 2014

#Now we want to compare means between all of the report years in relation to homicides p. cap.

#First, we check if the variances are equal
#Instead of the Levene test, I will use the Bartlett test to check if my variances are the same
bartlett.test(x=crime_marshall$homicides_percapita , g=crime_marshall$report_year)
#They are not equal - the groups are heterogenous

#Anova analysis
aov(homicides_percapita ~ report_year, data = crime_marshall)

#We first fit a linear model to the data and then use the Anova function to extract information
#about analysis of variance
homicides_year.lm <- lm(homicides_percapita ~ report_year, data = crime_marshall)
anova(homicides_year.lm)
#Results: there is a difference between at least two years in regards to homicide p. capita

#To know which years exactly are different from each other, we will now do a pairwise test
#between avery two groups (from years)
pairwise.t.test(x=crime_marshall$homicides_percapita, g=crime_marshall$report_year, 
                p.adjust.method = "bonferroni")
#Differences between 1999 and 1993; 2000 and 1991; 2000 and 1993; some comparisons cannot
#be seen (reached "max.print")

pairwise.wilcox.test(x=crime_marshall$homicides_percapita, g = crime_marshall$report_year,
                     p.adjust.method = "bonferroni")
#Results: no groups are significant; some comp. cannot be seen (same error from above)

#MANOVA#######################################################################################
#Just to learn how to implement MANOVA (Mehrfaktorielle Varianz Analysis ohne Messwiederholung)
#I will take two independent variables (year and city) and see how they influence my
#dependent variable (homicide rate)

