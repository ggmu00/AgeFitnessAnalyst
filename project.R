library("readxl")

my_data<-CardioGoodFitness
age<-CardioGoodFitness$Age
fitness<-CardioGoodFitness$Fitness
usage<-CardioGoodFitness$Usage
income<-CardioGoodFitness$Income

first_age_group<-0
second_age_group<-0
third_age_group<-0
fourth_age_group<-0


for(x in age){
  if(age[x]<=20 ){
    first_age_group=first_age_group+1
  }
  if(age[x]>20 && age[x]<=40){
    second_age_group=second_age_group+1
  }
  if(age[x]>40 && age[x]<=60){
    third_age_group=third_age_group+1
  }
  if(age[x]>60){
    fourth_age_group=fourth_age_group+1
  }
}
age_v <- c(first_age_group, second_age_group, third_age_group, fourth_age_group)
lbl<-c("0-20", "21-40","41-60","61+")

#this pie chart shows the frequency distribution for each age group
pie(age_v, labels = lbl)


#prints the central tendencies for the age column
mean(age)
median(age)

getMode<-function(f){
  val<-unique(f)
  val[which.max(tabulate(match(f, val)))]
  
}
getMode(age)

#shows the dispersion of the age groups
range(age)
sd(age)

#shows the people most likely to buy a treadmill based on demands of usage
t.test(usage, fitness)

#shows relation between age and fitness
age_fitness_anova <- aov(age ~ fitness, data = CardioGoodFitness)
summary(age_fitness_anova)

#shows relation between age and usage
age_usage_anova <- aov(age ~ usage, data = CardioGoodFitness)
summary(age_usage_anova)

#shows relation between usage and income
usage_income_anova<-aov(usage~income, data = CardioGoodFitness)
summary(usage_income_anova)





