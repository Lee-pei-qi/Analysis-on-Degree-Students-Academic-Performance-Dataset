# LEE PEI QI
# TP055473


# IMPORT DATA
data = read.csv("C:\\Users\\Acer\\Documents\\R Lab Exercise\\student.csv", header = TRUE)
data


# IMPORT LIBRARY
library(ggplot2)
require(reshape2)
library(plotrix)
library(gridExtra)
library(lessR)
library(dplyr)
library(RColorBrewer)
library(smplot)
library(ltm)
library(rcompanion)
library(polycor)

citation("polycor")

# OVERVIEW OF DATASET
View(data)     # view the dataset in table form
head(data)     # show the first 5 rows in the dataset
summary(data)  # brief summary of each attribute
nrow(data)     # calculate the number of rows in the dataset
ncol(data)     # calculate the number of columns in the dataset


#######
# categorize G3 into 
# "Distinction" : 15-20
# "Merit" : 11-14
# "Pass" : 6-10
# "Fail" : 0-5
#######



# ******************
# Q1: What are the differences between the students who score >=15 in G3 and the students who score <=5 in G3?

sample1 = filter(data,G3>=15)
sample2 = filter(data,G3<=5)
sample1_rcount = nrow(sample1)
sample2_rcount = nrow(sample2)


# ----------
# Analysis 1-1: Analysis on sex

# ~~~~~~
# EXTRA FEATURE 1: count() - Get the count of each value
# Ref: https://www.rdocumentation.org/packages/plyr/versions/1.8.7/topics/count
# ~~~~~~
# count the number of male and female students who score >=15 in G3
sex1 = cbind(count(sample1, sex), G3=">=15")

# count the number of male and female students who score <=5 in G3
sex2 = cbind(count(sample2, sex), G3="<=5")
sex_df = rbind(sex1, sex2)

# ~~~~~~
# EXTRA FEATURE 2: geom_col() - another type of bar chart
# EXTRA FEATURE 3: position ="dodge" - side-by-side bar charts
# EXTRA FEATURE 4: scale_fill_manual()
# Ref: https://ggplot2.tidyverse.org/reference/geom_bar.html
# Ref: https://ggplot2.tidyverse.org/reference/position_dodge.html
# ~~~~~~
# plot a bar chart
ggplot(sex_df,aes(x=sex,y=n,fill=G3)) + 
  geom_col(position ="dodge") +
  scale_fill_manual(values=c("#add8e6","#b19cd9")) +
  labs(title = "Academic Performance based on Gender",
       x = "Gender",
       y = "Count")+
  geom_text(aes(x=sex,y=n,label=n), position=position_dodge(width=0.8))


# ----------
# Analysis 1-2: Analysis on age groups 

# calculate the mean of G3 score for each age group
age1 = sample_frac(sample1,1) %>% select(G3,age) %>% group_by(age) %>% summarise(mean(G3)) %>% cbind(G3=">=15")
age2 = sample_frac(sample2,1) %>% select(G3,age) %>% group_by(age) %>% summarise(mean(G3)) %>% cbind(G3="<=5")

# ~~~~~~
# EXTRA FEATURE 5: scale_fill_gradient()
# EXTRA FEATURE 6: scale_fill_distiller() - blue palette
# ~~~~~~
# plot a bar chart
ggplot(age1) + 
  geom_col(aes(x=age,y=`mean(G3)`,fill=`mean(G3)`)) +
  scale_fill_gradient(low="yellow",high="red") +
  labs(title = "Academic Performance based on Age Group (Score >=15)",
       x = "Age Group",
       y = "Mean(G3)")

ggplot(age2) + 
  geom_col(aes(x=age,y=`mean(G3)`,fill=`mean(G3)`)) +
  scale_fill_distiller() +
  labs(title = "Academic Performance based on Age Group (Score <=5)",
       x = "Age Group",
       y = "Mean(G3)")


# ----------
# Analysis 1-3: Analysis on address

# ~~~~~~
# EXTRA FEATURE 7: summarise(x=n()) - count observations by group 
# Ref: https://www.guru99.com/r-aggregate-function.html
# ~~~~~~
# get the total count of students from Urban and Rural respectively
address = sample_frac(data,1) %>% select(G3,address) %>% group_by(address) %>% summarise(count=n())

address1 = sample_frac(sample1,1) %>% select(G3,address) %>% group_by(address) %>% summarise(`>=15`=n())
address2 = sample_frac(sample2,1) %>% select(G3,address) %>% group_by(address) %>% summarise(`<=5`=n())

# calculate the percentage (by dividing with total count)
add1_perc=matrix(round(address1$count/address$count*100,2))
add2_perc=matrix(round(address2$count/address$count*100,2))

# join into a data frame
address_df = cbind(address1,`<=5`=address2$`<=5`)
address_df

# ~~~~~~
# EXTRA FEATURE 8: melt() - reshape and elongate data frame
# EXTRA FEATURE 9: Merging 2 pie charts into 1
# Ref: https://www.journaldev.com/47883/r-melt-and-cast-function#:~:text=The%20melt%20%28%29%20function%20in%20R%20programming%20is,data%20values%20in%20a%20long%20data%20frame%20format.
# Ref: https://stackoverflow.com/questions/23984035/putting-two-pie-charts-in-one
# ~~~~~~
# reshaping the data frame
address_df_reshape = melt(address_df, id="address")
address_df_reshape = cbind(address_df_reshape,percentage=c(add1_perc,add2_perc))
address_df_reshape

# create a pie chart
pie(address_df_reshape$value, labels=paste0(address_df_reshape$address," (",address_df_reshape$variable,") ",
                                            address_df_reshape$percentage,"%"),
    col=c("green","red"), main="Pie Chart of Address",clockwise=TRUE)


# ----------
# Analysis 1-4: Analysis on school

school1 = sample_frac(sample1,1) %>% select(G3,school) %>% group_by(school) %>% summarise(round(mean(G3),2))
school2 = sample_frac(sample2,1) %>% select(G3,school) %>% group_by(school) %>% summarise(round(mean(G3),2))

# ~~~~~~ 
# EXTRA FEATURE 10: fill=school - fill based on variable
# EXTRA FEATURE 11: hjust=0.5 - Adjust text alignment (Center plot title)
# Ref: https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/
# ~~~~~~
# create a bar chart (score >=15)
ggplot(data=school1,aes(x=school,y=`round(mean(G3), 2)`)) + 
  geom_bar(stat="identity",aes(fill=school)) +
  geom_text(aes(label=`round(mean(G3), 2)`)) +
  labs(title = "Mean(G3>=15) based on School",
       x = "School",
       y = "Mean(G3)") + 
  theme(plot.title = element_text(hjust = 0.5))  

# create a bar chart (score <=5)
ggplot(data=school2,aes(x=school,y=`round(mean(G3), 2)`)) + 
  geom_bar(stat="identity",aes(fill=school)) +
  geom_text(aes(label=`round(mean(G3), 2)`)) +
  labs(title = "Mean(G3<=5) based on School",
       x = "School",
       y = "Mean(G3)") + 
  theme(plot.title = element_text(hjust = 0.5))

# mean of score in G3 for ALL students in both school
sample_frac(data,1) %>% select(G3,school) %>% group_by(school) %>% summarise(mean(G3))


# ----------
# Analysis 1-5: Analysis on parent education level

# ~~~~~~
# EXTRA FEATURE 12: Create the function to get mode
# Ref: https://www.tutorialspoint.com/r/r_mean_median_mode.htm#:~:text=The%20mode%20is%20the%20value%20that%20has%20highest,calculate%20mode%20of%20a%20data%20set%20in%20R.
# ~~~~~~
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# get mode of mother's education level for students who score >=15 in G3 (to cross-check with graph)
sample1_medu <- getmode(sample1$Medu)
sample1_medu

# get mode of father's education level for students who score >=15 in G3 (to cross-check with graph)
sample1_fedu <- getmode(sample1$Fedu)
sample1_fedu

# get mode of mother's education level for students who score <=5 in G3 (to cross-check with graph)
sample2_medu <- getmode(sample2$Medu)
sample2_medu

# get mode of father's education level for students who score <=5 in G3 (to cross-check with graph)
sample2_fedu <- getmode(sample2$Fedu)
sample2_fedu

# ~~~~~~
# EXTRA FEATURE 13: geom_count() - Plot a 2D frequency graph
# EXTRA FEATURE 14: Subtitle
# EXTRA FEATURE 15: scale_size_area(max_size) - Size of largest points
# Ref: https://www.rdocumentation.org/packages/ggplot2/versions/1.0.1/topics/scale_size_area
# ~~~~~~
# plot a frequency graph to display the parent's education level
ggplot(sample1, aes(x=Medu, y=Fedu)) +
  geom_count() +
  labs(title = "Parent's Education Level",
       subtitle = "G3>=15",
       x = "Mother's Education Level",
       y = "Father's Education Level") +
  scale_size_area(max_size = 15)

ggplot(sample2, aes(x=Medu, y=Fedu)) +
  geom_count() +
  labs(title = "Parent's Education Level",
       subtitle = "G3<=5",
       x = "Mother's Education Level",
       y = "Father's Education Level") +
  scale_size_area(max_size = 15)


# ----------
# Analysis 1-6: Analysis on alcohol consumption

# G3 score >=15 
sample1_dalc = rbind(count(sample1,Dalc),c(4,0),c(5,0))      # workday alcohol consumption
names(sample1_dalc) = c("Consumption Time","Student Count")
sample1_walc = count(sample1,Walc)                           # weekend alcohol consumption
names(sample1_walc) = c("Consumption Time","Student Count")

# combine workday and weekend alc, add in Day and G3 column, calculate percentage
sample1_alc = cbind(rbind(sample1_dalc,sample1_walc),Day=c(rep("Workday",5),rep("Weekend",5)),G3=rep(">=15",10)) %>% 
  mutate(Percentage=round(`Student Count`/sample1_rcount*100,2))

# G3 score <=5 
sample2_dalc = count(sample2,Dalc)
names(sample2_dalc) = c("Consumption Time","Student Count")
sample2_walc = count(sample2,Walc)
names(sample2_walc) = c("Consumption Time","Student Count")

sample2_alc = cbind(rbind(sample2_dalc,sample2_walc),Day=c(rep("Workday",5),rep("Weekend",5)),G3=rep("<=5",10)) %>% 
  mutate(Percentage=round(`Student Count`/sample2_rcount*100,2))

alc_df = rbind(sample1_alc,sample2_alc)

# ~~~~~~
# EXTRA FEATURE 16: facet_wrap() - produce multi-panel plots
# EXTRA FEATURE 17: geom_text(colour) - change color of label
# EXTRA FEATURE 18: position_dodge(width) - adjust label location
# EXTRA FEATURE 19: scale_colour_discrete()
# EXTRA FEATURE 20: coord_flip() - flip x and y coordinates
# EXTRA FEATURE 21: expand_limits() - specifying the value that should be included in the x or y scale
# Ref: https://www.statology.org/facet_wrap/
# Ref: https://www.delftstack.com/howto/r/scale_color_discrete-in-r/
# Ref: https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5/topics/coord_flip
# Ref: https://ggplot2.tidyverse.org/reference/expand_limits.html
# ~~~~~~
ggplot(alc_df,aes(x=`Consumption Time`,y=`Student Count`,fill=Day))+
  geom_bar(stat="identity",
           position="dodge")+
  facet_wrap(~G3)+
  geom_text(aes(label=paste0(Percentage,"%"),
                colour=Day),
            hjust=-0.2,
            position=position_dodge(width=0.7))+
  scale_colour_discrete(l=40) +
  coord_flip()+
  expand_limits(y=170)


# ----------
# Analysis 1-7: Analysis on past class failures

# ~~~~~~
# EXTRA FEATURE 22: par(bg) - change background color of plot
# EXTRA FEATURE 23: cex.main - change font size of title
# EXTRA FEATURE 24: font.lab - change font style of axis
# EXTRA FEATURE 25: family - change font family
# EXTRA FEATURE 26: col.main - change font color of title
# EXTRA FEATURE 27: bty - change box type
# EXTRA FEATURE 28: text() - label data points
# EXTRA FEATURE 29: legend on line graph
# Ref: https://r-coder.com/plot-r
# ~~~~~~
failure1 = rbind(count(sample1,failures),c(3,0))
failure2 = count(sample2,failures)

perc_failure1 = round(failure1$n/sample1_rcount*100,2)
perc_failure2 = round(failure2$n/sample2_rcount*100,2)

par(bg = "#f7f7f7")  # light gray background color
plot(failure1,type="o",xlab="Failures",ylab="Student Count",main="Past Class Failures of Students",
     cex.main=2,cex.lab=1.3,font.lab=2,family="serif",col.main="sienna2",bty="L",col="red") 
text(failure1,labels=perc_failure1,cex=0.8,po=3,col="red")
lines(failure2,type="o",xlab="Failures",ylab="Student Count",main="Past Class Failures of Students",col="blue")
text(failure2,labels=perc_failure2,cex=0.8,po=3,col="blue")
legend("topright",c(">=15","<=5"),fill=c("red","blue"),cex=1.5)


# ----------
# Analysis 1-8: Analysis on travel time

# get the count on travel time of students (score >=15)
tt1 = sample_frac(sample1,1) %>% select(traveltime) %>% group_by(traveltime) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100,2))

# get the count on travel time of students (score <=5)
tt2 = sample_frac(sample2,1) %>% select(traveltime) %>% group_by(traveltime) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100,2))

# ~~~~~~
# EXTRA FEATURE 30: par(mfrow) - create multiple plots at once
# EXTRA FEATURE 31: paste0() - concatenate strings without separator
# EXTRA FEATURE 32: sub - Subtitle on pie chart
# EXTRA FEATURE 33: Rainbow color palette
# EXTRA FEATURE 34: Legend on pie chart
# EXTRA FEATURE 35: Label on each section
# EXTRA FEATURE 36: dev.off() - reset par() option
# Ref: https://www.statology.org/par-function-in-r/
# Ref: https://r-lang.com/paste0-function-in-r-with-example/
# Ref: https://r-coder.com/pie-chart-r/
# Ref: https://www.educba.com/pie-chart-in-r/
# ~~~~~~
par(mfrow = c(1, 2))

pie(tt1$count,labels=paste0(tt1$percentage,"%"),radius=1,main="Travel Time to School",sub="G3>=15",
    cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(tt1)),clockwise=TRUE)
legend("topright",paste0(tt1$traveltime,"H"),cex=1,fill=rainbow(nrow(tt1)))

pie(tt2$count,labels=paste0(tt2$percentage,"%"),radius=1,main="Travel Time to School",sub="G3<=5",
    cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(tt2)),clockwise=TRUE)
legend("topright",paste0(tt2$traveltime,"H"),cex=1,fill=rainbow(nrow(tt2)))

dev.off() # reset par() option


# ----------
# Analysis 1-9: Analysis on study time

# get the count on study time of students (score >=15)
st1 = sample_frac(sample1,1) %>% select(studytime) %>% group_by(studytime) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100))

# get the count on study time of students (score <=5)
st2 = sample_frac(sample2,1) %>% select(studytime) %>% group_by(studytime) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100))

# ~~~~~~
# EXTRA FEATURE 37: par(mar) - margin of plot (bottom, left, top, right)
# EXTRA FEATURE 38: 3D pie chart
# EXTRA FEATURE 39: explode - separate pie section
# EXTRA FEATURE 40: topo.colors
# EXTRA FEATURE 41: change border color
# ~~~~~~
par(mfrow=c(1, 2), mar=c(5, 4.1, 9, 2.1))

pie3D(st1$count,labels=paste0(st1$percentage,"%"),explode=0.1, main="Weekly Study Time",sub="G3>=15",
      cex.main=1.8,cex.sub=1.5,col.sub="blue",col=topo.colors(nrow(st1)),border="white")
legend("bottomright",paste0(st1$studytime,"H"),cex=0.8,fill=topo.colors(nrow(st1)))

pie3D(st2$count,labels=paste0(st2$percentage,"%"),explode=0.1, main="Weekly Study Time",sub="G3<=5",
      cex.main=1.8,cex.sub=1.5,col.sub="blue",col=topo.colors(nrow(st2)),border="white")
legend("bottomleft",paste0(st2$studytime,"H"),cex=0.8,fill=topo.colors(nrow(st2)))

dev.off()


# ----------
# Analysis 1-10: Analysis on additional education support (schoolsup,famsup,paid)

# ~~~~~~
# EXTRA FEATURE 42: rename()
# Ref: https://www.sharpsightlabs.com/blog/rename-columns-in-r/
# ~~~~~~
ss1 = sample_frac(sample1,1) %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100)) %>% cbind(type=rep("schoolsup",2)) %>% rename(Support=schoolsup)
fs1 = sample_frac(sample1,1) %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100)) %>% cbind(type=rep("famsup",2)) %>% rename(Support=famsup)
paid1 = sample_frac(sample1,1) %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100)) %>% cbind(type=rep("paid",2)) %>% rename(Support=paid)
as1 = rbind(ss1,paid1,fs1)

ss2 = sample_frac(sample2,1) %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100)) %>% cbind(type=rep("schoolsup",2)) %>% rename(Support=schoolsup)
fs2 = sample_frac(sample2,1) %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100)) %>% cbind(type=rep("famsup",2)) %>% rename(Support=famsup)
paid2 = sample_frac(sample2,1) %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100)) %>% cbind(type=rep("paid",2)) %>% rename(Support=paid)
as2 = rbind(ss2,paid2,fs2)

# ~~~~~~
# EXTRA FEATURE 43: horizontal bar chart
# EXTRA FEATURE 44: position="stack"
# EXTRA FEATURE 45: element_text(color) - font color of title
# EXTRA FEATURE 46: element_text(size) - font size of title
# EXTRA FEATURE 47: element_text(face) - font style of title
# Ref: https://www.rdocumentation.org/packages/ggplot2/versions/3.3.5/topics/position_stack
# Ref: https://ggplot2.tidyverse.org/reference/theme.html
# ~~~~~~

plot1 = ggplot(data=as1,aes(x=count,y=Support)) + 
  geom_bar(stat="identity",aes(fill=type),position="stack") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_stack(vjust=0.5),colour="black") +
  labs(title = "Additional Support of Students",
       subtitle = "G3>=15",
       x = "Student Count") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

plot2 = ggplot(data=as2,aes(x=count,y=Support)) + 
  geom_bar(stat="identity",aes(fill=type),position="stack") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_stack(vjust=0.5),colour="black") +
  labs(title = "Additional Support of Students",
       subtitle = "G3<=5",
       x = "Student Count") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

grid.arrange(plot1,plot2,nrow=2)


# ----------
# Analysis 1-11: Analysis on attended nursery school

# ~~~~~~
# EXTRA FEATURE 48: PieChart() 
# EXTRA FEATURE 49: hole - doughnut or hole plot
# EXTRA FEATURE 50: Display percentage in the middle
# Ref: https://search.r-project.org/CRAN/refmans/lessR/html/PieChart.html
# ~~~~~~
PieChart(nursery, values="%", data=sample1, fill=1:2, main="Attended Nursery School (G3>=15)",cex=2)

PieChart(nursery, hole=0, values="%", data=sample2, fill=1:2, main="Attended Nursery School (G3<=5)",cex=2)


# ----------
# Analysis 1-12: Analysis on health status

hs1 = sample_frac(sample1,1) %>% select(health) %>% group_by(health) %>% summarise(`student count`=n()) %>% 
  mutate(percentage=round(`student count`/sample1_rcount*100)) %>% cbind(G3=rep(">=15",5))
hs2 = sample_frac(sample2,1) %>% select(health) %>% group_by(health) %>% summarise(`student count`=n()) %>% 
  mutate(percentage=round(`student count`/sample2_rcount*100)) %>% cbind(G3=rep("<=5",5))

# ~~~~~~
# EXTRA FEATURE 51: alpha=factor() - change color transparency based on var
# EXTRA FEATURE 52: scale_alpha_discrete() - alpha-transparency scales
# Ref: https://stackoverflow.com/questions/69508363/r-ggplot2-scale-alpha-discrete-to-display-in-legend
# ~~~~~~
ggplot(rbind(hs1,hs2),aes(x=health,y=`student count`,group=factor(G3)))+
  geom_bar(aes(alpha=factor(G3)),fill="orange",stat="identity",position="dodge")+
  scale_alpha_discrete(range=c(0.5,1.0))+
  labs(title="Health Status of Students")+
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=1))


# ----------
# Analysis 1-13: Analysis on school absences

# ~~~~~~
# EXTRA FEATURE 53: breaks - adjust bin width
# EXTRA FEATURE 54: add data label to histogram
# EXTRA FEATURE 55: grid.arrange() - multiple ggplot in one plot
# Ref: https://www.datacamp.com/community/tutorials/make-histogram-ggplot2?utm_campaignid=282657557&utm_adgroupid=1141293625354244&utm_device=c&utm_keyword=ggplot%20%20histogram&utm_matchtype=p&utm_network=o&utm_adpostion=&utm_creative=&utm_targetid=kwd-71331524913989:aud-807607822:loc-112&utm_loc_interest_ms=&utm_loc_physical_ms=158696&msclkid=20b6caede2e61b44ac04a888ddd4a1a0&utm_source=bing&utm_medium=cpc&utm_campaign=NEW%20Granular%20Topics%20(via%20DSA%20insights)%20%7C%20Worldwide&utm_term=ggplot%20%20histogram&utm_content=community%2Ftutorials%2Fmake-histogram-ggplot2#bins2
# Ref: https://stackoverflow.com/questions/30299529/ggplot2-define-plot-layout-with-grid-arrange-as-argument-of-do-call
# ~~~~~~
plot1 = ggplot(sample1, aes(x=absences)) + 
        geom_histogram(breaks=seq(0,25,by=1),colour="white",aes(fill=..count..)) +
        scale_fill_gradient("count",low="yellow",high="red") +
        labs(title="School Absences",subtitle="G3>=15") +
        geom_text(aes(label=..count..),stat="bin",vjust=0,breaks=seq(0,25,by=1))

plot2 = ggplot(sample2, aes(x=absences)) + 
        geom_histogram(breaks=seq(0,25,by=1),colour="white",aes(fill=..count..)) +
        scale_fill_gradient("count",low="yellow",high="red") +
        labs(subtitle="G3<=5") +
        geom_text(aes(label=..count..),stat="bin",vjust=0,breaks=seq(0,25,by=1))

grid.arrange(plot1,plot2,nrow=2)




# ******************
# Q2: What causes the students to regress in G3? 

# (score >=15 in G1&G2 but <15 in G3) & (score >=10 in G1&G2 but <10 in G3) & (score >5 in G1&G2 but <=5 in G3)
sample3 = rbind(filter(data,G1>=15 & G2>=15 & G3<15), filter(data,G1>=10 & G2>=10 & G3<10), filter(data,G1>5 & G2>5 & G3<=5))
sample3_rcount = nrow(sample3)

# score >=15 in all 3 tests
sample_all_dis = filter(data,G1>=15 & G2>=15 & G3>=15)
sample_all_dis_rcount = nrow(sample_all_dis)

# score <=5 in all 3 tests
sample_all_fail = filter(data,G1<=5 & G2<=5 & G3<=5)
sample_all_fail_rcount = nrow(sample_all_fail)


# ----------
# Analysis 2-1: Does the weekly study time of the students decrease?

# study time of students who regress in G3
st3 = sample_frac(sample3,1) %>% select(studytime) %>% group_by(studytime) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample3_rcount*100))

# --
# compare with students who score distinction in G3
par(mfrow=c(1, 2), mar=c(9, 4.1, 9, 2.1))

pie3D(st1$count,labels=paste0(st1$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who G3>=15",cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(st1)),border="white")
legend("bottomright",l,cex=0.8,fill=rainbow(nrow(st1)))

pie3D(st3$count,labels=paste0(st3$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Regress in G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(st3)),border="white")
legend("bottomleft",l,cex=0.8,fill=rainbow(nrow(st3)))


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)
st_all_dis = sample_frac(sample_all_dis,1) %>% select(studytime) %>% group_by(studytime) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sample_all_dis_rcount*100))

pie3D(st_all_dis$count,labels=paste0(st_all_dis$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Score >=15\nin G1,G2,G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(st_all_dis)),border="white")
legend("bottomright",l,cex=0.8,fill=rainbow(nrow(st_all_dis)))

pie3D(st3$count,labels=paste0(st3$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Regress\nin G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(st3)),border="white")
legend("bottomleft",l,cex=0.8,fill=rainbow(nrow(st3)))

dev.off()


# ----------
# Analysis 2-2: Will being in a romantic relationship cause the academic performance of students to decrease?

# count of students who regress in G3 & in a relationship
rr3 = sample_frac(sample3,1) %>% select(romantic) %>% group_by(romantic) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample3_rcount*100)) %>% cbind(type=rep("Regress",2))

# --
# compare with students who score distinction in G3
rr1 = sample_frac(sample1,1) %>% select(romantic) %>% group_by(romantic) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100)) %>% cbind(type=rep("G3>=15",2))
rr = rbind(rr1,rr3)

# ~~~~~~
# EXTRA FEATURE 56: barplot()
# EXTRA FEATURE 57: horiz=TRUE - horizontal R barplot
# EXTRA FEATURE 58: legend.text - R barplot legend
# EXTRA FEATURE 59: RColorBrewer() - color palette
# Ref: https://r-coder.com/barplot-r/#:~:text=Horizontal%20barplot%20By%20default%2C%20barplots%20in%20R%20are,bar%20chart%20setting%20the%20horiz%20argument%20to%20TRUE.
# ~~~~~~
# Horizontal barplot
p=barplot(height=rr$count,names=rr$romantic,main="Romantic Relationship vs Score",legend.text=rr$type,
        ylab="Romantic Relationship",xlab="Count",col=brewer.pal(length(rr),"Set2"),horiz=TRUE)
text(y=p,x=rr$count-5,labels=paste0(rr$percentage,"%"))


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)
rr_all_dis = sample_frac(sample_all_dis,1) %>% select(romantic) %>% group_by(romantic) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_dis_rcount*100)) %>% cbind(type=rep("G1,G2,G3>=15",2))
rr = rbind(rr_all_dis,rr3)

p=barplot(height=rr$count,names=rr$romantic,main="Romantic Relationship vs Score",legend.text=rr$type,
          xlab="Romantic Relationship",ylab="Count",col=brewer.pal(length(rr),"Set3"),beside=TRUE)
text(x=p,y=rr$count-2,labels=paste0(rr$percentage,"%"))


# --
# compare with students who fail in G3
sample_frac(sample2,1) %>% select(romantic) %>% group_by(romantic) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100))

# compare with students who fail in all 3 tests (G1,G2,G3)
sample_frac(sample_all_fail,1) %>% select(romantic) %>% group_by(romantic) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/nrow(sample_all_fail)*100))


# ----------
# Analysis 2-3: Do the students who regress in G3 show increase in absences?

abs3 = sample_frac(sample3,1) %>% select(absences) %>% group_by(absences) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample3_rcount*100))

# --
# compare with students who score distinction in G3 & students who score distinction in all 3 tests (G1,G2,G3)
plot1 = ggplot(sample3, aes(x=absences)) + 
  geom_histogram(breaks=seq(0,25,by=1),colour="white",aes(fill=..count..)) +
  scale_fill_gradient("count",low="yellow",high="red") +
  labs(title="School Absences",subtitle="Regress") +
  geom_text(aes(label=..count..),stat="bin",vjust=0.3,breaks=seq(0,25,by=1))

plot2 = ggplot(sample1, aes(x=absences)) + 
  geom_histogram(breaks=seq(0,25,by=1),colour="white",aes(fill=..count..)) +
  scale_fill_gradient("count",low="yellow",high="red") +
  labs(subtitle="G3>=15") +
  geom_text(aes(label=..count..),stat="bin",vjust=0.3,breaks=seq(0,25,by=1))

plot3 = ggplot(sample_all_dis, aes(x=absences)) + 
  geom_histogram(breaks=seq(0,25,by=1),colour="white",aes(fill=..count..)) +
  scale_fill_gradient("count",low="yellow",high="red") +
  labs(subtitle="G1,G2,G3>=15") +
  geom_text(aes(label=..count..),stat="bin",vjust=0.3,breaks=seq(0,25,by=1))

grid.arrange(plot1,plot2,plot3,nrow=3)

# absences of students in each category (in percentage)
# students who regress in G3
abs3

# students who score distinction in G3
sample_frac(sample1,1) %>% select(absences) %>% group_by(absences) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100))

# students who score distinction in all 3 tests (G1,G2,G3)
sample_frac(sample_all_dis,1) %>% select(absences) %>% group_by(absences) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_dis_rcount*100))


# ----------
# Analysis 2-4: Will participation in extra-curricular activities cause the academic performance of students to decrease?

eca3 = sample_frac(sample3,1) %>% select(activities) %>% group_by(activities) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample3_rcount*100,2))

# --
# compare with students who score distinction in G3 
eca1 = sample_frac(sample1,1) %>% select(activities) %>% group_by(activities) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100,2))

# ~~~~~~
# EXTRA FEATURE 60: lty - change border line type
# EXTRA FEATURE 61: density - add shading lines
# EXTRA FEATURE 62: angle - angle of the lines
# ~~~~~~
par(mfrow = c(1, 2))

pie(eca3$count,labels=paste0(eca3$percentage,"%"),radius=1,main="Extra-Curricular Activities",sub="Regress",
    cex.main=1.5,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(eca3)),clockwise=TRUE,lty=2,density=5,angle=45)
legend("topright",eca3$activities,cex=1,fill=rainbow(nrow(eca3)))

pie(eca1$count,labels=paste0(eca1$percentage,"%"),radius=1,main="Extra-Curricular Activities",sub="G3>=15",
    cex.main=1.5,cex.sub=1.5,col.sub="blue",col=c("#FFAD00","#44D62C"),clockwise=TRUE,lty=4,density=5,angle=75)
legend("topright",eca1$activities,cex=1,fill=c("#FFAD00","#44D62C"))


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)
eca_all_dist = sample_frac(sample_all_dis,1) %>% select(activities) %>% group_by(activities) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sample_all_dis_rcount*100,2))

pie(eca3$count,labels=paste0(eca3$percentage,"%"),radius=1,main="Extra-Curricular Activities",sub="Regress",
    cex.main=1.5,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(eca3)),clockwise=TRUE)
legend("topright",eca3$activities,cex=1,fill=rainbow(nrow(eca3)))

pie(eca_all_dist$count,labels=paste0(eca_all_dist$percentage,"%"),radius=1,main="Extra-Curricular Activities",
    sub="G1,G2,G3>=15",cex.main=1.5,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(eca_all_dist)),clockwise=TRUE)
legend("topright",eca_all_dist$activities,cex=1,fill=rainbow(nrow(eca_all_dist)))

dev.off() # reset par() option


# ----------
# Analysis 2-5: Does the alcohol consumption of the students increase?

# workday alcohol consumption of students who regress in G3
dalc3 = sample_frac(sample3,1) %>% select(Dalc) %>% group_by(Dalc) %>% summarise(`Student Count`=n()) %>% 
  rbind(c(4,0)) %>% cbind(Day=rep("Workday",5),G3=rep("Regress",5)) %>% 
  mutate(Percentage=round(`Student Count`/sample3_rcount*100)) %>% rename(`Consumption Time`=Dalc)

# weekend alcohol consumption of students who regress in G3
walc3 = sample_frac(sample3,1) %>% select(Walc) %>% group_by(Walc) %>% summarise(`Student Count`=n()) %>% 
  cbind(Day=rep("Weekend",5),G3=rep("Regress",5)) %>% mutate(Percentage=round(`Student Count`/sample3_rcount*100)) %>% 
  rename(`Consumption Time`=Walc)

# alcohol consumption of students who regress in G3
alc3 = rbind(dalc3,walc3)

# --
# compare with students who score distinction in G3 
sample1_alc = cbind(rbind(sample1_dalc,sample1_walc),Day=c(rep("Workday",5),rep("Weekend",5)),G3=rep(">=15",10)) %>% 
  mutate(Percentage=round(`Student Count`/sample1_rcount*100,2))

ggplot(rbind(sample1_alc,alc3),aes(x=`Consumption Time`,y=`Student Count`,fill=Day))+
  geom_bar(stat="identity",
           position="dodge")+
  facet_wrap(~G3)+
  geom_text(aes(label=paste0(Percentage,"%"),
                colour=Day),
            hjust=-0.2,
            position=position_dodge(width=0.7))+
  scale_colour_discrete(l=40) +
  coord_flip()+
  expand_limits(y=170)


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)

# workday alcohol consumption
dalc_all_dis = sample_frac(sample_all_dis,1) %>% select(Dalc) %>% group_by(Dalc) %>% summarise(`Student Count`=n()) %>% 
  rbind(c(4,0),c(5,0)) %>% cbind(Day=rep("Workday",5),G3=rep("G1,G2,G3>=15",5)) %>% 
  mutate(Percentage=round(`Student Count`/sample_all_dis_rcount*100)) %>% rename(`Consumption Time`=Dalc)

# weekend alcohol consumption
walc_all_dis = sample_frac(sample_all_dis,1) %>% select(Walc) %>% group_by(Walc) %>% summarise(`Student Count`=n()) %>% 
  cbind(Day=rep("Weekend",5),G3=rep("G1,G2,G3>=15",5)) %>% 
  mutate(Percentage=round(`Student Count`/sample_all_dis_rcount*100)) %>% rename(`Consumption Time`=Walc)

alc_all_dis = rbind(dalc_all_dis,walc_all_dis)

ggplot(rbind(alc_all_dis,alc3),aes(x=`Consumption Time`,y=`Student Count`,fill=Day))+
  geom_bar(stat="identity",
           position="dodge")+
  facet_wrap(~G3)+
  geom_text(aes(label=paste0(Percentage,"%"),
                colour=Day),
            hjust=-0.2,
            position=position_dodge(width=0.7))+
  scale_colour_discrete(l=40) +
  coord_flip()+
  expand_limits(y=170)


# ----------
# Analysis 2-6: Will health status affect the academic performance of students?

hs3 = sample_frac(sample3,1) %>% select(health) %>% group_by(health) %>% summarise(`student count`=n()) %>% 
  mutate(percentage=round(`student count`/sample3_rcount*100)) %>% cbind(G3=rep("regress",5))

# --
# compare with students who score distinction in G3 
ggplot(rbind(hs1,hs3),aes(x=health,y=`student count`))+
  geom_bar(aes(alpha=factor(G3)),fill="orange",stat="identity",position="dodge")+
  scale_alpha_discrete(range=c(0.5,1.0))+
  labs(title="Health Status of Students") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.8))


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)
hs_all_dis = sample_frac(sample_all_dis,1) %>% select(health) %>% group_by(health) %>% 
  summarise(`student count`=n()) %>% mutate(percentage=round(`student count`/sample_all_dis_rcount*100)) %>% 
  cbind(G3=rep("G1,G2,G3>=15",5))

ggplot(rbind(hs_all_dis,hs3),aes(x=health,y=`student count`))+
  geom_bar(aes(alpha=factor(G3)),fill="orange",stat="identity",position="dodge")+
  scale_alpha_discrete(range=c(0.5,1.0))+
  labs(title="Health Status of Students") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.8))




# ******************
# Q3: What are the reasons causing the students to progress and score better in G3?

# (score <15 in G1&G2 but >=15 in G3) & (score <10 in G1&G2 but >=10 in G3) & (score <5 in G1&G2 but >=5 in G3)
sample4 = rbind(filter(data,G1<15 & G2<15 & G3>=15), filter(data,G1<10 & G2<10 & G3>=10), filter(data,G1<=5 & G2<=5 & G3>5))
sample4_rcount = nrow(sample4)


# ----------
# Analysis 3-1: Will additional education support influence the academic performance of students?

# count of students who progress in G3 and have received additional education support
ss4 = sample_frac(sample4,1) %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample4_rcount*100)) %>% cbind(type=rep("schoolsup",2)) %>% rename(Support=schoolsup)
fs4 = sample_frac(sample4,1) %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample4_rcount*100)) %>% cbind(type=rep("famsup",2)) %>% rename(Support=famsup)
paid4 = sample_frac(sample4,1) %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample4_rcount*100)) %>% cbind(type=rep("paid",2)) %>% rename(Support=paid)
as4 = rbind(ss4,paid4,fs4)


# --
# compare with students who score distinction in G3

plot1 = ggplot(data=as1,aes(x=count,y=Support)) + 
        geom_bar(stat="identity",aes(fill=type),position="stack") +
        geom_text(aes(label=paste0(percentage,"%")),position=position_stack(vjust=0.6),colour="black") +
        labs(title = "Additional Support of Students",
             subtitle = "G3>=15",
             x = "Student Count") +
        theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
              plot.subtitle = element_text(hjust=0.5,face="bold"))

plot2 = ggplot(data=as4,aes(x=count,y=Support)) + 
        geom_bar(stat="identity",aes(fill=type),position="stack") +
        geom_text(aes(label=paste0(percentage,"%")),position=position_stack(vjust=0.6),colour="black") +
        labs(title = "Additional Support of Students",
             subtitle = "Progress",
             x = "Student Count") +
        theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
              plot.subtitle = element_text(hjust=0.5,face="bold"))

grid.arrange(plot1,plot2,nrow=2)


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)

ss_all_dis = sample_frac(sample_all_dis,1) %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_dis_rcount*100)) %>% cbind(type=rep("schoolsup",2)) %>% rename(Support=schoolsup)
fs_all_dis = sample_frac(sample_all_dis,1) %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_dis_rcount*100)) %>% cbind(type=rep("famsup",2)) %>% rename(Support=famsup)
paid_all_dis = sample_frac(sample_all_dis,1) %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_dis_rcount*100)) %>% cbind(type=rep("paid",2)) %>% rename(Support=paid)
as_all_dis = rbind(ss_all_dis,paid_all_dis,fs_all_dis)

plot3 = ggplot(data=as_all_dis,aes(x=count,y=Support)) + 
  geom_bar(stat="identity",aes(fill=type),position="stack") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_stack(vjust=0.6),colour="black") +
  labs(title = "Additional Support of Students",
       subtitle = "G1,G2,G3>=15",
       x = "Student Count") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

grid.arrange(plot3,plot2,nrow=2)


# --
# compare with students who fail in G3
ss2 = sample_frac(sample2,1) %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100)) %>% cbind(type=rep("schoolsup",2)) %>% rename(Support=schoolsup)
fs2 = sample_frac(sample2,1) %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100)) %>% cbind(type=rep("famsup",2)) %>% rename(Support=famsup)
paid2 = sample_frac(sample2,1) %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100)) %>% cbind(type=rep("paid",2)) %>% rename(Support=paid)
as2 = rbind(ss2,paid2,fs2)

plot4 = ggplot(data=as2,aes(x=count,y=Support)) + 
  geom_bar(stat="identity",aes(fill=type),position="stack") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_stack(vjust=0.6),colour="black") +
  labs(title = "Additional Support of Students",
       subtitle = "G3<=5",
       x = "Student Count") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

grid.arrange(plot4,plot2,nrow=2)


# --
# compare with students who fail in all 3 tests (G1,G2,G3)
ss_all_fail = sample_frac(sample_all_fail,1) %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_fail_rcount*100)) %>% cbind(type="schoolsup") %>% rename(Support=schoolsup)
fs_all_fail = sample_frac(sample_all_fail,1) %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_fail_rcount*100)) %>% cbind(type="famsup") %>% rename(Support=famsup)
paid_all_fail = sample_frac(sample_all_fail,1) %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample_all_fail_rcount*100)) %>% cbind(type="paid") %>% rename(Support=paid)
as_all_fail = rbind(ss_all_fail,fs_all_fail,paid_all_fail)

plot5 = ggplot(data=as_all_fail,aes(x=count,y=Support)) + 
  geom_bar(stat="identity",aes(fill=type),position="stack") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_stack(vjust=0.6),colour="black") +
  labs(title = "Additional Support of Students",
       subtitle = "G1,G2,G3<=5",
       x = "Student Count") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

grid.arrange(plot5,plot2,nrow=2)


# ----------
# Analysis 3-2: Does the weekly study time of the students increase?

st4 = sample_frac(sample4,1) %>% select(studytime) %>% group_by(studytime) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample4_rcount*100))

# --
# compare with students who score distinction in G3
par(mfrow=c(1, 2), mar=c(9, 4.1, 9, 2.1))

pie3D(st1$count,labels=paste0(st1$percentage,"%"),explode=0.1, main="Weekly Study Time",sub="Students who G3>=15",
      cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(st1)),border="white")
legend("bottomright",l,cex=0.8,fill=rainbow(nrow(st1)))

pie3D(st4$count,labels=paste0(st4$percentage,"%"),explode=0.1, main="Weekly Study Time",sub="Students who Progress in G3",
      cex.main=1.8,cex.sub=1.5,col.sub="blue",col=rainbow(nrow(st4)),border="white")
legend("bottomright",paste0(st4$studytime,"H"),cex=0.8,fill=rainbow(nrow(st4)))


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)
st_all_dis = sample_frac(sample_all_dis,1) %>% select(studytime) %>% group_by(studytime) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sample_all_dis_rcount*100))

pie3D(st_all_dis$count,labels=paste0(st_all_dis$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Score >=15\nin G1,G2,G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",
      col=rainbow(nrow(st_all_dis)),border="white")
legend("bottomright",l,cex=0.8,fill=rainbow(nrow(st_all_dis)))

pie3D(st4$count,labels=paste0(st4$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Progress in G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",
      col=rainbow(nrow(st4)),border="white")
legend("bottomright",paste0(st4$studytime,"H"),cex=0.8,fill=rainbow(nrow(st4)))


# --
# compare with students who fail in G3
st2 = sample_frac(sample2,1) %>% select(studytime) %>% group_by(studytime) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100))

pie3D(st2$count,labels=paste0(st2$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who G3<=5",cex.main=1.8,cex.sub=1.5,col.sub="blue",
      col=rainbow(nrow(st2)),border="white")
legend("bottomright",l,cex=0.8,fill=rainbow(nrow(st2)))

pie3D(st4$count,labels=paste0(st4$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Progress in G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",
      col=rainbow(nrow(st4)),border="white")
legend("bottomright",paste0(st4$studytime,"H"),cex=0.8,fill=rainbow(nrow(st4)))


# --
# compare with students who score fail in all 3 tests (G1,G2,G3)
st_all_fail = sample_frac(sample_all_fail,1) %>% select(studytime) %>% group_by(studytime) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sample_all_fail_rcount*100))

pie3D(st_all_fail$count,labels=paste0(st_all_fail$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Score <=5\nin G1,G2,G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",
      col=rainbow(nrow(st_all_fail)),border="white")
legend("bottomright",paste0(st_all_fail$studytime,"H"),cex=0.8,fill=rainbow(nrow(st_all_fail)))

pie3D(st4$count,labels=paste0(st4$percentage,"%"),explode=0.1, main="Weekly Study Time",
      sub="Students who Progress in G3",cex.main=1.8,cex.sub=1.5,col.sub="blue",
      col=rainbow(nrow(st4)),border="white")
legend("bottomright",paste0(st4$studytime,"H"),cex=0.8,fill=rainbow(nrow(st4)))

dev.off()


# ----------
# Analysis 3-3: Will the students score better when they want to take higher education?

he4 = sample_frac(sample4,1) %>% select(higher) %>% group_by(higher) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample4_rcount*100,2)) %>% cbind(type=rep("Progress",2))

# --
# compare with students who score distinction in G3 
he1 = sample_frac(sample1,1) %>% select(higher) %>% group_by(higher) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample1_rcount*100,2)) %>% cbind(type="G3>=15")

ggplot(rbind(he1,he4),aes(x=type,y=count))+
  geom_bar(aes(fill=higher),stat="identity",position="dodge")+
  labs(title="Higher Education vs Score") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9))


# --
# compare with students who score distinction in all 3 tests (G1,G2,G3)
he_all_dis = sample_frac(sample_all_dis,1) %>% select(higher) %>% group_by(higher) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sample_all_dis_rcount*100,2)) %>% 
  cbind(type="G1,G2,G3>=15")

ggplot(rbind(he_all_dis,he4),aes(x=type,y=count))+
  geom_bar(aes(fill=higher),stat="identity",position="dodge")+
  labs(title="Higher Education vs Score") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9))


# --
# compare with students who fail in G3 
he2 = sample_frac(sample2,1) %>% select(higher) %>% group_by(higher) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/sample2_rcount*100,2)) %>% cbind(type=rep("G3<=5",2))

ggplot(rbind(he2,he4),aes(x=type,y=count))+
  geom_bar(aes(fill=higher),stat="identity",position="dodge")+
  labs(title="Higher Education vs Score") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9))


# --
# compare with students who fail in all 3 tests (G1,G2,G3)
he_all_fail = sample_frac(sample_all_fail,1) %>% select(higher) %>% group_by(higher) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sample_all_fail_rcount*100,2)) %>% 
  cbind(type=rep("G1,G2,G3<=5",2))

ggplot(rbind(he_all_fail,he4),aes(x=type,y=count))+
  geom_bar(aes(fill=higher),stat="identity",position="dodge")+
  labs(title="Higher Education vs Score") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9))




# ******************
# Q4: What is the effect of alcohol consumption on the academic performance of the students?
  
  
# ----------
# Analysis 4-1: Analysis on health status

sample_data = select(data,c(Dalc,Walc,health,G1,G2,G3))

# ~~~~~~
# EXTRA FEATURE 63: aggregate() - group data and return summary statistics for each group
# Ref: https://www.datasciencemadesimple.com/aggregate-function-in-r/#:~:text=Aggregate%20%28%29%20Function%20in%20R%20Splits%20the%20data,the%20aggregate%20operations%20like%20sum%2Ccount%2Cmean%2C%20minimum%20and%20Maximum.
# ~~~~~~
# mean of Weekend alc con and test scores for each level of Workday alc con
aggregate(select(sample_data,c(Walc,health,G1,G2,G3)),by=list(sample_data$Dalc),FUN=mean)

# mean of Workday alc con and test scores for each level of Weekend alc con
aggregate(select(sample_data,c(Dalc,health,G1,G2,G3)),by=list(sample_data$Walc),FUN=mean)


# ----------
# Analysis 4-2: Analysis on the parent's cohabitation status (separated = more consumption?)

sample_data = select(data,c(Dalc,Walc,Pstatus,G1,G2,G3))

aggregate(select(sample_data,c(Dalc,Walc,G1,G2,G3)),by=list(sample_data$Pstatus),FUN=mean)


# ----------
# Analysis 4-3: Analysis on study time (high consumption = less study time?)
  
sample_data = select(data,c(Dalc,Walc,studytime,G1,G2,G3))

# mean of studytime and test scores for each level of Workday alc con
aggregate(select(data,c(studytime,G1,G2,G3)),by=list(data$Dalc),FUN=mean)

# mean of studytime and test scores for each level of Weekend alc con
aggregate(select(data,c(studytime,G1,G2,G3)),by=list(data$Walc),FUN=mean)

# mean of alcohol consumption and test scores for each study time
al_st = aggregate(select(sample_data,c(Dalc,Walc,G1,G2,G3)),by=list(sample_data$studytime),FUN=mean)

plot(x=al_st$Group.1,y=al_st$Dalc,type="o",xlab="Study Time",ylab="Dalc",
     main="Weekly Study Time vs Workday Alcohol Consumption",col="blue")
plot(x=al_st$Group.1,y=al_st$Walc,type="o",xlab="Study Time",ylab="Walc",
     main="Weekly Study Time vs Weekend Alcohol Consumption",col="blue")


# ----------
# Analysis 4-4: Analysis on family relationship (relationship weak = high consumption?)

# mean of famrel and test scores for each level of Workday alc con
aggregate(select(data,c(famrel,G1,G2,G3)),by=list(data$Dalc),FUN=mean)

# mean of famrel and test scores for each level of Weekend alc con
aggregate(select(data,c(famrel,G1,G2,G3)),by=list(data$Walc),FUN=mean)

sample_data = select(data,c(Dalc,Walc,famrel,G1,G2,G3))

# mean of alcohol consumption and test scores for each family relationship
al_fr = aggregate(select(sample_data,c(Dalc,Walc,G1,G2,G3)),by=list(sample_data$famrel),FUN=mean)

plot(x=al_fr$Group.1,y=al_fr$Dalc,type="o",xlab="Family Realtionship",ylab="Dalc",
     main="Family Realtionship Quality vs Workday Alcohol Consumption",col="blue")
plot(x=al_fr$Group.1,y=al_fr$Walc,type="o",xlab="Family Realtionship",ylab="Walc",
     main="Family Realtionship Quality vs Weekend Alcohol Consumption",col="blue")


# ----------
# Analysis 4-5: Analysis on free time after school & going out with friends

# find the relationship between free time & go out frequency
# mean of go out frequency for each level of free time
aggregate(select(data,goout),by=list(data$freetime),FUN=mean)

# mean of free time for each go out frequency
aggregate(select(data,freetime),by=list(data$goout),FUN=mean)

# mean of free time and go out frequency for each level of Workday alc con
aggregate(select(data,c(freetime,goout)),by=list(data$Dalc),FUN=mean)

# mean of free time and go out frequency for each level of Weekend alc con
aggregate(select(data,c(freetime,goout)),by=list(data$Walc),FUN=mean)

sample_data = select(data,c(Dalc,Walc,freetime,goout,G1,G2,G3))

# mean of alcohol consumption and test scores for each free time
al_ft = aggregate(select(sample_data,c(Dalc,Walc,G1,G2,G3)),by=list(sample_data$freetime),FUN=mean)

plot(x=al_ft$Group.1,y=al_ft$Dalc,type="o",xlab="Free Time",ylab="Dalc",
     main="Free Time vs Workday Alcohol Consumption",col="blue")
plot(x=al_ft$Group.1,y=al_ft$Walc,type="o",xlab="Free Time",ylab="Walc",
     main="Free Time vs Weekend Alcohol Consumption",col="blue")

# mean of alcohol consumption and test scores for each go out frequency
al_go = aggregate(select(sample_data,c(Dalc,Walc,G1,G2,G3)),by=list(sample_data$goout),FUN=mean)

plot(x=al_go$Group.1,y=al_go$Dalc,type="o",xlab="Go Out",ylab="Dalc",
     main="Go Out vs Workday Alcohol Consumption",col="blue")
plot(x=al_go$Group.1,y=al_go$Walc,type="o",xlab="Go Out",ylab="Walc",
     main="Go Out vs Weekend Alcohol Consumption",col="blue")


# ----------
# Analysis 4-6: Analysis on past class failures (high consumption = more failure)

# mean of failures for each level of Workday alc con
dalc_f = aggregate(select(data,c(failures)),by=list(data$Dalc),FUN=mean)

# mean of failures for each level of Weekend alc con
walc_f = aggregate(select(data,c(failures)),by=list(data$Walc),FUN=mean)

plot(x=dalc_f$Group.1,y=dalc_f$failures,type="o",xlab="Dalc",ylab="Failure",
     main="Past Class Failure vs Workday Alcohol Consumption",col="blue")
plot(x=walc_f$Group.1,y=walc_f$failures,type="o",xlab="Walc",ylab="Failure",
     main="Past Class Failure vs Weekend Alcohol Consumption",col="blue")

# mean of alcohol consumption and test scores for failures
al_f = sample_frac(data,1) %>% group_by(failures) %>% summarise(mean(Dalc),mean(Walc),
                                                                mean(G1),mean(G2),mean(G3))

plot(x=al_f$failures,y=al_f$`mean(Dalc)`,type="o",xlab="Failure",ylab="Dalc",
     main="Past Class Failure vs Workday Alcohol Consumption",col="blue")
plot(x=al_f$failures,y=al_f$`mean(Walc)`,type="o",xlab="Failure",ylab="Walc",
     main="Past Class Failure vs Weekend Alcohol Consumption",col="blue")




# ******************
# Q5: Why do students from urban areas perform better than students from rural areas? 


# ----------
# Analysis 5-1: Analysis on travel time (rural = longer travel time?)

# mean of travel time for each address
sample_frac(data,1) %>% select(address,traveltime) %>% group_by(address) %>% summarise(mean(traveltime))

# calculate the percentage of students based on address and travel time
x = sample_frac(data,1) %>% select(address,traveltime) %>% group_by(address,traveltime) %>% summarise(count=n())
x1 = filter(x,address=='R') %>% mutate(percentage=round(count/sum(count)*100,2))
x2 = filter(x,address=='U') %>% mutate(percentage=round(count/sum(count)*100,2))
x = rbind(x1,x2)

# ~~~~~~
# EXTRA FEATURE 64: multiple pie chart using ggplot
# EXTRA FEATURE 65: coord_polar() - stacked bar chart in polar coordinates
# EXTRA FEATURE 66: theme_void() - 
# Ref: https://www.geeksforgeeks.org/create-multiple-pie-charts-using-ggplot2-in-r/#:~:text=To%20plot%20multiple%20pie%20charts%20in%20R%20using,of%20these%20two%20variables%2C%20we%20use%20this%20method.
# Ref: https://statisticsglobe.com/theme_void-ggplot2-theme-r
# ~~~~~~
# plot a pie chart
ggplot(data=x, aes(x=" ", y=percentage, group=traveltime, colour=traveltime, fill=traveltime)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ address) + theme_void() +
  labs(title="Travel Time of Students based on Address") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of test scores based on address and travel time
sample_frac(data,1) %>% select(address,traveltime,G1,G2,G3) %>% group_by(address,traveltime) %>% 
  summarise(mean(G1),mean(G2),mean(G3))                           


# ----------
# Analysis 5-2: Analysis on parents' education level (urban = higher edu?)

# mean of parents' education level based on address
sample_frac(data,1) %>% select(address,Medu,Fedu) %>% group_by(address) %>% summarise(mean(Medu),mean(Fedu))

# mean of test scores based on parents' education level and address
x1 = sample_frac(data,1) %>% select(address,Medu,G1,G2,G3) %>% group_by(address,Medu) %>% 
  summarise(Medu_G1=mean(G1),Medu_G2=mean(G2),Medu_G3=mean(G3))
x2 = sample_frac(data,1) %>% select(address,Fedu,G1,G2,G3) %>% group_by(address,Fedu) %>% 
  summarise(mean(G1),mean(G2),mean(G3))
cbind(x1,Fedu=x2$Fedu,Fedu_G1=x2$`mean(G1)`,Fedu_G2=x2$`mean(G2)`,Fedu_G3=x2$`mean(G3)`)

medu_rural = data %>% filter(address=='R') %>% select(Medu,G3) %>% group_by(Medu) %>% summarise(G3=round(mean(G3),1))
fedu_rural = data %>% filter(address=='R') %>% select(Fedu,G3) %>% group_by(Fedu) %>% summarise(G3=round(mean(G3),1))

# line graph for education level against G3 score (Rural areas)
plot(medu_rural,type="o",xlab="Parents' Education Level",ylab="Mean (G3)",
     main="G3 Score based on Parents' Education Level\n(Rural Areas)",
     cex.main=2,cex.lab=1.3,font.lab=2,family="serif",col.main="sienna2",bty="L",col="red") 
text(medu_rural,labels=medu_rural$G3,cex=0.8,po=3,col="red")
lines(fedu_rural,type="o",col="blue")
text(fedu_rural,labels=fedu_rural$G3,cex=0.8,po=3,col="blue")
legend("right",c("Mother","Father"),fill=c("red","blue"),cex=1)

medu_urban = data %>% filter(address=='U') %>% select(Medu,G3) %>% group_by(Medu) %>% summarise(G3=round(mean(G3),1))
fedu_urban = data %>% filter(address=='U') %>% select(Fedu,G3) %>% group_by(Fedu) %>% summarise(G3=round(mean(G3),1))

# line graph for education level against G3 score (Urban areas)
plot(medu_urban,type="o",xlab="Parents' Education Level",ylab="Mean (G3)",
     main="G3 Score based on Parents' Education Level\n(Urban Areas)",
     cex.main=2,cex.lab=1.3,font.lab=2,family="serif",col.main="sienna2",bty="L",col="red") 
text(medu_urban,labels=medu_urban$G3,cex=0.8,po=3,col="red")
lines(fedu_urban,type="o",col="blue")
text(fedu_urban,labels=fedu_urban$G3,cex=0.8,po=3,col="blue")
legend("topright",c("Mother","Father"),fill=c("red","blue"),cex=1)


# ----------
# Analysis 5-3: Analysis on parents' job

# percentage of mother's job based on address
m = data %>% select(address,Mjob) %>% group_by(address,Mjob) %>% summarise(count=n())
m1 = filter(m,address=='R') %>% mutate(percentage=round(count/sum(count)*100,2))
m2 = filter(m,address=='U') %>% mutate(percentage=round(count/sum(count)*100,2))
m = rbind(m1,m2)

# percentage of father's job based on address
f = data %>% select(address,Fjob) %>% group_by(address,Fjob) %>% summarise(count=n())
f1 = filter(f,address=='R') %>% mutate(percentage=round(count/sum(count)*100,2))
f2 = filter(f,address=='U') %>% mutate(percentage=round(count/sum(count)*100,2))
f = rbind(f1,f2)

# plot a bar chart for Mother's Job against Address
plot1 = ggplot(data=m,aes(x=Mjob,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=address),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Mother's Job based on Address") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# plot a bar chart for Father's Job against Address
plot2 = ggplot(data=f,aes(x=Fjob,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=address),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Father's Job based on Address") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

grid.arrange(plot1,plot2,ncol=2)

# mean of test scores based on parents' job and address
x1 = sample_frac(data,1) %>% select(address,Mjob,G1,G2,G3) %>% group_by(address,Mjob) %>% 
  summarise(Mjob_G1=mean(G1),Mjob_G2=mean(G2),Mjob_G3=mean(G3))
x2 = sample_frac(data,1) %>% select(address,Fjob,G1,G2,G3) %>% group_by(address,Fjob) %>% 
  summarise(mean(G1),mean(G2),mean(G3))
cbind(x1,Fjob=x2$Fjob,Fjob_G1=x2$`mean(G1)`,Fjob_G2=x2$`mean(G2)`,Fjob_G3=x2$`mean(G3)`)

mjob_rural = data %>% filter(address=='R') %>% select(Mjob,G3) %>% group_by(Mjob) %>% 
  summarise(mother=round(mean(G3),1)) %>% rename(job=Mjob)
fjob_rural = data %>% filter(address=='R') %>% select(Fjob,G3) %>% group_by(Fjob) %>% 
  summarise(father=round(mean(G3),1))
job_rural = cbind(mjob_rural,father=fjob_rural$father)

# line graph for job against G3 score (Rural areas)
ggplot(job_rural, aes(x=factor(job),group=1)) +
  geom_line(aes(y=mother, color='mother')) +
  geom_line(aes(y=father, color='father')) +
  scale_color_manual(values=c('red', 'steelblue')) +
  labs(title="G3 Score based on Parents' Job",
       subtitle="Rural Areas") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

mjob_urban = data %>% filter(address=='U') %>% select(Mjob,G3) %>% group_by(Mjob) %>% 
  summarise(mother=round(mean(G3),1)) %>% rename(job=Mjob)
fjob_urban = data %>% filter(address=='U') %>% select(Fjob,G3) %>% group_by(Fjob) %>% 
  summarise(father=round(mean(G3),1))
job_urban = cbind(mjob_urban,father=fjob_urban$father)

# line graph for job against G3 score (Urban areas)
ggplot(job_urban, aes(x=factor(job),group=1)) +
  geom_line(aes(y=mother, color='mother')) +
  geom_line(aes(y=father, color='father')) +
  scale_color_manual(values=c('red', 'steelblue')) +
  labs(title="G3 Score based on Parents' Job",
       subtitle="Urban Areas") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

# get combo of parents' job with highest score
arrange(sample_frac(data,1) %>% select(address,Mjob,Fjob,G1,G2,G3) %>% group_by(address,Mjob,Fjob) %>% 
  summarise(mean(G1),mean(G2),mean(G3)), desc(`mean(G3)`))


# ----------
# Analysis 5-4: Analysis on internet access (rural = no internet access = low perf?)

# relationship between address and internet access
x = sample_frac(data,1) %>% select(address,internet) %>% group_by(address,internet) %>% summarise(count=n())
x1 = filter(x,address=='R') %>% mutate(percentage=round(count/sum(count)*100,2))
x2 = filter(x,address=='U') %>% mutate(percentage=round(count/sum(count)*100,2))
x = rbind(x1,x2)

ggplot(data=x, aes(x=" ", y=percentage, group=internet, colour=internet, fill=internet)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ address) + theme_void() +
  labs(title="Internet Access of Students based on Address") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of test scores based on address & internet access
x = sample_frac(data,1) %>% select(address,internet,G1,G2,G3) %>% group_by(address,internet) %>% 
  summarise(mean(G1),mean(G2),mean(G3))

ggplot(data=x, aes(x=internet, y=`mean(G3)`, group=internet, colour=internet, fill=internet)) +
  geom_col() +
  facet_grid(.~ address) +
  labs(title="G3 Score based on Address\nand Internet Access") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) + 
  geom_text(aes(label=round(`mean(G3)`,2)),color="black")


# ----------
# Analysis 5-5: Analysis on absences (rural = more absences but != low perf)

rural = data %>% filter(address=='R')
urban = data %>% filter(address=='U')

# relationship between absences and address
plot1 = ggplot(rural, aes(x=absences)) + 
  geom_histogram(breaks=seq(0,25,by=1),colour="white",aes(fill=..count..)) +
  scale_fill_gradient("count",low="yellow",high="red") +
  labs(title="School Absences",subtitle="Rural Area") +
  geom_text(aes(label=..count..),stat="bin",vjust=0.3,breaks=seq(0,25,by=1))

plot2 = ggplot(urban, aes(x=absences)) + 
  geom_histogram(breaks=seq(0,25,by=1),colour="white",aes(fill=..count..)) +
  scale_fill_gradient("count",low="yellow",high="red") +
  labs(subtitle="Urban Area") +
  geom_text(aes(label=..count..),stat="bin",vjust=0.3,breaks=seq(0,25,by=1))

grid.arrange(plot1,plot2,nrow=2)

# absences of students in each category (in percentage)
# rural area
rural %>% select(absences) %>% group_by(absences) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/nrow(rural)*100))

# urban area
urban %>% select(absences) %>% group_by(absences) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/nrow(urban)*100))

# mean of absences and test scores based on address
data %>% select(address,absences,G1,G2,G3) %>% group_by(address) %>% 
  summarise(mean(absences),mean(G1),mean(G2),mean(G3))

# get number of absences with the highest G3 score for each address
arrange(data %>% select(absences,address,G1,G2,G3) %>% group_by(absences,address) %>% 
  summarise(mean(G1),mean(G2),mean(G3)), desc(`mean(G3)`))


# ----------
# Analysis 5-6: Analysis on additional educational support (schoolsup,famsup,paid) (rural = less support = low perf?)

rural_rcount = nrow(rural)
urban_rcount = nrow(urban)

# percentage of additional educational support for Rural areas
ss_rural = rural %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/rural_rcount*100)) %>% cbind(type=rep("schoolsup",2)) %>% rename(Support=schoolsup)
fs_rural = rural %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/rural_rcount*100)) %>% cbind(type=rep("famsup",2)) %>% rename(Support=famsup)
paid_rural = rural %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/rural_rcount*100)) %>% cbind(type=rep("paid",2)) %>% rename(Support=paid)
as_rural = rbind(fs_rural,paid_rural,ss_rural)

# percentage of additional educational support for Urban areas
ss_urban = urban %>% select(schoolsup) %>% group_by(schoolsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/urban_rcount*100)) %>% cbind(type=rep("schoolsup",2)) %>% rename(Support=schoolsup)
fs_urban = urban %>% select(famsup) %>% group_by(famsup) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/urban_rcount*100)) %>% cbind(type=rep("famsup",2)) %>% rename(Support=famsup)
paid_urban = urban %>% select(paid) %>% group_by(paid) %>% summarise(count=n()) %>% 
  mutate(percentage=round(count/urban_rcount*100)) %>% cbind(type=rep("paid",2)) %>% rename(Support=paid)
as_urban = rbind(fs_urban,paid_urban,ss_urban)

plot1 = ggplot(data=as_rural,aes(x=Support,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=type),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Additional Education Support of Students",
       subtitle = "Rural Areas") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"),
        plot.subtitle = element_text(hjust=0.5,face="bold"))

plot2 = ggplot(data=as_urban,aes(x=Support,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=type),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(subtitle = "Urban Areas") +
  theme(plot.subtitle = element_text(hjust=0.5,face="bold"))

grid.arrange(plot1,plot2,nrow=2)

# mean of test scores for each support based on address
x1 = data %>% select(address,schoolsup,G1,G2,G3) %>% group_by(address,schoolsup) %>% 
  summarise(school_G1=mean(G1),school_G2=mean(G2),school_G3=mean(G3))
x2 = data %>% select(address,famsup,G1,G2,G3) %>% group_by(address,famsup) %>% 
  summarise(mean(G1),mean(G2),mean(G3))
x3 = data %>% select(address,paid,G1,G2,G3) %>% group_by(address,paid) %>% 
  summarise(mean(G1),mean(G2),mean(G3))
cbind(x1,famsup=x2$famsup,fam_G1=x2$`mean(G1)`,fam_G2=x2$`mean(G2)`,fam_G3=x2$`mean(G3)`,
      paid=x3$paid,paid_G1=x3$`mean(G1)`,paid_G2=x3$`mean(G2)`,paid_G3=x3$`mean(G3)`)

# get combo of support with the highest G3 score for each address
arrange(data %>% select(address,schoolsup,famsup,paid,G1,G2,G3) %>% 
          group_by(address,schoolsup,famsup,paid) %>% 
          summarise(mean(G1),mean(G2),mean(G3)), desc(`mean(G3)`))




# ******************
# Q6: How family can affect the academic performance of the students?


# ----------
# Analysis 6-1: Analysis on parents' education level with parent's job (teacher = high edu?)

sample_data = data %>% filter(Mjob=='teacher' || Fjob=='teacher') 

# ~~~~~~
# EXTRA FEATURE 67: geom_jitter()
# Ref: https://ggplot2.tidyverse.org/reference/geom_jitter.html
# ~~~~~~
# plot a jitter geom to display the education level of parents as teacher 
ggplot(sample_data, aes(Medu, Fedu)) + 
  geom_jitter() +
  labs(title="Education Level of Teacher",
       x="Mother's Education Level",
       y="Father's Education Level") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# get the most count of parents' education level for parents' job as teacher
arrange(sample_data %>% select(Medu,Fedu) %>% group_by(Medu,Fedu) %>% summarise(count=n()), 
        desc(count))


sample_data2 = data %>% filter(Medu>2 || Fedu>2)

# plot a jitter geom to display the jobs of parents with high education level
ggplot(sample_data2, aes(Mjob, Fjob)) + 
  geom_jitter() +
  labs(title="Jobs of Parents with High Education Level",
       x="Mother's Job",
       y="Father's Job") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# get the most count of parents' job for parents with high education level
arrange(sample_data2 %>% select(Mjob, Fjob) %>% group_by(Mjob, Fjob) %>% summarise(count=n()), 
        desc(count))


# ----------
# Analysis 6-2: Analysis on parent's cohabitation status with family relationship

# get the count of family relationship from 1 to 5 based on each Pstatus
x=data %>% select(Pstatus,famrel) %>% group_by(Pstatus,famrel) %>% summarise(count=n())
x1 = filter(x,Pstatus=='A') %>% mutate(percentage=round(count/sum(count)*100,2))
x2 = filter(x,Pstatus=='T') %>% mutate(percentage=round(count/sum(count)*100,2))
x = rbind(x1,x2)

ggplot(data=x, aes(x=" ", y=percentage, group=famrel, colour=famrel, fill=famrel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ Pstatus) +theme_void()+
  labs(title="Family Relationship based on\nParent's Cohabitation Status") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) 

# ~~~~~~
# EXTRA FEATURE 68: geom_voilin()
# EXTRA FEATURE 69: stat_summary(fun.y=mean) - add mean points into violin plot
# Ref: http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization#:~:text=The%20function%20geom_violin%20%28%29%20is,used%20to%20produce%20a%20violin%20plot.
# ~~~~~~
x = data %>% select(Pstatus,famrel,G1,G2,G3)

# plot a violin plot (G3 score for each category of Pstatus & family relationship)
ggplot(data=x, aes(x=famrel, y=G3, group=famrel, colour=famrel, fill=famrel)) +
  geom_violin() +
  facet_grid(.~ Pstatus) +
  labs(title="G3 Score based on Family Relationship\nand Parent's Cohabitation Status") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) + 
  stat_summary(fun.y=mean,geom="point",size=2,color="red")

# mean of famrel for each category of Pstatus 
data %>% select(Pstatus,famrel) %>% group_by(Pstatus) %>% summarise(mean(famrel))

# get the combo of Pstatus and famrel with highest score
arrange(x %>% group_by(Pstatus,famrel) %>% summarise(mean(G3)), desc(`mean(G3)`))


# ----------
# Analysis 6-3: Analysis on family relationship with family size

# get the count of family relationship from 1 to 5 based on each family size
x=data %>% select(famsize,famrel) %>% group_by(famsize,famrel) %>% summarise(count=n())
x1 = filter(x,famsize=='GT3') %>% mutate(percentage=round(count/sum(count)*100,2))
x2 = filter(x,famsize=='LE3') %>% mutate(percentage=round(count/sum(count)*100,2))
x = rbind(x1,x2)

ggplot(data=x, aes(x=" ", y=percentage, group=famrel, colour=famrel, fill=famrel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ famsize) +theme_void() +
  labs(title="Family Relationship based on\nFamily Size") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# ~~~~~~
# EXTRA FEATURE 70: scale_fill_brewer()
# EXTRA FEATURE 71: theme_minimal() - A minimalistic theme with no background annotations
# ~~~~~~
x = data %>% select(famsize,famrel,G1,G2,G3)

# plot a violin plot (G3 score for each category of famsize & family relationship)
ggplot(data=x, aes(x=famrel,y=G3,group=famrel,fill=factor(famrel))) +
  geom_violin() +
  facet_grid(.~ famsize) +
  labs(title="G3 Score based on Family Relationship\nand Family Size") + 
  stat_summary(fun.y=mean,geom="point",size=2,color="blue") +
  scale_fill_brewer(palette="Dark2") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of famrel & tests for each category of famsize 
data %>% select(famsize,famrel,G1,G2,G3) %>% group_by(famsize) %>% 
  summarise(mean(famrel),mean(G1),mean(G2),mean(G3))


# ----------
# Analysis 6-4: Analysis on family relationship with study time

# find the relationship between family relationship and study time
x=data %>% select(studytime,famrel) %>% group_by(famrel,studytime) %>% summarise(count=n())

i=1
z=data.frame()
for(i in 1:nlevels(factor(x$famrel))){
  y = filter(x,famrel==i) %>% mutate(percentage=round(count/sum(count)*100,2))
  z = rbind(z,y)
}

ggplot(data=z, aes(x=" ", y=percentage, group=studytime, colour=studytime, fill=studytime)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ famrel) + theme_void() +
  labs(title="Study Time based on\nFamily Relationship") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))


x = data %>% select(studytime,famrel,G1,G2,G3)

# plot a violin plot (G3 score for each category of studytime & family relationship)
ggplot(data=x, aes(x=studytime,y=G3,group=studytime,fill=factor(studytime))) +
  geom_violin() +
  facet_grid(.~ famrel) +
  labs(title="G3 Score based on Family Relationship\nand Study Time") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) + 
  stat_summary(fun.y=mean,geom="point",size=2,color="blue") +
  scale_fill_brewer(palette="RdBu")

# mean of studytime & tests for each category of famrel 
data %>% select(studytime,famrel,G1,G2,G3) %>% group_by(famrel) %>% 
  summarise(mean(studytime),mean(G1),mean(G2),mean(G3))

# mean of famrel & tests for each category of studytime
data %>% select(studytime,famrel,G1,G2,G3) %>% group_by(studytime) %>% 
  summarise(mean(famrel),mean(G1),mean(G2),mean(G3))

# get the combo of studytime and famrel that has highest G3 score
arrange(x %>% select(studytime,famrel,G3) %>% group_by(studytime,famrel) %>% summarise(mean(G3)), 
        desc(`mean(G3)`))


# ----------
# Analysis 6-5: Analysis on family relationship with family educational support

# get the count of famrel based on family educational support
x=data %>% select(famsup,famrel) %>% group_by(famsup,famrel) %>% summarise(count=n())
x1 = filter(x,famsup=='no') %>% mutate(percentage=round(count/sum(count)*100,2))
x2 = filter(x,famsup=='yes') %>% mutate(percentage=round(count/sum(count)*100,2))
x = rbind(x1,x2)

ggplot(data=x, aes(x=" ", y=percentage, group=famrel, colour=famrel, fill=famrel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ famsup) +theme_void() +
  labs(title="Family Relationship based on\nFamily Educational Support") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))


x = data %>% select(famsup,famrel,G1,G2,G3)

# plot a violin plot (G3 score for each category of famsup & family relationship)
ggplot(data=x, aes(x=famrel,y=G3,group=famrel,fill=factor(famrel))) +
  geom_violin() +
  facet_grid(.~ famsup) +
  labs(title="G3 Score based on Family Relationship\nand Family Educational Support") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) + 
  stat_summary(fun.y=mean,geom="point",size=2,color="orange")

# mean of famrel & tests for each category of famsup 
data %>% select(famsup,famrel,G1,G2,G3) %>% group_by(famsup) %>% 
  summarise(mean(famrel),mean(G1),mean(G2),mean(G3))

# get the combo of famsup and famrel that has highest G3 score
arrange(x %>% select(famsup,famrel,G3) %>% group_by(famsup,famrel) %>% summarise(mean(G3)), 
        desc(`mean(G3)`))


# ----------
# Analysis 6-6: Analysis on family relationship with go out

# find the relationship between family relationship and go out
x=data %>% select(goout,famrel) %>% group_by(famrel,goout) %>% summarise(count=n())

i=1
z=data.frame()
for(i in 1:nlevels(factor(x$famrel))){
  y = filter(x,famrel==i) %>% mutate(percentage=round(count/sum(count)*100,2))
  z = rbind(z,y)
}

ggplot(data=z, aes(x=" ", y=percentage, group=goout, colour=goout, fill=goout)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ famrel) +theme_void() +
  labs(title="Go Out Frequency based on\nFamily Relationship") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))


x = data %>% select(goout,famrel,G1,G2,G3)

# plot a violin plot (G3 score for each category of goout & family relationship)
ggplot(data=x, aes(x=goout,y=G3,group=goout,fill=factor(goout))) +
  geom_violin() +
  facet_grid(.~ famrel) +
  labs(title="G3 Score based on Family Relationship\nand Go Out Frequency") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) + 
  stat_summary(fun.y=mean,geom="point",size=2,color="purple")

# mean of goout for each category of famrel 
data %>% select(goout,famrel) %>% group_by(famrel) %>% summarise(mean(goout))

# mean of famrel for each category of goout 
data %>% select(goout,famrel) %>% group_by(goout) %>% summarise(mean(famrel))

# get the combo of goout and famrel that has highest G3 score
arrange(x %>% select(goout,famrel,G3) %>% group_by(goout,famrel) %>% summarise(mean(G3)), 
        desc(`mean(G3)`))




# ******************
# Q7: What causes the students from MS to perform better than students from GP in G3?


# ----------
# Analysis 7-1: Analysis on sex with school (MS = more male students?)

# get the count of students based on sex and school
x = data %>% select(sex,school) %>% group_by(school,sex) %>% summarise(count=n())
x1 = filter(x,school=='GP') %>% mutate(percentage=round(count/sum(count)*100,2))
x2 = filter(x,school=='MS') %>% mutate(percentage=round(count/sum(count)*100,2))
x = rbind(x1,x2)

ggplot(data=x,aes(x=sex,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on Sex and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 group by sex and school
data %>% select(sex,school,G3) %>% group_by(sex,school) %>% summarise(mean(G3))

# ~~~~~~
# EXTRA FEATURE 72: geom_dotplot()
# Ref: https://ggplot2.tidyverse.org/reference/geom_dotplot.html
# ~~~~~~
# dotplot 
ggplot(data, aes(x=sex, y=G3, fill=school)) +
  geom_dotplot(binaxis="y", stackdir="center", dotsize=0.2) + 
  labs(title="G3 Score based on Sex and School") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))


# ----------
# Analysis 7-2: Analysis on age with school (MS = more 18, less 19?)

# get the count of students based on age and school
x = data %>% select(age,school) %>% group_by(school,age) %>% summarise(count=n())
x1 = filter(x,school=='GP') %>% mutate(percentage=round(count/sum(count)*100,2))
x2 = filter(x,school=='MS') %>% mutate(percentage=round(count/sum(count)*100,2))
x = rbind(x1,x2)

ggplot(data=x,aes(x=age,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on Age and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))


# mean of G3 group by age and school
data %>% select(age,school,G3) %>% group_by(school,age) %>% summarise(mean(G3))

# ~~~~~~
# EXTRA FEATURE 73: Display mean line in facetted plot
# Ref: https://www.tutorialspoint.com/how-to-display-mean-line-per-group-in-facetted-graph-using-ggplot2-in-r
# ~~~~~~
ggplot(data,aes(x=age,y=G3,colour=school)) +
  geom_point() +
  facet_grid(~school) +
  geom_line(aes(y=mean(G3))) +
  labs(title = "G3 Score based on Age and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))


# ----------
# Analysis 7-3: Analysis on address with school (MS = more Urban?)

# count of students group by address and school
x1 = data %>% select(address,school,G3) %>% filter(school=='GP') %>% group_by(address) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sum(count)*100,2))  %>% 
  cbind(school=rep("GP",2))
x2 = data %>% select(address,school,G3) %>% filter(school=='MS') %>% group_by(address) %>% 
  summarise(count=n()) %>% mutate(percentage=round(count/sum(count)*100,2))  %>% 
  cbind(school=rep("MS",2))
x = rbind(x1,x2)

ggplot(data=x,aes(x=address,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on Address and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 group by address and school
data %>% select(address,school,G3) %>% group_by(school,address) %>% summarise(mean(G3))

# ~~~~~~
# EXTRA FEATURE 74: geom_density()
# ~~~~~~
ggplot(data, aes(G3, colour=address)) +
  geom_density() +
  facet_grid(~school) + 
  labs(title="G3 Score based on Address and School") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))


# ----------
# Analysis 7-4: Analysis on reason with school (MS = more choose course)

# percentage of students based on school and reason
x = sample_frac(data,1) %>% select(school,reason) %>% group_by(school,reason) %>% 
  summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

# plot the relationship between school and reason into a pie chart
ggplot(data=x, aes(x=" ", y=percentage, group=reason, colour=reason, fill=reason)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  facet_grid(.~ school) +theme_void() + 
  labs(title="G3 Score based on Reason and School") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))


# calculate the mean of G3 based on school and reason
y = sample_frac(data,1) %>% select(school,reason,G3) %>% group_by(school,reason) %>% summarise(mean(G3)) 
z = cbind(x,`mean(G3)`=y$`mean(G3)`)
z


# ----------
# Analysis 7-5: Analysis on travel time with school (MS = short traveltime?)

# percentage of students based on school and traveltime
x = sample_frac(data,1) %>% select(school,traveltime) %>% group_by(school,traveltime) %>% 
  summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

ggplot(data=x,aes(x=traveltime,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on Travel Time and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# ~~~~~~
# EXTRA FEATURE 75: geom_boxplot()
# EXTRA FEATURE 76: stacked boxplot
# Ref: https://r-graph-gallery.com/265-grouped-boxplot-with-ggplot2.html
# ~~~~~~
ggplot(data, aes(x=factor(traveltime),y=G3,fill=school)) + 
  geom_boxplot() + 
  labs(title="G3 Score based on Travel Time and School") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 score based on traveltime and school
sample_frac(data,1) %>% select(school,traveltime,G3) %>% group_by(school,traveltime) %>% summarise(mean(G3)) 


# ----------
# Analysis 7-6: Analysis on studytime with school (MS = long studytime?)

# percentage of students based on school and studytime
x = sample_frac(data,1) %>% select(school,studytime) %>% group_by(school,studytime) %>% 
  summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

p=barplot(height=x$percentage,names=x$studytime,main="Student Count based on Study Time and School",
          legend.text=factor(x$school),
          ylab="Percentage",xlab="Study Time",col=brewer.pal(length(x),"Set2"))
text(x=p,y=x$percentage-1,labels=paste0(x$percentage,"%"))

ggplot(data, aes(x=factor(studytime),y=G3,fill=school)) + 
  geom_boxplot() + 
  labs(title="G3 Score based on Study Time and School") +
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 score based on studytime and school
sample_frac(data,1) %>% select(school,studytime,G3) %>% group_by(school,studytime) %>% 
  summarise(mean(G3)) 


# ----------
# Analysis 7-7: Analysis on failures with school (MS = less failures?)

# mean of failures for each school
aggregate(select(data,c(failures)),by=list(data$school),FUN=mean)

# percentage of students based on school and failures
x = sample_frac(data,1) %>% select(school,failures) %>% group_by(school,failures) %>% 
  summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

ggplot(data=x,aes(x=failures,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on Failures and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 score based on failures and school
sample_frac(data,1) %>% select(school,failures,G3) %>% group_by(school,failures) %>% 
  summarise(mean(G3)) 


# ----------
# Analysis 7-8: Analysis on schoolsup with school (MS = more support?)

# get the count of students based on schoolsup and school
x=data %>% select(school,schoolsup) %>% group_by(school,schoolsup) %>% summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

ggplot(data=x,aes(x=schoolsup,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on School Support and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 score based on schoolsup and school
sample_frac(data,1) %>% select(school,schoolsup,G3) %>% group_by(school,schoolsup) %>% 
  summarise(mean(G3)) 


# ----------
# Analysis 7-9: Analysis on higher with school (MS = more higher?)

# get the count of students based on higher and school
x=data %>% select(school,higher) %>% group_by(school,higher) %>% summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

ggplot(data=x,aes(x=higher,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on Higher Education and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 score based on higher and school
sample_frac(data,1) %>% select(school,higher,G3) %>% group_by(school,higher) %>% 
  summarise(mean(G3)) 


# ----------
# Analysis 7-10: Analysis on alcohol consumption with school (MS = less consumption?)

# get the count of students based on Dalc and school
x=data %>% select(school,Dalc) %>% group_by(school,Dalc) %>% summarise(count=n()) %>% 
  rename(`Consumption Time`=Dalc) %>% cbind(type=rep("Dalc",10))
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

# get the count of students based on Walc and school
y=data %>% select(school,Walc) %>% group_by(school,Walc) %>% summarise(count=n()) %>% 
  rename(`Consumption Time`=Walc) %>% cbind(type=rep("Walc",10))
y1 = filter(y, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
y2 = filter(y, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
y = rbind(y1,y2)

ggplot(data=rbind(x,y),aes(x=`Consumption Time`,y=percentage,colour=school)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~type) +
  labs(title = "Students based on Alcohol Consumption and School",
       y="Student Percentage") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of alcohol consumption based on school
data %>% select(school,Dalc,Walc) %>% group_by(school) %>% summarise(mean(Dalc),mean(Walc))


# mean of G3 score based on alcohol consumption and school
x = data %>% select(school,Dalc,G3) %>% group_by(school,Dalc) %>% summarise(mean(G3)) %>% 
  rename(`Consumption Time`=Dalc) %>% cbind(type=rep("Dalc",10))
y = data %>% select(school,Walc,G3) %>% group_by(school,Walc) %>% summarise(mean(G3)) %>% 
  rename(`Consumption Time`=Walc) %>% cbind(type=rep("Walc",10))

ggplot(data=rbind(x,y),aes(x=`Consumption Time`,y=`mean(G3)`,colour=school)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~type) +
  labs(title = "G3 Score based on Alcohol Consumption and School",
       y="Mean of G3 Score") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))


# ----------
# Analysis 7-11: Analysis on health status with school (MS = more healthy?)

# get the count of students based on health and school
x=data %>% select(school,health) %>% group_by(school,health) %>% summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

ggplot(data=x,aes(x=health,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  geom_text(aes(label=paste0(percentage,"%")),position=position_dodge2(width=0.9),colour="black") +
  labs(title = "Students based on Health Status and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of G3 score based on health and school
sample_frac(data,1) %>% select(school,health,G3) %>% group_by(school,health) %>% 
  summarise(mean(G3)) 


# ----------
# Analysis 7-12: Analysis on absences with school

# get the count of students based on absences and school
x=data %>% select(school,absences) %>% group_by(school,absences) %>% summarise(count=n()) 
x1 = filter(x, school=='GP') %>% mutate(percentage=round(count/sum(count)*100,1))
x2 = filter(x, school=='MS') %>% mutate(percentage=round(count/sum(count)*100,1))
x = rbind(x1,x2)

ggplot(data=x,aes(x=absences,y=percentage)) + 
  geom_bar(stat="identity",aes(fill=school),position="dodge") +
  labs(title = "Students based on Absences and School") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold"))

# mean of absences based on school
data %>% select(school,absences) %>% group_by(school) %>% summarise(mean(absences))

# get the combo with highest G3 mean score based on absences and school
arrange(sample_frac(data,1) %>% select(school,absences,G3) %>% group_by(school,absences) %>% 
  summarise(mean(G3)), desc(`mean(G3)`))




# ******************
# Q8: Which attribute has the greatest influence on the academic performance of students?


# ----------
# Analysis 8-1: Study time

# ~~~~~~
# EXTRA FEATURE 77: plots the best-fit line of a scatterplot
# EXTRA FEATURE 78: sm_statCorr() - prints statistical values, such as p- and R-values
# Ref: https://smin95.github.io/dataviz/basics-of-ggplot2-and-correlation-plot.html
# ~~~~~~
ggplot(data, aes(x=studytime, y=G3)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Study Time vs G3") +
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold")) + 
  sm_statCorr()


# ----------
# Analysis 8-2: Family Relationship

# convert G3 score to rank
spearman_sample = mutate(data,
                         G3_rank = case_when(
                           G3 >= 15  ~ 4,
                           G3 >10 ~ 3,
                           G3 >5 ~ 2,
                           G3 <=5 ~ 1))

# ~~~~~~
# EXTRA FEATURE 79: sm_corr_theme() - theme suitable for correlation plots
# EXTRA FEATURE 80: corr_method='spearman' - get results from Spearman's correlation test rather than Pearson's
# ~~~~~~
ggplot(spearman_sample, aes(x=famrel, y=G3_rank)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Family Relationship vs G3") + 
  sm_corr_theme() + 
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) +
  sm_statCorr(corr_method = 'spearman')


# ----------
# Analysis 8-3: Parents' Education Level

# correlation between Mother's Education Level and G3
ggplot(spearman_sample, aes(x=Medu, y=G3_rank)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Mother's Education Level vs G3") + 
  sm_corr_theme() + 
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold")) +
  sm_statCorr(corr_method = 'spearman')

# correlation between Father's Education Level and G3
ggplot(spearman_sample, aes(x=Fedu, y=G3_rank)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Father's Education Level vs G3") + 
  sm_corr_theme() + 
  theme(plot.title = element_text(hjust=0.5,color="blue",size=18,face="bold")) +
  sm_statCorr(corr_method = 'spearman')


# ----------
# Analysis 8-4: Travel Time

ggplot(data, aes(x=traveltime, y=G3)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Travel Time vs G3") + 
  sm_corr_theme() + 
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) +
  sm_statCorr()


# ----------
# Analysis 8-5: Weekend Alcohol Consumption

ggplot(spearman_sample, aes(x=Walc, y=G3_rank)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Weekend Alcohol Consumption vs G3") + 
  sm_corr_theme() + 
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) +
  sm_statCorr(corr_method = 'spearman')

# ~~~~~~
# EXTRA FEATURE 81: polychor() - function to get Spearman's correlation
# ~~~~~~
# using another function to get Spearman's correlation
polychor(spearman_sample$Walc, spearman_sample$G3_rank)


# ----------
# Analysis 8-6: Paid class

# convert paid with value 'yes' and 'no' to 1 and 0
sample = data %>% select(paid,G3)
sample$paid = ifelse(sample$paid=="yes",1,0)

# ~~~~~~
# EXTRA FEATURE 82: Point-Biserial Correlation - binary & continuous
# Ref: https://www.r-bloggers.com/2021/07/point-biserial-correlation-in-r-quick-guide/#:~:text=Basically%2C%20It%20is%20used%20to%20measure%20the%20relationship,the%20point-biserial%20correlation%20between%20two%20variables%20in%20R.https://stackoverflow.com/questions/53211392/switch-statement-with-multiple-conditions-in-r
# ~~~~~~
# Point-Biserial Correlation - binary & continuous
cor.test(sample$G3,sample$paid)


# ~~~~~~
# EXTRA FEATURE 83: convert numerical values to categorical values
# Ref: https://stackoverflow.com/questions/53211392/switch-statement-with-multiple-conditions-in-r
# ~~~~~~
# convert G3 score to categorical values
cramerV_sample = mutate(data,
                     category = case_when(
                       G3 >= 15  ~ "Distinction",
                       G3 >10 ~ "Merit",
                       G3 >5 ~ "Pass",
                       G3 <=5 ~ "Fail"))

# ~~~~~~
# EXTRA FEATURE 84: dcast() - convert a column values to column names
# Ref: https://www.tutorialspoint.com/how-to-convert-a-column-values-to-column-names-in-r
# ~~~~~~
# convert category values as column name
category_sample = cramerV_sample %>% select(paid,category) %>% group_by(paid,category) %>% 
  dcast(paid~category)
category_sample = data.matrix(category_sample)

# ~~~~~~
# EXTRA FEATURE 85: Fisher's Exact Test - significant association between two categorical variables
# EXTRA FEATURE 86: Cramer's V - categorical variables
# Ref: https://www.statology.org/correlation-between-categorical-variables/
# Ref: https://www.statology.org/fishers-exact-test/
# ~~~~~~
# calculate significant association
fisher.test(category_sample)

# calculate Cramer's V correlation
cramerV(category_sample)


# ----------
# Analysis 8-7: Higher Education

# convert higher with value 'yes' and 'no' to 1 and 0
sample = data %>% select(higher,G3)
sample$higher = ifelse(sample$higher=="yes",1,0)

cor.test(sample$G3,sample$higher)

# ~~~~~~
# EXTRA FEATURE 87: biserial.cor() - Point-Biserial Correlation
# ~~~~~~
biserial.cor(sample$G3,sample$higher,level=2)


# convert category values as column name
category_sample = cramerV_sample %>% select(higher,category) %>% group_by(higher,category) %>% 
  dcast(higher~category)
category_sample = data.matrix(category_sample)

# calculate Cramer's V
fisher.test(category_sample)
cramerV(category_sample)


# ----------
# Analysis 8-8: Go Out Frequency

ggplot(spearman_sample, aes(x=goout, y=G3_rank)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Go Out Frequency vs G3") + 
  sm_corr_theme() + 
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) +
  sm_statCorr(corr_method = 'spearman')


# ----------
# Analysis 8-9: Past Class Failures

ggplot(data, aes(x=failures, y=G3)) +
  geom_point(shape=21, fill='#0f993d', color='white', size=3) +
  labs(title = "Past Class Failures vs G3") + 
  sm_corr_theme() + 
  theme(plot.title=element_text(hjust=0.5,color="blue",size=18,face="bold")) +
  sm_statCorr()


# ----------
# Analysis 8-10: Internet Access

# convert internet with value 'yes' and 'no' to 1 and 0
sample = data %>% select(internet,G3)
sample$internet = ifelse(sample$internet=="yes",1,0)

cor.test(sample$G3,sample$internet)


# convert category values as column name
category_sample = cramerV_sample %>% select(internet,category) %>% group_by(internet,category) %>% 
  dcast(internet~category)
category_sample = data.matrix(category_sample)

# calculate Cramer's V
fisher.test(category_sample)
cramerV(category_sample)


# ----------
# Analysis 8-11: Address

# convert address with value 'U' and 'R' to 1 and 0
sample = data %>% select(address,G3)
sample$address = ifelse(sample$address=="U",1,0)

cor.test(sample$G3,sample$address)


# convert category values as column name
category_sample = cramerV_sample %>% select(address,category) %>% group_by(address,category) %>% 
  dcast(address~category)
category_sample = data.matrix(category_sample)

# calculate Cramer's V
fisher.test(category_sample)
cramerV(category_sample)


# ----------
# Analysis 8-12: School

# convert school with value 'MS' and 'GP' to 1 and 0
sample = data %>% select(school,G3)
sample$school = ifelse(sample$school=="MS",1,0)

cor.test(sample$G3,sample$school)


# convert category values as column name
category_sample = cramerV_sample %>% select(school,category) %>% group_by(school,category) %>% 
  dcast(school~category)
category_sample = data.matrix(category_sample)

# calculate Cramer's V
fisher.test(category_sample)
cramerV(category_sample)
