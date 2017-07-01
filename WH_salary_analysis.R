library(data.table);library(ggplot2);library(plyr);library(magrittr);library(gender);library(pdftools)
source("/R/ggthemes.R")

#data source: https://www.documentcloud.org/documents/3883248-07012017-Report-Final.html#text/p3
#ripped text from PDF and cleaned up manually in bbedit
salary <- fread("white_house_salaries.txt") %>% data.frame()
names(salary) <- c("Name","Status","Salary","Title")
salary$Salary <- gsub("\\$|,","",salary$Salary) %>% as.numeric()
salary$name <- strsplit(salary$Name," ") %>% lapply(function(e) e[2]) %>% unlist()

#guess gender by first name
gender <- gender(salary$name)
gender <- ddply(gender,.(name),summarize,gender=gender[1])
salary <- merge(salary,gender,by="name",all.x=T)
salary$Gender <- factor(salary$gender)

#what's the overall gender balance? 
gender_count <- ddply(salary,.(Gender),summarize,n=length(name))
ggplot(data=gender_count,aes(x=Gender,y=n))+theme_minimal()+
  geom_bar(stat="identity",fill=NA,col="black")

#is income significantly different by gender across the staff? 
ttest <- t.test(subset(salary,Gender=="male")$Salary,subset(salary,Gender=="female")$Salary) #yup
ggplot(data=subset(salary,!is.na(Gender)),aes(x=Gender,y=Salary,fill=Gender))+theme_minimal()+
  geom_point(position=position_jitter(width=.11),col="grey")+
  geom_boxplot(alpha=0.5)

#next steps: correcting for seniority, age, and job duties





