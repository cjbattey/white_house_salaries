library(data.table);library(ggplot2);library(plyr);library(magrittr);library(gender);library(pdftools);library(ggridges)
source("/R/ggthemes.R")

#data source: https://www.documentcloud.org/documents/3883248-07012017-Report-Final.html#text/p3
#ripped text from PDF and cleaned up manually in bbedit
trump <- fread("data/trump2017.txt") %>% data.frame()
names(trump) <- c("Name","Status","Salary","Title","President","Year")
trump$Salary <- gsub("\\$|,","",trump$Salary) %>% as.numeric()
trump <- trump[-2]
trump$firstname <- strsplit(as.character(trump$Name)," ") %>% lapply(function(e) e[2]) %>% unlist()

#obama data (WH provided csv - thanks obama. via https://obamawhitehouse.archives.gov/briefing-room/disclosures/annual-records/2015)
obama <- fread("data/obama2015.csv") %>% data.frame()
obama <- obama[-c(2,4)]
obama$President <- "Obama"
obama$year <- 2015
obama$firstname <- strsplit(as.character(obama$Name)," ") %>% lapply(function(e) e[2]) %>% unlist()
names(obama) <- names(trump)

#GW Bush 2004 (via Washington Post: http://www.washingtonpost.com/wp-srv/politics/administration/whbriefing/2004stafflistb.html)
bush <- read.delim("data/bush2004.txt",header = F)
bush$President <- "Bush"
bush$Year <- 2004
bush$V2 <- gsub("\\$|,","",bush$V2) %>% as.numeric()
bush$firstname <- strsplit(as.character(bush$V1)," ") %>% lapply(function(e) e[2]) %>% unlist()
names(bush) <- names(trump)

#Clinton 1993 (via Washington Post: https://www.washingtonpost.com/archive/politics/1993/11/01/salaries-at-clintons-white-house/9c96f5b6-02c5-4888-87ee-dc547d8d93f0/?utm_term=.aa70a4af1649)
clinton <- readLines("data/clinton1993.txt")
clinton <- clinton[clinton != ""]
clinton <- lapply(clinton,function(e){unlist(strsplit(e,","))})
for(i in 1:length(clinton)){ #shift salary one column right if no title was listed
  if(length(clinton[[i]])==3){
    clinton[[i]][3:4] <- clinton[[i]][2:3]
    clinton[[i]][2] <- "no title listed"
  }
}
clinton <- do.call(rbind.data.frame,clinton)
clinton$Salary <- apply(clinton[,3:4],1,function(e) paste0(e[1],e[2]))
clinton$Salary <- gsub("\\$","",clinton$Salary) %>% as.numeric()#2 salaries still NA... 
names(clinton) <- c("Name","Title","S1","S2","Salary")
clinton <- clinton[c("Name","Salary","Title")]
clinton$President <- "Clinton"
clinton$Year <- 1993
clinton$firstname <- strsplit(as.character(clinton$Name)," ") %>% lapply(function(e) e[1]) %>% unlist()

wh <- rbind(trump,obama,bush,clinton)
wh <- subset(wh,Salary>1000)
wh$President <- factor(wh$President,levels=levels(factor(wh$President))[c(2,1,3,4)])
#write.table(wh,"data/white_house_salaries.txt",sep="\t",row.names = F,col.names = T)

#guess gender by first name
gender <- gender(wh$firstname)
gender <- ddply(gender,.(name),summarize,gender=gender[1])
wh <- merge(wh,gender,by.x="firstname",by.y="name",all.x=T)
wh$Gender <- factor(wh$gender)

#what's the overall gender balance? 
gender_count <- ddply(wh,.(Gender,President),summarize,n=length(Name))
ggplot(data=gender_count,aes(x=Gender,y=n))+theme_minimal()+facet_grid(~President)+
  geom_bar(stat="identity",fill=NA,col="black")

#is income significantly different by gender across the staff? 

test_table <- ddply(wh,.(President),function(e){#nonparametric rank-based test (wilcox)
  wtest <- wilcox.test(subset(e,Gender=="male")$Salary,subset(e,Gender=="female")$Salary,conf.int = T)
  p <- wtest$p.value
  diff <- wtest$estimate
  CI_low <- wtest$conf.int[1]
  CI_high <- wtest$conf.int[2]
  c(p=round(p,4),diff=diff,CI_low,CI_high)
})
names(test_table) <- c("President","p","Median Difference","CI_0.05","CI_0.95")
test_table$Year <- c(1993,2004,2015,2017)
test_table$CI_low <- test_table$`Median Difference`-test_table$CI_0.05
test_table$CI_high <- test_table$`Median Difference`+test_table$CI_0.95

ddply(wh,.(President),function(e){ #t-test version for those who prefer means
  ttest <- t.test(subset(e,Gender=="male")$Salary,subset(e,Gender=="female")$Salary)
  p <- ttest$p.value
  diff <- ttest$estimate[1] - ttest$estimate[2]
  c(p=round(p,4),diff=diff)
})

violin_plot <- ggplot(data=subset(wh,!is.na(Gender)),aes(x=Gender,y=Salary,fill=Gender))+ #default ggplot colors eerily on-point here
  ylim(0,200000)+
  theme_minimal()+
  #theme(panel.grid.major.y=element_line(color="grey"))+
  facet_grid(~President)+
  geom_point(position=position_jitter(width=.25),size=0.5,col="grey")+
  geom_violin(alpha=0.5,draw_quantiles = .5)

line_plot <- ggplot(data=test_table,aes(x=Year,y=`Median Difference`))+
  theme_minimal()+
  scale_y_continuous(breaks = c(0,10000,20000,30000),limits=c(-1000,35000))+
  geom_line(stat="identity")+
  geom_point()+
  geom_errorbar(aes(ymin=CI_0.05,ymax=CI_0.95))

layout <- matrix(c(1,1,1,1,
                   1,1,1,1,
                   1,1,1,1,
                   1,1,1,1,
                   1,1,1,1,
                   2,2,2,2,
                   2,2,2,2),byrow=T,nrow=7)
grid.arrange(violin_plot,line_plot,layout_matrix=layout)

#TLDLATG all 4 presidents paid men significantly more than women *when not taking job duties into account*, 
#though Obama could make a solid case for parity depending on your choice of significance cutoff and preference 
#for parametric vs. rank-based statistical tests. The gap shrunk from Clinton to Obama, then reached a new 
#peak under Trump. The biggest difference in Trump's white house relative to Bush and Obama is the lack of 
#low-paid men, rather than a drop in high-paid women. 




