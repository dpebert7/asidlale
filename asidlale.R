# Ebert
# 14 July 2016
# Asidlale report data

# Goal: Investigate results of Asidlale data over the last 4 years. 
# Is Asidlale doing well?

library(ggplot2)
library(reshape2)

asi = read.csv("asidlale_raw_anonymized.csv")


# data overview ----

#table counting particpants
table(asi$asidlale, asi$year)

# mean of asidlale students is higher
mean(asi[asi$asidlale==1,"score14"])
mean(asi[asi$asidlale==0,"score14"])

# Refactor creche column
table(asi$creche)
levels(asi$creche) = list("Dutch" = "Dutch",
                      "kwaNomusa" = c("kwaNomusa", "Kwanomusa", "KwaNomusa"),
                      "Raphs" = "Raphs",
                      "Sbukosezwe" = "Sbukosezwe",
                      "none" = "none",
                      "other" = c("Athandweni", "Elimi", "Gauteng", "Hallelujah", "Hem-hem Creche", 
                                  "iThemba", "KwaShelembe", "Mbutsane", "Mdunge", "Mpumuza", "Nakekelo", "Nathu Creche",
                                  "Ndabezinhle", "Sbongi", "Siyazama", "SiyaZama", "Thanduxolo", "Thinasonke",
                                  "Unakekelo", "uThando"))
table(asi$creche)
table(asi$creche, asi$year)



colnames(asi)
asi$score_fine = apply(cbind(asi$cut, asi$stick, asi$playdough, asi$lacing, asi$peg_cards, asi$laminated_shape), 
                       MARGIN = 1, FUN=mean, na.rm=TRUE)

asi$score_gross = apply(cbind(asi$follow_instruct, asi$hop, asi$clapping, asi$dance_freeze, asi$beanbag), 
                       MARGIN = 1, FUN=mean, na.rm=TRUE)

asi$score_cognitive = apply(cbind(asi$self_pic, asi$matching, asi$draw_lines, asi$wooden_puzz,
                                  asi$six_puzz, asi$classification, asi$five_puzz), 
                       MARGIN = 1, FUN=mean, na.rm=TRUE)

# Make matrix showing avg asidlale scores over 4 years by creche category ----
years = c("2013", "2014", "2015", "2016")
categories = c("year", "asidlale", "creche", "not_asidlale_creche", "not_asidlale", "not_creche")


sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2016){
  sum_results[as.character(i),1]=i
  sum_results[as.character(i),2]=mean(asi[(asi$asidlale==1 & asi$year==i),"score14"])/14 #average from all asidlale by year
  sum_results[as.character(i),3]=mean(asi[(asi$creche!="none" & asi$year==i),"score14"])/14 #average from all creches by year
  sum_results[as.character(i),4]=mean(asi[(asi$creche!="none" & asi$asidlale==0 & asi$year==i),"score14"])/14 #average from all NON-asi creches by year
  sum_results[as.character(i),5]=mean(asi[(asi$asidlale==0 & asi$year==i),"score14"])/14 #average from all NON-asidlale by year
  sum_results[as.character(i),6]=mean(asi[(asi$creche=="none" & asi$year==i),"score14"])/14 #average from all NON-creches by year
}

#sum_results
sum_results$creche = NULL #optional to get rid of two less interesting intermediate categories.
sum_results$not_asidlale = NULL

sum_results_long = melt(sum_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = sum_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Asidlale Results by Creche Type") +
  xlab("Year") + ylab("Average Score")
ggsave("images/results_by_creche_type.png")

# Compare Mabane and Nobanda results ----
categories = c("year", "Nobanda", "Mabane")

school_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(school_results) = c("2013", "2014", "2015", "2016")
colnames(school_results) = categories

for(i in 2013:2016){
  school_results[as.character(i),1]=i
  school_results[as.character(i),2]=mean(asi[(asi$school=="Nobanda" & asi$year==i),"score14"])/14 #average from all Nobanda by year
  school_results[as.character(i),3]=mean(asi[(asi$school=="Mabane" & asi$year==i),"score14"])/14 #average from all Mabane by year
}
school_results_long = melt(school_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = school_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Asidlale Results by School") +
  xlab("Year") + ylab("Average Score")
ggsave("images/results_by_school.png")


# Does the drop go away when we only look at Nobanda? ----
years = c("2013", "2014", "2015", "2016")
categories = c("year", "asidlale", "creche", "not_asidlale_creche", "not_asidlale", "not_creche")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2016){
  sum_results[as.character(i),1]=i
  sum_results[as.character(i),2]=mean(
    asi[(asi$asidlale==1 & asi$school=="Nobanda" & asi$year==i),"score14"])/14 #average from all asidlale by year
  sum_results[as.character(i),3]=mean(
    asi[(asi$creche!="none" & asi$school=="Nobanda" & asi$year==i),"score14"])/14 #average from all creches by year
  sum_results[as.character(i),4]=mean(
    asi[(asi$creche!="none" & asi$school=="Nobanda" & asi$asidlale==0 & asi$year==i),"score14"])/14 #average from all NON-asi creches by year
  sum_results[as.character(i),5]=mean(
    asi[(asi$asidlale==0 & asi$school=="Nobanda" & asi$year==i),"score14"])/14 #average from all NON-asidlale by year
  sum_results[as.character(i),6]=mean(
    asi[(asi$creche=="none" & asi$school=="Nobanda" & asi$year==i),"score14"])/14 #average from all NON-creches by year
}
sum_results_long = melt(sum_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = sum_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Asidlale Results for Nobanda by Creche Type") +
  xlab("Year") + ylab("Average Score")
ggsave("images/nobanda_by_creche_type.png")


# Does the drop appear stronger at Mabane? ----
years = c("2013", "2014", "2015", "2016")
categories = c("year", "asidlale", "creche", "not_asidlale_creche", "not_asidlale", "not_creche")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2016){
  sum_results[as.character(i),1]=i
  sum_results[as.character(i),2]=mean(
    asi[(asi$asidlale==1 & asi$school=="Mabane" & asi$year==i),"score14"])/14 #average from all asidlale by year
  sum_results[as.character(i),3]=mean(
    asi[(asi$creche!="none" & asi$school=="Mabane" & asi$year==i),"score14"])/14 #average from all creches by year
  sum_results[as.character(i),4]=mean(
    asi[(asi$creche!="none" & asi$school=="Mabane" & asi$asidlale==0 & asi$year==i),"score14"])/14 #average from all NON-asi creches by year
  sum_results[as.character(i),5]=mean(
    asi[(asi$asidlale==0 & asi$school=="Mabane" & asi$year==i),"score14"])/14 #average from all NON-asidlale by year
  sum_results[as.character(i),6]=mean(
    asi[(asi$creche=="none" & asi$school=="Mabane" & asi$year==i),"score14"])/14 #average from all NON-creches by year
}
sum_results_long = melt(sum_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = sum_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Asidlale Results for Mabane by Creche Type") +
  xlab("Year") + ylab("Average Score")
ggsave("images/mabane_by_creche_type.png")


# Results by Creche? ----

table(asi$year, asi$creche)

years = c("2013", "2014", "2015", "2016")
categories = c("year", "kwaNomusa", "Raphs", "Dutch", "Sbukosezwe", "other", "none")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2016){
  sum_results[as.character(i),1]=i
  sum_results[as.character(i),2]=mean(asi[(asi$creche=="kwaNomusa" & asi$year==i),"score14"])/14 #average from all kwaNomusa by year
  sum_results[as.character(i),3]=mean(asi[(asi$creche=="Raphs" & asi$year==i),"score14"])/14 #average from all Raphs by year
  sum_results[as.character(i),4]=mean(asi[(asi$creche=="Dutch" & asi$year==i),"score14"])/14 #average from all Dutch by year
  sum_results[as.character(i),5]=mean(asi[(asi$creche=="Sbukosezwe" & asi$year==i),"score14"])/14 #average from all Sbukosezwe by year
  sum_results[as.character(i),6]=mean(asi[(asi$creche=="other" & asi$year==i),"score14"])/14 #average from all other by year
  sum_results[as.character(i),7]=mean(asi[(asi$creche=="none" & asi$year==i),"score14"])/14 #average from all none by year
}
sum_results["2013","Dutch"]=NA #Remove this entry since there was only one student from Dutch that year
sum_results_long = melt(sum_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = sum_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Asidlale Results by Creche") +
  xlab("Year") + ylab("Average Score")
ggsave("images/results_by_creche.png")


# Make matrix showing avg scores for each task over 4 years by creche category ----

make_graph = function(attribute){
  years = c("2013", "2014", "2015", "2016")
  categories = c("year", "asidlale", "not_asidlale_creche", "not_creche")
  
  sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
  rownames(sum_results) = years
  colnames(sum_results) = categories
  
  for(i in 2013:2016){
    sum_results[as.character(i),1]=i
    sum_results[as.character(i),2]=mean(asi[(asi$asidlale==1 & asi$year==i),attribute]) #average from all asidlale by year
    sum_results[as.character(i),3]=mean(asi[(asi$creche!="none" & asi$asidlale==0 & asi$year==i),attribute]) #average from all NON-asi creches by year
    sum_results[as.character(i),4]=mean(asi[(asi$creche=="none" & asi$year==i),attribute]) #average from all NON-creches by year
  }
  
  sum_results_long = melt(sum_results, id="year")
  
  # ggplot
  theme_set(theme_minimal(base_size = 18))
  ggplot(data = sum_results_long, 
         aes(x=year,y=value, colour=variable)) +
    geom_line(size=2) +
    ggtitle(paste("Results by Creche Type: ", attribute, sep = "")) +
    xlab("Year") + ylab("Average Score")
  ggsave(paste("images/detailed_images/", attribute, ".png", sep = ""))
}


tasks = colnames(asi)[14:ncol(asi)]
for(i in 1:length(tasks)){
  make_graph(tasks[i])
}
