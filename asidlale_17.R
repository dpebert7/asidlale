# Ebert
# 27 February 2017
# Asidlale report data

# Goal: Investigate results of Asidlale data over the last 4 years. 
# Is Asidlale doing well?

library(ggplot2)
library(reshape2)

asi = read.csv("~/Desktop/Documents/GitRepos/asidlale/asidlale_raw_anonymized.csv")
asi$score14 = asi$score14/14


# data overview 2016----

#table counting particpants
table(asi$asidlale, asi$school)

# mean of asidlale students is higher
mean(asi[asi$asidlale==1,"score14"])
mean(asi[asi$asidlale==0,"score14"])

# Refactor creche column
table(asi$creche)
sum(table(asi$creche))
levels(asi$creche) = list("Dutch" = "Dutch",
                          "kwaNomusa" = c("kwaNomusa", "Kwanomusa", "KwaNomusa"),
                          "Sbukosezwe" = "Sbukosezwe",
                          "Sunrise" = "Sunrise",
                          "Raphs" = c("Raphs", "St Raphaels"),
                          "Umkhuleko" = "Umhuleko",
                          "Home" = "Home",
                          "None" = c("None", "none"),
                          "iThemba" = "iThemba",
                          "other" = c("Athandweni", "Elimi", "Gauteng", "Hallelujah", "Hem-hem Creche", "KwaShelembe", "Mbutsane", "Mdunge", "Mpumuza", "Nakekelo", "Nathu Creche",
                                      "Ndabezinhle", "Sbongi", "Siyazama", "SiyaZama", "Thanduxolo", "Thinasonke",
                                      "Unakekelo", "uThando", "Baby Mama", "Ekuthuleni", "Gugu Creche", "Khulani", "Power",
                                      "Sbongisbusiso", "Selby", "Shayamoya", "Siboni esihle", "Siyakhula",
                                      "Thandolwethu", "Gugu Creche"))
table(asi$creche)
sum(table(asi$creche)) # Should be the same sum as before
table(asi$creche, asi$school, asi$asidlale)



colnames(asi)
asi$score_fine = apply(cbind(asi$cut, asi$stick, asi$playdough, asi$lacing, asi$peg_cards, asi$laminated_shape), 
                       MARGIN = 1, FUN=mean, na.rm=TRUE)

asi$score_gross = apply(cbind(asi$follow_instruct, asi$hop, asi$clapping, asi$dance_freeze, asi$beanbag), 
                        MARGIN = 1, FUN=mean, na.rm=TRUE)

asi$score_cognitive = apply(cbind(asi$self_pic, asi$matching, asi$draw_lines, asi$wooden_puzz,
                                  asi$six_puzz, asi$classification, asi$five_puzz, asi$read_book), 
                            MARGIN = 1, FUN=mean, na.rm=TRUE)

# Make matrix showing avg asidlale scores over 4 years by creche category ----
years = c("2013", "2014", "2015", "2016", "2017")
categories = c("year", "asidlale", "creche", "not_asidlale_creche", "not_asidlale", "not_creche")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2017){
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



# Compare Mabane and Nobanda results ----
categories = c("year", "Nobanda", "Mabane")

school_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(school_results) = c("2013", "2014", "2015", "2016", "2017")
colnames(school_results) = categories

for(i in 2013:2017){
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



# Does the drop go away when we only look at Nobanda? ----
years = c("2013", "2014", "2015", "2016", "2017")
categories = c("year", "asidlale", "creche", "not_asidlale_creche", "not_asidlale", "not_creche")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2017){
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



# Does the drop appear stronger at Mabane? ----
years = c("2013", "2014", "2015", "2016", "2017")
categories = c("year", "asidlale", "creche", "not_asidlale_creche", "not_asidlale", "not_creche")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2017){
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



# Results by Creche? ----

table(asi$year, asi$creche)

years = c("2013", "2014", "2015", "2016", "2017")
categories = c("year", "kwaNomusa", "Raphs", "Dutch", "Sbukosezwe", "other", "none")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2017){
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



# Make matrix showing avg scores for each task over 4 years by creche category ----

make_graph_asi_vs_not_asi = function(attribute){
  years = c("2013", "2014", "2015", "2016", "2017")
  categories = c("year", "asidlale", "not_asidlale_creche", "not_creche")
  
  sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
  rownames(sum_results) = years
  colnames(sum_results) = categories
  
  for(i in 2013:2017){
    sum_results[as.character(i),1]=i
    sum_results[as.character(i),2]=mean(asi[(asi$asidlale==1 & asi$year==i),attribute]) #average from all asidlale by year
    sum_results[as.character(i),3]=mean(asi[(asi$creche!="None" & 
                                               asi$asidlale==0 & 
                                               asi$creche != "Dutch" &
                                               asi$year==i),attribute]) #average from all NON-asi creches by year
    sum_results[as.character(i),4]=mean(asi[(asi$creche=="None" & asi$year==i),attribute]) #average from all NON-creches by year
  }
  
  print(attribute)
  print(sum_results)
  sum_results_long = melt(sum_results, id="year")
  
  # ggplot
  theme_set(theme_minimal(base_size = 18))
  ggplot(data = sum_results_long, 
         aes(x=year,y=value, colour=variable)) +
    geom_line(size=2) +
    ggtitle(paste("Results by Creche Type: ", attribute, sep = "")) +
    xlab("Year") + ylab("Average Score") +
    scale_y_continuous(limits = c(1, 3))
}


tasks = colnames(asi)[9:ncol(asi)]
for(i in 1:length(tasks)){
  print(make_graph_asi_vs_not_asi(tasks[i]))
}



# 2017 analysies ----

#####################################
#####   Things Steph can say:   #####
#####################################
#
# ASIDLALE RESULTS:
# 
# Genearlly, asidlale kids consistently do better in fine- and gross-motor skills than their non-Asidlale peers. 
#     By contrast, asidlale and non-asidlale kids do about equally well in cognitive tasks.
#
# 69% of asidlale kids have mastered most tests of gross motor skills.
#     By comparison, 35% of non-asidlale creche kids and 33% of non-creche kids are.
#
# 38% of asidlale kids are proficient in most tests of fine motor skills.
#     By comparison, 11% of non-asidlale creche kids and 10% of non-creche kids are.
#
# 75% of asidlale kids are proficient in most tests of cognitive skills.
#     By comparison, 58% of non-asidlale creche kids and 40% of non-creche kids are.
#
# 89% of asidlale kids are proficient in most tests of all types.
#     By comparison, 62% of non-asidlale creche kids and 48% of non-creche kids are.
#
#
#
# ECHS RESULTS:
# 
# ECHS results are tentative, since this year's sample only includes 11 kids, of whom 8 were also enrolled in
#     an asidlale creche. The other 3 were not enrolled in a chreche.
#
# Among kids who did NOT attend creche, the 3 kids in the ECHS programme averaged higher than the kids
#     who weren't in ECHS. (2.2 average out of 3  vs.  2.0 out of 3)
#
# Among kids who DID attend an asidlale creche, the 8 kids in the ECHS programme averaged nearly 
#     the same compared to those who weren't in ECHS. (2.33 average out of 3  vs.  2.37 out of 3)
# 
# The sample size is too small for statistical significance, but the 3 kids who are in ECHS but not creche
#     perform much better than their peers who don't attend creche, especially in cognitive tasks.
#
#
#


########################
# ASIDLALE COMPARISONS #
########################

# (Note that the Dutch creche is excluded)
sum(
  length(asi[asi$year==2017 & asi$asidlale == 1,]$score_gross),
  length(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche !="None" & asi$creche != "Dutch",]$score_gross),
  length(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche !="None" & asi$creche == "Dutch",]$score_gross),
  length(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche == "None",]$score_gross)
) # Should be 160



# GROSS MOTOR SKILLS
score_cutoff = 2.5
mean(asi[asi$year==2017 & asi$asidlale == 1,]$score_gross > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche !="None" & asi$creche != "Dutch",]$score_gross > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche == "None",]$score_gross > score_cutoff)

# 69% of asidlale kids have mastered most tests of gross motor skills.
# By comparison, 35% of non-asidlale creche kids and 33% of non-creche kids are.


# FINE MOTOR SKILLS
score_cutoff = 2.0  #<--- Note that this cutoff is lower! Fine motor skills are hard
mean(asi[asi$year==2017 & asi$asidlale == 1,]$score_fine > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche !="None" & asi$creche != "Dutch",]$score_fine > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche == "None",]$score_fine > score_cutoff)

# 38% of asidlale kids are proficient in most tests of fine motor skills.
# By comparison, 11% of non-asidlale creche kids and 10% of non-creche kids are.


# COGNITIVE SKILLS
score_cutoff = 2.0  #<--- Note that this cutoff is lower! Cognitive tasks are also harder
mean(asi[asi$year==2017 & asi$asidlale == 1,]$score_cognitive > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche !="None" & asi$creche != "Dutch",]$score_cognitive > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche == "None",]$score_cognitive > score_cutoff)

# 75% of asidlale kids are proficient in most tests of cognitive skills.
# By comparison, 58% of non-asidlale creche kids and 40% of non-creche kids are.


# ALL SKILLS COMBINED
score_cutoff = 2.0  #<--- Note that this cutoff is lower! Fine motor skills are hard
mean(asi[asi$year==2017 & asi$asidlale == 1,]$score14/14 > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche !="None" & asi$creche != "Dutch",]$score14/14 > score_cutoff)
mean(asi[asi$year==2017 & asi$asidlale == 0 & asi$creche == "None",]$score14/14 > score_cutoff)

# 89% of asidlale kids are proficient in most tests of all types.
# By comparison, 62% of non-asidlale creche kids and 48% of non-creche kids are.





########################
### ECHS COMPARISONS ###
########################

sum(
  length(asi[asi$year==2017 & asi$echs == 0,]$score_gross),
  length(asi[asi$year==2017 & asi$echs == 1 & asi$asidlale == 0,]$score_gross),
  length(asi[asi$year==2017 & asi$echs == 1 & asi$asidlale == 1,]$score_gross)
) # Should be 160


# GROSS MOTOR SKILLS
# No creche: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$creche == "None",]$score_gross)# >= score_cutoff)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$creche == "None",]$score_gross)# >= score_cutoff)

# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$asidlale == 1,]$score_gross)# >= score_cutoff)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$asidlale == 1,]$score_gross)# >= score_cutoff)


# FINE MOTOR SKILLS
# No creche: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$creche == "None",]$score_fine)# >= score_cutoff)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$creche == "None",]$score_fine)# >= score_cutoff)

# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$asidlale == 1,]$score_fine)# >= score_cutoff)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$asidlale == 1,]$score_fine)# >= score_cutoff)


# COGNITIVE SKILLS
# No creche: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$creche == "None",]$score_cognitive)# >= score_cutoff)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$creche == "None",]$score_cognitive)# >= score_cutoff)

# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$asidlale == 1,]$score_cognitive)# >= score_cutoff)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$asidlale == 1,]$score_cognitive)# >= score_cutoff)


# ALL SKILLS COMBINED
# No creche: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$creche == "None",]$score14/14)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$creche == "None",]$score14/14)

# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==2017 & asi$echs == 1 & asi$asidlale == 1,]$score14/14)
mean(asi[asi$year==2017 & asi$echs == 0 & asi$asidlale == 1,]$score14/14)



##################################
### Graphs of skills by creche ###
##################################


graph_task_by_creche = function(attribute){
  years = c("2013", "2014", "2015", "2016", "2017")
  categories = c("year", "kwaNomusa", "Sbukosezwe", "Raphs", "Umkhuleko", "Home", "iThemba") #, "other", "Dutch", "None")
  
  sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
  rownames(sum_results) = years
  colnames(sum_results) = categories
  
  for(i in 2013:2017){
    sum_results[as.character(i),1]=i
    sum_results[as.character(i),2]=mean(asi[(asi$year == i & asi$creche=="kwaNomusa"),attribute])
    sum_results[as.character(i),3]=mean(asi[(asi$year == i & asi$creche=="Sbukosezwe"),attribute])
    sum_results[as.character(i),4]=mean(asi[(asi$year == i & asi$creche=="Raphs"),attribute])
    sum_results[as.character(i),5]=mean(asi[(asi$year == i & asi$creche=="Umkhuleko"),attribute])
    sum_results[as.character(i),6]=mean(asi[(asi$year == i & asi$creche=="Home"),attribute])
    sum_results[as.character(i),7]=mean(asi[(asi$year == i & asi$creche=="iThemba"),attribute])
    #sum_results[as.character(i),8]=mean(asi[(asi$year == i & asi$creche=="other"),attribute])
    #sum_results[as.character(i),9]=mean(asi[(asi$year == i & asi$creche=="Dutch"),attribute])
    #sum_results[as.character(i),10]=mean(asi[(asi$year== i & asi$creche=="None"),attribute])
  }
  
  print(attribute)
  print(sum_results)
  sum_results_long = melt(sum_results, id="year")
  
  # ggplot
  theme_set(theme_minimal(base_size = 18))
  ggplot(data = sum_results_long, 
         aes(x=year,y=value, colour=variable)) +
    geom_line(size=2) +
    ggtitle(paste("Results by Creche Type: ", attribute, sep = "")) +
    xlab("Year") + ylab("Average Score") +
    scale_y_continuous(limits = c(1, 3))
}


tasks = colnames(asi)[9:ncol(asi)]
for(i in 1:length(tasks)){
  print(graph_task_by_creche(tasks[i]))
}


# Send Sam fine, cognitive, and gross by school


###########################
### MAKE SCHOOL PROFILE ###
###########################

make_school_profile = function(creche_name){
  #years = c("2013", "2014", "2015", "2016", "2017")
  this_year = 2017
  tasks = colnames(asi)[9:ncol(asi)]
  
  sum_results = as.data.frame((matrix(nrow=length(tasks), ncol=4)))
  colnames(sum_results) = c("school_average_last_year","school_average_this_year", "population_average", "difference_from_average")
  rownames(sum_results) = tasks

  for(i in 1:length(tasks)){
    sum_results[i,1] = round(mean(asi[(asi$year == this_year-1 & asi$creche == creche_name), tasks[i]]),2)
    sum_results[i,2] = round(mean(asi[(asi$year == this_year & asi$creche == creche_name), tasks[i]]),2)
    sum_results[i,3] = round(mean(asi[(asi$year == this_year), tasks[i]]),2)
    sum_results[i,4] = round(sum_results[i,2]-sum_results[i,3], 2)
  }
  
  sum_results = sum_results[!is.na(sum_results$population_average),] # Remove NA's
  sum_results[order(sum_results$difference_from_average),] # Sort by "difference_from_average"
  
  write.csv(sum_results, file = paste(creche_name, "_creche_results_2017.csv", sep = ""))
  
  return(sum_results)
}

schools = c("kwaNomusa", "Sbukosezwe", "Raphs", "Umkhuleko", "Home", "iThemba", "other", "Dutch", "None")
for(i in 1:length(schools)){
  print(" ")
  print(" ")
  print(" ")
  print(" ")
  print(schools[i])
  print(make_school_profile(schools[i]))
}
