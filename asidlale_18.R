# Ebert
# 1 March 2018
# Updated 22 March 2018 to include ECHS
# Asidlale report data

# Goal: Investigate results of Asidlale data over the last 4 years. 
# Is Asidlale doing well?

thisyear = 2018
lastyear = 2017

library(ggplot2)
library(reshape2)

asi = read.csv("asidlale_raw_2018.csv") #File added to .gitignore. Use next line for anonymized file
#asi = read.csv("asidlale_raw_anonymized_2018.csv")
asi$mean14 = asi$score14/14

table(asi$year)

mean(asi$score14[asi$year==thisyear])
mean(asi$score14[asi$year==lastyear])
# mean is about the same in 2017 and 2018


# data overview 2018----
asithisyear = asi[asi$year == thisyear,]

#table counting particpants
table(asithisyear$asidlale, asithisyear$school)
table(asithisyear$asidlale, asithisyear$creche)
table(asithisyear$school, asithisyear$creche)

# mean of asidlale students is higher. Yay!
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
                          "none" = c("None", "none", "Home"),
                          "iThemba" = "iThemba",
                          "other" = c("Athandweni", "Elimi", "Gauteng", "Hallelujah", "Hem-hem Creche", "KwaShelembe", "Mbutsane", "Mdunge", "Mpumuza", "Nakekelo", "Nathu Creche",
                                      "Ndabezinhle", "Sbongi", "Siyazama", "SiyaZama", "Thanduxolo", "Thinasonke",
                                      "Unakekelo", "uThando", "Baby Mama", "Ekuthuleni", "Gugu Creche", "Khulani", "Power",
                                      "Sbongisbusiso", "Selby", "Shayamoya", "Siboni esihle", "Siyakhula",
                                      "Thandolwethu", "Gugu Creche", "Mbubu", "Bantu", "Honey Dew", "Snathung", "Elandskop", "Kwamdunge",
                                      "Masakhane", "Mashaka", "Masijabule", "Mooi River", "new", "Sbongisbusiso", "Shayamoya", "Uthando", "Siyancenga"))
asithisyear = asi[asi$year == thisyear,]
table(asi$creche)
sum(table(asi$creche)) # Should be the same sum as before
sum(table(asi$creche[asi$year==thisyear])) # Should be 122 in 2018
table(asi$creche, asi$school, asi$asidlale)


asithisyear = asi[asi$year == thisyear,]
table(asithisyear$creche)
sum(table(asithisyear$creche)) # Should be the same sum as before
sum(table(asithisyear$creche[asithisyear$year==thisyear])) # Should be 122 in 2018
table(asithisyear$creche, asithisyear$school, asithisyear$asidlale)

colnames(asi)
asi$score_fine = apply(cbind(asi$cut, asi$stick, asi$playdough, asi$lacing, asi$peg_cards, asi$laminated_shape), 
                       MARGIN = 1, FUN=mean, na.rm=TRUE)

asi$score_gross = apply(cbind(asi$follow_instruct, asi$hop, asi$clapping, asi$dance_freeze, asi$beanbag), 
                        MARGIN = 1, FUN=mean, na.rm=TRUE)

asi$score_cognitive = apply(cbind(asi$self_pic, asi$matching, asi$draw_lines, asi$wooden_puzz,
                                  asi$six_puzz, asi$classification, asi$five_puzz, asi$read_book), 
                            MARGIN = 1, FUN=mean, na.rm=TRUE)

# Make matrix showing avg asidlale scores over 5 years by creche category ----
years = c("2013", "2014", "2015", "2016", "2017", "2018")
categories = c("year", "asidlale", "creche", "not_asidlale_creche", "not_asidlale", "not_creche")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2018){
  sum_results[as.character(i),1]=i
  sum_results[as.character(i),2]=mean(asi[(asi$asidlale==1 & asi$year==i),"score14"], na.rm = TRUE)/14 #average from all asidlale by year
  sum_results[as.character(i),3]=mean(asi[(asi$creche!="none" & asi$year==i),"score14"], na.rm = TRUE)/14 #average from all creches by year
  sum_results[as.character(i),4]=mean(asi[(asi$creche!="none" & asi$asidlale==0 & asi$year==i),"score14"], na.rm = TRUE)/14 #average from all NON-asi creches by year
  sum_results[as.character(i),5]=mean(asi[(asi$asidlale==0 & asi$year==i),"score14"], na.rm = TRUE)/14 #average from all NON-asidlale by year
  sum_results[as.character(i),6]=mean(asi[(asi$creche=="none" & asi$year==i),"score14"], na.rm = TRUE)/14 #average from all NON-creches by year
}

sum_results
sum_results$creche = NULL #optional to get rid of two less interesting intermediate categories.
sum_results$not_asidlale = NULL

sum_results_long = melt(sum_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = sum_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Historical Asidlale Results by Creche Type") +
  xlab("Year") + ylab("Average Score")
ggsave("images/5_year_overview.png", width = 24, height = 16, units = "cm")



# Compare school results ----
categories = c("year", "Nobanda", "Mabane", "Mbubu", "Sweetwaters")
school_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(school_results) = c("2013", "2014", "2015", "2016", "2017", "2018")
colnames(school_results) = categories

for(i in 2013:2018){
  school_results[as.character(i),1]=i
  school_results[as.character(i),2]=mean(asi[(asi$school=="Nobanda" & asi$year==i),"score14"])/14 #average from all Nobanda by year
  school_results[as.character(i),3]=mean(asi[(asi$school=="Mabane" & asi$year==i),"score14"])/14 
  school_results[as.character(i),4]=mean(asi[(asi$school=="Mbubu" & asi$year==i),"score14"])/14
  school_results[as.character(i),5]=mean(asi[(asi$school=="Sweetwaters" & asi$year==i),"score14"])/14 
}
school_results_long = melt(school_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = school_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Asidlale Results by School") +
  xlab("Year") + ylab("Average Score")

ggsave("images/5_year_overview_by_school.png", width = 24, height = 16, units = "cm")



# Results by Creche ----

table(asi$year, asi$creche)

years = c("2013", "2014", "2015", "2016", "2017", "2018")
categories = c("year", "kwaNomusa", "Raphs", "Dutch", "Sbukosezwe", "iThemba", "other", "none")

sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
rownames(sum_results) = years
colnames(sum_results) = categories

for(i in 2013:2018){
  sum_results[as.character(i),1]=i
  sum_results[as.character(i),2]=mean(asi[(asi$creche=="kwaNomusa" & asi$year==i),"score14"])/14 #average from all kwaNomusa by year
  sum_results[as.character(i),3]=mean(asi[(asi$creche=="Raphs" & asi$year==i),"score14"])/14 #average from all Raphs by year
  sum_results[as.character(i),4]=mean(asi[(asi$creche=="Dutch" & asi$year==i),"score14"])/14 #average from all Dutch by year
  sum_results[as.character(i),5]=mean(asi[(asi$creche=="Sbukosezwe" & asi$year==i),"score14"])/14 #average from all Sbukosezwe by year
  sum_results[as.character(i),6]=mean(asi[(asi$creche=="iThemba" & asi$year==i),"score14"])/14 #average from all Sbukosezwe by year
  sum_results[as.character(i),7]=mean(asi[(asi$creche=="other" & asi$year==i),"score14"])/14 #average from all other by year
  sum_results[as.character(i),8]=mean(asi[(asi$creche=="none" & asi$year==i),"score14"])/14 #average from all none by year
}

table(asi$year, asi$creche) # Check student numbers to make sure it's reasonable
sum_results["2013","Dutch"]=NA #Remove this entry since there was only one student from Dutch that year
sum_results["2016","iThemba"]=NA #Remove this entry since there was only one student from Dutch that year
sum_results_long = melt(sum_results, id="year")

# ggplot
theme_set(theme_minimal(base_size = 18))
ggplot(data = sum_results_long, 
       aes(x=year,y=value, colour=variable)) +
  geom_line(size=2) +
  ggtitle("Asidlale Results by Creche") +
  xlab("Year") + ylab("Average Score")
ggsave("images/5_year_overview_by_creche.png", width = 24, height = 16, units = "cm")


########################################################
# Make matrix showing avg scores for each task over years by creche category ----
########################################################

######
# NOT RUN IN 2018:
######

make_graph_asi_vs_not_asi = function(attribute){
  years = c("2013", "2014", "2015", "2016", "2017", "2018")
  categories = c("year", "asidlale", "not_asidlale_creche", "not_creche")
  
  sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
  rownames(sum_results) = years
  colnames(sum_results) = categories
  
  for(i in 2013:2018){
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






# 2018 analyses ---


########################
# ASIDLALE COMPARISONS #
########################

lastyear = 2017
thisyear = 2018

sum(
  length(asi[asi$year==thisyear & asi$asidlale == 1,]$score_gross),
  length(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche !="None",]$score_gross),
  length(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche == "None",]$score_gross)
) # Should match number of students in year. 122 for 2018



# GROSS MOTOR SKILLS
score_cutoff = 2.5
mean(asi[asi$year==thisyear & asi$asidlale == 1,]$score_gross > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche !="none",]$score_gross > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche == "none",]$score_gross > score_cutoff)

# 70% of asidlale kids have mastered most tests of gross motor skills.
# By comparison, 52% of non-asidlale creche kids and 47% of non-creche kids are.


# FINE MOTOR SKILLS
score_cutoff = 2.0  #<--- Note that this cutoff is lower! Fine motor skills are hard
mean(asi[asi$year==thisyear & asi$asidlale == 1,]$score_fine > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche !="none" & asi$creche != "Dutch",]$score_fine > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche == "none",]$score_fine > score_cutoff)

# 28% of asidlale kids are proficient in most tests of fine motor skills.
# By comparison, 14% of non-asidlale creche kids and 9% of non-creche kids are.


# COGNITIVE SKILLS
score_cutoff = 2.0  #<--- Note that this cutoff is also lower! Cognitive tasks are also hard
mean(asi[asi$year==thisyear & asi$asidlale == 1,]$score_cognitive > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche !="none",]$score_cognitive > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche == "none",]$score_cognitive > score_cutoff)

# 77% of asidlale kids are proficient in most tests of cognitive skills.
# By comparison, 52% of non-asidlale creche kids and 44% of non-creche kids are.


# ALL SKILLS COMBINED
score_cutoff = 2.0
mean(asi[asi$year==thisyear & asi$asidlale == 1,]$score14/14 > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche !="none",]$score14/14 > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche == "none",]$score14/14 > score_cutoff)

# 80% of asidlale kids are proficient in most tests of all types.
# By comparison, 76% of non-asidlale creche kids and 47% of non-creche kids are.


score_cutoff = 2.5 # same skills, higher cutoff
mean(asi[asi$year==thisyear & asi$asidlale == 1,]$score14/14 > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche !="none",]$score14/14 > score_cutoff)
mean(asi[asi$year==thisyear & asi$asidlale == 0 & asi$creche == "none",]$score14/14 > score_cutoff)

# 28% of asidlale kids are highly proficient in most tests of all types.
# By comparison, 7% of non-asidlale creche kids and 9% of non-creche kids are.





#####################################
#####   Things Steph can say:   #####
#####################################
#
# ASIDLALE RESULTS:
# 
# Generally, asidlale kids consistently perform better in all types of skills than both
#   non-asidlale and non-creche kids.
#
# GROSS MOTOR SKILLS
# 70% of asidlale kids have mastered most tests of gross motor skills.
# By comparison, 52% of non-asidlale creche kids and 47% of non-creche kids are.

# FINE MOTOR SKILLS
# 28% of asidlale kids are proficient in most tests of fine motor skills.
# By comparison, 14% of non-asidlale creche kids and 9% of non-creche kids are.

# COGNITIVE SKILLS
# 77% of asidlale kids are proficient in most tests of cognitive skills.
# By comparison, 52% of non-asidlale creche kids and 44% of non-creche kids are.

# ALL SKILLS COMBINED
# 80% of asidlale kids are proficient in most tests of all types.
# By comparison, 76% of non-asidlale creche kids and 47% of non-creche kids are.

# 28% of asidlale kids are *highly* proficient in most tests of all types.
# By comparison, 7% of non-asidlale creche kids and 9% of non-creche kids are.







##################################
### Graphs of skills by creche ###
##################################

###
# NOT RUN IN 2018
###

graph_task_by_creche = function(attribute){
  years = c("2013", "2014", "2015", "2016", "2017", "2018")
  categories = c("year", "kwaNomusa", "Sbukosezwe", "Raphs", "Umkhuleko", "Home", "iThemba") #, "other", "Dutch", "None")
  
  sum_results = as.data.frame((matrix(nrow=length(years), ncol=length(categories))))
  rownames(sum_results) = years
  colnames(sum_results) = categories
  
  for(i in 2013:2018){
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
  #years = c("2013", "2014", "2015", "2016", "2017", "2018")
  this_year = 2018
  #tasks = colnames(asi)[14:ncol(asi)]
  tasks = c("cut", "follow_instruct", "self_pic", "matching", "draw_lines", "hop", "playdough",
            "clapping", "classification", "five_puzz", "dance_freeze", "laminated_shape",
            "beanbag", "read_book", "score_fine", "score_gross", "score_cognitive", "mean14")
  
  sum_results = as.data.frame((matrix(nrow=length(tasks), ncol=4)))
  colnames(sum_results) = c("Creche Avg Last Year","Creche Average This year", "Population Average", "Difference From Average")
  rownames(sum_results) = tasks
  
  decimal_places = 1 # should be 1 or 2
  for(i in 1:length(tasks)){
    sum_results[i,1] = round(mean(asi[(asi$year == this_year-1 & asi$creche == creche_name), tasks[i]], na.rm = TRUE),decimal_places)
    sum_results[i,2] = round(mean(asi[(asi$year == this_year & asi$creche == creche_name), tasks[i]], na.rm = TRUE),decimal_places)
    sum_results[i,3] = round(mean(asi[(asi$year == this_year), tasks[i]], na.rm = TRUE),decimal_places)
    sum_results[i,4] = round(sum_results[i,2]-sum_results[i,3], decimal_places)
  }
  
  #sum_results = sum_results[!is.na(sum_results$population_average),] # Remove NA's
  #sum_results[order(sum_results$difference_from_average),] # Sort by "difference_from_average"
  
  write.csv(sum_results, file = paste("creche_results/",creche_name, "_creche_results_2017.csv", sep = ""))
  #Someday it would be good to put these into separate sheets in ONE Excel file.
  
  return(sum_results)
}

schools = c("kwaNomusa", "Sbukosezwe", "Raphs", "Umkhuleko", "iThemba", "Sunrise", "other", "none")
for(i in 1:length(schools)){
  print(" ")
  print(" ")
  print(" ")
  print(" ")
  print(schools[i])
  print(make_school_profile(schools[i]))
}






########################
### ECHS COMPARISONS ###
########################

sum(
  length(asi[asi$year==thisyear & asi$echs == 0,]$score_gross),
  length(asi[asi$year==thisyear & asi$echs == 1 & asi$asidlale == 0,]$score_gross),
  length(asi[asi$year==thisyear & asi$echs == 1 & asi$asidlale == 1,]$score_gross)
) # Should be 122 in 2018


# GROSS MOTOR SKILLS
# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==thisyear & asi$echs == 1 & asi$asidlale == 1,]$score_gross)# >= score_cutoff)
mean(asi[asi$year==thisyear & asi$echs == 0 & asi$asidlale == 1,]$score_gross)# >= score_cutoff)


# FINE MOTOR SKILLS
# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==thisyear & asi$echs == 1 & asi$asidlale == 1,]$score_fine)# >= score_cutoff)
mean(asi[asi$year==thisyear & asi$echs == 0 & asi$asidlale == 1,]$score_fine)# >= score_cutoff)


# COGNITIVE SKILLS
# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==thisyear & asi$echs == 1 & asi$asidlale == 1,]$score_cognitive)# >= score_cutoff)
mean(asi[asi$year==thisyear & asi$echs == 0 & asi$asidlale == 1,]$score_cognitive)# >= score_cutoff)

# ALL SKILLS COMBINED
# Asidlale kids: ECHS vs NO ECHS
mean(asi[asi$year==thisyear & asi$echs == 1 & asi$asidlale == 1,]$score14/14)
mean(asi[asi$year==thisyear & asi$echs == 0 & asi$asidlale == 1,]$score14/14)


####################
### ECHS RESULTS ###
####################

table(asithisyear$echs, asithisyear$asidlale)

# ECHS results will only compare the 8 echs students to the 53 other students in asidlale.
#   THat is, we will ignore the 61 students who neither took part in ECHS nor asidlale
#
# Among kids who DID attend an asidlale creche, the 8 kids in the ECHS programme averaged just a
#     little better than their peers. (2.40 vs 2.35 average out of 3)
# 
# Among kids who DID attend an asidlale creche, the 8 kids in the ECHS programme performed a little
#     WORSE than their peers in gross motor skills (2.57 vs 2.68 average out of 3).
#     However, they performed a lot better in fine motor skills (2.17 vs 1.79 average out of 3).
#     Still need to check if this is statistically significant.
#
# Comparing fine skills among asidlale students
t.test(asi[asi$year==thisyear & asi$echs == 1 & asi$asidlale == 1,]$score_fine, 
       asi[asi$year==thisyear & asi$echs == 0 & asi$asidlale == 1,]$score_fine, 
       var.equal=TRUE, 
       paired=FALSE)

# It's close, but not (even when setting var.equal to TRUE). 
# I would avoid making strong statements about how ECHS students 
#     compare to other asidlale students.

# Comparing fine skills among ALL students
t.test(asi[asi$year==thisyear & asi$echs == 1,]$score_fine, 
       asi[asi$year==thisyear & asi$echs == 0,]$score_fine, 
       var.equal=TRUE, 
       paired=FALSE,
       conf.level = 0.95,
       alternative = "greater")

# ECHS students perform better than other students in fine motor skills.

#
#
# The sample size is too small for statistical significance, but the 3 kids who are in ECHS but not creche
#     perform much better than their peers who don't attend creche, especially in cognitive tasks.
