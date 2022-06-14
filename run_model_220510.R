### aim 1 analysis, taco bell
### impact of city/state policy rollout pre national rollout
### diff-in-diff with restaurant level random effects

current_warning <- getOption("warn") #not display warnings
options(warn = -1)
#options(warn = current_warning)
options("scipen"=100)
dev.off()

### install and load packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages(c("lme4", "lmerTest")) #random effects and tesing significance
#install.packages("plm")
library(lme4)
library(plm)
library(lmerTest)
#install.packages("stargazer")
library(stargazer)
#install.packages("table1")
library(table1)
library(tableone)
library(broom)
#install.packages("car")
library(car)
#install.packages("usmap") #show state abbr in ggplot map
library(usmap)
library(maps)
library(car) #testing joint significance
#install.packages("zoo") #fill in NAs with the first non-NA value in a column
library(zoo)
#install.packages("sqldf")
library(sqldf)


setwd("H:/Menu Labeling in California/CAmenulabeling")
getwd()

analysis_df <- read.csv("ca_ds_220506.csv", stringsAsFactors = FALSE)
analysis_df <- analysis_df[,-1] #this removes the column with index numbers


### preparing data ----

analysis_df$tract_num <- substr(analysis_df$tract_num, 2, 12) #removing " " in the var to assure it is a numerical var
analysis_df$YEARNO <- substr(analysis_df$YEARNO, 2, 5) # removing letter Y at beginning of year number

analysis_df <- analysis_df  %>%
  mutate(relative_month = DW_MONTH - 253, 
         monthno = case_when(MONTHNAME == "JANUARY" ~ 1,
                             MONTHNAME == "FEBRUARY" ~ 2,
                             MONTHNAME == "MARCH" ~ 3,
                             MONTHNAME == "APRIL" ~ 4,
                             MONTHNAME == "MAY" ~ 5,
                             MONTHNAME == "JUNE" ~ 6,
                             MONTHNAME == "JULY" ~ 7,
                             MONTHNAME == "AUGUST" ~ 8,
                             MONTHNAME == "SEPTEMBER" ~ 9,
                             MONTHNAME == "OCTOBER" ~ 10,
                             MONTHNAME == "NOVEMBER" ~ 11,
                             MONTHNAME == "DECEMBER" ~ 12),
         occasion = case_when(DW_OCCASION == 1 ~ "InStore",
                              DW_OCCASION == 2 ~ "DriveThrough",
                              DW_OCCASION == 3 ~ "InStore"))

#check variable creation
sqldf('select distinct MONTHNAME, monthno from analysis_df order by monthno')
sqldf('select distinct DW_MONTH, relative_month from analysis_df order by DW_MONTH')

#matched$holiday <- ifelse(matched$month==12, 1, 0) ##CHECK WITH ERILIA - not needed



analysis_df1 <- analysis_df %>%
  #filter(complete.cases(analysis_df)) %>%
  #mutate(id = group_indices(., address, tract_num, ownership, concept)) %>% ####Already created rest_id previously - exists in dataset
  #select(restaurant_id, treatment, monthno, address:tract_num) %>%
  arrange(restaurant_id, monthno) %>% #find out for how many months a restaurant was open
  group_by(restaurant_id, treatment, DW_OCCASION) %>% #have to group by DW_OCCASION to get only one count for each restarurant 
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_month=max(rank))
summary(analysis_df1$rank) #sanity check, the max should be 106


#ALREADY CREATED RELATIVE MONTH AND POST VARS - CHECK WITH ERILIA WHETHER FIRST MONTH OF IMPLEMENTATION SHOULD BE 1 OR 0

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator

#matched$relative2 <- matched$monthno - matched$entry #month 0 is first month of ML
#matched$post <- ifelse(matched$relative2<0, 0, 1)

# summary stats for restaurants continuously open for 106 months -- ideally both numbers should be the same but this is just something to not for future
length(unique(analysis_df1$restaurant_id[analysis_df1$open_month==106&analysis_df1$treatment==1])) #29
length(unique(analysis_df1$restaurant_id[analysis_df1$open_month==106&analysis_df1$treatment==0])) #33

# month as relative and factor
# set month 1 as ref group

analysis_df1$relative2.factor <- factor(analysis_df1$relative_month)

# calculate open_month both before and after ML
analysis_df1 <- analysis_df1 %>%
  group_by(restaurant_id, treatment, post, DW_OCCASION) %>%
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))%>%
  ungroup()
analysis_df1$open_before <- ifelse(analysis_df1$post==0, analysis_df1$open_before, analysis_df1$open_month-analysis_df1$open_before)
analysis_df1$open_after <- ifelse(analysis_df1$post==1, analysis_df1$open_after, analysis_df1$open_month-analysis_df1$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- analysis_df1 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<=23&relative_month>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<=17&relative_month>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<=11&relative_month>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<=5&relative_month>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, DW_OCCASION, after6,after12,after18,after24) %>%
  distinct()
analysis_df1 <- merge(analysis_df1, tmp, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
tmp <- analysis_df1 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<0&relative_month>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<0&relative_month>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<0&relative_month>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<0&relative_month>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, before6,before12,before18,before24) %>%
  distinct()
analysis_df1 <- merge(analysis_df1, tmp, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
analysis_df1$open6 <- ifelse(analysis_df1$before6==1&analysis_df1$after6==1,1,0)
analysis_df1$open12 <- ifelse(analysis_df1$before12==1&analysis_df1$after12==1,1,0)
analysis_df1$open18 <- ifelse(analysis_df1$before18==1&analysis_df1$after18==1,1,0)
analysis_df1$open24 <- ifelse(analysis_df1$before24==1&analysis_df1$after24==1,1,0)
rm(tmp)

analysis_df1 <- within(analysis_df1, relative2.factor<-relevel(relative2.factor, ref="-3"))
#analysis_df1$id_match <- paste0(analysis_df1$restaurant_id, analysis_df1$match_place) #use restaurant_id

analysis_df1 <- analysis_df1 %>%
  mutate(cal_perorder = cal/count,
         totalfat_perorder = total_fat/count,
         carb_perorder = carb/count, 
         protein_perorder = protein/count, 
         satfat_perorder = sat_fat/count, 
         sugar_perorder = sugar/count, 
         fiber_perorder = fiber/count, 
         sodium_perorder = sodium/count)


### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML

####THIS IS THE PLM MODEL TO USE (and following ggplot)
mod.factor <- plm(formula = cal_perorder~treatment*relative2.factor+as.factor(monthno),
                  data = analysis_df1%>%filter((relative_month>=-30&relative_month<=-3)|(relative_month>=2&relative_month<=29)), 
                  index = "restaurant_id", model = "within")
tidy_mod.factor <- tidy(mod.factor,conf.level = 0.95,conf.int = TRUE)

tidy_mod.factor1 <- tidy_mod.factor[-c(1), ]

# clean data
tidy_mod.factor1 <- tidy_mod.factor1 %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|cal_perorder", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treatment:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))

  #mutate(low = ifelse(group==0,low,low+low[1:56])) %>%
  #mutate(high = ifelse(group==0,high,high+high[1:56]))

# add year and month factor as covariate
summary(tidy_mod.factor1$calorie) #[-202,117]
summary(tidy_mod.factor1$diff) #[-180,28]
ggplot(data=tidy_mod.factor1,aes(x=month, y=calorie,color=as.character(group))) + 
  geom_hline(yintercept = -300, color="grey", linetype="dashed", size=0.5) +
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor1%>%filter(!is.na(diff)),aes(x=month, y=diff*1-300), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor1%>%filter(!is.na(diff)&p<0.05),aes(x=month, y=diff*1-300), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -600, ymax = 200, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-150, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=8, y=-250, label="In-Store Beverage \n Under-counting", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-15, y=-225, label="   P<0.05", size=3) + 
  geom_point(aes(x=-16.5,y=-225),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-600,200),breaks=seq(-600,200,50),
                     sec.axis = sec_axis(~(.+300)/1, name="Difference", breaks = seq(-300,500,50))) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Figure 1. Effect of In-Store Menu Labeling on Calories Purchased in California", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treatment and comparison group. \n calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant") + 
  scale_color_discrete(name="Order Type", labels=c("Drive-Through (no menu labeling)", "In-Store (with menu labeling)")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
      axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
      axis.title.y = element_text(size = 12),
      legend.text=element_text(size=10),
      plot.caption=element_text(hjust=0, vjust=-15, face="italic"))
                                   
ggsave("tables and plots/fig1cal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")



#table1 diff in diff, overall ----
#set up table shell
table1 <- data.frame(matrix(data=NA, nrow=1,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("ca"))
rownames(table1) <- c("ca")


# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor1 %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,1] <- paste0(sprintf('%.1f',treat$beta)," (",sprintf('%.1f',treat$low),", ",sprintf('%.1f',treat$high),")")
comp <- tidy_mod.factor1 %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,2] <- paste0(sprintf('%.1f',comp$beta)," (",sprintf('%.1f',comp$low),", ",sprintf('%.1f',comp$high),")")
table1[1,3] <- paste0(sprintf('%.1f',treat$beta-comp$beta)," (",sprintf('%.1f',treat$low-comp$low),", ",sprintf('%.1f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor1 %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,4] <- paste0(sprintf('%.1f',treat$beta)," (",sprintf('%.1f',treat$low),", ",sprintf('%.1f',treat$high),")")
comp <- tidy_mod.factor1 %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,5] <- paste0(sprintf('%.1f',comp$beta)," (",sprintf('%.1f',comp$low),", ",sprintf('%.1f',comp$high),")")
table1[1,6] <- paste0(sprintf('%.1f',treat$beta-comp$beta)," (",sprintf('%.1f',treat$low-comp$low),", ",sprintf('%.1f',treat$high-comp$high),")")


#################################################################################################################################################################
#daypart
daypart_df <- read.csv("ca_daypart_220505.csv", stringsAsFactors = FALSE)
daypart_df <- daypart_df[,-1] #this removes the column with index numbers


### preparing data ----

daypart_df$tract_num <- substr(daypart_df$tract_num, 2, 12) #removing " " in the var to assure it is a numerical var
daypart_df$YEARNO <- substr(daypart_df$YEARNO, 2, 5) # removing letter Y at beginning of year number

daypart_df <- daypart_df  %>%
  mutate(relative_month = DW_MONTH - 253, 
         monthno = case_when(MONTHNAME == "JANUARY" ~ 1,
                             MONTHNAME == "FEBRUARY" ~ 2,
                             MONTHNAME == "MARCH" ~ 3,
                             MONTHNAME == "APRIL" ~ 4,
                             MONTHNAME == "MAY" ~ 5,
                             MONTHNAME == "JUNE" ~ 6,
                             MONTHNAME == "JULY" ~ 7,
                             MONTHNAME == "AUGUST" ~ 8,
                             MONTHNAME == "SEPTEMBER" ~ 9,
                             MONTHNAME == "OCTOBER" ~ 10,
                             MONTHNAME == "NOVEMBER" ~ 11,
                             MONTHNAME == "DECEMBER" ~ 12),
         occasion = case_when(DW_OCCASION == 1 ~ "InStore",
                              DW_OCCASION == 2 ~ "DriveThrough",
                              DW_OCCASION == 3 ~ "InStore"),
         char_daypart = case_when(DW_DAYPART == 1 ~ "Late Night",
                                  DW_DAYPART == 2 ~ "Breakfast", 
                                  DW_DAYPART == 3 ~ "Lunch",
                                  DW_DAYPART == 4 ~ "Afternoon",
                                  DW_DAYPART == 5 ~ "Dinner",
                                  DW_DAYPART == 6 ~ "Evening"))

#check variable creation
sqldf('select distinct MONTHNAME, monthno from daypart_df order by monthno')
sqldf('select distinct DW_MONTH, relative_month from daypart_df order by DW_MONTH')

daypart_df1 <- daypart_df %>%
  #filter(complete.cases(daypart_df)) %>%
  #mutate(id = group_indices(., address, tract_num, ownership, concept)) %>% ####Already created rest_id previously - exists in dataset
  #select(restaurant_id, treatment, monthno, address:tract_num) %>%
  arrange(restaurant_id, monthno) %>% #find out for how many months a restaurant was open
  group_by(restaurant_id, treatment, DW_OCCASION, DW_DAYPART) %>% #have to group by DW_OCCASION to get only one count for each restarurant 
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_month=max(rank))
summary(daypart_df1$rank) #sanity check, the max should be 106


#ALREADY CREATED RELATIVE MONTH AND POST VARS - CHECK WITH ERILIA WHETHER FIRST MONTH OF IMPLEMENTATION SHOULD BE 1 OR 0

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator

#matched$relative2 <- matched$monthno - matched$entry #month 0 is first month of ML
#matched$post <- ifelse(matched$relative2<0, 0, 1)

# summary stats for restaurants continuously open for 106 months -- ideally both numbers should be the same but this is just something to not for future
length(unique(daypart_df1$restaurant_id[daypart_df1$open_month==106&daypart_df1$treatment==1])) #29
length(unique(daypart_df1$restaurant_id[daypart_df1$open_month==106&daypart_df1$treatment==0])) #33

# month as relative and factor
# set month 1 as ref group

daypart_df1$relative2.factor <- factor(daypart_df1$relative_month)

# calculate open_month both before and after ML
daypart_df1 <- daypart_df1 %>%
  group_by(restaurant_id, treatment, post, DW_OCCASION, DW_DAYPART) %>%
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))%>%
  ungroup()
daypart_df1$open_before <- ifelse(daypart_df1$post==0, daypart_df1$open_before, daypart_df1$open_month-daypart_df1$open_before)
daypart_df1$open_after <- ifelse(daypart_df1$post==1, daypart_df1$open_after, daypart_df1$open_month-daypart_df1$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp_dp <- daypart_df1 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<=23&relative_month>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<=17&relative_month>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<=11&relative_month>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<=5&relative_month>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, DW_OCCASION, after6,after12,after18,after24) %>%
  distinct()
daypart_df1 <- merge(daypart_df1, tmp_dp, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
tmp_dp <- daypart_df1 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<0&relative_month>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<0&relative_month>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<0&relative_month>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<0&relative_month>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, before6,before12,before18,before24) %>%
  distinct()
daypart_df1 <- merge(daypart_df1, tmp_dp, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
daypart_df1$open6 <- ifelse(daypart_df1$before6==1&daypart_df1$after6==1,1,0)
daypart_df1$open12 <- ifelse(daypart_df1$before12==1&daypart_df1$after12==1,1,0)
daypart_df1$open18 <- ifelse(daypart_df1$before18==1&daypart_df1$after18==1,1,0)
daypart_df1$open24 <- ifelse(daypart_df1$before24==1&daypart_df1$after24==1,1,0)
rm(tmp_dp)

daypart_df1 <- within(daypart_df1, relative2.factor<-relevel(relative2.factor, ref="-3"))
#daypart_df1$id_match <- paste0(daypart_df1$restaurant_id, daypart_df1$match_place) #use restaurant_id

daypart_df1 <- daypart_df1 %>%
  mutate(cal_perorder = cal/count,
         totalfat_perorder = total_fat/count,
         carb_perorder = carb/count, 
         protein_perorder = protein/count, 
         satfat_perorder = sat_fat/count, 
         sugar_perorder = sugar/count, 
         fiber_perorder = fiber/count, 
         sodium_perorder = sodium/count)


### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML

####THIS IS THE PLM MODEL TO USE (and following ggplot)

#create loop to run analysis for each daypart separately and then rbind before creating plot using ggplot below

unique(daypart_df1$DW_DAYPART)

tidy_mod.factor_alldaypart <- NULL
for (i in 1:6) {
  mod.factor_dp <- plm(formula = cal_perorder~treatment*relative2.factor+as.factor(monthno),
                    data = daypart_df1%>%filter((relative_month>=-30&relative_month<=-3)|(relative_month>=2&relative_month<=29)&DW_DAYPART==i), 
                    index = "restaurant_id", model = "within")
  tidy_mod.factor_dp <- tidy(mod.factor_dp,conf.level = 0.95,conf.int = TRUE)

  tidy_mod.factor_dp <- tidy_mod.factor_dp[-c(1), ]

  # clean data
  tidy_mod.factor_dp <- tidy_mod.factor_dp %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|cal_perorder", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treatment:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(daypart = i)
  
    tidy_mod.factor_alldaypart <- rbind(tidy_mod.factor_alldaypart,tidy_mod.factor_dp)
    tidy_mod.factor_alldaypart <- tidy_mod.factor_alldaypart[!is.na(tidy_mod.factor_alldaypart$diff),]
}

# add year and month factor as covariate
summary(tidy_mod.factor_alldaypart$calorie) #[-202,117]
summary(tidy_mod.factor_alldaypart$diff) #[-180,28]


tidy_mod.factor_alldaypart <- tidy_mod.factor_alldaypart %>%
  mutate(char_daypart = case_when(daypart == 1 ~ "Late Night",
                                  daypart == 2 ~ "Breakfast", 
                                  daypart == 3 ~ "Lunch",
                                  daypart == 4 ~ "Afternoon",
                                  daypart == 5 ~ "Dinner",
                                  daypart == 6 ~ "Evening"))

ggplot(data=tidy_mod.factor_alldaypart%>%filter(daypart!=2),aes(x=month,y=diff,color=factor(char_daypart))) + #
  geom_point(size=1) + geom_line() +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-800, ymax=200, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=-250, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-800,200),breaks=seq(-800,200,50)) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Figure 2. Effect of menu labeling on calories purchased in California, by meal time", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \n **breakfast excluded in this figure") + 
  scale_color_manual(name="Meal time",values=c("hotpink","olivedrab3","#13B0E4", "purple", "orange")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic")) 

#ggsave("tables and plots/fig2DAYPARTcal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")



#table1 diff in diff, overall ----
#set up table shell
table2 <- data.frame(matrix(data=NA, nrow=1,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("ca"))
rownames(table1) <- c("ca")


# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor_dp1 %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,1] <- paste0(sprintf('%.1f',treat$beta)," (",sprintf('%.1f',treat$low),", ",sprintf('%.1f',treat$high),")")
comp <- tidy_mod.factor_dp1 %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,2] <- paste0(sprintf('%.1f',comp$beta)," (",sprintf('%.1f',comp$low),", ",sprintf('%.1f',comp$high),")")
table1[1,3] <- paste0(sprintf('%.1f',treat$beta-comp$beta)," (",sprintf('%.1f',treat$low-comp$low),", ",sprintf('%.1f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor_dp1 %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,4] <- paste0(sprintf('%.1f',treat$beta)," (",sprintf('%.1f',treat$low),", ",sprintf('%.1f',treat$high),")")
comp <- tidy_mod.factor_dp1 %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,5] <- paste0(sprintf('%.1f',comp$beta)," (",sprintf('%.1f',comp$low),", ",sprintf('%.1f',comp$high),")")
table1[1,6] <- paste0(sprintf('%.1f',treat$beta-comp$beta)," (",sprintf('%.1f',treat$low-comp$low),", ",sprintf('%.1f',treat$high-comp$high),")")


#################################################################################################################################################################
#food categories
cat_df <- read.csv("ca_category_220506.csv", stringsAsFactors = FALSE)
cat_df <- cat_df[,-1] #this removes the column with index numbers


### preparing data ----

cat_df$tract_num <- substr(cat_df$tract_num, 2, 12) #removing " " in the var to assure it is a numerical var
cat_df$YEARNO <- substr(cat_df$YEARNO, 2, 5) # removing letter Y at beginning of year number

cat_df <- cat_df  %>%
  mutate(relative_month = DW_MONTH - 253, 
         monthno = case_when(MONTHNAME == "JANUARY" ~ 1,
                             MONTHNAME == "FEBRUARY" ~ 2,
                             MONTHNAME == "MARCH" ~ 3,
                             MONTHNAME == "APRIL" ~ 4,
                             MONTHNAME == "MAY" ~ 5,
                             MONTHNAME == "JUNE" ~ 6,
                             MONTHNAME == "JULY" ~ 7,
                             MONTHNAME == "AUGUST" ~ 8,
                             MONTHNAME == "SEPTEMBER" ~ 9,
                             MONTHNAME == "OCTOBER" ~ 10,
                             MONTHNAME == "NOVEMBER" ~ 11,
                             MONTHNAME == "DECEMBER" ~ 12),
         occasion = case_when(DW_OCCASION == 1 ~ "InStore",
                              DW_OCCASION == 2 ~ "DriveThrough",
                              DW_OCCASION == 3 ~ "InStore"),
         char_cat = case_when(DW_CATEGORY == 1 ~ "Beverage",
                              DW_CATEGORY == 2 ~ "Burrito",
                              DW_CATEGORY == 3 ~ "Dessert",
                              DW_CATEGORY == 4 ~ "Other(Entree)",
                              DW_CATEGORY == 5 ~ "Salad",
                              DW_CATEGORY == 6 ~ "Side",
                              DW_CATEGORY == 7 ~ "Substitution",
                              DW_CATEGORY == 8 ~ "Taco",
                              DW_CATEGORY == 9 ~ "Other"))

#check variable creation
sqldf('select distinct MONTHNAME, monthno from cat_df order by monthno')
sqldf('select distinct DW_MONTH, relative_month from cat_df order by DW_MONTH')

cat_df1 <- cat_df %>%
  #filter(complete.cases(cat_df)) %>%
  #mutate(id = group_indices(., address, tract_num, ownership, concept)) %>% ####Already created rest_id previously - exists in dataset
  #select(restaurant_id, treatment, monthno, address:tract_num) %>%
  arrange(restaurant_id, monthno) %>% #find out for how many months a restaurant was open
  group_by(restaurant_id, treatment, DW_OCCASION, DW_CATEGORY) %>% #have to group by DW_OCCASION to get only one count for each restarurant 
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_month=max(rank))
summary(cat_df1$rank) #sanity check, the max should be 106


#ALREADY CREATED RELATIVE MONTH AND POST VARS - CHECK WITH ERILIA WHETHER FIRST MONTH OF IMPLEMENTATION SHOULD BE 1 OR 0

# get relative month for pre-/post-labeling, month of labeling=1
# set up post ML indicator

#matched$relative2 <- matched$monthno - matched$entry #month 0 is first month of ML
#matched$post <- ifelse(matched$relative2<0, 0, 1)

# summary stats for restaurants continuously open for 106 months -- ideally both numbers should be the same but this is just something to not for future
length(unique(cat_df1$restaurant_id[cat_df1$open_month==106&cat_df1$treatment==1])) #29
length(unique(cat_df1$restaurant_id[cat_df1$open_month==106&cat_df1$treatment==0])) #33

# month as relative and factor
# set month 1 as ref group

cat_df1$relative2.factor <- factor(cat_df1$relative_month)

# calculate open_month both before and after ML
cat_df1 <- cat_df1 %>%
  group_by(restaurant_id, treatment, post, DW_OCCASION, DW_CATEGORY) %>%
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))%>%
  ungroup()
cat_df1$open_before <- ifelse(cat_df1$post==0, cat_df1$open_before, cat_df1$open_month-cat_df1$open_before)
cat_df1$open_after <- ifelse(cat_df1$post==1, cat_df1$open_after, cat_df1$open_month-cat_df1$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp_cat <- cat_df1 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<=23&relative_month>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<=17&relative_month>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<=11&relative_month>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<=5&relative_month>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, DW_OCCASION, after6,after12,after18,after24) %>%
  distinct()
cat_df1 <- merge(cat_df1, tmp_cat, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
tmp_cat <- cat_df1 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<0&relative_month>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<0&relative_month>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<0&relative_month>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<0&relative_month>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, before6,before12,before18,before24) %>%
  distinct()
cat_df1 <- merge(cat_df1, tmp_cat, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
cat_df1$open6 <- ifelse(cat_df1$before6==1&cat_df1$after6==1,1,0)
cat_df1$open12 <- ifelse(cat_df1$before12==1&cat_df1$after12==1,1,0)
cat_df1$open18 <- ifelse(cat_df1$before18==1&cat_df1$after18==1,1,0)
cat_df1$open24 <- ifelse(cat_df1$before24==1&cat_df1$after24==1,1,0)
rm(tmp_cat)

cat_df1 <- within(cat_df1, relative2.factor<-relevel(relative2.factor, ref="-3"))
#cat_df1$id_match <- paste0(cat_df1$restaurant_id, cat_df1$match_place) #use restaurant_id

cat_df1 <- cat_df1 %>%
  mutate(cal_perorder = cal/count,
         totalfat_perorder = total_fat/count,
         carb_perorder = carb/count, 
         protein_perorder = protein/count, 
         satfat_perorder = sat_fat/count, 
         sugar_perorder = sugar/count, 
         fiber_perorder = fiber/count, 
         sodium_perorder = sodium/count)


### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML

####THIS IS THE PLM MODEL TO USE (and following ggplot)

#create loop to run analysis for each daypart separately and then rbind before creating plot using ggplot below

unique(cat_df1$DW_CATEGORY)

tidy_mod.factor_allcat <- NULL
for (i in 1:9) {
  mod.factor_cat <- plm(formula = cal_perorder~treatment*relative2.factor+as.factor(monthno),
                       data = cat_df1%>%filter((relative_month>=-30&relative_month<=-3)|(relative_month>=2&relative_month<=29)&DW_CATEGORY==i), 
                       index = "restaurant_id", model = "within")
  tidy_mod.factor_cat <- tidy(mod.factor_cat,conf.level = 0.95,conf.int = TRUE)
  
  tidy_mod.factor_cat <- tidy_mod.factor_cat[-c(1), ]
  
  # clean data
  tidy_mod.factor_cat <- tidy_mod.factor_cat %>%
    dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
    rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
    filter(!grepl("as.factor|cal_perorder", month)) %>%
    mutate(group=c(rep(0,55),rep(1,55))) %>%
    add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
    add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
    mutate(month=as.integer(gsub("treatment:relative2.factor|relative2.factor","",month))) %>%
    mutate(month=ifelse(month>0,month+1,month)) %>%
    arrange(group,month) %>%
    mutate(diff = ifelse(group==1,coef.month,NA)) %>%
    mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56])) %>%
    mutate(food_cat = i)
  
  tidy_mod.factor_allcat <- rbind(tidy_mod.factor_allcat,tidy_mod.factor_cat)
  tidy_mod.factor_allcat <- tidy_mod.factor_allcat[!is.na(tidy_mod.factor_allcat$diff),]
}

# add year and month factor as covariate
summary(tidy_mod.factor_allcat$calorie) #[-416,490]
summary(tidy_mod.factor_allcat$diff) #[-267,99]

tidy_mod.factor_allcat <- tidy_mod.factor_allcat %>%
  mutate(char_cat = case_when(food_cat == 1 ~ "Beverage",
                              food_cat == 2 ~ "Burrito",
                              food_cat == 3 ~ "Dessert",
                              food_cat == 4 ~ "Other(Entree)",
                              food_cat == 5 ~ "Salad",
                              food_cat == 6 ~ "Side",
                              food_cat == 7 ~ "Substitution",
                              food_cat == 8 ~ "Taco",
                              food_cat == 9 ~ "Other"))

ggplot(data=tidy_mod.factor_allcat%>%filter(!food_cat %in% c(9,5,7)),aes(x=month,y=diff,color=char_cat)) + 
  geom_point(size=2) + geom_line(size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  ggplot2::annotate("rect", xmin=-3, xmax=3, ymin=-300, ymax=100, fill = "grey") + #add shaded area
  ggplot2::annotate(geom="label", x=0, y=50, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-300,100),breaks=seq(-300,100,25)) +
  scale_x_continuous(breaks=c(seq(-48,-3,3),seq(3,56,3))) + #select which months to display
  labs(title="Figure 3. Effect of menu labeling on calories purchased in California, by food category", x="Month", y="Calories in difference", 
       caption="calorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \n **excluding salad, substitution, and other") + 
  scale_color_manual(name="Food Category",values=c("#3399FF","#000000","#FF3300", "#FF9933", "#FF00FF", "#00CC99")) + 
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"),
        panel.grid.minor = element_blank()) 

#ggsave("tables and plots/fig3FOODCATcal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")

#table1 diff in diff, overall ----
#set up table shell
table3 <- data.frame(matrix(data=NA, nrow=1,ncol=6)) %>%
  setNames(c("diff_treat_3_12","diff_comp_3_12","did_3_12","diff_treat_13_24","diff_comp_13_24","did_13_24")) %>%
  add_column(location=c("ca"))
rownames(table1) <- c("ca")


# diff in labeled group, months 3-12
#columns 1-3
treat <- tidy_mod.factor_cat %>% filter(group==1&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,1] <- paste0(sprintf('%.1f',treat$beta)," (",sprintf('%.1f',treat$low),", ",sprintf('%.1f',treat$high),")")
comp <- tidy_mod.factor_dp1 %>% filter(group==0&month>=-8&month<=12) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,2] <- paste0(sprintf('%.1f',comp$beta)," (",sprintf('%.1f',comp$low),", ",sprintf('%.1f',comp$high),")")
table1[1,3] <- paste0(sprintf('%.1f',treat$beta-comp$beta)," (",sprintf('%.1f',treat$low-comp$low),", ",sprintf('%.1f',treat$high-comp$high),")")
#columns 4-6
treat <- tidy_mod.factor_dp1 %>% filter(group==1&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,4] <- paste0(sprintf('%.1f',treat$beta)," (",sprintf('%.1f',treat$low),", ",sprintf('%.1f',treat$high),")")
comp <- tidy_mod.factor_dp1 %>% filter(group==0&((month>=-8&month<0)|(month>=13&month<=24))) %>%
  mutate(post=ifelse(month<0,0,1)) %>% group_by(post) %>%
  summarise(beta=mean(calorie),low=mean(low),high=mean(high)) %>% ungroup() %>%
  mutate(beta=ifelse(post==1,beta-beta[1],beta),low=ifelse(post==1,low-low[1],low),high=ifelse(post==1,high-high[1],high)) %>%
  filter(post==1)
table1[1,5] <- paste0(sprintf('%.1f',comp$beta)," (",sprintf('%.1f',comp$low),", ",sprintf('%.1f',comp$high),")")
table1[1,6] <- paste0(sprintf('%.1f',treat$beta-comp$beta)," (",sprintf('%.1f',treat$low-comp$low),", ",sprintf('%.1f',treat$high-comp$high),")")

#######################################################################################################################################################################

#create a food only dataset - exclude beverage, salad, substitution, and other

catfood_df <- cat_df %>%
  filter(!DW_CATEGORY %in% c(1,5,7,9))

#check that beverage records were deleted
sqldf('select distinct DW_CATEGORY, char_cat from catfood_df order by DW_CATEGORY')

ls(catfood_df)
cols <- c("")

#group and aggregate
foodonly_df <- catfood_df %>%
  select(-c(DW_CATEGORY, char_cat)) 

foodonly_df1 <- foodonly_df %>%
  group_by(restaurant_id, DW_OCCASION, YEARNO, QTRNO, MONTHNAME, relative_month, monthno, treatment) %>%
  summarise_at(vars(cal:total_revenue), sum) %>%
  ungroup() 

#difference

foodonly_df2 <- foodonly_df1 %>%
  arrange(restaurant_id, monthno) %>% #find out for how many months a restaurant was open
  group_by(restaurant_id, treatment, DW_OCCASION) %>% #have to group by DW_OCCASION to get only one count for each restarurant 
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_month=max(rank))
summary(foodonly_df2$rank) #sanity check, the max should be 106


# summary stats for restaurants continuously open for 106 months -- ideally both numbers should be the same but this is just something to not for future
length(unique(foodonly_df2$restaurant_id[foodonly_df2$open_month==106&foodonly_df2$treatment==1])) #29
length(unique(foodonly_df2$restaurant_id[foodonly_df2$open_month==106&foodonly_df2$treatment==0])) #33

# month as relative and factor
# set month 1 as ref group

foodonly_df2$relative2.factor <- factor(foodonly_df2$relative_month)

# calculate open_month both before and after ML
foodonly_df2 <- foodonly_df2 %>%
  group_by(restaurant_id, treatment, post, DW_OCCASION) %>%
  mutate(rank=row_number(restaurant_id)) %>%
  mutate(open_before=max(rank)) %>%
  mutate(open_after=max(rank))%>%
  ungroup()
foodonly_df2$open_before <- ifelse(foodonly_df2$post==0, foodonly_df2$open_before, foodonly_df2$open_month-foodonly_df2$open_before)
foodonly_df2$open_after <- ifelse(foodonly_df2$post==1, foodonly_df2$open_after, foodonly_df2$open_month-foodonly_df2$open_after)

#create indicators for restaurants that were open for at least 6, 12, 18 and 24 months before and after ML
tmp <- foodonly_df2 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<=23&relative_month>=0) %>% mutate(n=n()) %>% mutate(after24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<=17&relative_month>=0) %>% mutate(n=n()) %>% mutate(after18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<=11&relative_month>=0) %>% mutate(n=n()) %>% mutate(after12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<=5&relative_month>=0) %>% mutate(n=n()) %>% mutate(after6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, DW_OCCASION, after6,after12,after18,after24) %>%
  distinct()
foodonly_df2 <- merge(foodonly_df2, tmp, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
tmp <- foodonly_df2 %>%
  group_by(restaurant_id, treatment, DW_OCCASION) %>%
  filter(relative_month<0&relative_month>= -24) %>% mutate(n=n()) %>% mutate(before24 = ifelse(n==24, 1,0)) %>%
  filter(relative_month<0&relative_month>= -18) %>% mutate(n=n()) %>% mutate(before18 = ifelse(n==18, 1,0)) %>%
  filter(relative_month<0&relative_month>= -12) %>% mutate(n=n()) %>% mutate(before12 = ifelse(n==12, 1,0)) %>%
  filter(relative_month<0&relative_month>= -6) %>% mutate(n=n()) %>% mutate(before6 = ifelse(n==6, 1,0)) %>%
  dplyr::select(restaurant_id, treatment, before6,before12,before18,before24) %>%
  distinct()
foodonly_df2 <- merge(foodonly_df2, tmp, by=c("restaurant_id", "treatment", "DW_OCCASION"), all = TRUE)
foodonly_df2$open6 <- ifelse(foodonly_df2$before6==1&foodonly_df2$after6==1,1,0)
foodonly_df2$open12 <- ifelse(foodonly_df2$before12==1&foodonly_df2$after12==1,1,0)
foodonly_df2$open18 <- ifelse(foodonly_df2$before18==1&foodonly_df2$after18==1,1,0)
foodonly_df2$open24 <- ifelse(foodonly_df2$before24==1&foodonly_df2$after24==1,1,0)
rm(tmp)

foodonly_df2 <- within(foodonly_df2, relative2.factor<-relevel(relative2.factor, ref="-3"))


foodonly_df2 <- foodonly_df2 %>%
  mutate(cal_perorder = cal/count,
         totalfat_perorder = total_fat/count,
         carb_perorder = carb/count, 
         protein_perorder = protein/count, 
         satfat_perorder = sat_fat/count, 
         sugar_perorder = sugar/count, 
         fiber_perorder = fiber/count, 
         sodium_perorder = sodium/count)


### cal=group*month(factor), month as factor ----
# get num of restaurants in each month
#ignore results 2 months before and after ML

####THIS IS THE PLM MODEL TO USE (and following ggplot)
mod.factor_fd <- plm(formula = cal_perorder~treatment*relative2.factor+as.factor(monthno),
                  data = foodonly_df2%>%filter((relative_month>=-30&relative_month<=-3)|(relative_month>=2&relative_month<=29)), 
                  index = "restaurant_id", model = "within")
tidy_mod.factor_fd <- tidy(mod.factor_fd,conf.level = 0.95,conf.int = TRUE)

tidy_mod.factor_fd1 <- tidy_mod.factor_fd[-c(1), ]

# clean data
tidy_mod.factor_fd1 <- tidy_mod.factor_fd1 %>%
  dplyr::select(term,estimate,p.value,conf.low,conf.high) %>%
  rename(month=term,coef.month=estimate,p=p.value,low=conf.low,high=conf.high) %>%
  filter(!grepl("as.factor|cal_perorder", month)) %>%
  mutate(group=c(rep(0,55),rep(1,55))) %>%
  add_row(month="-3",coef.month=0,group=0,low=0,high=0) %>%
  add_row(month="-3",coef.month=0,group=1,low=0,high=0) %>%
  mutate(month=as.integer(gsub("treatment:relative2.factor|relative2.factor","",month))) %>%
  mutate(month=ifelse(month>0,month+1,month)) %>%
  arrange(group,month) %>%
  mutate(diff = ifelse(group==1,coef.month,NA)) %>%
  mutate(calorie=ifelse(group==0,coef.month,coef.month+coef.month[1:56]))


# add year and month factor as covariate
summary(tidy_mod.factor_fd1$calorie) #[-16.33,80]
summary(tidy_mod.factor_fd1$diff) #[-7.62,18.12]
ggplot(data=tidy_mod.factor_fd1,aes(x=month, y=calorie,color=as.character(group))) + 
  geom_hline(yintercept = -150, color="grey", linetype="dashed", size=0.5) +
  geom_point(size=1) + geom_line() +
  geom_line(data=tidy_mod.factor_fd1%>%filter(!is.na(diff)),aes(x=month, y=diff*1-150), color="orange") + #add diff between 2 groups
  geom_point(data=tidy_mod.factor_fd1%>%filter(!is.na(diff)&p<0.05),aes(x=month, y=diff*1-150), color="orange") + #highlight significant months with dots
  ggplot2::annotate("rect", xmin = -3, xmax = 3, ymin = -200, ymax = 150, fill = "grey") + #add grey shaded area
  ggplot2::annotate(geom="label", x=0, y=-50, label="Menu labeling \n implementation \n and adjustment period", size=3) + #add label for ML
  #ggplot2::annotate(geom="label", x=8, y=-250, label="In-Store Beverage \n Under-counting", size=3) + #add label for ML
  ggplot2::annotate(geom="label", x=-15, y=-100, label="   P<0.05", size=3) + 
  geom_point(aes(x=-16.5,y=-100),color="orange",size=1) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-200,150),breaks=seq(-200,150,50),
                     sec.axis = sec_axis(~(.+150)/1, name="Difference", breaks = seq(-150,500,50))) +
  scale_x_continuous(breaks=c(seq(-30,-3,3),seq(3,30,3))) + #select which months to display
  labs(title="Figure 1. Effect of In-Store Menu Labeling on Food Calories Purchased in California", x="Month", y="Calories", 
       caption="Orange lined represents the difference between treatment and comparison group. \ncalorie = treat + month(relative) + treat*month(relative) + ∑month_1-12 + ∑restaurant \n\n**Excluding: Beverage, Salad, Substitution and Other categories") + 
  scale_color_discrete(name="Order Type", labels=c("Drive-Through (no menu labeling)", "In-Store (with menu labeling)")) +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10),
        plot.caption=element_text(hjust=0, vjust=-15, face="italic"))

ggsave("tables and plots/fig1FOODONLYcal=treat+month_monthFE_restaurantFE.jpeg", dpi="retina")


## Difference in Difference Main Analysis with Pre and Post Period Plotted

trend <- NULL
presum <- "treatment:relative2.factor-4 + treatment:relative2.factor-5 + treatment:relative2.factor-6 + treatment:relative2.factor-7 + treatment:relative2.factor-8"

tmp1 <- tidy_mod.factor_fd1 %>% 
  filter(month>=-30&month<0&!is.na(diff)) %>% dplyr::select(month, diff) %>%
  mutate(month = -month) %>% arrange(month) %>% mutate(pre_mean = sum(diff[1:6])/6)
tmp2 <- tidy_mod.factor_fd1 %>% 
  filter(month>=1&month<=30&!is.na(diff)) %>% dplyr::select(month, diff) %>%
  arrange(month) %>% rename(post_mean = diff)
tmp1 <- merge(tmp1,tmp2,by="month") %>% group_by(month) %>% arrange(month) %>%
  mutate(mean = post_mean - pre_mean) %>% dplyr::select(-diff)
tmp <- data.frame(matrix(data=0,nrow=28,ncol=1)) %>% setNames("p")

for (i in 4:29) {
  tmp$p[i-1] <- linearHypothesis(mod.factor_fd, paste0(presum," = 6*treatment:relative2.factor",i))[2,4]
}
tmp1 <- cbind(tmp1, tmp)

trend <- rbind(trend,tmp1)


#plot

summary(trend$mean) #[-5.92,10.34]
ggplot() + 
  geom_line(data=trend, aes(x=month, y=mean)) + 
  geom_point(data=trend%>%filter(p<0.05), aes(x=month, y=mean)) +
  ggplot2::annotate(geom="label", x=8, y=25, label="   p<0.05", size=3) + 
  geom_point(aes(x=7.5,y=25),color="black",size=1) +
  geom_hline(yintercept = 0, color="grey", linetype="dashed", size=0.5) +
  coord_cartesian(expand = FALSE, clip = "off") + 
  scale_y_continuous(limits=c(-50,50),breaks=seq(-50,50,10)) +
  scale_x_continuous(breaks=seq(3,30,1)) +
  labs(title="Figure 2. Effect of In-Store Menu Labeling on Food Calories Purchased Difference Estimate over Time", x="Month", y="Difference-in-Difference Estimate", 
       caption="**Excluding: Beverage, Salad, Substitution and Other categories") + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(hjust = 0.5, size = 16), #position/size of title
        axis.title.x = element_text(vjust=-1, size = 12), #vjust to adjust position of x-axis
        axis.title.y = element_text(size = 12),
        legend.text=element_text(size=10), 
        plot.caption=element_text(hjust=0, vjust=-1, face="italic"),
        panel.grid.minor = element_blank())

ggsave("tables and plots/fig2-diff-in-diff-foodonly.jpeg", dpi="retina")
