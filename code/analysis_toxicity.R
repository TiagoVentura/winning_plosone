# ------------------------------------------------------------------------------- #
# Paper: Winning
# Author: Calvo, Weisboard, Aruguete and Ventura
# Last update: November 9, 2022
# Estimating Toxicity Scores 
# ------------------------------------------------------------------------------- #


# Packages ----------------------------------------------------------------

rm(list = ls(all=TRUE))
library(igraph)
library(rdrobust)
library(readstata13)
library(foreign)
library(Hmisc)
library(lattice)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(pipeR)
library(magrittr)
library(lubridate)


# Open the data -----------------------------------------------------------

# here authors need to work with the hydrated tweets

load("data_brazil.Rdata")

# Get the text ------------------------------------------------------------

summary(net)

text <- E(net)$text 
idRT <- E(net)$tweetidRT
timeRT <- E(net)$timeRT # user retweeted
timeT <- E(net)$timeT # user originally tweeted

# Change the format
format.str <- "%a %b %d %H:%M:%S %z %Y"
date.rt<-as.POSIXct(strptime(timeRT, format.str, tz = "GMT"), tz = "GMT")
#date.rt2 <- format(date.rt, tz="America/New_York")
class(date.rt)


date.t <-as.POSIXct(strptime(timeT, format.str, tz = "GMT"), tz = "GMT")
#data.t <- format(date.t,tz="America/New_York")
class(date.t)


my.time<-as.integer(date.rt-date.t)

# Create a data frame
text_id <- tibble(idRT, text, date.rt, date.t, my.time) 

# check adjudication time
as_datetime(1538949906, origin="1970-01-01", tz="America/New_York")
ymd_hms("2018-10-07 18:05:06",  tz = "America/New_York" )
as.POSIXct("2018-10-07 18:05:06",  tz = "America/New_York" )

text_id <- text_id %>% 
                mutate(date.rt_est=with_tz(ymd_hms(date.rt), tz = "America/New_York"), 
                       date.t_est=with_tz(ymd_hms(date.t), tz = "America/New_York"), 
                       call=ymd_hms("2018-10-07 18:05:06",  tz = "America/New_York" ), 
                       center_tele_sec=(date.rt_est-call), 
                       centertele=as.numeric(center_tele_sec)) 


# Filter to analyze only 6hrs before and after the debate
text_ide <- text_id %>% 
              filter(centertele>-21600 & centertele <21600) %>% 
              mutate(lntimeelapsed=log(my.time +1)) 

cut <- cut(text_ide$centertele,100, include.lowest = TRUE)
tmp <- aggregate(text_ide$lntimeelapsed, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((text_ide$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
        mutate(treat=ifelse(margin>0, "After the announce", "Before the announce"))
pal <- RColorBrewer::brewer.pal(n=5, "RdBu")

# Make sure it works

ggplot() +
  geom_point(data = data, aes(margin, y, fill=treat, color=treat),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data=data,
              aes(y=y, margin, 
                  group=treat), color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("UK") +
  xlab("Six hours window before and after announcement") +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  ylab("Time-to-retweet") + 
  coord_cartesian(ylim=c(5,10)) + 
  scale_color_manual(name="", values=c(pal[5], pal[1])) +
  scale_fill_manual(name="", values=c(pal[5], pal[1])) +
  theme_minimal(base_size = 22) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=22), 
        legend.key.size = unit(1.5, "cm"))



#Estimate toxicity -------------------------------------------------------

library(peRspective)
library(tictoc)
library(rebus)


text_idw <- text_ide %>%
            select(idRT, text) %>% # only idRT here
            distinct() 


## Run the complete data

text_idw <- text_idw %>% rowid_to_column()
text_idw$rowid <- as.character(text_idw$rowid)

l =list()

tic("Scores BR")
for(i in 1:nrow(text_idw)){
l[[i]] <- text_idw %>%  
              slice(i) %>%
              drop_na() %>%
             prsp_stream(text = text,
               text_id = rowid,
               score_model = c("TOXICITY"),
               safe_output = T,
               verbose = T,
               language="en")
print(i)
}
toc()

br_score <- bind_rows(l)
#save(br_score, file="br_toxicity_scores.Rdata")

br_scores_join <- left_join(text_idw, br_score, by=c("rowid"="text_id")) 
br_scores_join <- select(br_scores_join, -rowid)
id <- br_scores_join %>% count(idRT) %>%
        filter(n==2)

# removing a duplicate
br_scores_join<- br_scores_join %>% group_by(idRT) %>% 
                          summarise(TOXICITY=mean(TOXICITY))

#save(br_scores_join, file="br_toxicity_scores.Rdata")


# Merging back ------------------------------------------------------------

# Merge with the main data
br_scores_join <- left_join(text_id, br_scores_join, by="idRT")

sum(is.na(br_scores_join$TOXICITY))



# Add variables needed for tox --------------------------------------------

# add membership and renaming
br_scores_join$nameHub <- E(net)$namehub
br_scores_join$nameAuth <- E(net)$nameauth
br_scores_join$membershipAuth <- E(net)$membership.auth
br_scores_join$membershipHub <- E(net)$membership.hub
br_scores_join$idRT_ <- E(net)$tweetidRT

sum(br_scores_join$idRT_==br_scores_join$idRT)

# Estimating Toxicity -----------------------------------------------------
minute_conv<-function(x){
  (x/1000)/60
}

as.POSIXct(1538949906, origin="1970-01-01")
colnames(br_scores_join)

# add membership

# clean the data
d <- br_scores_join %>% 
        mutate(
          timetweet=as.numeric(dateTweet), 
          timeRT=as.numeric(dateRT),
          bolsotele=1538949906, 
          centertele=as.numeric(dateRT)-bolsotele,
          centertele_h=minute_conv(as.numeric(centertele))/60,
          timeElapsed=as.numeric(dateRT)-as.numeric(dateTweet),
          lntimeelapsed=log(timeElapsed+1))

d <- d %>% mutate(after=ifelse(centertele>0, "1", "0"))


# Overall Adjudication ---------------------------------------------------

before <- d %>% 
  filter((centertele >-22200 & centertele < 22200) , 
         after==0)

mean(before$timeElapsed)/1000
mean(before$lntimeelapsed)

after <-  d %>% filter((centertele >-22200 & centertele < 22200) , 
                       after==1)


estimate <- t.test(before$timeElapsed, after$timeElapsed)$estimate
pvalue <- t.test(before$timeElapsed, after$timeElapsed)$p.value
meandiff <- estimate[[1]] - estimate[[2]]

databrazil_ <- tibble(mean=(estimate)/1000, mean_diff_sec=meandiff/1000, pvalue, sample="Brazil", treatment=c("Before", "After"))


# winner and Losers -----------------------------------------------
d <- d %>% mutate(Toxicity=ifelse(TOXICITY>.5, 1, 0)) 
# check com

d_red_dem <- d %>% 
  filter((centertele >-22200 & centertele < 22200) & 
           timetweet>1478642403)  %>% # time tweet doesn change anything
  filter(membershipAuth==1 | membershipAuth==2) %>%
  mutate(party="Haddad (Losers)")

d_red_rep <- d %>% 
  filter((centertele >-22200 & centertele < 22200) & 
           timetweet>1478642403)  %>%
  filter(membershipAuth==11)  %>%
  mutate(party="Bolsonaro (Winners)")

d_red_auth <- bind_rows(d_red_dem, d_red_rep)


# Nice discontinuity graph

cut <- cut(d_red_dem$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_red_dem$lntimeelapsed, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_red_dem$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_dem <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Announcement", "Before the Announcement")) %>% 
  mutate(party="Haddad (Losers)", 
         minute=minute_conv(margin)/60)

cut <- cut(d_red_rep$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_red_rep$lntimeelapsed, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_red_rep$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_rep <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Announcement", "Before the Announcement")) %>%
  mutate(party="Bolsonaro (Winners)", minute=minute_conv(margin)/60) 


# bind 

data <- bind_rows(data_dem, data_rep)
data <- data %>% 
  mutate(minunte=minute_conv(margin))


# Relevel

data <- data %>% 
  mutate(party=as_factor(party), 
         party=fct_relevel(party, 
                           "Haddad (Losers)", "Bolsonaro (Winners)"))

# Color 

pal <- RColorBrewer::brewer.pal(n=5, "RdBu")

# winners

ggplot() +
  geom_point(data = data_rep, aes(minute, y, fill=treat, color=treat),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_rep,
              aes(y=y, minute, 
                  group=treat), color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Winners") +
  xlab("Six hours window before and after the election is called") +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  ylab("Time-to-retweet") + 
  coord_cartesian(ylim=c(5,10)) + 
  scale_color_manual(name="", values=c(pal[5], pal[1])) +
  scale_fill_manual(name="", values=c(pal[5], pal[1])) +
  theme_minimal(base_size = 22) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=22), 
        legend.key.size = unit(1.5, "cm"))

# losers

ggplot() +
  geom_point(data = data_dem, aes(minute, y, fill=treat, color=treat),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_dem,
              aes(y=y, minute, 
                  group=treat), color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Loosers") +
  xlab("Six hours window before and after the election is called") +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  ylab("Time-to-retweet") + 
  coord_cartesian(ylim=c(5,10)) + 
  scale_color_manual(name="", values=c(pal[5], pal[1])) +
  scale_fill_manual(name="", values=c(pal[5], pal[1])) +
  theme_minimal(base_size = 22) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=22), 
        legend.key.size = unit(1.5, "cm"))


# Together

data <- data %>% mutate(party=fct_rev(party))


ggplot() +
  geom_point(data = data, aes(minute, y, fill=party, color=party),
             na.rm=T,size=4, shape=21, alpha=1)+
  stat_smooth(data = data_rep,
              aes(y=y, minute, 
                  group=treat), fill=pal[1], color="black")  +
  stat_smooth(data = data_dem,
              aes(y=y, minute, 
                  group=treat), fill=pal[5], color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Brazil: Government x Opposition") +
  xlab("Six hours window before and after the election is called") +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  ylab("Time-to-retweet") + 
  coord_cartesian(ylim=c(5,10)) + 
  scale_color_manual(name="", values=c(pal[1], pal[5])) +
  scale_fill_manual(name="", values=c(pal[1], pal[5])) +
  theme_minimal(base_size = 22) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=22), 
        legend.key.size = unit(1.5, "cm"))




# Toxicity ----------------------------------------------------------------

# Community Together
cut <- cut(d_red_dem$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_red_dem$Toxicity, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_red_dem$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_dem <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Announcement", "Before the Announcement")) %>% 
  mutate(party="Haddad (Losers)", 
         minute=minute_conv(margin)/60)

cut <- cut(d_red_rep$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_red_rep$Toxicity, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_red_rep$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_rep <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Announcement", "Before the Announcement")) %>%
  mutate(party="Bolsonaro (Winners)", minute=minute_conv(margin)/60) 



data <- bind_rows(data_dem, data_rep)

data <- data %>% 
  mutate(minunte=minute_conv(margin))

# Relevel
data <- data %>% 
  mutate(party=as_factor(party), 
         party_labels=fct_relevel(party, "Haddad (Losers)", "Bolsonaro (Winners)")) # double check this

levels(data$party_labels)

# Within the bandwidth

pal <- RColorBrewer::brewer.pal(n=5, "RdBu")

ggplot(data=data) +
  geom_point(data = data, aes(minute*1000, y, fill=party, color=party),
             na.rm=T,size=4, shape=21, alpha=1)+
  stat_smooth(data = data_rep,
              aes(y=y, minute*1000, 
                  group=treat), fill=pal[1], color="black")  +
  stat_smooth(data = data_dem,
              aes(y=y, minute*1000, 
                  group=treat), fill=pal[5], color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Brazil: Government x Opposition") +
  xlab("Six hours window before and after the election is called") +
  geom_vline(xintercept=0, linetype="dashed", size=1) +
  ylab("Toxicity Scores") + 
  scale_color_manual(name="", values=c(pal[5], pal[1])) +
  scale_fill_manual(name="", values=c(pal[5], pal[1])) +
  theme_minimal(base_size = 22) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title=element_text(size=22), 
        legend.key.size = unit(1.5, "cm")) +
  ylim(0, 0.3)





