# ------------------------------------------------------------------------------- #
# Paper: Winning
# Authors: Calvo, Weisboard, Aruguete and Ventura
# Last update: October 15, 2022
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

# Functions ---------------------------------------------------------------

minute_conv<-function(x){
  (x/1000)/60
}


# Open the data -----------------------------------------------------------

# Here researchers need to hydrate the tweet ids provided by in the github repo

d <- read_csv("data_brazil_hydrated.csv")

# Process the data ----------------------------------------------------------

d <- d %>% 
      mutate(
        timetweet=dateTweet*1000, 
        timeRT=dateRT*1000,
        bolsotele=1538949906*1000, 
        centertele=timeRT-bolsotele,
        centertele_h=minute_conv(centertele)/60,
        lntimeelapsed=log(timeElapsed+1), 
        after=ifelse(centertele>0, "1", "0")) %>%
    mutate_at(vars(friendsT, friendsRT, followersRT, followersT),
              ~ as.numeric(as.character(.))) %>% 
    mutate_at(vars(friendsT, friendsRT, followersRT, followersT), 
              list(ln=~log(.x + 1))) # cool trick with mutate_at


d <- d %>% mutate(importante=followersRT-followersT)



# Overall Adjudication ----------------------------------------------------
before <- d %>% 
            filter((centertele >-22200200 & centertele < 22200200) , 
                        after==0, 
               timetweet>1478642403)

after <-  d %>% filter((centertele >-22200200 & centertele < 22200200) , 
                       after==1, 
                       timetweet>1478642403)


estimate <- t.test(before$timeElapsed, after$timeElapsed)$estimate
pvalue <- t.test(before$timeElapsed, after$timeElapsed)$p.value
meandiff <- estimate[[1]] - estimate[[2]]

databrazil <- tibble(mean=(estimate)/1000, mean_diff_sec=meandiff/1000, pvalue, sample="Brazil", treatment=c("Before", "After"))


# Winner and Losers -----------------------------------------------

d_red_dem <- d %>% 
  filter((centertele >-22200200 & centertele < 22200200) & 
           timetweet>1478642403)  %>% # time tweet doesn change anything
          filter(membershipAuth==3) %>%
    mutate(party="Haddad (Losers)")

d_red_rep <- d %>% 
  filter((centertele >-22200200 & centertele < 22200200) & 
           timetweet>1478642403)  %>%
  filter(membershipAuth==1)  %>%
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

# Bolsonaro: winners

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


# Haddad: losers

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


# Plot both Together

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




# Authorities ---------------------------------------------------

d_dem_au <- d %>% 
  filter((centertele >-22200200 & centertele < 22200200) & 
           timetweet>1478642403)  %>% # time tweet doesn change anything
  filter(membershipAuth==3) %>%
  mutate(party="Haddad", 
         auth=ifelse(followersRT_ln>10, "High-Authority (Democrats)", 
                                        "Low Authority (Democrats)"))


d_dem <- d_dem_au %>% split(.$auth)

d_red_au <- d %>% 
  filter((centertele >-22200200 & centertele < 22200200) & 
           timetweet>1478642403)  %>%
  filter(membershipAuth==1)  %>%
  mutate(party="Bolso", 
         auth=ifelse(followersRT_ln>10, "High-Authority (Republicans)", 
                     "Low-Authority (Republicans)"))


# Split
d_rep <- d_red_au %>% split(.$auth)
d_dem <- d_dem_au %>% split(.$auth)

# Bind
d_auth <- map(1:2, ~ bind_rows(d_rep[[.x]], d_dem[[1]]))



# Nice discontinuity graph

# Losers
cut <- cut(d_dem[[1]]$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_dem[[1]]$lntimeelapsed, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_dem[[1]]$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_dem_high <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Call", "Before the Call")) %>% 
  mutate(party="Haddad (Losers)", 
         minute=minute_conv(margin)/60)

cut <- cut(d_dem[[2]]$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_dem[[2]]$lntimeelapsed, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_dem[[2]]$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_dem_low <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Call", "Before the Call")) %>% 
  mutate(party="Haddad (Losers)", 
         minute=minute_conv(margin)/60)


# Winners
cut <- cut(d_rep[[1]]$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_rep[[1]]$lntimeelapsed, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_rep[[1]]$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_rep_high <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Call", "Before the Call")) %>% 
  mutate(party="Bolsonaro (Winners)", 
         minute=minute_conv(margin)/60)

cut <- cut(d_rep[[2]]$centertele,100, include.lowest = TRUE)
tmp <- aggregate(d_rep[[2]]$lntimeelapsed, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((d_rep[[2]]$centertele), by=list(cut = cut), FUN=mean, na.rm=T)
data_rep_low <- data.frame(margin = tmp1$x, y = tmp$x) %>% 
  mutate(treat=ifelse(margin>0, "After the Call", "Before the Call")) %>% 
  mutate(party="Bolsonaro (Winners)", 
         minute=minute_conv(margin)/60)


# Within the bandwidth

pal <- RColorBrewer::brewer.pal(n=5, "RdBu")

# High authorities losers

ggplot() +
  geom_point(data = data_dem_high, aes(minute, y, fill=treat, color=treat),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_dem_high,
              aes(y=y, minute, 
                  group=treat), color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("High Authority (Losers)") +
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




# Low authorities losers

ggplot() +
  geom_point(data = data_dem_low, aes(minute, y, fill=treat, color=treat),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_dem_low,
              aes(y=y, minute, 
                  group=treat), color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Low Authority (Losers)") +
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



# High authorities winners

ggplot() +
  geom_point(data = data_rep_high, aes(minute, y, fill=treat, color=treat),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_rep_high,
              aes(y=y, minute, 
                  group=treat), color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("High Authority (Winners)") +
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


# Low authorities losers

ggplot() +
  geom_point(data = data_rep_low, aes(minute, y, fill=treat, color=treat),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_rep_low,
              aes(y=y, minute, 
                  group=treat), color="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Low Authority (Winners)") +
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

data_high <- bind_rows(data_dem_high, data_rep_high)

data_high <- data_high %>% 
  mutate(party=as_factor(party), 
         party=fct_relevel(party, "Bolsonaro (Winners)", 
                                  "Haddad (Losers)"))

ggplot() +
  geom_point(data = data_high, aes(minute, y, fill=party, color=party),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_rep_high,
              aes(y=y, minute, 
                  group=treat), fill=pal[1], color="black")  +
  stat_smooth(data = data_dem_high,
              aes(y=y,minute, 
                  group=treat), fill=pal[5], color="black")  +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Brazil: High Authority") +
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



# Together

data_low <- bind_rows(data_dem_low, data_rep_low)

data_low <- data_low %>% 
  mutate(party=as_factor(party), 
         party=fct_relevel(party, "Bolsonaro (Winners)", 
                                  "Haddad (Losers)"))

ggplot() +
  geom_point(data = data_low, aes(minute, y, fill=party, color=party),
             na.rm=T,size=4, shape=21, alpha=1) +
  stat_smooth(data = data_rep_low,
              aes(y=y, minute, 
                  group=treat), fill=pal[1], color="black")  +
  stat_smooth(data = data_dem_low,
              aes(y=y, minute, 
                  group=treat), fill=pal[5], color="black")  +
  geom_hline(yintercept=0, linetype="dotted") +
  ggtitle("Brazil: Low Authority") +
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



# Estimating the models.  -------------------------------------------------

# Function to extract coefficients


extract <- function(mod,outcome) {
  out <- tibble(outcome=outcome, 
                    h=mod$bws[1],
                    tau=mod$Estimate[2],
                    PVALrb=mod$pv[3], 
                    CIlrb=mod$ci[3],
                    CIurb=mod$ci[6], 
                    error=0, 
                    N = mod$Nh[[1]] + mod$Nh[[2]]) }


#Winners vs losers
model1 <- d %>% 
    filter((centertele >-22200200 & centertele < 22200200) & 
             timetweet>1478642403) %>%
    mutate(condition=ifelse(membershipAuth==1, "Winners", 
                      ifelse(membershipAuth==3, "Losers", 
                        NA))) %>%
    split(.$condition) %>%
    map(., ~rdrobust(.x$lntimeelapsed, .x$centertele))
  
res_membership <- map(1:length(model1), ~ extract(model1[[.x]], outcome="Time-to-Retweet") %>% 
                   mutate(Condition=names(model1)[[.x]], 
                          Model="Membership")) %>%
      bind_rows()      


# Winner auth

model2 <- d %>% 
  filter((centertele >-22200200 & centertele < 22200200),  
           timetweet>1478642403, membershipAuth==1 ) %>%
  mutate(condition=ifelse(followersRT_ln<10, "Winners_Low_Authority", 
                          ifelse(followersRT_ln>10, "Winners_High_Authority", 
                                 NA))) %>%
  split(.$condition) %>%
  map(., ~rdrobust(.x$lntimeelapsed, .x$centertele))


summary(model2$Winners_Low_Authorithy)

res_dem_auth <- map(1:length(model2), ~ extract(model2[[.x]], outcome="Time-to-Retweet") %>% 
                     mutate(Condition=names(model2)[[.x]], 
                            Model="Authority Bolsonaristas")) %>%
  bind_rows()      



# losers auth
model3 <- d %>% 
  filter((centertele >-22200200 & centertele < 22200200),  
         timetweet>1478642403, membershipAuth==3 ) %>%
  mutate(condition=ifelse(followersRT_ln<10, "Losers_Low_Authority", 
                          ifelse(followersRT_ln>10, "Losers_High_Authority", 
                                 NA))) %>%
  split(.$condition) %>%
  map(., ~rdrobust(.x$lntimeelapsed, .x$centertele))

res_rep_auth <- map(1:length(model3), ~ extract(model3[[.x]], outcome="Time-to-Retweet") %>% 
                      mutate(Condition=names(model3)[[.x]], 
                             Model="Authority Petistas")) %>%
  bind_rows()      

# Combine the results 
results_tr <- bind_rows(res_membership, res_rep_auth, res_dem_auth) %>%
  mutate(Condition=str_replace_all(Condition, "_", " "), 
         conditionft = as_factor(Condition), 
         conditionft= fct_relevel(conditionft,
                                  "Winners", "Winners High Authority", "Winners Low Authority", 
                                  "Losers", "Losers High Authority", "Losers Low Authority"), 
         conditionft = fct_rev(conditionft))


ggplot(results_tr, aes(x=conditionft, y=tau, ymin=CIlrb, ymax=CIurb))  +
  geom_pointrange(color="steelblue", fill="steelblue", shape=21, size=1) + 
  coord_flip() +
  labs(x="", y="Adjudication Effect", 
  title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="red") +
  ylim(c(-3, 1)) +
  theme_gray() + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title.x  = element_text(size=22, face="italic", hjust=1), 
        axis.text.y =  element_text(size=22))  # Again, mostly negative



# New graph 
results_tr <- results_tr %>% 
  mutate_if(is.numeric, ~round(.x, digits=2)) %>%
  mutate(h_new=round(minute_conv(h), digits=2), 
         label=paste0("Tau = ", tau, "; N =", N , "; Bandwidth =", h_new))


library(ggrepel)

myfont <- "Palatino Linotype"

ggplot(results_tr, aes(x=conditionft, y=tau, ymin=CIlrb, ymax=CIurb, label=label))  +
  coord_flip() +
  geom_label_repel(
    fontface = "italic", fill="white", 
    color = "black",
    box.padding = unit(0.35, "lines"),
    segment.color = "gray", 
    nudge_x = .3,
    nudge_y = -.1,
    arrow = arrow(length = unit(0.01, 'npc')),
    point.padding = unit(0.01, 'npc'), size=6)   +
  geom_pointrange(color="black", fill="white", shape=21, size=2) +
  labs(x="", y="Adjudication Effect", 
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        axis.title = element_text(size=22, hjust=1, family=myfont), 
        axis.text.y = element_text(size=22, hjust=1, family=myfont), 
        axis.text.x = element_text(size=16, hjust=1, family=myfont)) +
  ylim(-3, 1) 



# extract parameters for table 1

function_parameters <- function(model_winner, model_loser, cat, case) {
  
  drift <- model_winner$beta_p_l[[1]] - model_loser$beta_p_l[[1]]  
  adj_premium <- model_winner$beta_p_r[[1]] - model_loser$beta_p_r[[1]]
  
  tibble(case=case, category=cat, drift=drift, adj_premium=adj_premium,)
  
}


# Community

d_w_l <- function_parameters(model1$Winners, model1$Losers, "Winners x Losers", "Brazil")
d_high <- function_parameters(model2$Winners_High_Authority,model3$Losers_High_Authority, "High Authority", "Brazil")
d_low <- function_parameters(model2$Winners_Low_Authority,model3$Losers_Low_Authority, "Low Authority", "Brazil")

bind_rows(d_w_l, d_high, d_low)

