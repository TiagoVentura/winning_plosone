# ------------------------------------------------------------------------------- #
# Paper: Adjudication
# Author: Calvo, Weisboard,  Ventura and Aruguete
# Last update: Octobet 15
# Thoerethical Graph
# ------------------------------------------------------------------------------- #


# Packages ----------------------------------------------------------------

rm(list = ls(all=TRUE))
library(tidyverse)
library(RColorBrewer)
library(showtext)
library(ggpubr)
library(forcats)


# basics ------------------------------------------------------------------

path_tiago <- "/home/venturat/Dropbox/CalvoTiago/adjudication"
setwd(path_tiago)
output <- "/home/venturat/Dropbox/Apps/Overleaf/Event Adjudication in Social Media plos one"
#source("C:/Users/Tiago Ventura/Dropbox/artigos/Ventura Partisan Effects Rd/code/functions.r")
list.files(output)

# Fonts

font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")
font_add("Palatino", "pala.ttf")
font_add_google("Muli", "Muli")



font_families()
showtext_auto() 

myFont1 <- "Montserrat"
myFont2 <- "Roboto"
myFont3 <- "Palatino"
myFont4 <- "Muli"
# Graph -------------------------------------------------------------------

RColorBrewer::display.brewer.all()
pal=RColorBrewer::brewer.pal(9, "Spectral")

d <- tibble::tibble(label=fct_inorder(as.factor(c("Winner", "Loser", 
                                              "Adjudication Premium"))),
                    y=c(20, 20, 20), 
                    x=c(20, 20, 20))

levels(d$label)
windows()
# ggplot structure


ggplot(d) +
  
  geom_line(aes(y=y, x=x,colour=label), size=5) +
  
  # set up a theme
  
  theme_minimal() +
  
  # Draw the Lines
  
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  

  # Impose limits
  
  ylim(c(0, 8)) + xlim(0, 10) +

  # State of Dialogue

  geom_segment(aes(y=6, yend=6, x=0, xend=3), 
               line = unit(0.3, "inches"), 
               size=1) +
  
  # Information Drift : Winner
  
  geom_segment(aes(y=6, yend=3, x=3, xend=5), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[1], 
               linetype = "dashed") +

  # Information Drift : loser 
  
  geom_segment(aes(y=6, yend=5, x=3, xend=5), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[9], 
               linetype = "dashed") +
  
  # Information Drift : observable
  
  geom_segment(aes(y=6, yend=4, x=3, xend=5), 
               line = unit(0.3, "inches"), 
               size=1) +
  
  # Adjudication
  
  geom_segment(aes(y=0, yend=8, x=5, xend=5), 
               line = unit(0.3, "inches"), 
               size=1, alpha=.2) +
  
  # adjudication : Winner
  
  geom_segment(aes(y=1, yend=5.5, x=5, xend=10), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[1], 
               linetype = "dashed") +
  
  # Information Drift : loser 
  
  geom_segment(aes(y=4.2, yend=7, x=5, xend=10), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[9], 
               linetype = "dashed") +
  
  # Mark for Adjudication Premium
  
  geom_segment(aes(y=4.2, yend=1, x=5, xend=5), 
               line = unit(0.3, "inches"), 
               size=1.5, color=pal[4], 
               linetype = "solid") +
  
  geom_segment(aes(y=1, yend=1, x=5.1, xend=4.9), 
             line = unit(0.3, "inches"), 
             size=1.5, color=pal[4], 
             linetype = "solid") +
  
  geom_segment(aes(y=4.2, yend=4.2, x=5.1, xend=4.9), 
               line = unit(0.3, "inches"), 
               size=1.5, color=pal[4], 
               linetype = "solid") +
  
  # Information Drift : observable
  
  geom_segment(aes(y=2.2, yend=6.5, x=5, xend=10), 
               line = unit(0.3, "inches"), 
               size=2)  +
  
  # Labels : State of Dialogue
  annotate(geom = "text", 
         x = .5, y = 6.325, 
         label = "State of Dialogue", hjust = "left", 
         color="Black", family=myFont4, 
         size=16) +

  # Labels : Adjudication
  annotate(geom = "text", 
         x = 3, y = .5, 
         label = "Adjudication", hjust = "left", 
         color="Black", family=myFont4, 
         size=16)  +

  # Information drift
  
  geom_bracket(xmin = 3.1, xmax = 4.9, label="",
                  y.position = 6.3, tip.length = 0.01)  +
  
  # Labels : information drift
  annotate(geom = "text", 
           x = 3.2, y = 6.5, 
           label = "Information Drift", hjust = "left", 
           color="Black", family=myFont4, 
           size=16)  +
  
  
  # Add legend
  
  scale_colour_manual(name="",
                      values=c(pal[1], pal[9], pal[5])) +
  
  theme(legend.position = "bottom", 
        legend.text = element_text(size=28, 
                                   family = myFont4), 
        legend.key = element_rect(colour="gray90", fill="gray90"), 
        legend.spacing.x = unit(1.0, 'cm'), 
        axis.text  = element_blank(), 
        axis.title = element_blank())



ggsave(paste0(output, "/theory.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Second format -----------------------------------------------------------


d <- tibble::tibble(label=fct_inorder(as.factor(c("Winner", "Loser", 
                                                  "Information Drift"))),
                    y=c(20, 20, 20), 
                    x=c(20, 20, 20))

dev.off()
# ggplot structure


ggplot(d) +
  
  geom_line(aes(y=y, x=x,colour=label), size=5) +
  
  # set up a theme
  
  theme_minimal() +
  
  # Draw the Lines
  
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  
  
  # Impose limits
  
  ylim(c(0, 8)) + xlim(0, 10) +
  
  # State of Dialogue
  
  geom_segment(aes(y=6, yend=6, x=0, xend=3), 
               line = unit(0.3, "inches"), 
               size=1) +
  
  # Information Drift : Winner
  
  geom_segment(aes(y=6, yend=3, x=3, xend=5), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[1], 
               linetype = "dashed") +
  
  # Information Drift : loser 
  
  geom_segment(aes(y=6, yend=5, x=3, xend=5), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[9], 
               linetype = "dashed") +
  
  # Information Drift : observable
  
  geom_segment(aes(y=6, yend=4, x=3, xend=5), 
               line = unit(0.3, "inches"), 
               size=1) +
  
  # Adjudication
  
  geom_segment(aes(y=0, yend=8, x=5, xend=5), 
               line = unit(0.3, "inches"), 
               size=1, alpha=.2) +
  
  # adjudication : Winner
  
  geom_segment(aes(y=1, yend=5.5, x=5, xend=10), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[1], 
               linetype = "dashed") +
  
  # Information Drift : loser 
  
  geom_segment(aes(y=4.2, yend=7, x=5, xend=10), 
               line = unit(0.3, "inches"), 
               size=1, color=pal[9], 
               linetype = "dashed") +
  
  # Mark for Adjudication Premium
  
  #geom_segment(aes(y=4.2, yend=1, x=5.1, xend=5.1), 
  #             line = unit(0.3, "inches"), 
  #             size=1.5, color=pal[4], 
  #             linetype = "solid") +
  
  #geom_segment(aes(y=1, yend=1, x=5.0, xend=5.2), 
  #             line = unit(0.3, "inches"), 
  #             size=1.5, color=pal[4], 
  #             linetype = "solid") +
  
  #geom_segment(aes(y=4.2, yend=4.2, x=5.0, xend=5.2), 
  #             line = unit(0.3, "inches"), 
  #             size=1.5, color=pal[4], 
  #             linetype = "solid") +
  
  
  # Mark for drift Premium
  
  geom_segment(aes(y=5, yend=3, x=4.9, xend=4.9), 
               line = unit(0.3, "inches"), 
               size=1.5, color="darkgreen", 
               linetype = "solid") +
  
  geom_segment(aes(y=5, yend=5, x=4.8, xend=5), 
               line = unit(0.3, "inches"), 
               size=1.5, color="darkgreen", 
               linetype = "solid") +
  
  geom_segment(aes(y=3, yend=3, x=4.8, xend=5), 
               line = unit(0.3, "inches"), 
               size=1.5, color="darkgreen", 
               linetype = "solid") +
  
  
  # Information Drift : observable
  
  geom_segment(aes(y=2.2, yend=6.5, x=5, xend=10), 
               line = unit(0.3, "inches"), 
               size=2)  +
  
  # Labels : State of Dialogue
  annotate(geom = "text", 
           x = .5, y = 6.325, 
           label = "State of Dialogue", hjust = "left", 
           color="Black", family=myFont4, 
           size=24) +
  
  # Labels : Adjudication
  annotate(geom = "text", 
           x = 3, y = .5, 
           label = "Adjudication", hjust = "left", 
           color="Black", family=myFont4, 
           size=24)  +
  

  # Add legend
  
  scale_colour_manual(name="",
                      values=c(pal[1], pal[9],"darkgreen")) +
  
  theme(legend.position = "bottom", 
        legend.text = element_text(size=62, 
                                   family = myFont4), 
        legend.key = element_rect(colour="gray90", fill="gray90"), 
        legend.spacing.x = unit(1.0, 'cm'), 
        axis.text = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(family=myFont4, size=62)) +
  ylab("Latency (Time-to-Retweet)") 


ggsave(paste0(output, "/theory_new.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



