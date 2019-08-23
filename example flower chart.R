# Create flower charts
# Created by Eliot McKinley (@etmckinley) and Cheuk Hei Ho (@tacticsplatform)
# August 22, 2019

#load Libraries
library(tidyverse)
library(ggforce)
library(ggnewscale)
library(randomNames)  # for generating random names
library(randomcoloR) # for generating random colors

#Create some fake data

player.data=data.frame(
  Player = randomNames(20, name.order="first.last", name.sep=" "),
  A = runif(20),
  B = runif(20),
  C = runif(20),
  D = runif(20),
  E = runif(20),
  F = runif(20),
  G = runif(20),
  H = runif(20)
)

#generate random colors
colors = randomColor(8)

#fill in with whatever color scheme you'd like
color.north = colors[1]
color.northeast = colors[2]
color.east = colors[3]
color.southeast = colors[4]
color.south = colors[5]
color.southwest = colors[6]
color.west = colors[7]
color.northwest = colors[8]

#parameters to help change petal sizes and positions if desired
pos=4.5
offset=2
width=3

#plot the flower chart
ggplot(data=player.data%>% 
               mutate(Player=fct_reorder(Player, -A))) + # this is to reorder so that facets are in order of your chosen variable ("A" in this case)
  geom_ellipse(aes(x0 = -1*pos, y0 = pos, a = 6, b = width, angle = 315*pi/180, m1 = 3, alpha=H),color="transparent", fill=color.northwest) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  geom_ellipse(aes(x0 = -1*pos-offset, y0 = 0, a = 6, b = width, angle = 180* pi/180, m1 = 3, alpha=G),color="transparent", fill=color.west) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  geom_ellipse(aes(x0 = -1*pos, y0 = -1*pos, a = 6, b = width, angle = 225*pi/180, m1 = 3, alpha=F),color="transparent", fill=color.southwest) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  geom_ellipse(aes(x0 = 0, y0 = -1*pos-offset, a = 6, b = width, angle = 270* pi/180, m1 = 3, alpha=E),color="transparent", fill=color.south) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  geom_ellipse(aes(x0 = pos, y0 = -1*pos, a = 6, b = width, angle = 135*pi/180, m1 = 3, alpha=D),color="transparent", fill=color.southeast) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  geom_ellipse(aes(x0 = pos+offset, y0 = 0, a = 6, b = width, angle =  0* pi/180, m1 = 3, alpha=C),color="transparent", fill=color.east) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  geom_ellipse(aes(x0 = pos, y0 = pos, a = 6, b = width, angle = 45*pi/180, m1 = 3, alpha=B),color="transparent", fill=color.northeast ) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  geom_ellipse(aes(x0 = 0, y0 = pos+offset, a = 6, b = width, angle = 90* pi/180, m1 = 3, alpha=A),color="transparent", fill=color.north) +
  scale_alpha_continuous(range=c(.01,1), limits=c(0,1))+
  new_scale("alpha")+
  theme_void()+
  theme(strip.text = element_text(color = "black", size=10, margin=margin(b=5)),
        legend.position = "none") +
  facet_wrap(~Player, ncol=5)+
  coord_fixed()

#plot a key for the flower chart with some parameters
alpha.set=0.95
font.size=5

ggplot() +
  geom_ellipse(aes(x0 = -1*pos, y0 = pos, a = 6, b = width, angle = 315*pi/180, m1 = 3), alpha = alpha.set,  color="transparent", fill=color.northwest) +
  annotate("curve", x = -13, y = 13, xend = -9, yend = 9, curvature = -0.3, color=color.northwest, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = -13.5, y = 13, color=color.northwest, size=font.size, label="H", hjust=1, vjust=0)+
  geom_ellipse(aes(x0 = -1*pos-offset, y0 = 0, a = 6, b = width, angle = 180* pi/180, m1 = 3), alpha= alpha.set ,color="transparent", fill=color.west) +
  annotate("curve", x = -18, y = 2, xend = -13.2, yend = 0, curvature = 0.3, color=color.west, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = -18.5, y = 2, color=color.west, size=font.size, label="G", hjust=1, vjust=0)+
  geom_ellipse(aes(x0 = -1*pos, y0 = -1*pos, a = 6, b = width, angle = 225*pi/180, m1 = 3), alpha = alpha.set,color="transparent", fill=color.southwest) +
  annotate("curve", x = -13, y = -13, xend = -9, yend = -9, curvature = 0.3, color=color.southwest, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = -13.5, y = -13.2, color=color.southwest, size=font.size, label="F", hjust=1, vjust=0)+
  geom_ellipse(aes(x0 = 0, y0 = -1*pos-offset, a = 6, b = width, angle = 270* pi/180, m1 = 3), alpha = alpha.set,color="transparent", fill=color.south) +
  annotate("curve", x = -3, y = -17, xend = 0, yend = -13, curvature = 0.3, color=color.south, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = -3.5, y = -17.5, color=color.south, size=font.size, label="E", hjust=1, vjust=0)+
  geom_ellipse(aes(x0 = pos, y0 = -1*pos, a = 6, b = width, angle = 135*pi/180, m1 = 3), alpha = alpha.set,color="transparent", fill=color.southeast) +
  annotate("curve", x = 13, y = -13, xend = 9, yend = -9, curvature = -.3, color=color.southeast, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = 13.5, y = -13.5, color=color.southeast, size=font.size, label="D", hjust=0, vjust=0)+
  geom_ellipse(aes(x0 = pos+offset, y0 = 0, a = 6, b = width, angle =  0* pi/180, m1 = 3), alpha = alpha.set,color="transparent", fill=color.east) +
  annotate("curve", x = 18, y = 2, xend = 13.2, yend = 0, curvature = -0.3, color=color.east, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = 18.5, y = 2, color=color.east, size=font.size, label="C", hjust=0, vjust=0)+
  geom_ellipse(aes(x0 = pos, y0 = pos, a = 6, b = width, angle = 45*pi/180, m1 = 3), alpha = alpha.set,color="transparent", fill=color.northeast ) +
  annotate("curve", x = 13, y = 13, xend = 9, yend = 9, curvature = 0.3, color=color.northeast, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = 13.5, y = 13, color=color.northeast, size=font.size, label="B", hjust=0, vjust=0)+
  geom_ellipse(aes(x0 = 0, y0 = pos+offset, a = 6, b = width, angle = 90* pi/180, m1 = 3), alpha = alpha.set,color="transparent", fill=color.north) +
  annotate("curve", x = 3, y = 17, xend = 0, yend = 13, curvature = 0.3, color=color.north, size=1, arrow = arrow(length = unit(0.02, "npc")))+
  annotate("text", x = 3.5, y = 17.5, color=color.north, size=font.size, label="A", hjust=0, vjust=0)+
  theme_void()+
  coord_fixed()