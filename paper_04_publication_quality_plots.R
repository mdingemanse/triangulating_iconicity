# publication quality figures

# Packages
list.of.packages <- c("tidyverse","readxl","extrafont",
                      "ggthemes","ggrepel","gghalves","ggbeeswarm","viridis","cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

font_import()


# Data --------------------------------------------------------------------


d <- readr::read_csv("data/ideophones_coded_guessed_rated.csv")

d.ratings.full <- readr::read_csv("data/ideophones_rated.csv")


# make a copy dp of the data in which we convert some measures to factors for easy plotting & faceting
tofactors <- paste(c("language|category|study|group",names(d[grep('C_',names(d))])),collapse = "|")
dp <- d
dp[,grep(tofactors,names(dp))] <- lapply(dp[,grep(tofactors,names(dp))], as.factor)


# Ggplot theme ------------------------------------------------------------

library(extrafont)
loadfonts(device="win")

old <- theme_set(theme_tufte())
theme_set(old)
theme_update(text = element_text(family="Gill Sans MT"))


# Figure 3 ----------------------------------------------------------------


# panel A: scores by study and cumulative iconicity
pA <- ggplot(data=dp, aes(x=study,y=score,fill=C_cumulative,colour=C_cumulative)) +
  ylim(0,1) + theme(legend.position="none") +
  labs(title="Guessability by study",
       x="study",
       y="guessability (% correct)") +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  stat_summary(fun.y=median,geom="point",size=8,shape=21,stroke=1,fill="white") +
  geom_dotplot(stackgroups=T,dotsize=1.5,binwidth=0.01,binaxis="y",stackdir = "center") +
  NULL

pB <- ggplot(data=dp, aes(x=C_cumulative,y=score_z,colour=C_cumulative)) +
  theme(legend.position="none") +
  labs(title="Guessability (both studies) by cumulative iconicity",
       x="cumulative iconicity",
       y=expression('guessability ('~italic(z)~')')) +
  geom_half_boxplot() +
  geom_half_point() +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  NULL

plot_grid(pA,pB,labels=c("A","B"),label_size=14,rel_widths = c(1.4,2))
ggsave("figures/pub_fig3-panelAB.png",height=4,width=9,bg="white",dpi=600)
ggsave("figures/pub_fig3-panelAB.pdf",height=4,width=9,bg="white")

# variant with shadow for use in paper
# panel A: scores by study and cumulative iconicity
pA <- ggplot(data=dp, aes(x=study,y=score,fill=C_cumulative,colour=C_cumulative)) +
  ylim(0,1) + theme(legend.position="none") + 
  labs(title="Guessability by study",
       x="study",
       y="guessability (% correct)") +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  stat_summary(fun.y=median,geom="point",size=8,shape=21,stroke=1,fill="white") +
  ggfx::with_shadow(
    geom_dotplot(stackgroups=T,dotsize=1.5,binwidth=0.01,binaxis="y",stackdir = "center"),
    sigma=0,
    x_offset=1.6,
    y_offset=1.6,
    colour="black") +
  NULL

pB <- ggplot(data=dp, aes(x=C_cumulative,y=score_z,colour=C_cumulative)) +
  theme(legend.position="none") +
  labs(title="Guessability (both studies) by cumulative iconicity",
       x="cumulative iconicity",
       y=expression('guessability ('~italic(z)~')')) +
  geom_half_boxplot() +
  ggfx::with_shadow(
    geom_half_point(),
    sigma=0,
    x_offset=1.6,
    y_offset=1.6,
    colour="black") +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  NULL

plot_grid(pA,pB,labels=c("A","B"),label_size=14,rel_widths = c(1.4,2))
ggsave("figures/pub_fig3-panelAB-shadow.png",height=3,width=9,bg="white",dpi=600)
ggsave("figures/pub_fig3-panelAB-shadow.pdf",height=3,width=9,bg="white")


# Figure 4 ----------------------------------------------------------------



#panel A: ratings by study
pA <- d |>  ggplot(aes(x=score_z,y=rating)) + 
  theme(legend.position = c(0.2,0.9),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("white","black")) +
  geom_point(aes(fill=study),
             position="jitter",
             shape=21,
             colour="black") +
  geom_smooth(method=loess,colour="black",alpha=0.5) +
  xlab("guessability (z)") + ylab("rating") +
  NULL

#panel B: ratings by semantic domain
pB <- d |>
  filter(category != "Other") |>
  ggplot(aes(x=reorder(category,-rating),y=rating,color=category)) +
  theme(legend.position = c(0.2,0.9),
        legend.title = element_blank()) +
  geom_half_boxplot(aes(middle=mean(rating)),show.legend=F) +
  geom_half_point(show.legend=F) +
  scale_colour_viridis_d(option="D") +
  xlab("category") + scale_x_discrete(labels = c("Sound","Motion","Shape", 
                                                 "Texture","Colour/Visual"))

plot_grid(pA,pB,labels=c("A","B"),label_size=14,rel_widths = c(1.6,2))
ggsave("figures/pub_fig4-panel_ratings.png",height=4,width=9,bg="white",dpi=600)
ggsave("figures/pub_fig4-panel_ratings.pdf",height=4,width=9,bg="white")

# variant with shadow for possible use in paper

#panel A: ratings by study
pA <- d |>  ggplot(aes(x=score_z,y=rating)) +   
  theme(legend.position = c(0.2,0.9),
        legend.title = element_blank()) +
  scale_fill_manual(values=c("white","black")) +
  geom_point(aes(fill=study),
             position="jitter",
             shape=21,
             colour="black") +
  geom_smooth(method=loess,colour="black",alpha=0.5) +
  xlab("guessability (z)") + ylab("rating") +
  NULL

#panel B: ratings by semantic domain
pB <- d |>
  filter(category != "Other") |>
  ggplot(aes(x=reorder(category,-rating),y=rating,color=category)) +
  geom_half_boxplot(aes(middle=mean(rating)),show.legend=F) +
  ggfx::with_shadow(
    geom_half_point(show.legend=F),
    sigma=0,
    x_offset=1.6,
    y_offset=1.6,
    colour="black"
  ) +
  scale_colour_viridis_d(option="D") +
  xlab("category") + scale_x_discrete(labels = c("Sound","Motion","Shape", 
                                                 "Texture","Colour/Visual"))

plot_grid(pA,pB,labels=c("A","B"),label_size=14,rel_widths = c(1.4,2))
ggsave("figures/pub_fig4-panel_ratings-shadow.png",height=3,width=9,bg="white",dpi=600)
ggsave("figures/pub_fig4-panel_ratings-shadow.pdf",height=3,width=9,bg="white")


# Figure 5 ----------------------------------------------------------------



# panel A: ratings by study and cumulative iconicity
pA <- ggplot(data=dp, 
             aes(x="",
                 y=rating,
                 colour=C_cumulative,
                 fill=C_cumulative)) +
  ylim(1,5) + theme(legend.position="none") + 
  labs(title="Iconicity ratings for 239 ideophones",
       x="all ideophones",
       y="mean iconicity rating") +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  geom_beeswarm(cex=2.5) +
  NULL

pB <- ggplot(data=dp, 
             aes(x=C_cumulative,
                 y=rating,
                 colour=C_cumulative,
                 fill=C_cumulative)) +
  ylim(1,5) + theme(legend.position="none") +
  labs(title="... by independently coded structural correspondences",
       x="cumulative iconicity",
       y="mean iconicity rating") +
  geom_half_boxplot(fill=NA) +
  geom_half_point() +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  NULL

plot_grid(pA,pB,labels=c("A","B"),label_size=14,rel_widths = c(1.4,2))
ggsave("figures/pub_fig5-panelAB_ratings.png",height=4,width=9,bg="white",dpi=600)
ggsave("figures/pub_fig5-panelAB_ratings.pdf",height=4,width=9,bg="white")




# variant with shadow 

pA <- ggplot(data=dp, 
             aes(x="",
                 y=rating,
                 colour=C_cumulative,
                 fill=C_cumulative)) +
  ylim(1,5) + theme(legend.position="none") + 
  labs(title="Iconicity ratings for 239 ideophones",
       x="all ideophones",
       y="mean iconicity rating") +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  ggfx::with_shadow(
    geom_beeswarm(cex=2.5),
    sigma=0,
    x_offset=1.4,
    y_offset=1.4,
    colour="black"
  ) +
  NULL

pB <- ggplot(data=dp, 
             aes(x=C_cumulative,
                 y=rating)) +
  ylim(1,5) + theme(legend.position="none") +
  labs(title="... by independently coded structural correspondences",
       x="cumulative iconicity",
       y="mean iconicity rating") +
  geom_half_boxplot(aes(colour=C_cumulative)) +
  ggfx::with_shadow(
    geom_half_point(aes(colour=C_cumulative,fill=C_cumulative),
                    shape=21),
    sigma=0,
    x_offset=1.6,
    y_offset=1.6,
    colour="black") +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  NULL

plot_grid(pA,pB,labels=c("A","B"),label_size=14,rel_widths = c(1.4,2))
ggsave("figures/pub_fig5-panelAB_ratings_shadow.png",height=4,width=9,bg="white",dpi=600)
ggsave("figures/pub_fig5-panelAB_ratings_shadow.pdf",height=4,width=9,bg="white")



# Figure 6 ----------------------------------------------------------------



d|>
  select(-C_length,-C_aspect,-C_magnitude,-C_weight) |>
  pivot_longer(C_modality:C_weight_tone,names_to="correlate",values_to="present") |>
  filter(present!=0) |>
  mutate(features="features") |>
  group_by(ideophone) |>
  pivot_wider(names_from="features",values_from="correlate",values_fn=list) |>
  mutate(label=paste(ideophone,meaning,features,sep="\n"))->dat

# Overview plot

dat$category <- factor(dat$category,levels=c("Sound","Motion","Shape","Texture","ColorVisual","Other"))
dat |>
  ggplot(aes(x=rating_z,y=score_z,color=C_cumulative))+geom_point()+
  labs(x="rating (z)",y="guessabing accuracy (z)",colour="cumulative iconicity")+
  scale_colour_viridis(option="plasma",begin=0.3,end=0.9) +
  facet_wrap(~category)
ggsave("figures/pub_fig6-z_by_domain.png",height=4,width=9,bg="white",dpi=600)


dat |>
  dplyr::filter(category != "Other") |>
  ggplot(aes(x=rating_z,y=score_z,color=C_cumulative)) +
  theme(panel.grid.major = element_line(colour="#cccccc",linewidth = 0.2),
        panel.spacing.x = unit(1.5,"lines"),
#        legend.position = c(0.85,0.2),
        legend.title = element_text(size=10),
        legend.key.size = unit(12,"pt")) +
  geom_point() +
  ggfx::with_shadow(
    geom_point(aes(colour=C_cumulative,fill=C_cumulative),
                    shape=21),
    sigma=0,
    x_offset=1.6,
    y_offset=1.6,
    colour="black") +
  labs(x="rating (z)",y="guessabing accuracy (z)",colour="cumulative iconicity",fill="cumulative iconicity")+
  scale_colour_viridis(option="plasma",begin=0.3,end=0.9) +
  scale_fill_viridis(option="plasma",begin=0.3,end=0.9) +
  facet_wrap(~category,nrow=1)
ggsave("figures/pub_fig6-z_by_domain-shadow.png",height=2,width=9,bg="white",dpi=600)

