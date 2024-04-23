library(ggrepel)
library(tidyverse)
library(readxl)
library(viridis)
#### For some reason writing the d dataset from paper_coding_2_analysis.Rmd
#### to utf-8 and then reading it in here as a utf-8 file didn't work :(

#### So I just make it again here because that works
# get consensus coding data
d = read_excel("data/ideophones_coded.xlsx") |> arrange(filename)

# add guessability scores from the Collabra and Language studies.

d.scores = read_excel("data/ideophones_guessability.xlsx") |>
  dplyr::select(-category)
d <- left_join(d,d.scores,by=c("ideophone","language","study" = "paper"))


# add Z score to make scores more comparable in plots across studies
d <- d |>
  group_by(study) |>
  mutate(score_z = scale(score,center=T,scale=T))

# get ratings data
d.ratings <- read_xlsx("data\\ideophones_rated_means.xlsx") |>
  dplyr::select(-category,-list,-item)
d <- left_join(d,d.ratings,by=c("filename","study","language"))

# add z score for ratings
d$rating_z <- scale(d$rating,center=T,scale=T)

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
  facet_wrap(~category)+
  scale_colour_viridis(option="plasma")+
  theme_minimal()

## Compute correlations between guesses and ratings

sound <- dat|>filter(category=="Sound")
cor.test(sound$score_z,sound$rating_z)
motion <- dat|>filter(category=="Motion")
cor.test(motion$score_z,motion$rating_z)
shape <- dat|>filter(category=="Shape")
cor.test(shape$score_z,shape$rating_z)
texture <- dat|>filter(category=="Texture")
cor.test(texture$score_z,texture$rating_z)
colour <- dat|>filter(category=="ColorVisual")
cor.test(colour$score_z,colour$rating_z)

## Looking at the cumulative iconicity measures by category

d|>
  group_by(category)|>
  summarise(modality=mean(C_modality),
            aspect_iterative=mean(C_iterative),
            irregular=mean(C_irregular),
            length_closure=mean(C_closure),
            length_punctual=mean(C_punctual),
            length_long=mean(C_long),
            magnitude_voice=mean(C_weight_voice),
            magnitude_vowel=mean(C_weight_vowel),
            magnitude_tone=mean(C_weight_tone
                             ))|>
  write_tsv("data/mean_cumulative_iconicity_feats.tsv")

# correlations between cumulative iconicity and ratings/guesses, by domain 

cor.test(sound$score_z,sound$C_cumulative)
cor.test(sound$rating_z,sound$C_cumulative)
cor.test(motion$score_z,motion$C_cumulative)
cor.test(motion$rating_z,motion$C_cumulative)
cor.test(colour$score_z,colour$C_cumulative)
cor.test(colour$rating_z,colour$C_cumulative)


# Add info on ideophones of note here

interesting <- c("berabera","gblogblogblo","choki choki","boo boo","gokugoku","kpa","tòlontòlontòlon",
                 "kpótókpótó","gbóvúú",
                 "slʔẽẽk",
                 "gayagaya","blʔoo::k","slʔẽẽk","giarigiari")

interesting_meaning <- c("lighter brown, pale-coloured")
                 
dat|>
  mutate(interest=ifelse(ideophone %in% interesting|meaning %in% interesting_meaning,ideophone,NA))->dat

dat|>
  filter(category=="Sound")|>
  ggplot(aes(x=rating_z,y=logodds,size=C_cumulative,color=C_cumulative,label=interest))+
  geom_point()+scale_fill_viridis(option="plasma")+
  scale_colour_viridis(option="plasma")+theme_tufte()+
  scale_size_continuous(limits=c(1, 4), breaks=seq(1, 4, by=1))+
  guides(color= guide_legend(), size=guide_legend())+
  labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity",color="Cumulative iconicity")+
  geom_text_repel(size=4)

dat|>
  filter(category=="ColorVisual")|>
  ggplot(aes(x=rating_z,y=logodds,size=C_cumulative,color=C_cumulative,label=interest))+
  geom_point()+scale_fill_viridis(option="plasma")+
  scale_colour_viridis(option="plasma",limits=c(1, 4), breaks=seq(1, 4, by=1))+theme_tufte()+
  scale_size_continuous(limits=c(1, 4), breaks=seq(1, 4, by=1))+
  guides(color= guide_legend(), size=guide_legend())+
  labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity",color="Cumulative iconicity")+
  geom_text_repel(size=4)
