#### For some reason writing the d dataset from paper_coding_2_analysis.Rmd
#### to utf-8 and then reading it in here as a utf-8 file didn't work :(

#### So I just make it again here because that works
# get consensus coding data
d = read_excel("data\\ideophones_coded.xlsx") %>% arrange(filename)

# add guessability scores from the Collabra and Language studies.

d.scores = read_excel("data\\ideophones_guessability.xlsx") %>%
  dplyr::select(-category)
d <- left_join(d,d.scores,by=c("ideophone","language","study" = "paper"))

# add logodds (for when we run stats: it's more sensible to predict against logodds than raw proportion correct)
d <- d %>%
  group_by(study) %>%
  mutate(logodds = probitlink(score))

# add Z score to make scores more comparable in plots across studies
d <- d %>%
  group_by(study) %>%
  mutate(score_z = scale(score,center=T,scale=T))

# get ratings data
d.ratings <- read_xlsx("data\\ideophones_rated_means.xlsx") %>%
  dplyr::select(-category,-list,-item)
d <- left_join(d,d.ratings,by=c("filename","study","language"))

# add z score for ratings
d$rating_z <- scale(d$rating,center=T,scale=T)

d%>%
  select(-C_length,-C_aspect,-C_magnitude,-C_weight)%>%
  pivot_longer(C_modality:C_weight_tone,names_to="correlate",values_to="present")%>%
  filter(present!=0)%>%
  mutate(features="features")%>%
  group_by(ideophone)%>%
  pivot_wider(names_from="features",values_from="correlate",values_fn=list)%>%
  mutate(label=paste(ideophone,meaning,features,sep="\n"))->dat

# Overview plot
dat%>%
  ggplot(aes(x=rating_z,y=logodds,size=C_cumulative))+geom_point()+
  labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity")+
  facet_wrap(~category)+theme_tufte()


dat%>%
  filter(category=="Sound")%>%
  ggplot(aes(x=rating_z,y=logodds,size=C_cumulative,color=C_cumulative))+geom_point()+scale_fill_viridis(option="plasma")+scale_colour_viridis(option="plasma")+theme_tufte()+ scale_size_continuous(limits=c(1, 4), breaks=seq(1, 4, by=1))+guides(color= guide_legend(), size=guide_legend())+labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity",color="Cumulative iconicity")
