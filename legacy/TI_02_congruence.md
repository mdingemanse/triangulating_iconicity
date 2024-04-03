Triangulating iconicity: From structure mapping to guessability
================
Mark Dingemanse & Stella Punselie
Updated 2020-06-26

Code notebook for a study of the relation between linguistically
informed iconicity coding and experimentally collected guessability
scores.

## Setup

``` r
# Packages
list.of.packages <- c("tidyverse","readxl","writexl","ggthemes","gghalves","ggbeeswarm","viridis","lme4","VGAM")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# useful functions
`%notin%` <- function(x,y) !(x %in% y) 
mean.na <- function(x) mean(x, na.rm = T)
sd.na <- function(x) sd(x, na.rm = T)

# custom functions
source("TI_functions.R")
```

## Data

Here we load the ground truth version of the coded data and add the
independently collected guessability scores from the 2016 Collabra and
Language papers.

``` r
# get coding data
d = read_excel("data\\ideophones_coded.xlsx") %>% arrange(filename)

coding_categories <- d %>% dplyr::select(matches('_'),-matches('notes|meaning')) %>% names()

# add guessability scores from the Collabra and Language studies.

d.scores = read_excel("data\\ideophones_guessability.xlsx") %>%
  dplyr::select(-category)
d <- left_join(d,d.scores,by=c("ideophone","language","study" = "paper"))

# add logodds (for when we run stats: it's more sensible to predict against logodds than raw proportion correct)
d <- d %>%
  group_by(study) %>%
  mutate(logodds = probitlink(score))

# add Z score to account for differences in base success rate of Collabra and Language Studies
d <- d %>%
  group_by(study) %>%
  mutate(score_z = scale(score,center=T,scale=T))
```

We add congruency measures that link F and M elements.

  - TO DISCUSS: We need to think about the evidential value of each of
    the congruency measures. Most apply to many words, which is good. If
    some of them only capture a few (as `C_durative` and `C_weight_tone`
    seem to do), they are not so interesting because they don’t really
    capture recurring iconic mappings in the data.
  - Discussed: This made us realize a need to name `M_durative` to
    `M_long`, covering extent not just in time but also in space (a very
    concrete kind of mapping for `F_finallength`.

<!-- end list -->

``` r
# using ifelse() statements that essentially say, if these conditions are true, use 1, otherwise 0
d <- d %>%
  mutate(C_modality = ifelse(M_sound == 1,1,0),
         C_iterative = ifelse(F_redup == 1 & M_distribution == 1,1,0),
         C_irregular = ifelse(F_redupmod == 1 & M_irregular == 1,1,0),
         C_closure = ifelse(F_closedsyllable == 1 & M_abrupt == 1,1,0),
         C_punctual = ifelse(F_monosyllabic == 1 & M_punctual == 1,1,0),
         C_long = ifelse(F_finallength == 1 & M_long == 1,1,0),
         C_weight_voice = ifelse((F_voice == 0 & M_weight == 0) | (F_voice == 2 & M_weight) == 2,1,0),
         C_weight_vowel = ifelse((F_vowelquality == 0 & M_weight == 0) | (F_vowelquality == 2 & M_weight == 2),1,0),
         C_weight_tone = ifelse((F_intonation == 0 & M_weight == 2) | (F_intonation == 2 & M_weight == 0),1,0)
         )

# are some mappings mutually exclusive? then it might be slightly more elegant to combine them
d %>%
  mutate(C_event = C_iterative + C_punctual) %>%
  dplyr::select(ideophone,meaning_NL,language,C_iterative,C_punctual,C_event) %>%
  filter(C_event > 1)
```

    ## # A tibble: 0 x 7
    ## # Groups:   study [0]
    ## # ... with 7 variables: study <chr>, ideophone <chr>, meaning_NL <chr>,
    ## #   language <chr>, C_iterative <dbl>, C_punctual <dbl>, C_event <dbl>

``` r
d %>%
  mutate(C_exclusive = C_long + C_closure) %>%
  dplyr::select(ideophone,meaning_NL,language,C_iterative,C_closure,C_exclusive) %>%
  filter(C_exclusive > 1)
```

    ## # A tibble: 0 x 7
    ## # Groups:   study [0]
    ## # ... with 7 variables: study <chr>, ideophone <chr>, meaning_NL <chr>,
    ## #   language <chr>, C_iterative <dbl>, C_closure <dbl>, C_exclusive <dbl>

``` r
# add some cumulative iconicity scores
d <- d %>%
  mutate(C_cumulative = C_modality + C_iterative + C_irregular + C_closure + C_punctual + C_long + C_weight_voice + C_weight_vowel + C_weight_tone) %>%
  mutate(C_any1 = ifelse(C_cumulative == 0,0,1),
         C_atleast2 = ifelse(C_cumulative > 1,1,0),
         C_ternary = ifelse(C_cumulative == 0,0,
                            ifelse(C_cumulative == 1,1,
                                   ifelse(C_cumulative == 2,2,3))))

# cumulative scores can be further refined by considering only features that work for this sample — e.g., since C_irregular only adds noise, it may be excluded 
d <- d %>%
  mutate(C_simple = C_modality + C_iterative + C_closure + C_punctual + C_weight_tone + C_weight_vowel)

# write this dataset for use in the next step
write.csv(d,file="data/ideophones_coded_guessed.csv")

d %>%
  group_by(C_cumulative) %>%
  summarise(n=n(),score=mean.na(score))
```

    ## # A tibble: 6 x 3
    ##   C_cumulative     n score
    ##          <dbl> <int> <dbl>
    ## 1            0    93 0.551
    ## 2            1    77 0.604
    ## 3            2    42 0.622
    ## 4            3    23 0.697
    ## 5            4     2 0.900
    ## 6            5     2 0.85

``` r
d %>%
  group_by(C_ternary) %>%
  summarise(n=n(),score=mean.na(score))
```

    ## # A tibble: 4 x 3
    ##   C_ternary     n score
    ##       <dbl> <int> <dbl>
    ## 1         0    93 0.551
    ## 2         1    77 0.604
    ## 3         2    42 0.622
    ## 4         3    27 0.723

``` r
d %>%
  group_by(C_simple) %>%
  summarise(n=n(),score=mean.na(score))
```

    ## # A tibble: 6 x 3
    ##   C_simple     n score
    ##      <dbl> <int> <dbl>
    ## 1        0   109 0.559
    ## 2        1    75 0.600
    ## 3        2    40 0.642
    ## 4        3    11 0.759
    ## 5        4     3 0.867
    ## 6        5     1 0.9

## Plots

A quick first look at some congruency measures. Uses a custom plotting
function `icoplot()`, which is a wrapper for a ggplot object constructed
with `geom_dotplot`.

``` r
# make a copy dp of the data in which we convert some measures to factors for easy plotting & facetting
tofactors <- paste(c("language|category|study|group",names(d[grep('C_',names(d))])),collapse = "|")
dp <- d
dp[,grep(tofactors,names(dp))] <- lapply(dp[,grep(tofactors,names(dp))], as.factor)

# all data points by language
icoplot() # icoplot() is loaded from TI_functions.R
```

    ## [1] "No variable specified"
    ## [1] "Plotting language"

![](figures_md/icoplots-1.png)<!-- -->

``` r
icoplot("C_modality")
```

    ## [1] "Plotting C_modality"

![](figures_md/icoplots-2.png)<!-- -->

``` r
icoplot("C_iterative")
```

    ## [1] "Plotting C_iterative"

![](figures_md/icoplots-3.png)<!-- -->

``` r
icoplot("C_irregular")
```

    ## [1] "Plotting C_irregular"

![](figures_md/icoplots-4.png)<!-- -->

``` r
icoplot("C_punctual")
```

    ## [1] "Plotting C_punctual"

![](figures_md/icoplots-5.png)<!-- -->

``` r
icoplot("C_long")
```

    ## [1] "Plotting C_long"

![](figures_md/icoplots-6.png)<!-- -->

``` r
icoplot("C_weight_voice")
```

    ## [1] "Plotting C_weight_voice"

![](figures_md/icoplots-7.png)<!-- -->

``` r
icoplot("C_weight_vowel")
```

    ## [1] "Plotting C_weight_vowel"

![](figures_md/icoplots-8.png)<!-- -->

``` r
icoplot("C_weight_tone")
```

    ## [1] "Plotting C_weight_tone"

![](figures_md/icoplots-9.png)<!-- -->

``` r
icoplot("C_cumulative")
```

    ## [1] "Plotting C_cumulative"

![](figures_md/icoplots-10.png)<!-- -->

``` r
icoplot("C_any1")
```

    ## [1] "Plotting C_any1"

![](figures_md/icoplots-11.png)<!-- -->

``` r
icoplot("C_atleast2")
```

    ## [1] "Plotting C_atleast2"

![](figures_md/icoplots-12.png)<!-- -->

``` r
icoplot("C_ternary")
```

    ## [1] "Plotting C_ternary"

![](figures_md/icoplots-13.png)<!-- -->

``` r
# blank plot can be useful as background for slides
ggplot(data=dp, aes(x=study,y=score)) +
  theme_tufte() + ylim(0,1) + theme(legend.position="none",axis.title.x=element_blank(),plot.margin=margin(0,0,10,0)) + 
  stat_summary(fun.y=median,geom="point",size=8,shape=21,stroke=1,fill="white",colour="#c9c9c9") +
  geom_dotplot(colour="white",fill="#c9c9c9",stackgroups=T,dotsize=1.5,binwidth=0.01,binaxis="y",stackdir = "center")
```

![](figures_md/icoplots-14.png)<!-- -->

### Tinkering with plots

Just trying different ways of visualizing the data to find the most
helpful and transparent one.

``` r
icoplot("C_simple")
```

    ## [1] "Plotting C_simple"

![](figures_md/visuals-1.png)<!-- -->

``` r
ggplot(data=dp, aes(x="all",y=score_z)) +
  theme_tufte() +
  geom_half_boxplot() +
  geom_half_point(side="l") +
  geom_half_violin(side="r") +
  NULL
```

![](figures_md/visuals-2.png)<!-- -->

``` r
ggplot(data=dp, aes(x=C_simple,y=logodds)) +
  theme_tufte() +
  geom_half_boxplot() +
  geom_half_point() +
  NULL
```

![](figures_md/visuals-3.png)<!-- -->

``` r
ggplot(data=dp, aes(x=C_simple,y=score_z)) +
  theme_tufte() +
  geom_half_boxplot() +
  geom_half_point() +
  NULL
```

![](figures_md/visuals-4.png)<!-- -->

``` r
ggplot(data=dp, aes(x=C_simple,y=logodds,color=C_simple)) +
  theme_tufte() +
  geom_half_boxplot() +
  geom_half_dotplot() +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  NULL
```

![](figures_md/visuals-5.png)<!-- -->

``` r
ggplot(data=dp, aes(x=C_ternary,y=logodds,color=C_ternary,fill=C_ternary)) +
  theme_tufte() +
  geom_half_violin() +
  geom_half_point_panel() +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  facet_grid(~ study)
```

![](figures_md/visuals-6.png)<!-- -->

``` r
ggplot(data=dp, aes(x=C_ternary,y=logodds,color=C_ternary,fill=C_ternary)) +
  theme_tufte() +
  geom_half_violin() +
  geom_half_dotplot(dotsize=0.8) +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  facet_grid(~ study)
```

![](figures_md/visuals-7.png)<!-- -->

``` r
ggplot(data=dp, aes(x=C_atleast2,y=logodds,color=C_atleast2,fill=C_atleast2)) +
  theme_tufte() +
  geom_half_violin() +
  geom_half_dotplot(dotsize=0.4,stackratio=0.8,method="histodot") +
  scale_fill_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  scale_colour_viridis(option="plasma",discrete=T,begin=0.3,end=0.9) +
  facet_grid(~ study)
```

![](figures_md/visuals-8.png)<!-- -->

``` r
ggplot(data=dp, aes(x=category,y=logodds,color=category)) +
  theme_tufte() +
  geom_half_violin() +
  geom_half_point_panel() +
  NULL
```

![](figures_md/visuals-9.png)<!-- -->

``` r
ggplot(data=dp, aes(x=language,y=logodds,color=language)) +
  theme_tufte() +
  geom_half_violin() +
  geom_half_point_panel() +
  NULL
```

![](figures_md/visuals-10.png)<!-- -->

``` r
ggplot(data=d, aes(x=language,y=C_ternary,color=language)) +
  theme_tufte() +
  geom_half_violin() +
  geom_half_point_panel() +
  NULL
```

![](figures_md/visuals-11.png)<!-- -->

## Exploring congruency blind spots

What are we not capturing? Our coding and congruency measures are
designed to be reproducible and human-codable. They are not exhaustive,
and there may well be aspects that we are missing. Some ideophones may
also be guessed relatively high for reasons other than form-meaning
analogies (e.g., they had a very unattractive foil or presented
unforeseen confounds). Here we explore the congruency data to find
highly guessed ideophones that our coding did not (yet) identify as
having iconic mappings. We also look at the other end: ideophones coded
as having an iconic mapping but not doing well in the binary forced
choice task.

If we sort by score most of the blindspots are from the Collabra study,
which had a less forbidding design and therefore a higher baseline
score.

``` r
d %>%
  filter(C_cumulative == 0, logodds >0) %>%
  dplyr::select(filename,ideophone,meaning,meaning_NL,language,logodds,score) %>%
  arrange(-score) %>% ungroup %>%
  slice(1:10)
```

    ## # A tibble: 10 x 8
    ##    study  filename  ideophone meaning    meaning_NL  language logodds score
    ##    <chr>  <chr>     <chr>     <chr>      <chr>       <chr>      <dbl> <dbl>
    ##  1 Colla~ mushimus~ mushimus~ humid      vochtig     Japanese   1.48  0.931
    ##  2 Langu~ Japanese~ pan pan   prall gef~ tot knappe~ Japanese   1.28  0.9  
    ##  3 Colla~ hetoheto  hetoheto  exhausted  afgemat     Japanese   1.09  0.862
    ##  4 Colla~ kibikibi  kibikibi  energetic  energiek    Japanese   1.09  0.862
    ##  5 Langu~ Korean_1~ humurhum~ overripe,~ papperig    Korean     1.04  0.85 
    ##  6 Colla~ debudebu  debudebu  fat        dik         Japanese   0.945 0.828
    ##  7 Colla~ furafura  furafura  dizzy      duizelig    Japanese   0.945 0.828
    ##  8 Colla~ betabeta  betabeta  clinging   plakkerig   Japanese   0.702 0.759
    ##  9 Colla~ hikuhiku  hikuhiku  twitchy    geagiteerd  Japanese   0.702 0.759
    ## 10 Colla~ sekaseka  sekaseka  hastily    haastig     Japanese   0.702 0.759

``` r
# the reverse is also interesting: which ideophones were coded as having iconic mappings yet were not guessed greatly?

d %>%
  filter(C_cumulative > 0, logodds < -0.25) %>%
  dplyr::select(filename,ideophone,meaning,meaning_NL,C_cumulative,language,logodds,score) %>%
  arrange(score) %>% ungroup() %>%
  slice(1:10)
```

    ## # A tibble: 10 x 9
    ##    study filename ideophone meaning meaning_NL C_cumulative language
    ##    <chr> <chr>    <chr>     <chr>   <chr>             <dbl> <chr>   
    ##  1 Lang~ Semai_2~ pl<U+0252>~s      of som~ geluid va~            1 Semai   
    ##  2 Lang~ Siwu_28~ fututu    purely~ sneeuwwit             1 Siwu    
    ##  3 Lang~ Semai_1~ bjuukbj<U+025B>~ bright~ fel gekle~            2 Semai   
    ##  4 Lang~ Semai_2~ pl<U+025B>~spl<U+0252>~s  of peo~ geluid va~            2 Semai   
    ##  5 Lang~ Ewe_17_~ takataka  walk w~ struinen              1 Ewe     
    ##  6 Lang~ Ewe_21_~ kpa<U+0256>ikpa~ walkin~ met je be~            1 Ewe     
    ##  7 Lang~ Japanes~ garigari  scrapi~ schrapend~            1 Japanese
    ##  8 Lang~ Korean_~ pasakpas~ crispy~ bros                  1 Korean  
    ##  9 Lang~ Korean_~ p'arutp'~ fresh ~ het groen~            1 Korean  
    ## 10 Lang~ Semai_1~ <U+025F>rweet<U+025F>r~ upward~ omhoog kl~            1 Semai   
    ## # ... with 2 more variables: logodds <dbl>, score <dbl>

## Stats

As the visualizations already make clear there is a non-trivial positive
relation between the presence of iconic mappings (in a cumulative sense)
and the guessability score determined in experimental work.

``` r
# simplest thing we can do is a correlation

cor.test(d$logodds,d$C_cumulative)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  d$logodds and d$C_cumulative
    ## t = 5.0454, df = 237, p-value = 9.009e-07
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1921331 0.4216661
    ## sample estimates:
    ##       cor 
    ## 0.3114351

``` r
cor.test(d$score_z,d$C_cumulative)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  d$score_z and d$C_cumulative
    ## t = 6.0524, df = 237, p-value = 5.537e-09
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2506235 0.4709135
    ## sample estimates:
    ##       cor 
    ## 0.3658824
