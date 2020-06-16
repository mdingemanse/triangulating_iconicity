Triangulating iconicity: Coding, consistency & calibration
================
Mark Dingemanse & Stella Punselie
Updated 2020-06-16

## Coding consistency

Code notebook for combining coding from two coders, working out
consistency, and recording coding/recoding decisions.

## Setup

``` r
# Packages
list.of.packages <- c("tidyverse","readxl","writexl","irr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# useful functions
`%notin%` <- function(x,y) !(x %in% y) 
mean.na <- function(x) mean(x, na.rm = T)
sd.na <- function(x) sd(x, na.rm = T)
```

## Data

Let’s first load the original data and generate derivative XLS files to
aid in the calibration.

``` r
d1 = read_excel("data\\ideophones_coded_MD_original.xlsx") %>% arrange(filename)
d2 = read_excel("data\\ideophones_coded_SP_original.xlsx") %>% arrange(filename)
d2 <- d2[,-c(1:7)] # remove non-unique fields except filename prior to merge

d <- merge(d1,d2,by="filename")

# sanity check: are all ideophones in d1 and d2 also in d? (0 = yes)
d1$filename[d1$filename %notin% d$filename]
```

    ## character(0)

``` r
d2$filename[d2$filename %notin% d$filename]
```

    ## character(0)

``` r
d.F <- d %>%
  dplyr::select(c("filename","ideophone"),starts_with("F_")) %>%
  dplyr::select(-starts_with("F_notes"),everything()) # put note fields at end
write_xlsx(d.F,path="data\\ideophones_F_consistency.xlsx")

d.M <- d %>%
  dplyr::select(c("filename","ideophone"),starts_with("M_")) %>%
  dplyr::select(-starts_with("M_notes"),everything()) # put note fields at end
write_xlsx(d.M,path="data\\ideophones_M_consistency.xlsx")

# function to get the names of coding categories (note: this presupposes there is a coder _MD, change as needed)
get_coding_categories <- function(data) {
  coder_1_names <- data %>% dplyr::select(ends_with("_MD")) %>% names()
  gsub('_MD','',coder_1_names)
}
```

## Consistency

We compute coding consistency for the original coding to establish which
categories of the coding scheme are usable as is and which need
recalibration or reformulation. We use `irr` package to compute %
agreement and Cohen’s Kappa.

``` r
# build functions to make it less repetitive
get_agree <- function(data,cat) {
  # takes a dataset and a coding category, looks up fields of that coding category, and computes agreement
  agree(dplyr::select(data,starts_with(cat)))$value
}
get_kappa <- function(data,cat) {
  # takes a dataset and a coding category, looks up fields of that coding category, and computes kappa
  kappa2(dplyr::select(data,starts_with(cat)))$value
}

# sanity check: does the function work? yes
get_agree(d.F,cat='F_redup')
```

    ## [1] 99.16318

``` r
# get list of coding categories and create dataframes for the consistency values
F_categories <- get_coding_categories(d.F)
F_consistency <- as.data.frame(F_categories) %>% dplyr::rename(category = F_categories)
M_categories <- get_coding_categories(d.M)
M_consistency <- as.data.frame(M_categories) %>% dplyr::rename(category = M_categories)

F_consistency$agree <- NA
F_consistency$kappa <- NA
for (i in 1:(length(F_categories)-1)) { # -1 to exclude the notes column
  F_consistency$agree[i] <- (get_agree(data=d.F,cat=F_categories[i]))
  F_consistency$kappa[i] <- (get_kappa(data=d.F,cat=F_categories[i]))
}
F_consistency
```

    ##           category    agree     kappa
    ## 1          F_redup 99.16318 0.9794479
    ## 2   F_monosyllabic 99.16318 0.9629342
    ## 3   F_partialredup 95.39749 0.7078564
    ## 4 F_closedsyllable 99.58159 0.9912489
    ## 5   F_vowelquality 95.39749 0.9281635
    ## 6          F_voice 72.38494 0.5876941
    ## 7     F_finalvowel 95.81590 0.8147000
    ## 8     F_intonation 87.02929 0.4484068
    ## 9          F_notes       NA        NA

For Form, mean agreement in the original coding is **93%** and the
lowest Kappa values are for F\_intonation, F\_voice, and
F\_partialredup, pointing to the need to resolve inconsistencies and
sharpen those coding categories.

``` r
M_consistency$agree <- NA
M_consistency$kappa <- NA
for (i in 1:(length(M_categories)-1)) {
  M_consistency$agree[i] <- (get_agree(data=d.M,cat=M_categories[i]))
  M_consistency$kappa[i] <- (get_kappa(data=d.M,cat=M_categories[i]))
}
M_consistency
```

    ##         category    agree     kappa
    ## 1        M_sound 98.74477 0.9612076
    ## 2 M_distribution 86.13445 0.7133367
    ## 3    M_irregular 84.10042 0.4503752
    ## 4       M_weight 59.41423 0.3496325
    ## 5     M_punctual 95.39749 0.7069446
    ## 6     M_durative 79.07950 0.2208893
    ## 7       M_abrupt 96.65272 0.7469561
    ## 8        M_notes       NA        NA

For Meaning, mean agreement in the original coding is **86%** and the
lowest Kappa values are for M-durative, M\_weight, and M\_irregular,
pointing to the need to resolve inconsistencies and sharpen those coding
categories.

## Outcomes

Using the consistency scores as a guide, we revisited the most
problematic categories to (i) recalibrate coders’ understanding of the
categories, (ii) reformulate instructions and (iii) in some cases rename
the coding category to reflect the revisions.

### Form

1.  *F\_redupmod* was renamed (originally F\_partialredup) and the
    coding instructions were sharpened to reflect the fact that we want
    to capture both partial and modified forms of reduplication.
2.  *F\_voice*: coding instructions were clarified to note that this
    question privileges consonants that crosslinguistically often enter
    into voicing oppositions (plosives and fricatives) as well as
    nasals, documented for their iconic uses across languages. This is
    close to Westermann’s use of this feature. We also considered a more
    continuous version of this feature that would weight or classify
    consonants based on their place on the sonority scale, but decided
    against it because the coding was already too fine-grained.
3.  *F\_finallength* was renamed (originally F\_finalvowel) to capture
    word final lengthening of any final sound, whether final vowel or
    nasal, glide or trill.
4.  *F\_intonation*: coding instructions were clarified to include a
    decision procedure for forms with falling or rising intonation.

### Meaning

1.  *M\_irregularity*: coding instructions were modified to indicate
    that the primary semantic property here is one of irregularity in
    the sense of imbalance (a highly regular ridged pattern would not
    qualify)
2.  *M\_weight*: coding instructions were sharpened to focus on binary
    oppositions of size, weight and brightness and to exclude
    Westermann’s hard/soft distinction, which we found orthogonal and
    conflicting in some cases. This also means that the middle value
    essentially means neutral or “not applicable”. It may be advisable
    to further break this up this feature, as there is no guarantee that
    size, weight and brightness pattern similarly across languages.
3.  *M\_durative*: coding instructions were sharpened to clarify that
    this targets events, not states and to clarify its relation to
    M\_distribution.
