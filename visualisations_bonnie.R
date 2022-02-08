library(tidyverse)
library(plotly)
library(ggthemes)
library(shiny)
library(readxl)
library(VGAM)
library(viridis)
library(RColorBrewer)

#### For some reason writing the d dataset from paper_coding_2_analysis.Rmd
#### to a utf-8 encoded csv file and then reading it in here as a utf-8 file
###  didn't read the characters correctly :(

#### So I just make it again here because that works
# get consensus coding data
d = read_excel("shinydat\\ideophones_coded.xlsx") %>% arrange(filename)

# add guessability scores from the Collabra and Language studies.

d.scores = read_excel("shinydat\\ideophones_guessability.xlsx") %>%
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
d.ratings <- read_xlsx("shinydat\\ideophones_rated_means.xlsx") %>%
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

# Define UI for app
ui <- fluidPage(
  titlePanel("Triangulating iconicity"),
  h4("Coding scheme"),
  fluidRow(includeHTML("codingscheme.html")),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("semdom","Choose a semantic domain",c("All domains",dat$category)),
      selectInput("lang","Choose a language",c("All languages",dat$language)),
      selectInput("study","Choose a study",c("Both studies",dat$study)),
      checkboxInput("pointlabels","Label points",FALSE)),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotlyOutput(outputId = "plot"),
      p("The size of the dots corresponds to cumulative iconicity")
      
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    semdoms <- input$semdom
    langs <- input$lang
    studies <- input$study
    
    if(input$semdom=="All domains"){semdoms <- dat$category}
    if(input$lang=="All languages"){langs <- dat$language}
    if(input$study=="Both studies"){studies <- dat$study}
    
    dat%>%
      filter(study %in% studies)%>%
      filter(language %in% langs)%>%
      filter(category %in% semdoms)->filtered_dat
    
    if(input$pointlabels==TRUE){
    # plots with labels
    if(input$semdom!="All domains"){
      # plots for a single domain, with labels
      filtered_dat%>%
        ggplot(aes(x=rating_z,y=logodds,size=C_cumulative,label=label,color=C_cumulative))+
        geom_point()+scale_fill_viridis(option="plasma")+scale_colour_viridis(option="plasma")+
        theme_tufte()+labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity")+
        xlim(min(dat$rating_z)-0.2,max(dat$rating_z)+0.2)+ylim(min(dat$logodds)-0.2,max(dat$logodds)+0.2)+
        geom_text(data=filtered_dat,aes(x=rating_z,y=logodds,label=ideophone),size=4)->plot
    }else{
      # plot for all domains, with labels
      filtered_dat%>%
        ggplot(aes(x=rating_z,y=logodds,size=C_cumulative,label=label,color=category))+
        geom_point()+theme_dark()+scale_color_brewer(palette = "Accent")+
        labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity")+
        xlim(min(dat$rating_z)-0.2,max(dat$rating_z)+0.2)+ylim(min(dat$logodds)-0.2,max(dat$logodds)+0.2)+
        geom_text(data=filtered_dat,aes(x=rating_z,y=logodds,label=ideophone),size=4)->plot
    }
      
    }else{
    # plots without labels
      if(input$semdom!="All domains"){
        # plots for a single domain, without labels
        filtered_dat%>%
          ggplot(aes(x=rating_z,y=logodds,size=C_cumulative,label=label,color=C_cumulative))+
          geom_point()+scale_fill_viridis(option="plasma")+scale_colour_viridis(option="plasma")+
          theme_tufte()+labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity")+
          xlim(min(dat$rating_z)-0.2,max(dat$rating_z)+0.2)+ylim(min(dat$logodds)-0.2,max(dat$logodds)+0.2)->plot
      }else{
        # plot for all domains, without labels
        filtered_dat%>%
          ggplot(aes(x=rating_z,y=logodds,size=C_cumulative,label=label,color=category))+
          geom_point()+theme_dark()+scale_color_brewer(palette = "Accent")+
          labs(x="rating (z)",y="guessability (log odds)",size="Cumulative iconicity")+
          xlim(min(dat$rating_z)-0.2,max(dat$rating_z)+0.2)+ylim(min(dat$logodds)-0.2,max(dat$logodds)+0.2)->plot
      } 
    }
      ggplotly(plot)
    
  })
  
}

shinyApp(ui, server)
