library(tidyverse)
library(plotly)
library(ggthemes)
library(shiny)
library(readxl)
library(VGAM)
library(viridis)

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
  mutate(label=paste(ideophone,paste("'",meaning,"'",sep=""),language))->dat
  
# Define UI for app
ui <- fluidPage(
  titlePanel("Triangulating iconicity"),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("semdom","Choose a semantic domain",c("All domains",dat$category)),
      selectInput("lang","Choose a language",c("All languages",dat$language)),
      selectInput("study","Choose a study",c("Both studies",dat$study)),
      checkboxInput("pointlabels","Label points",FALSE)),
      
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotlyOutput(outputId = "plot")
      
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
    
    print(input$labelpoints)
    if(input$pointlabels==TRUE){
    filtered_dat%>%
      ggplot(aes(x=rating_z,y=logodds,colour=C_cumulative,label=label))+geom_point()+scale_fill_viridis(option="plasma") +
      scale_colour_viridis(option="plasma")+theme_tufte()+geom_text(data=filtered_dat,aes(x=rating_z,y=logodds,colour=C_cumulative,label=ideophone))->plot
    }
    else{
      filtered_dat%>%
        ggplot(aes(x=rating_z,y=logodds,colour=C_cumulative,label=label))+geom_point()+scale_fill_viridis(option="plasma") +
        scale_colour_viridis(option="plasma")+theme_tufte()->plot
    }
      
      ggplotly(plot)
    
  })
  
}

shinyApp(ui, server)
