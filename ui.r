library(rsconnect)
library(shiny)
library(shinythemes)
library(wordcloud)

# Define UI for application that plots random distributions
shinyUI(navbarPage("Next word prediction model",fluidRow(column(6,tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                                     tags$div(
                                       #HTML('<textarea width="60%" id="text" rows="3" cols="30" class="form-control"></textarea>'),
                                       h3("Enter your text here"),
                                       tags$textarea(id = 'text', placeholder = 'Type here', rows = 3, class='form-control',""),
                                       
                                       
                                       HTML('<script type="text/javascript"> 
                                            document.getElementById("text").focus();
                                            </script>'),
                                       
                                       HTML("<div id='buttons'>"),
                                       uiOutput("prediction1",inline = T),
                                       uiOutput("prediction2",inline = T),
                                       uiOutput("prediction3",inline = T)),
                                     HTML("</div>"),align="center")
                              
                              
                             
                              ))
                   
