#Libraries were imported to make sure that no problems will be caused while implementing the default functions of Rshiny package.
# Libraries ---------------------------------------------------------------
if (!require("shiny"))
  install.packages("shiny")
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("tidyr"))
  install.packages("tidyr")
if (!require("shinythemes"))
  install.packages("shinythemes")

library(shiny, lib.loc = "~/R_libs2")
library(ggplot2, lib.loc = "~/R_libs2")
library(dplyr, lib.loc = "~/R_libs2")
library(tidyr, lib.loc = "~/R_libs2")
library(shinythemes)

# Global R ----------------------------------------------------------------

#Global R variables that are declared/or being renamed to be used in both user interface and server files so that there shall be no problem while coding for other functions.
#Additionally there shall be no confusion of variable names, checkstyle errors if we define them at the start of the coding in each files listed.

Master <- read.csv("~/buddie_data.csv", stringsAsFactors = FALSE)

Master <- Master %>%
  select(
    Age = age,
    Gender = gender,
    Title = year,
    Year = year_whole,
    Ethnicity = ethnicity,
    Am.Ind.AK.Native = ethnicity_american_indian__alaska_native,
    Asian.Asian.Ame = ethnicity_asian__asian_american,
    Black.Afr.Am = ethnicity_black__african_american,
    Latino.His.His.Am = ethnicity_latino__hispanic__hispanic_american,
    Native.Haw.Oth.Pac.Isl = ethnicity_native_hawaiian__other_pacific_islander,
    White.Eur.Am = ethnicity_white__european_american,
    URM = urm,
    URM.Ethnic = urm_from_ethnicity,
    First.Gen = fgen,
    International = international,
    Country.State = country_state,
    Major = current_major,
    Same.Major.Entering = same_major_entering,
    Same.Major.Graduating = same_major_graduating,
    University = university,
    Subject = subject,
    Course.Number = course_number,
    Semester = semester,
    Section = section,
    Grade.Percent = final_grade_percent,
    Grade.Letter = final_grade_letter,
    Exam1.Percent = exam_1_percent,
    Exam2.Percent = exam_2_percent,
    Exam3.Percent = exam_3_percent,
    Exam4.Percent = exam_4_percent,
    Final.Exam.Percent = final_exam_percent,
    Avg.Exams.Percent = avg_exams_percent,
    Lecture.Percent = total_lecture_percent,
    c1:st23
  )

MetaData <- data.frame(
  Variable = names(Master),
  Type = as.vector(unlist(lapply(Master, class))),
  stringsAsFactors = FALSE
) %>%
  mutate(Type = ifelse(Type == "integer", "numeric", Type))

Demographics <- Master %>%
  select(Age:Section)

Variables <- Master %>%
  select(Grade.Percent:st23)


# User Interface ----------------------------------------------------------
ui <-
  # User Interface ----------------------------------------------------------
# The section of code is used for the introductory page of BUDDIE. When we run the ui.R file we come across the BUDDIE home page where it gives us the breif introduction
# about the application and shows the abbrevation of BUDDIE.
navbarPage(
  id = 'mainnavbar',
  "BUDDIE",
  
  # BUDDIE Tool ------------------------------------------------------------------
  #This section of code is used to list the independent, dependent variables and demographics. This also shows the graph dimeansions, list of variables
  # involved in the demographics and label the graph with the variables.
  tabPanel(value = 'BUDDIEtab', "Tool",
           fluidPage(
             titlePanel("Tool"),
             sidebarLayout(
               sidebarPanel(
                 strong("Select the independent variable"),
                 selectInput("indselect", NULL, choices = c(names(Variables), names(Demographics))),
                 br(),
                 strong("Select the dependent variable"),
                 selectInput("depselect", NULL, choices = c(names(Variables), names(Demographics))),
                 br(),
                 strong("Broken down by (first demographic):"),
                 selectInput("demselect1", NULL, choices = c("None", names(Demographics))),
                 br(),
                 strong("Broken down by (second) demographic):"),
                 selectInput("demselect2", NULL, choices = c("None", names(Demographics))),
                 br(),
                 
                 #adds bookmark button to ui
                 bookmarkButton(),
                 
                 #adds download button to ui
                 actionButton("buttonDownload", "Download Dataset"),
                 conditionalPanel(
                   condition = "input.buttonDownload == true",
                   selectInput(
                     "selectDownload",
                     label = h3("Select File Type"),
                     choices = list(
                       "JPEG" = 1,
                       "SVG" = 2,
                       selected = 1
                     )
                   ),
                   downloadButton("downloadData", "Download")
                 ),
                 
                 hr(),
                 fluidRow(column(2, verbatimTextOutput("downloadValue")))
                 
               ),
               mainPanel(plotOutput('mainplot', height = "600px", width = "auto"))
             )
           )),
  
  # Introduction ------------------------------------------------------------
  tabPanel(
    value = "introtab",
    "About",
    
    fluidRow(
      shiny::HTML("<br><br><center> 
                                            <h1>About BUDDIE</h1> 
                                            <h4>Biology URM Diversity Data Interactive Explorer</h4>
                                            </center>
                                            <br>
                                            <br>"),
      style = "height:250px;"),
    
    fluidRow(
      div(align = "center",
          tags$span(h4("Frequently Asked Questions (FAQ)"), 
                    style = "font-weight:bold"
          ))
    ),
    
    fluidRow(
      column(3),
      column(6,
             tags$ul(
               tags$li(h5("What is BUDDIE?")),
                    tags$ul(tags$li(h6("BUDDIE, Biology Undergraduate Diversity Data Interactive Explorer, is a tool created to allow research scientists and data analysts to explore trends and export data related to undergraduates of underrepresented minority (URM) status in biology and STEM classes. The goal of BUDDIE is to create an accessible web page that allows analysts within the research network to explore data trends within a unified tool. Research scientists will be able to export plots in multiple formats, save data configurations through a shareable URL, filter the master data file to accommodate their research and many more quality of life features wrapped in a fresh user interface designed with diversity and accessibility in mind."))),
               tags$li(h5("What is the goal of BUDDIE?")),
                    tags$ul(tags$li(h6("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Donec ultrices tincidunt arcu non sodales neque sodales ut. Diam maecenas ultricies mi eget mauris pharetra et ultrices neque. Et tortor consequat id porta nibh venenatis cras. Elementum nibh tellus molestie nunc non blandit. Volutpat lacus laoreet non curabitur gravida arcu ac tortor. Luctus venenatis lectus magna fringilla urna. Pellentesque habitant morbi tristique senectus et netus et. Egestas diam in arcu cursus euismod. Nulla facilisi morbi tempus iaculis urna id volutpat. Lacus suspendisse faucibus interdum posuere. Diam maecenas sed enim ut. Amet dictum sit amet justo donec enim diam vulputate ut. Viverra ipsum nunc aliquet bibendum enim facilisis gravida neque convallis. Morbi tristique senectus et netus et malesuada fames ac. Eget nulla facilisi etiam dignissim diam quis. Sit amet justo donec enim diam vulputate ut pharetra. Sit amet aliquam id diam maecenas ultricies mi eget mauris. Gravida in fermentum et sollicitudin ac. Purus semper eget duis at."))),
               tags$li(h5("Who is BUDDIE for?")),
                    tags$ul(tags$li(h6("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Donec ultrices tincidunt arcu non sodales neque sodales ut. Diam maecenas ultricies mi eget mauris pharetra et ultrices neque. Et tortor consequat id porta nibh venenatis cras. Elementum nibh tellus molestie nunc non blandit. Volutpat lacus laoreet non curabitur gravida arcu ac tortor. Luctus venenatis lectus magna fringilla urna. Pellentesque habitant morbi tristique senectus et netus et. Egestas diam in arcu cursus euismod. Nulla facilisi morbi tempus iaculis urna id volutpat. Lacus suspendisse faucibus interdum posuere. Diam maecenas sed enim ut. Amet dictum sit amet justo donec enim diam vulputate ut. Viverra ipsum nunc aliquet bibendum enim facilisis gravida neque convallis. Morbi tristique senectus et netus et malesuada fames ac. Eget nulla facilisi etiam dignissim diam quis. Sit amet justo donec enim diam vulputate ut pharetra. Sit amet aliquam id diam maecenas ultricies mi eget mauris. Gravida in fermentum et sollicitudin ac. Purus semper eget duis at."))),
               tags$li(h5("Who funds BUDDIE?")),
                    tags$ul(tags$li(h6("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Donec ultrices tincidunt arcu non sodales neque sodales ut. Diam maecenas ultricies mi eget mauris pharetra et ultrices neque. Et tortor consequat id porta nibh venenatis cras. Elementum nibh tellus molestie nunc non blandit. Volutpat lacus laoreet non curabitur gravida arcu ac tortor. Luctus venenatis lectus magna fringilla urna. Pellentesque habitant morbi tristique senectus et netus et. Egestas diam in arcu cursus euismod. Nulla facilisi morbi tempus iaculis urna id volutpat. Lacus suspendisse faucibus interdum posuere. Diam maecenas sed enim ut. Amet dictum sit amet justo donec enim diam vulputate ut. Viverra ipsum nunc aliquet bibendum enim facilisis gravida neque convallis. Morbi tristique senectus et netus et malesuada fames ac. Eget nulla facilisi etiam dignissim diam quis. Sit amet justo donec enim diam vulputate ut pharetra. Sit amet aliquam id diam maecenas ultricies mi eget mauris. Gravida in fermentum et sollicitudin ac. Purus semper eget duis at."))),
             )
      ),
      column(3)
    ),
    
    fluidRow(
      div(align = "center",
          tags$span(h4("_________________________________________"), 
                    style = "font-weight:bold"
          ))
    ),

    fluidRow(
      div(align = "center",
          tags$span(h4("Contact Us"), 
                    style = "font-weight:bold"
          ))
    ),
    # Contact Us
    fluidRow(
      column(3),
      column(6,
             shiny::HTML("<h6>BUDDIE is sponsored by __________, with financial support from __________. Feel free to reach out if you have any questions, comments, or concerns.</h6>")
      ),
      column(3)
    ),
    
    
    fluidRow(
      column(3),
      
      # Jordan Harshman
      column(2,
             div(class="panel panel-default", 
                 div(class="panel-body",  width = "600px",
                     align = "center",
                     div(
                       tags$img(src = "man_beard_1.svg", 
                                width = "50px", height = "50px")
                     ),
                     div(
                       tags$h5("Jordan Harshman"),
                       tags$h6( tags$i("Department of Chemistry and Biochemistry - Auburn University"))
                     ),
                     div(
                       "jth0083@auburn.edu"
                     ),
                     div(
                       "(334) 844-7126"
                     )
                 )
             )
      ),
      # Person 2
      column(2,
             div(class="panel panel-default",
                 div(class="panel-body",  width = "600px", 
                     align = "center",
                     div(
                       tags$img(src = "man.svg", 
                                width = "50px", height = "50px")
                     ),
                     div(
                       tags$h5("Person 2"),
                       tags$h6( tags$i("Person 2 Role - Institution"))
                     ),
                     div(
                       "Person 2 Contact Email"
                     ),
                     div(
                       "Person 2 Contact Phone"
                     )
                 )
             )
      ),
      # Person 3
      column(2,
             div(class="panel panel-default",
                 div(class="panel-body",  width = "600px", 
                     align = "center",
                     div(
                       tags$img(src = "woman.svg", 
                                width = "50px", height = "50px")),
                     div(
                       tags$h5("Person 3"),
                       tags$h6( tags$i("Person 3 Role - Institution"))
                     ),
                     div(
                       "Person 3 Contact Email"
                     ),
                     div(
                       "Person 3 Contact Phone"
                     )
                 )
             )
      ),
      column(3)
      
    ),
    
    fluidPage(
      #select a theme
      theme = shinytheme("flatly"),
      
      title = "BUDDIE",
      mainPanel(
        width = 12,
      )
    )
  )
)
