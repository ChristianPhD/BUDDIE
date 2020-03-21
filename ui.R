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
    "Info",
    fluidPage(
      #select a theme
      theme = shinytheme("flatly"),
      
      title = "BUDDIE",
      mainPanel(
        width = 12,
        strong(
          "Biology URM Diversity Data Interactive Explorer (BUDDIE)",
          style = "font-family: Times New Roman; font-size: 30px; color: #055C8B"
        ),
        
        p(
          "This will be some introduction to the BUDDIE site with instructions on how to use it. As a user, I want to improve the introduction page of BUDDIE by adding textual description about the application.
                                  BUDDIE page has got some textual description in the home page right now but the description is not enough to tell what exactly the application is doing. So, I would improve the home page by including more descriptions about the application's working, outputs and expectation of the application.",
          style = "font-family: 'Times New Roman'; font-size: 26px; color: white;"
        )
      )
    )
  )
)
