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
if (!require("ggvis"))
  install.packages("ggvis")
if (!require("png"))
  install.packages("png")

library(shiny, lib.loc = "~/R_libs2")
library(ggplot2, lib.loc = "~/R_libs2")
library(dplyr, lib.loc = "~/R_libs2")
library(tidyr, lib.loc = "~/R_libs2")
library(shinythemes)
library(ggvis)
library(png)

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
ui <- function(request) {
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
                 # downloadButton("downloadData", "Download"),
                 
                 
                 actionButton("buttonDownload", "Download Image"),
                 conditionalPanel(
                   condition = "input.buttonDownload == true",
                   radioButtons(inputId = "selectDownload",
                     label = h3("Select File Type"),
                     choices = list("png","pdf")
                   ),
                   downloadButton("downloadData", "Download")
                 ),
                 
                 hr(),
                 fluidRow(column(2, verbatimTextOutput("downloadValue")))
                 
               ),
               mainPanel(plotOutput('mainplot', height = "600px", width = "auto"))
             ),
                mainPanel(textOutput("selected_var")
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
  ),
  
  # Variable Descriptions ------------------------------------------------------------
  
  # This section is used to describe all of the independent, dependent, and demographic variables used in the tool
  
  tabPanel(
    
    value = "variabletab",
    
    "Variable Descriptions",
    
    fluidPage(
      
      theme = shinytheme("flatly"),
      
      title = "Variable Descriptions",
      
      mainPanel(
        
        width = 12,
        
        
        
        h1("BUDDIE Variable Descriptions",
           
           style = "font-family: Helvetica; font-size: 30px; color: #2B3E51"
           
        ),
        
        strong("General Information",
               
               style = "font-family: Helvetica; font-size: 20px; color:  #2B3E51"
               
        ),
        
        p("This is the general information that each student is asked when filling out surveys.",
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Scale Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("0 = no, 1 = yes"),
          
          tags$li("0 = Female (she/her), 1 = Male (he/his) 3 = They/Them"),
          
          tags$li("1 = Freshman, 2 = Sophomore, 3 = Junior, 4 = Senior, 5 = Other"),
          
          tags$li("0 = non-urm (a/aa, w)"),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Variable Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("Gender: Which pronouns do you prefer to describe yourself?"),
          
          tags$li("Title: What is your year in school?"),
          
          tags$li("Year: What is your year in school? (whole years only)"),
          
          tags$li("Ethnicity: What is your race?"),
          
          tags$li("URM: urm in raw data (student reported)"),
          
          tags$li("URM.Ethnic: urm determined from reported ethnicity"),
          
          tags$li("Fist.Gen: Are you a first generation college student?"),
          
          tags$li("International: Are you an international student?"),
          
          tags$li("Country.State: If you are an international student, what country are you from? If you are from the US, what state?"),
          
          tags$li("Major: What is(are) your current probable academic major(s)/minor(s)?"),
          
          tags$li("Same.Major.Entering: Are these the same major(s)/minor(s) you intended to pursue when you started at college?"),
          
          tags$li("Same.Major.Graduating: Do you intend to graduate with the same major(s)/minor(s)?"),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        strong("Student Assessment of Learning Gains",
               
               style = "font-family: Helvetica; font-size: 20px; color:  #2B3E51"
               
        ),
        
        p("This modified assessment measures the students' confidence in performing a variety of tasks commonly associated with learning goals of science courses.",
          
          br(),
          
          "Original authors: Sehoya Cotner, Cissy Ballen, D. Christopher Brooks, and Randy Moore in the Journal of College Science Teaching in 2011, Volume 40, Issue 5.",
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Scale Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("1 = Not confident, 2 = A little confident, 3 = Somewhat confident, 4 = Highly confident, 5 = Extremely confident"),
          
          tags$li("Confidence: The relative confidence students have in a variety of tasks performed in science. Therefore, a higher score indicates greater confidence."),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Variable Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("c1: Discuss scientific concepts with my friends or family"),
          
          tags$li("c2: Think critically about scientific findings I read about in the media"),
          
          tags$li("c3: Read the scientific literature (current papers, written by scientists, in scientific journals"),
          
          tags$li("c4: Determine what is - and is not - valid scientific evidence"),
          
          tags$li("c5: Make an argument using scientific evidence"),
          
          tags$li("c6: Pose questions that can be addressed by collecting and evaluating scientific evidence"),
          
          tags$li("c7: Present scientific results in writing or orally"),
          
          tags$li("c8: Understand scientific processes behind important scientific issues in the media"),
          
          tags$li("c9: Understand the science content of this course"),
          
          tags$li("c10: Use scientific thinking to solve problems outside of this course"),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        strong("Motivational and Self-Regulated Learning Questionnaire",
               
               style = "font-family: Helvetica; font-size: 20px; color:  #2B3E51"
               
        ),
        
        p("This scale measures motivational beliefs (self-efficacy, intrinsic value, and test anxiety) and self-regulated learning strategies (cogntitive strategies and self-regulation). As administered, only the intrinsic value and test anxiety (motivational beliefs) and self-regulation (self-regulated learning strategies) scales were used.",
          
          br(),
          
          "Original authors: Paul Pintrich & Elisabeth V. DeGroot in the Journal of Educational Psychology, Volume 90, Issue 1, in 1990.",
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Scale Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("1 = Not at all true of me, 7 = Very true of me"),
          
          tags$li("Intrinsic value: The value that students place on the content learned in the course (i.e. how interesting or useful it is). Therefore, the larger the score, the greater the instrinsic value of the content"),
          
          tags$li("Test anxiety: The relative levels of anxiety experienced during high stakes testing. Therefore, the higher the score, the greater the anxiety."),
          
          tags$li("Self-Regulation: The ability to montior one's own learning strategies (i.e. being aware of learning). Therefore, the higher the score, the more self-regulated the student is."),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Variable Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("mslq1: I prefer courses that are challenging so I can learn new things"),
          
          tags$li("mslq2: I work on practice exercises and answer end of chapter qestions even when I don't have to"),
          
          tags$li("mslq3: I am so nervous during a test that I cannot remember facts I have learned"),
          
          tags$li("mslq4: It is important for me to lear what is being taught in this course"),
          
          tags$li("mslq5: I like what I am learning in this course"),
          
          tags$li("mlsq6: I work hard to get a good grade even when I don't like a course"),
          
          tags$li("mlsq7: I think I will be able to use waht I learn in htis course in later studies"),
          
          tags$li("mlsq8: Even when study materials are dull and uninteresting, I keep working until I finish"),
          
          tags$li("mlsq9: I often choose to write about topics I will learn something from even if they require more work"),
          
          tags$li("mlsq10: I have an uneasy, upset feeling when I take a test"),
          
          tags$li("mlsq11: Even when I do poorly on a test I try to learn from my mistakes"),
          
          tags$li("mlsq12: I think that what I am learning in this course in useful for me to know"),
          
          tags$li("mlsq13: I think that what we are learning in this course is interesting"),
          
          tags$li("mlsq14: I worry a great deal about tests"),
          
          tags$li("mlsq15: Understanding this subject is important to me"),
          
          tags$li("mlsq16: When I take a test I think about how poorly I am doing"),
          
          tags$li("mlsq17: I ask myself questions to make sure I know the material I have been studying"),
          
          tags$li("mlsq18: When work is hard I either give up or study only the easy parts"),
          
          tags$li("mlsq19: Before I begin studying I think about the things I will need to do to learn"),
          
          tags$li("mlsq20: When I'm reading I stop once in a while and go over what I have read"),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        strong("Social Context Assessment of Learning Environment",
               
               style = "font-family: Helvetica; font-size: 20px; color:  #2B3E51"
               
        ),
        
        p("This modified version of the SCALE measures general relations between students and instructors where students take on roles as learners or instructors.",
          
          br(),
          
          "Original authors: J.D. Walker & Paul Baepler in 2016.",
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Scale Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("1 = Strongly disagree, 2 = Disagree, 3 = Somewhat disagree, 4 = Undecided, 5 = Somewhat agree, 6 = Agree, 7 = Strongly agree"),
          
          tags$li("Student-Student: The relative degree to which a student interacts with other students in their classroom. Therefore, the higher the score, the more interactions a student feels they have with their other students during class."),
          
          tags$li("Student-Instructor: The relative degree to which a student interacts with the instructor in their classroom. Therefore, the higher the score, the more interactions a student feels they have with the instructor during class."),
          
          tags$li("Student-As-Instructor: The relative degree to which a student interacts with other students in their classroom in a manner that resembles an instructor. Therefore, the higher the score, the more interactions a student feels they have with other students during class in an instructional manner."),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Variable Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("scale1: I can persuade my classmates why my ideas are relevant to the problems we encounter in this course"),
          
          tags$li("scale2: I’ve spoken informally with the instructor before, during, or after class"),
          
          tags$li("scale3: I can explain my thought process from start to finish to others in course"),
          
          tags$li("scale4: During this course, I often have a chance to discuss material with some of my classmates"),
          
          tags$li("scale5: Other students have explained a concept(s) to me"),
          
          tags$li("scale6: I’ve learned something from my classmates"),
          
          tags$li("scale7: I’ve learned something from my classmates"),
          
          tags$li("scale8: The instructor seems to care about me"),
          
          tags$li("scale9: I am acquainted with the instructor"),
          
          tags$li("scale10: The instructor knows my name"),
          
          tags$li("scale11: I can help others in this course learn"),
          
          tags$li("scale12: The students sitting near me rely on each other for help in learning course material"),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        strong("Social Identities and Attitude Scale",
               
               style = "font-family: Helvetica; font-size: 20px; color:  #2B3E51"
               
        ),
        
        p("This scale measures stereotype threat and identity aspects as they relate to the classroom from the perspective of individual students.",
          
          br(),
          
          "Original author: Katherine Picho & Scott Brown in the Journal of Advanced Academics, Volume 22, Issue 3 in 2011.",
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Scale Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("1 = Strongly disagree, 2 = Disagree, 3 = Somewhat disagree, 4 = Undecided, 5 = Somewhat agree, 6 = Agree, 7 = Strongly agree"),
          
          tags$li("Biology Identification: The relative value of biology and percieve it as useful to their careers. Therefore, a higher score means students value biology and see it as valuable."),
          
          tags$li("Gender Identification: The extent to which one's gender forms a central part of one's self-concept. Therefore, a higher score means that student sees their gender as a greater part of their self-concept."),
          
          tags$li("Gender Stigma Consciousness: Extent to which one is chronically self-conscious of stigma attached to one’s gender. Therefore, a higher score means that student is more chronically aware of stigmas attached to their gender."),
          
          tags$li("Ethnicity Identification: The extent to which one's gender forms a central part of one's self-concept. Therefore, a higher score means that student sees their gender as a greater part of their self-concept."),
          
          tags$li("Ethnicity Stigma Consciousness: Extent to which one is chronically self-conscious of stigma attached to one’s gender. Therefore, a higher score means that student is more chronically aware of stigmas attached to their gender."),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25"),
        
        p("Variable Key",
          
          style = "font-family: Helvetica; font-size: 15px; color:  #2B3E51; line-height: 0.1"
          
        ),
        
        tags$ul(
          
          tags$li("st1: My gender influences how I feel about myself"),
          
          tags$li("st2: My ethnicity affects how I interact with people of other ethnicities"),
          
          tags$li("st3: My gender affects how people treat me"),
          
          tags$li("st4: Biology is important to me"),
          
          tags$li("st5: My gender is central in defining who I am"),
          
          tags$li("st6: My ethnicity influences how teachers interact with me"),
          
          tags$li("st7: My ethnicity is an important reflection of who I am"),
          
          tags$li("st8: Most people judge me on the basis of my gender"),
          
          tags$li("st9: Being good at biology will be useful to me in my future career"),
          
          tags$li("st10: My gender affects how people act towards me"),
          
          tags$li("st11: People from other ethnic groups interpret my behavior based on my ethnicity"),
          
          tags$li("st12: My biology abilities are important to my academic success"),
          
          tags$li("st13: My identity is strongly tied to my gender"),
          
          tags$li("st14: Doing well in biology matters to me"),
          
          tags$li("st15: I feel a strong attachment to my ethnicity"),
          
          tags$li("st16: I value biology"),
          
          tags$li("st17: My ethnicity affects how my peers interact with me"),
          
          tags$li("st18: Doing well in biology is critical to my future success"),
          
          tags$li("st19: My gender influences how teachers interpret my behavior"),
          
          tags$li("st20: I value my ethnic background"),
          
          tags$li("st21: People of the opposite sex interpret my behavior based on my gender"),
          
          tags$li("st22: I am connected to my ethnic heritage"),
          
          tags$li("st23: Most people judge me on the basis of my ethnicity"),
          
          style = "font-family: Helvetica; font-size: 14px; color:  #2B3E51"
          
        ),
        
        p(" ", br(), style = "line-height: .25")
        
      )
      
    )
  )
  
)
}
