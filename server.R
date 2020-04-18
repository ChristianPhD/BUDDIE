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
if (!require("png"))
  install.packages("png")

library(shiny, lib.loc = "~/R_libs2")
library(ggplot2, lib.loc = "~/R_libs2")
library(dplyr, lib.loc = "~/R_libs2")
library(tidyr, lib.loc = "~/R_libs2")
library(ggvis)
library(png)

# Mock Input --------------------------------------------------------------

#Testing Inputs

input <- data.frame(
  indselect = "c6",
  depselect = "Grade.Letter",
  demselect1 = "Gender",
  demselect2 = "URM",
  stringsAsFactors = FALSE
)

# Global R ----------------------------------------------------------------
#Global R variables that are declared/or being renamed to be used in both user interface and server files so that there shall be no problem while coding for other functions.
#Additionally there shall be no confusion of variable names, checkstyle errors if we define them at the start of the coding in each files listed.
Master <- read.csv("~/buddie_data.csv", stringsAsFactors = FALSE)

#enables bookmarking functionality
enableBookmarking(store = "url")

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
  ) %>%
  mutate_at(vars(c1:st23), as.character)

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



# Server ------------------------------------------------------------------
#This section of code is used to give the rendor plot input, output varibales and restrictions so that we can add varibales, run as many combinations as possible.
server <- function(input, output, session) {
  output$selected_var <- renderText({
    paste ("Please visit the Glossary and Graph Variable Descrption tabs for greater understanding of variables (independent and dependent).", input$var)
  })
  output$mainplot <- renderPlot({
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    #Set progress message
    progress$set(message = "Generating plot... ", value = 0)
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1/5, detail = "Initializing application.")
    Sys.sleep(0.1)
    
    # Increment the progress bar, and update the detail text.
    progress$inc(2/5, detail = "Saving bookmark state")
    Sys.sleep(0.1)
    
    #Allows for Data Download
    
    # Increment the progress bar, and update the detail text.
    progress$inc(3/5, detail = "Saving image export state.")
    Sys.sleep(0.1)
    
  output$downloadData <- downloadHandler(
      filename = function() {
        #Specify File Name
        paste("BUDDIE", tolower(input$selectDownload), sep = ".")
      },
      content = function(file) {
        if (input$selectDownload == "PNG") {
          ggsave(file, plot = mainplot, device = "png")
        }
        if (input$selectDownload == "PDF") {
          ggsave(file, plot = mainplot, device = "pdf")
        }
      }
    )
    
                 
    IndType <- MetaData$Type[MetaData$Variable == input$indselect]
    DepType <- MetaData$Type[MetaData$Variable == input$depselect]
    DemType1 <- MetaData$Type[MetaData$Variable == input$demselect1]
    DemType2 <- MetaData$Type[MetaData$Variable == input$demselect2]
    
    #limits dependent list to numeric variables if independent variable is numeric
    numVariables <- MetaData$Variable[MetaData$Type == "numeric"]
    observeEvent(
      input$indselect,
      if (IndType == "numeric") {
        if (DepType == "character") { #allows input$depselect to still be a choice so the user can see the option they chose to cause the error
          updateSelectInput(session, "depselect", NULL, choices = c(input$depselect, numVariables), selected = input$depselect)
        }
        else {
          updateSelectInput(session, "depselect", NULL, choices = numVariables, selected = input$depselect)
        }
      }
      else {
        updateSelectInput(session, "depselect", NULL, choices = c(names(Variables), names(Demographics)), selected = input$depselect)
      }
    )
    
    
    if (length(DemType1) == 0 & length(DemType2) == 0) {
      Master.fil <- Master %>%
        drop_na(!!input$indselect) %>%
        drop_na(!!input$depselect)
    } 
    else if (length(DemType1) == 1 & length(DemType2) == 0) {
      Master.fil <- Master %>%
        drop_na(!!input$indselect) %>%
        drop_na(!!input$depselect) %>%
        drop_na(!!input$demselect1)
    } 
    else if (length(DemType1) == 1 & length(DemType2) == 1) {
      Master.fil <- Master %>%
        drop_na(!!input$indselect) %>%
        drop_na(!!input$depselect) %>%
        drop_na(!!input$demselect1) %>%
        drop_na(!!input$demselect2)
    }
    
    #Increment the progress bar, and update the detail text.
    progress$inc(4/5, detail = "Setting dependent and independent variables to be numeric.")
    Sys.sleep(0.1)
    
    #Making the dependent and independent variables to be numeric and the lengths of demographics to be 0.
    if (DepType == "numeric" & IndType == "numeric" & length(DemType1) == 0 & length(DemType2) == 0) {
      mainplot <-
        ggplot(Master.fil,aes_string(x = input$indselect, y = input$depselect)) +
        geom_point(alpha = .3, size = 2) +
        geom_smooth(method = "lm") +
        theme_bw() +
        theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16))
      
    } 
    else if (DepType == "numeric" & IndType == "numeric" & length(DemType1) == 1 & length(DemType2) == 0) {
      
      #This section is used to add color to the independent and dependent variables in the ggplot and also include the size of the text.
      if (DemType1 == "character") {
        mainplot <-
          ggplot(Master.fil, aes_string(
              x = input$indselect,
              y = input$depselect,
              color = input$demselect1
            )
          ) +
          geom_point(alpha = .3, size = 2) +
          geom_smooth(method = "lm") +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(axis.text = element_text(size = 14),
                axis.title = element_text(size = 16))
      } 
      
      #This section is again the same but the difference is that demType is changing from character to numeric which make it another variable to analyse.
      else if (DemType1 == "numeric") {
        mainplot <-
          ggplot(
            Master.fil,
            aes_string(
              x = input$indselect,
              y = input$depselect,
              color = input$demselect1
            )
          ) +
          geom_point(alpha = .3, size = 2) +
          geom_smooth(method = "lm") +
          theme_bw() +
          scale_color_gradient2() +
          theme(axis.text = element_text(size = 14),
                axis.title = element_text(size = 16))
      }
    } 
    
    #Making the dependent and independent variables to be numeric and the lengths of demographics to be 1.
    else if (DepType == "numeric" &
               IndType == "numeric" &
               length(DemType1) == 1 & length(DemType2) == 1) {
      if (DemType2 == "character") {
        mainplot <-
          ggplot(
            Master.fil,
            aes_string(
              x = input$indselect,
              y = input$depselect,
              color = input$demselect1
            )
          ) +
          geom_point(alpha = .3, size = 2) +
          geom_smooth(method = "lm") +
          facet_wrap(as.formula(paste("~", input$demselect2))) +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(axis.text = element_text(size = 14),
                axis.title = element_text(size = 16))
      } 
      else if (DemType2 == "numeric") {
        stop(
          "You selected a numeric variable as the second demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead."
        )
      } 
    } 
    
    #Making the dependent numeric and independent variables character by letting lengths of demographics to be 0.
    else if (DepType == "numeric" &
               IndType == "character" &
               length(DemType1) == 0 & length(DemType2) == 0) {
      mainplot <-
        ggplot(Master.fil,
               aes_string(x = input$indselect, y = input$depselect)) +
        geom_boxplot(alpha = .3) +
        theme_bw() +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16))  
    } 
    
    #Making the dependent variable data type numeric and independent variables to be character and the lengths of demographics to be 1 and 0 respectively.
    else if (DepType == "numeric" &
               IndType == "character" &
               length(DemType1) == 1 & length(DemType2) == 0) {
      if (DemType1 == "character") {
        mainplot <-
          ggplot(
            Master.fil,
            aes_string(
              x = input$indselect,
              y = input$depselect,
              color = input$demselect1
            )
          ) +
          geom_boxplot(alpha = .3, size = 1) +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(axis.text = element_text(size = 14),
                axis.title = element_text(size = 16))
      } 
      else if (DemType1 == "numeric") {
        stop(
          "You selected a numeric variable as the first demographic variable; cannot plot a numeric variable as the grouping variable. Try selecting this demographic variable as the independent variable instead."
        )
      }   
    } 
    
    #Making the dependent variable data type numeric and independent variables to be character and the lengths of demographics to be 1 and 1 respectively.
    else if (DepType == "numeric" &
               IndType == "character" &
               length(DemType1) == 1 & length(DemType2) == 1) {
      if (DemType2 == "character") {
        mainplot <-
          ggplot(
            Master.fil,
            aes_string(
              x = input$indselect,
              y = input$depselect,
              color = input$demselect1
            )
          ) +
          geom_boxplot(alpha = .3, size = 1) +
          facet_wrap(as.formula(paste("~", input$demselect2))) +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(axis.text = element_text(size = 14),
                axis.title = element_text(size = 16))
      } 
      else if (DemType2 == "numeric") {
        stop(
          "You selected a numeric variable as the second demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead."
        )
        
      }    
    } 
    
    #Making the dependent variable data type character and independent variables to be numeric and the lengths of demographics to be 0 and 0 respectively.
    else if (DepType == "character" &
               IndType == "numeric" &
               length(DemType1) == 0 & length(DemType2) == 0) {
      stop(
        "We cannot display a graph with a numeric independent variable and a categorical dependent variable. Please select either a categorical independent variable in addition to the categorical dependent variable or a continuous dependent variable."
      )
      
    } 
    else if (DepType == "character" &
               IndType == "character" &
               length(DemType1) == 0 & length(DemType2) == 0) {
      Master.filRastor <- Master.fil %>%
        group_by_at(vars(input$indselect, input$depselect)) %>%
        summarise(Count = n()) %>%
        ungroup() %>%
        group_by_at(vars(input$indselect)) %>%
        mutate(Percent = Count / sum(Count) * 100)
      
      mainplot <-
        ggplot(
          Master.filRastor,
          aes_string(
            x = input$indselect,
            y = input$depselect,
            fill = "Percent"
          )
        ) +
        geom_raster() +
        geom_text(aes(label = round(Percent, 1))) +
        scale_fill_gradient(low = "white", high = "indianred4") +
        theme_bw() +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          panel.grid.major = element_blank()
        )   
    } 
    
    #Making the dependent variable data type character and independent variables to be character and the lengths of demographics to be 1 and 0 respectively.
    else if (DepType == "character" &
               IndType == "character" &
               length(DemType1) == 1 & length(DemType2) == 0) {
      if (DemType1 == "character") {
        Master.filRastor <- Master.fil %>%
          group_by_at(vars(input$indselect, input$depselect, input$demselect1)) %>%
          summarise(Count = n()) %>%
          ungroup() %>%
          group_by_at(vars(input$demselect1, input$indselect)) %>%
          mutate(Percent = Count / sum(Count) * 100)
        
        #GG plot with color combinations, scales and sizes.
        mainplot <-
          ggplot(
            Master.filRastor,
            aes_string(
              x = input$indselect,
              y = input$depselect,
              fill = "Percent"
            )
          ) +
          geom_raster() +
          geom_text(aes(label = round(Percent, 1))) +
          facet_wrap(as.formula(paste("~", input$demselect1))) +
          scale_fill_gradient(low = "white", high = "indianred4") +
          theme_bw() +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            panel.grid.major = element_blank()
          )
        
      } 
      else if (DemType1 == "numeric") {
        stop(
          "You selected a numeric variable as the first demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead."
        )
      }
    } 
    else if (DepType == "character" &
               IndType == "character" &
               length(DemType1) == 1 & length(DemType2) == 1) {
      if (DemType2 == "character") {
        Master.filRastor <- Master.fil %>%
          group_by_at(
            vars(
              input$indselect,
              input$depselect,
              input$demselect1,
              input$demselect2
            )
          ) %>%
          summarise(Count = n()) %>%
          ungroup() %>%
          group_by_at(vars(input$demselect1, input$demselect2, input$indselect)) %>%
          mutate(Percent = Count / sum(Count) * 100)
        
        # This section is used to round the output values shown and to scale the gg plot by sizes and using colors to shows the plot.
        mainplot <-
          ggplot(
            Master.filRastor,
            aes_string(
              x = input$indselect,
              y = input$depselect,
              fill = "Percent"
            )
          ) +
          geom_raster() +
          geom_text(aes(label = round(Percent, 1))) +
          facet_grid(as.formula(paste(
            input$demselect2, "~", input$demselect1
          ))) +
          scale_fill_gradient(low = "white", high = "indianred4") +
          theme_bw() +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            panel.grid.major = element_blank()
          )
      } 
      else if (DemType2 == "numeric") {
        stop(
          "You selected a numeric variable as the second demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead."
        )
      }
      
      
    } 
    else{
      stop("I have not programmed this combination of options yet... sorry :(")
    }
    
    # Increment the progress bar, and update the detail text.
    progress$inc(5/5, detail = "Generate graph.")
    Sys.sleep(0.1)
    
    #plots the mainplot
    mainplot
  })
}

#
# MasterRastor <- Master %>%
#   group_by(mslq1, final_grade_letter, urm) %>%
#   summarise(Count = n()) %>%
#   ungroup() %>%
#   group_by(mslq1, final_grade_letter) %>%
#   mutate(Percent = Count / sum(Count) * 100)
#
# ggplot(MasterRastor, aes(x = mslq1, y = final_grade_letter, fill = Percent)) +
#   geom_raster() +
#   scale_fill_gradient(low = "white", high = "indianred4") +
#   facet_grid(. ~ urm) +
#   theme_bw()
#
# MasterRastor <- Master %>%
#   group_by(mslq1, final_grade_letter, urm, gender) %>%
#   summarise(Count = n()) %>%
#   ungroup() %>%
#   group_by(mslq1, final_grade_letter, urm) %>%
#   mutate(Percent = Count / sum(Count) * 100)
#
# ggplot(MasterRastor, aes(x = mslq1, y = final_grade_letter, fill = Percent)) +
#   geom_raster() +
#   scale_fill_gradient(low = "white", high = "indianred4") +
#   facet_grid(gender ~ urm) +
#   theme_bw()
