# Libraries ---------------------------------------------------------------
if(!require("shiny"))
  install.packages("shiny")
if(!require("ggplot2"))
  install.packages("ggplot2")
if(!require("dplyr"))
  install.packages("dplyr")
if(!require("tidyr"))
  install.packages("tidyr")

library(shiny, lib.loc="~/R_libs2")
library(ggplot2, lib.loc="~/R_libs2")
library(dplyr, lib.loc="~/R_libs2")
library(tidyr, lib.loc="~/R_libs2")

# Mock Input --------------------------------------------------------------
input <- data.frame(
  indselect = "c6",
  depselect = "Grade.Letter",
  demselect1 = "Gender",
  demselect2 = "URM",
  stringsAsFactors = FALSE
)

# Global R ----------------------------------------------------------------
Master <- read.csv("~/buddie_data.csv", stringsAsFactors = FALSE)

#enables bookmarking functionality
enableBookmarking(store = "url")

Master <- Master %>%
  select(Age = age,
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
         c1:st23) %>%
  mutate_at(vars(c1:st23), as.character)

MetaData <- data.frame(Variable = names(Master),
                       Type = as.vector(unlist(lapply(Master, class))),
                       stringsAsFactors = FALSE) %>%
  mutate(Type = ifelse(Type == "integer", "numeric", Type))

Demographics <- Master %>%
  select(Age:Section)

Variables <- Master %>%
  select(Grade.Percent:st23)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$mainplot <- renderPlot({
    
    #Basic Progress Bar Functionality -- see withProgress API for details on how to further implement
    withProgress(message = 'Calculation in progress',
      detail = 'This may take a while...', value = 0, {
      for (i in 1:5) {
        incProgress(1/5)
        Sys.sleep(0.25)
      }
    })
    
  #server function for bookmarking
  output$out <- renderText({
    if (input$caps)
      toupper(input$txt)
    else
      input$txt
  })
  
  #Allows for Data Download
  
  #TODO: DETERMINE PROPER DOWNLOADING FILE TYPES
  
  dataset <- 'mainplot'
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BUDDIEData", Sys.Date(), ".", input$selectDownload, sep = "")
    },
    content = function(file) {
      if(selectDownload == 1) {
        #TODO: DETERMINE CORRECT CONTENTTYPE VARIABLES
        #contentType = image/jpeg
        
        #TODO: DETERMINE CORRECT WRITE FUNCTION
        write.table(dataset, file)
      }
      if(selectDownload == 2) {
        #TODO: DETERMINE CORRECT CONTENTTYPE VARIABLES
        #contentType = svg
        
        #TODO: DETERMINE CORRECT WRITE FUNCTION
        write.table(dataset, file)
      }
    }
  )
    
    IndType <- MetaData$Type[MetaData$Variable == input$indselect]
    DepType <- MetaData$Type[MetaData$Variable == input$depselect]
    DemType1 <- MetaData$Type[MetaData$Variable == input$demselect1]
    DemType2 <- MetaData$Type[MetaData$Variable == input$demselect2]
    
    if(length(DemType1) == 0 & length(DemType2) == 0){
    Master.fil <- Master %>%
      drop_na(!!input$indselect) %>%
      drop_na(!!input$depselect)
    }else if(length(DemType1) == 1 & length(DemType2) == 0){
      Master.fil <- Master %>%
        drop_na(!!input$indselect) %>%
        drop_na(!!input$depselect) %>%
        drop_na(!!input$demselect1)
    }else if(length(DemType1) == 1 & length(DemType2) == 1){
      Master.fil <- Master %>%
        drop_na(!!input$indselect) %>%
        drop_na(!!input$depselect) %>%
        drop_na(!!input$demselect1) %>%
        drop_na(!!input$demselect2)
    }
  
    if(DepType == "numeric" & IndType == "numeric" & length(DemType1) == 0 & length(DemType2) == 0){
      
      mainplot <- 
        ggplot(Master.fil, aes_string(x = input$indselect, y = input$depselect)) +
          geom_point(alpha = .3, size = 2) +
          geom_smooth(method = "lm") +
          theme_bw() +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
      
    }else if(DepType == "numeric" & IndType == "numeric" & length(DemType1) == 1 & length(DemType2) == 0){
      
      if(DemType1 == "character"){
        mainplot <- 
          ggplot(Master.fil, aes_string(x = input$indselect, y = input$depselect, color = input$demselect1)) +
          geom_point(alpha = .3, size = 2) +
          geom_smooth(method = "lm") +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
      }else if(DemType1 == "numeric"){
        mainplot <- 
          ggplot(Master.fil, aes_string(x = input$indselect, y = input$depselect, color = input$demselect1)) +
          geom_point(alpha = .3, size = 2) +
          geom_smooth(method = "lm") +
          theme_bw() +
          scale_color_gradient2() +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
      }
      
    }else if(DepType == "numeric" & IndType == "numeric" & length(DemType1) == 1 & length(DemType2) == 1){
      
      if(DemType2 == "character"){
        mainplot <- 
          ggplot(Master.fil, aes_string(x = input$indselect, y = input$depselect, color = input$demselect1)) +
          geom_point(alpha = .3, size = 2) +
          geom_smooth(method = "lm") +
          facet_wrap(as.formula(paste("~", input$demselect2))) +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
      }else if(DemType2 == "numeric"){
          stop("You selected a numeric variable as the second demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead.")
      }
      
    }else if(DepType == "numeric" & IndType == "character" & length(DemType1) == 0 & length(DemType2) == 0){
      
      mainplot <- 
        ggplot(Master.fil, aes_string(x = input$indselect, y = input$depselect)) +
        geom_boxplot(alpha = .3) +
        theme_bw() +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
      
    }else if(DepType == "numeric" & IndType == "character" & length(DemType1) == 1 & length(DemType2) == 0){
      
      if(DemType1 == "character"){
        mainplot <- 
          ggplot(Master.fil, aes_string(x = input$indselect, y = input$depselect, color = input$demselect1)) +
          geom_boxplot(alpha = .3, size = 1) +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
      }else if(DemType1 == "numeric"){
        stop("You selected a numeric variable as the first demographic variable; cannot plot a numeric variable as the grouping variable. Try selecting this demographic variable as the independent variable instead.")
      }
      
    }else if(DepType == "numeric" & IndType == "character" & length(DemType1) == 1 & length(DemType2) == 1){
      
      if(DemType2 == "character"){
        mainplot <- 
          ggplot(Master.fil, aes_string(x = input$indselect, y = input$depselect, color = input$demselect1)) +
          geom_boxplot(alpha = .3, size = 1) +
          facet_wrap(as.formula(paste("~", input$demselect2))) +
          theme_bw() +
          scale_color_brewer(palette = "Set1") +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16))
      }else if(DemType2 == "numeric"){
        
        stop("You selected a numeric variable as the second demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead.")
      
      }
      
      }else if(DepType == "character" & IndType == "numeric" & length(DemType1) == 0 & length(DemType2) == 0){
      
        stop("We cannot display a graph with a numeric independent variable and a categorical dependent variable. Please select either a categorical independent variable in addition to the categorical dependent variable or a continuous dependent variable.")
      
      }else if(DepType == "character" & IndType == "character" & length(DemType1) == 0 & length(DemType2) == 0){
      
      Master.filRastor <- Master.fil %>%
        group_by_at(vars(input$indselect, input$depselect)) %>%
        summarise(Count = n()) %>%
        ungroup() %>%
        group_by_at(vars(input$indselect)) %>%
        mutate(Percent = Count / sum(Count) * 100)
      
      mainplot <-
        ggplot(Master.filRastor, aes_string(x = input$indselect, y = input$depselect, fill = "Percent")) +
        geom_raster() +
        geom_text(aes(label = round(Percent, 1))) +
        scale_fill_gradient(low = "white", high = "indianred4") +
        theme_bw() +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          panel.grid.major = element_blank())

    }else if(DepType == "character" & IndType == "character" & length(DemType1) == 1 & length(DemType2) == 0){
      
      if(DemType1 == "character"){
        Master.filRastor <- Master.fil %>%
          group_by_at(vars(input$indselect, input$depselect, input$demselect1)) %>%
          summarise(Count = n()) %>%
          ungroup() %>%
          group_by_at(vars(input$demselect1, input$indselect)) %>%
          mutate(Percent = Count / sum(Count) * 100)
        
        mainplot <-
          ggplot(Master.filRastor, aes_string(x = input$indselect, y = input$depselect, fill = "Percent")) +
          geom_raster() +
          geom_text(aes(label = round(Percent, 1))) +
          facet_wrap(as.formula(paste("~", input$demselect1))) +
          scale_fill_gradient(low = "white", high = "indianred4") +
          theme_bw() +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            panel.grid.major = element_blank())
        
      }else if(DemType1 == "numeric"){
        stop("You selected a numeric variable as the first demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead.")
      }
      
      
    }else if(DepType == "character" & IndType == "character" & length(DemType1) == 1 & length(DemType2) == 1){
      
      if(DemType2 == "character"){
        Master.filRastor <- Master.fil %>%
          group_by_at(vars(input$indselect, input$depselect, input$demselect1, input$demselect2)) %>%
          summarise(Count = n()) %>%
          ungroup() %>%
          group_by_at(vars(input$demselect1, input$demselect2, input$indselect)) %>%
          mutate(Percent = Count / sum(Count) * 100)
        
        mainplot <-
          ggplot(Master.filRastor, aes_string(x = input$indselect, y = input$depselect, fill = "Percent")) +
          geom_raster() +
          geom_text(aes(label = round(Percent, 1))) +
          facet_grid(as.formula(paste(input$demselect2, "~", input$demselect1))) +
          scale_fill_gradient(low = "white", high = "indianred4") +
          theme_bw() +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            panel.grid.major = element_blank())
        
      }else if(DemType2 == "numeric"){
        stop("You selected a numeric variable as the second demographic variable; cannot plot a numeric variable as the facetting variable. Try selecting this demographic variable as the independent variable instead.")
      }
      
      
    }else{
      stop("I have not programmed this combination of options yet... sorry :(")
    }
    
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
