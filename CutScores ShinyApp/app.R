require(shiny)
require(shinydashboard)

require(irtoys)

require(tidyverse)


Sys.setlocale('LC_ALL','C') # apparently to avoid string error in a particular locale

pLocs <- read_csv("deidentified_pLocs.csv")


ui <- fluidPage(
  
  # Application title
  titlePanel("Grammar & Punctuation Multistage Test Cut Scores"),
  
  tabsetPanel(type = "tabs",
        
              tabPanel("Upload File",
                       
                       box(width = 4,
                           
                           fileInput("file", "Choose the Item Manager file in csv format",
                                     accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                           
                           selectInput("selectTest", label = h4("Select test"),
                                       choices = list("Test 1" = 1, "Test 2" = 2),
                                       selected = 1)
                           ),
                       box(width = 6,
                           dataTableOutput("datTab"))
                       ),

              
              tabPanel("Summary Table and Plots with Cut scores", 
                       
                       box(width = 4, 
                           
                           selectizeInput("select1", label = h4("Generate plots to determine cutscores from:"), 
                                   choices = list("Stage 1 to Stage 2" = 1, "Stage 2 to Stage 3" = 2), 
                                   selected = NULL, multiple = FALSE, width = "100%"),
                           
                           conditionalPanel(condition = "input.select1 == 2",
                                        selectizeInput("select2", label = h4("For this specific route/path"),
                                                    choices=list("1A2A" = 1,  "1A2B" = 2, "1A2C" = 3),
                                                    selected = NULL, multiple = FALSE)),
                           hr(),
                           sliderInput("plotRange", "Adjust range of plotted ability/difficulty estimates:",
                                       min = -6, max = 6,
                                       value = c(-5.5, 5), step = 0.01, width = "100%"),
                           hr(),
                           dataTableOutput("summaryTab")),
                       
                       box(width = 1),
                       
                       box(width = 7, 
                           
                           plotOutput("itemPlot"),
                           checkboxInput("showCutLogits", 
                                         label = "Show vertical lines that indicate cut-scores' equivalent in logits",
                                         value = FALSE, width = "100%"),
                           hr(),
                           
                           plotOutput("cutscorePlot"),
                           hr(),
                           
                           selectizeInput("selectYear", "Generate histogram of ability estimates for the selected cohort/s",
                                          choices = c("Year 2", "Year 3", "Year 4", "Year 5", "Year 6"),
                                          selected = c("Year 2", "Year 3", "Year 4", "Year 5", "Year 6"), multiple = TRUE, width = "100%"),                          
                           checkboxInput("splitPlot", label = "Split plot by year level",
                                         value = FALSE, width = "100%"),
                           plotOutput("distPlot"),
                           hr()
 
                          

                          )
                       )
  )
)


server <- function(input, output, session) {
  
 itemInfo <- reactive({
   
   inFile <- input$file
   
   if (is.null(inFile))
     return(NULL)
   
   validModules <- c("1A", "2A", "2B", "2C", "3A", "3B", "3C")
    
   read_csv(inFile$datapath, na = c("", "NA", "#N/A")) %>%  
    select(item = Identifier, test1modules = Module_Test1, test2modules = Module_Test2,
           location = Location, recommended = Recommended,
           contentDomain = `Content Domain`, type = Type,
           grammarLevel = `Grammar Level`, subdomain = Subdomains) %>%
     mutate(test1modules = ifelse(test1modules %in% validModules, test1modules, NA),
            test2modules = ifelse(test2modules %in% validModules, test2modules, NA),
            location = ifelse(location == "*", NA, location),
            location = as.numeric(location)) %>% 
    arrange(item)

  })
  
  dat <- reactive({
    
    if(is.null(itemInfo()))
      return(NULL)
    
    if(input$selectTest == 1){xxx <- itemInfo() %>% select(-test2modules) %>% 
      rename(module = test1modules) %>% na.omit(module) %>% mutate(test = input$selectTest)}
    if(input$selectTest == 2){xxx <- itemInfo() %>% select(-test1modules) %>% 
      rename(module = test2modules) %>% na.omit(module) %>% mutate(test = input$selectTest)}
    
    yyy <- xxx %>%
      na.omit() %>% 
      arrange(module, item) %>% 
      select(test, module, item, location, contentDomain, recommended)
    
    return(yyy)
  })
  
  output$datTab <- renderDataTable({
    dat()
    })
  
  output$summaryTab <- renderDataTable({
    
    if(is.null(itemInfo()))
      return(NULL)
    
    summary <- dat() %>% 
      group_by(test, module) %>% 
      summarise(count = n(),
                min = min(location),
                mean = mean(location),
                max = max(location))
    })
  
  
  x_min <- reactive({input$plotRange[1]})
  x_max <- reactive({input$plotRange[2]})
  
    
 datCutscorePlot <- reactive({
   
   for(ii in unique(dat()$module)){
     a1<- dat() %>% filter(module == ii)
     zero <- rep(0, nrow(a1))
     slope<- rep(1, nrow(a1))
     zb<-cbind(slope,a1$location,zero)
     eval(parse(text=paste0("p", ii,"<-zb")))
     rm(zb)
   }
   
   xx <- seq(-6, 6, by=0.01)
   
   
   trf<-function(zb){
     cap<-capture.output(aa<-irtoys::trf(ip=zb,  x=xx))
     return(aa)
   }
   tif<-function(zb){
     cap<-capture.output(bb<-irtoys::tif(ip=zb,  x=xx))
     return(bb)
   }
   
   
   # first to second stage
   if(input$select1 == 1){
     df1 <- tibble(x = trf(p1A)$x,
                   p1A = trf(p1A)$f,
                   p2A = tif(p2A)$f,
                   p2B = tif(p2B)$f,
                   p2C = tif(p2C)$f) %>% 
       mutate(diffAB = abs(p2B-p2A), diffBC = abs(p2C-p2B))}
   
   if(input$select1 == 2 & input$select2 == 1){
     df1 <- tibble(x = trf(rbind(p1A, p2A))$x,
                   p1A2A = trf(rbind(p1A, p2A))$f,
                   p3A = tif(p3A)$f,
                   p3B = tif(p3B)$f,
                   p3C = tif(p3C)$f) %>%
       mutate(diffAB = abs(p3B-p3A), diffBC = abs(p3C-p3B))}
   
   if(input$select1 == 2 & input$select2 == 2){
     df1 <- tibble(x = trf(rbind(p1A, p2B))$x,
                   p1A2B = trf(rbind(p1A, p2B))$f,
                   p3A = tif(p3A)$f,
                   p3B = tif(p3B)$f,
                   p3C = tif(p3C)$f) %>%
       mutate(diffAB = abs(p3B-p3A), diffBC = abs(p3C-p3B))}
   
   if(input$select1 == 2 & input$select2 == 3){
     df1 <- tibble(x = trf(rbind(p1A, p2C))$x,
                   p1A2C = trf(rbind(p1A, p2C))$f,
                   p3A = tif(p3A)$f,
                   p3B = tif(p3B)$f,
                   p3C = tif(p3C)$f) %>%
       mutate(diffAB = abs(p3B-p3A), diffBC = abs(p3C-p3B))}
   
   
   df2 <- df1 %>% 
     mutate(minAB = min(diffAB), minBC = min(diffBC)) %>%
     mutate(cut1 = ifelse(diffAB == minAB, 1, 0),
            cut2 = ifelse(diffBC == minBC, 1, 0)) %>%
     select(-contains("AB"), -contains("BC")) %>%
     gather(module, y, starts_with("p")) %>%
     mutate(module = str_sub(module, 2, -1L))
   
   return(df2)
   
   })
    
    
    
    cuts <- reactive({
      datCutscorePlot() %>% filter(cut1 == 1 | cut2 == 1) %>%
      spread(module, y)
    }) 
    

    
    output$cutscorePlot <- renderPlot({
      
      if(is.null(itemInfo()))
        return(NULL)  
      
      p <- ggplot() +
        geom_line(data = datCutscorePlot(),
                  aes(x = x, y = y, group = module, colour = module), size = .75) +
        scale_y_continuous(labels = function (x) floor(x)) +
        labs(y = "Expected Score", x = "Ability")
      
      if(input$select1 == 1){
        plot <-  p + geom_segment(data = cuts(), aes(x = x, y = `1A`, xend = x, yend = -Inf)) +
          geom_segment(data = cuts(), aes(x = x, y = `1A`, xend = -Inf, yend = `1A`))  +
          annotate("text", label = paste0(round(cuts()$`1A`[1],1)), x = x_min(), y = cuts()$`1A`[1]+.35, colour = "maroon") +
          annotate("text", label = paste0(round(cuts()$`1A`[2],1)), x = x_min(), y = cuts()$`1A`[2]+.35, colour = "maroon") +
          labs(title = paste0("Test ", input$selectTest, " First to Second Stage"), subtitle = "Module 1A")
        }
      
      if(input$select1 == 2 & input$select2 == 1){
        plot <- p + geom_segment(data = cuts(), aes(x = x, y = `1A2A`, xend = x, yend = -Inf)) +
          geom_segment(data = cuts(), aes(x = x, y = `1A2A`, xend = -Inf, yend = `1A2A`))  +
          annotate("text", label = paste0(round(cuts()$`1A2A`[1],1)), x = x_min(), y = cuts()$`1A2A`[1]+1, colour = "maroon") +
          annotate("text", label = paste0(round(cuts()$`1A2A`[2],1)), x = x_min(), y = cuts()$`1A2A`[2]+1, colour = "maroon") +
          labs(title = paste0("Test ", input$selectTest, " Second to Third Stage"), subtitle = "Route: Module 1A to Module 2A")
        }
      
      if(input$select1 == 2 & input$select2 == 2){
        plot <- p + geom_segment(data = cuts(), aes(x = x, y = `1A2B`, xend = x, yend = -Inf)) +
          geom_segment(data = cuts(), aes(x = x, y = `1A2B`, xend = -Inf, yend = `1A2B`))  +
          annotate("text", label = paste0(round(cuts()$`1A2B`[1],1)), x = x_min(), y = cuts()$`1A2B`[1]+1, colour = "maroon") +
          annotate("text", label = paste0(round(cuts()$`1A2B`[2],1)), x = x_min(), y = cuts()$`1A2B`[2]+1, colour = "maroon") +
          labs(title = paste0("Test ", input$selectTest, " Second to Third Stage"), subtitle = "Route: Module 1A to Module 2B")
        }
      
      if(input$select1 == 2 & input$select2 == 3){
        plot <- p + geom_segment(data = cuts(), aes(x = x, y = `1A2C`, xend = x, yend = -Inf)) +
          geom_segment(data = cuts(), aes(x = x, y = `1A2C`, xend = -Inf, yend = `1A2C`))  +
          annotate("text", label = paste0(round(cuts()$`1A2C`[1],1)), x = x_min(), y = cuts()$`1A2C`[1]+1, colour = "maroon") +
          annotate("text", label = paste0(round(cuts()$`1A2C`[2],1)), x = x_min(), y = cuts()$`1A2C`[2]+1, colour = "maroon") +
          labs(title = paste0("Test ", input$selectTest, " Second to Third Stage"), subtitle = "Route: Module 1A to Module 2C")
        }
      
      return(plot + theme(legend.position="top") + 
               xlim(input$plotRange[1], input$plotRange[2]) +
               annotate("text", label = paste0(round(cuts()$x[1],2), "\nlogits"), x = cuts()$x[1]+.5, y = -1, colour = "maroon") +
               annotate("text", label = paste0(round(cuts()$x[2],2), "\nlogits"), x = cuts()$x[2]+.5, y = -1, colour = "maroon") )
      })
    
    output$distPlot<- renderPlot({
      
      if(is.null(itemInfo()))
        return(NULL)
      
      plot <- pLocs %>% 
        mutate(yearLevel = paste0("Year ", yearLevel)) %>% 
        filter(yearLevel %in% input$selectYear) %>% 
        ggplot(aes(x = location)) +
        xlim(x_min(), x_max()) +
        geom_histogram(fill = "grey70", colour = "blue", binwidth = .3) +
        labs(title = "Distribution of Trial Participants' Abilities",
             x ="Ability")
      
      if(input$splitPlot == TRUE){
        return(plot + facet_wrap(~yearLevel, ncol = 1))
        } 
      
      return(plot)
      
      })
    
    output$itemPlot <- renderPlot({
      
      if(is.null(itemInfo()))
        return(NULL)
      
      plot <- dat() %>% select(module, item, location) %>% 
        mutate(module = as.factor(module)) %>% 
        ggplot(aes(x = location, y = module)) +
        xlim(x_min(), x_max()) +
        geom_point() +
        labs(title = paste0("Item Difficulties Across Test ", input$selectTest, " Modules"),
             x = "Item Difficulty",
             y = paste0("Test ", input$selectTest, " Modules"))
      
      if(input$showCutLogits == TRUE){
        return(plot + geom_vline(xintercept = cuts()$x[1], colour = "blue", linetype = 2) +
                 geom_vline(xintercept = cuts()$x[2], colour = "blue", linetype = 2))
        }
      
      return(plot)
      
      })
    
    
    } # server ends here



# Run the application 
shinyApp(ui = ui, server = server)