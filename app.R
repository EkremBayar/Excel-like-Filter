library(shiny)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(janitor)
library(reactable)


# UI
ui <- fluidPage(
  
    column(
      width = 12,
      
      # Application title
      titlePanel("Excel-like Filtering in Shiny Apps"),
      p("Created by", tags$a(href="https://www.linkedin.com/in/ekrem-bayar/", "Ekrem Bayar", target="_blank")),
      
      
      fluidRow(
        
        # Reactable
        column(width = 8, reactableOutput("table"), style = "border: 1px solid black;"),
        
        column(
          width = 4,
          
          # Collapsed Panel
          bsCollapse(
            id = "collapsed_panel", open = "Panel 2",
            bsCollapsePanel(
              value = "Panel 1",style = "primary", "Filters",
              actionButton("collapse_filter_apply", "Apply"),
              actionButton("collapse_filter_clear", "Clear"),
              br(),br(),
              
              fluidRow(
                column(
                  width = 12,
                  virtualSelectInput(
                    inputId = "vsi_variables",
                    label = "Variable Names:",
                    choices = NULL,
                    selected = NULL,
                    search = TRUE,
                    multiple = T
                  )
                )
                
              ),
              hr(),
              
              # Dynamic Inputs
              fluidRow(
                div(style = "clear:both;"),
                column(width = 12,uiOutput("filter_panel"))
              )
              
            )
          )
        )
      )
      
    )

    

    
)

# SERVER
server <- function(input, output, session) {
  
  # Reactive List
  rvList <- reactiveValues(DFRaw = NULL, DFFiltered = NULL, Variables = NULL, Inputs = NULL)
  
  # Reactive Dataframe & Update VirtualSelectInput
  observe({
    rvList$DFRaw <- clean_names(starwars) %>% mutate(Date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), nrow(starwars)))
    
    updateVirtualSelect(session = session, 
                        inputId = "vsi_variables",
                        label = "Variable Names:", 
                        choices = unique(rvList$Variables[,"Variable"]),
                        selected = NULL)

  })
  
  # Reactive Dataframe: Gives the details about dataset and creates input ids for UI
  observe({
    res <- data.frame(
      Class = sapply(rvList$DFRaw, class)
    ) %>%
      rownames_to_column("Variable") %>%
      filter(Class != "list") %>%
      mutate(
        Input = paste0("lapply_var_",Variable),
        Cond = paste0("lapply_cond_",Variable),
        Label = str_to_title(gsub('[[:punct:] ]+',' ',Variable))
      )
    res <- bind_rows(
      res,
      res %>% 
        filter(Class %in% c("integer", "numeric"), str_detect(Input, "lapply_var_")) %>% 
        mutate(Input = paste0(Input,"_2"))
    ) %>% distinct()
    
    rvList$Variables <- res
  })
  
  # Save values of the inputs on the UI as a reactive dataframe
  observe({
    temp <- rvList$Variables %>% pull(Variable)
    classes <- rvList$Variables %>% pull(Class)
    
    temp <- c(paste0("lapply_var_", temp), paste0("lapply_var_", temp, "_2"),paste0("lapply_cond_", temp))
    
    res <- sapply(temp, function(x){input[[x]]})
    res <- data.frame(Variables = temp, Values = I(res), Class = c(classes, classes, classes)) %>% 
      mutate(Colname = str_remove_all(str_remove_all(str_remove_all(Variables, "lapply_cond_"),"lapply_var_"), "_2")) %>% 
      arrange(Colname) %>% 
      filter(!(Class %in% c("factor", "character", "Date") & str_detect(Variables, "lapply_cond_"))) %>% 
      distinct()
    rvList$Inputs <- res
  })
  
  # renderUI produces inputs on the UI dynamically by using lapply. 
  # Moreover, the inputs are sensitive according to class of the each variable
  output$filter_panel <- renderUI({
    
    if(length(input$vsi_variables) > 0){
      
      lapply(input$vsi_variables, function(y){
        
        temp <- rvList$Variables %>% filter(Variable == y)
        
        rvvalues <- isolate(rvList$Inputs %>% filter(Variables == (temp %>% filter(!str_detect(Input, paste0(y, "_2"))) %>% pull(Input))))
        
        rvconds <- isolate(rvList$Inputs %>% filter(Variables == unique(temp$Cond)) %>%  pull(Values))
        
        if(unique(temp$Class) == "integer" | unique(temp$Class) == "numeric"){
          
          rvvalues2 <- isolate(rvList$Inputs %>% filter(Variables == (temp %>% filter(str_detect(Input, paste0(y, "_2"))) %>% pull(Input))))
          
          
          tagList(
            div(
              div(style="float:left;",
                  selectInput(unique(temp$Cond), label = "Condition",
                              choices = c(">", "<", ">=", "<=", "Between", "==", "!="),
                              selected = rvconds, width = 100
                  )),
              
              div(style="float:left; margin-left:5px;",
                  numericInput(rvvalues$Variables, label = rvvalues$Colname, value = rvvalues$Values, width = 95)),
              
              conditionalPanel(
                condition = paste0("input.",unique(temp$Cond)," == 'Between'"),
                div(style="float:left; margin-left:5px;",
                    numericInput(rvvalues2$Variables, label = paste0(rvvalues2$Colname, " Max."), value = rvvalues2$Values, width = 95))
              )
              
              
            ),
            div(style = "clear:both;")
            
          )
          
        }else if(temp$Class == "character" | temp$Class == "factor"){
          
          choices_value <- rvList$DFRaw %>% select(all_of(y)) %>% pull(y) %>% unique() %>% sort()
          
          tagList(
            virtualSelectInput(
              inputId = temp$Input,
              label = temp$Label,
              choices = choices_value,
              selected = unlist(rvvalues),
              search = TRUE,stateInput = F,
              multiple = T
            ),
            div(style = "clear:both;")
            
          )
          
        }else if(temp$Class == "Date"){
          
          tagList(
            div(
              airDatepickerInput(
                inputId = temp$Input, label = temp$Label,
                placeholder = "Placeholder",
                multiple = 2, todayButton = T,
                value = rvvalues[[1]],
                clearButton = TRUE
              ),style = "position: relative; z-index: 0;"),
            div(style = "clear:both;")
            
          )
        }
        
      }
      
      )
      
    }
    
  })
  
  # Filter Dataset
  observeEvent(input$collapse_filter_apply,{
    
    if(length(input$vsi_variables) > 0){
      
      filters <- rvList$Inputs[!sapply(rvList$Inputs$Values, is.null), ]
      
      filter_list <- c()
      for(i in unique(filters$Colname)){
        
        temp <- filters %>% filter(Colname == i)
        
        if(unique(temp$Class) == "character" | unique(temp$Class) == "factor"){
          
          fp <- paste0(temp$Colname, " %in% c(", paste0('"',unlist(temp$Values),'"', collapse = ", "), ")")
          filter_list <- c(filter_list, fp)
          
        }else if(unique(temp$Class) == "Date"){
          
          val <- temp$Values[[1]]
          
          if(length(val) == 1){
            fp <- paste0(temp$Colname, " >= ", "'",val,"'")
          }else{
            fp <- paste0(temp$Colname, " >= ", "'",as.character(val[1]),"', ", temp$Colname, " < ", "'",as.character(val[2]), "'")
          }
          filter_list <- c(filter_list, fp)
          
        }else if(unique(temp$Class) == "integer" | unique(temp$Class) == "numeric"){
          
          #print(temp)
          
          val <- temp %>% filter(str_detect(Variables, "lapply_var_"))
          
          
          val1 <- unlist(val %>% filter(!str_sub(Variables, -2) == "_2") %>% pull(Values))
          
          
          condname <- temp %>% filter(str_detect(Variables, "lapply_cond_"))
          
          if(condname$Values == "Between"){
            
            val2 <- unlist(val %>% filter(str_sub(Variables, -2) == "_2") %>% pull(Values))
            
            if(!is.na(val1) & !is.na(val2)){
              fp <- paste0(condname$Colname, " >= ", val1,", ", condname$Colname, " <= ", val2)
              filter_list <- c(filter_list, fp)
            }
            
          }else{
            
            if(!is.na(val1)){
              fp <- paste0(condname$Colname, condname$Values, val1)
              filter_list <- c(filter_list, fp)
            }
            
          }
          
          
        }
        
      }
      
      data <- eval(
        parse(
          text = paste0(
            "filter(rvList$DFRaw,",
            paste0(filter_list, collapse = ", "),
            ")"
          )
        )
      )
      
      rvList$DFFiltered <- data
      
    }else{
      rvList$DFFiltered <- NULL
    }
    
    
    
  })
  
  
  # Clear Filter
  observeEvent(input$collapse_filter_clear,{
    
    updateVirtualSelect(session = session, 
                        inputId = "vsi_variables",
                        label = "Variable Names:", 
                        choices = unique(rvList$Variables$Variable),
                        selected = NULL)
    
    rvList$DFFiltered <- NULL
    
  })
  
  
  
  # Reactable
  output$table <- renderReactable({
    if(is.null(rvList$DFFiltered)){
      df <- rvList$DFRaw
    }else{
      df <- rvList$DFFiltered
    }
    
    reactable(
      df,
      showPageSizeOptions = TRUE,
      sortable = T,
      filterable = T,
      selection = "multiple",
      onClick = "select",
      highlight = T,
      bordered = T,
      compact = T,
      striped = T,
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#C6E0B4", boxShadow = "inset 2px 0 0 0 #ffa62d")
      ),
      defaultColDef = colDef(
        
        headerStyle = list(background = "darkgray", color = "black")
      ),
      style = "z-index: 0;"
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)