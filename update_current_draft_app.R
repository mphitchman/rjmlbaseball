library(shiny)
library(DT)
library(dplyr)

# Initial data frames draft matrix, available hitters, available pitchers
load('rdata/matrix.rda')
matrix <- matrix |> mutate(pick_and_team = paste(pick," - ", current_team))
load('rdata/hit_draft.rda')
load('rdata/pit_draft.rda')


initial_df1 <- matrix
initial_df2 <- hit_draft 
initial_df3 <- pit_draft


teams <- sort(unique(matrix$orig_team))


ui <- fluidPage(
  tags$style(HTML('table.dataTable {color: black !important;}')),
  navbarPage(title='2027 RJML Draft',
             tabPanel("Draft Matrix",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     # Dropdown to select a Name from df2 (hitters) and df3 (pitchers) (dynamically updated)
                                     selectInput("name_select", "Select Player to Draft:", choices = NULL, selected = NULL),
                                     
                                     # Dropdown to select the row in df1 (draft matrix) to update
                                     selectInput("row_select", "Select Draft Pick:", choices = initial_df1$pick_and_team),
                                     
                                     # Button to perform the action
                                     actionButton("assign_button", "Make Pick and Update Tables"),
                                     
                                     # Button to save updated tables
                                     actionButton("save_button","Save updated .rda files")
                                     
                                  
                        ),
                        mainPanel(width=9,fluidRow(dataTableOutput("picks"))),
                      )
             ),
             tabPanel("Hitters",fluidRow(dataTableOutput("hit"))),
             tabPanel("Pitchers",fluidRow(dataTableOutput("pit")))
  )
)


server <- function(input, output, session) {
  # Use reactiveValues to store and manage the data frames
  rv <- reactiveValues(df1 = initial_df1, 
                       df2 = initial_df2, 
                       df3 = initial_df3)
  
  # Observer to dynamically update the Name dropdown choices from df2, df3
  
  # dynamically adjust the selected pick to be first available
  
  observe({
    # Find the first pick whose corresponding `player` is an empty string
    first_available_pick <- rv$df1 %>%
      filter(player == "") %>%
      pull(pick_and_team) %>%
      first()
    
 #   current_owner <- rv$df1 %>%
#      filter(pick == first_available_pick) %>%
#      pull(current_team) %>%
#      first() 
    
    # Update the selectInput's selected value
    if (!is.na(first_available_pick)) {
      updateSelectInput(session,
                        "row_select",
                        selected = first_available_pick)
    }
    

    
    
    
  })
  
  # dynamically update draftable names
  
  observe({
    updateSelectInput(session, 
                      "name_select", 
                      choices = list(
                        "Hitters" = rv$df2$Name[which(rv$df2$Team == 'avail')],
                        "Pitchers" = rv$df3$Name[which(rv$df3$Team == 'avail')],
                        ' ' = ''),
                      selected = ''
                      )
  })
  
  
  
  # Render Table 1 (df1) using the DT package for better interactivity
  output$picks <- renderDT({
    datatable(rv$df1, 
              selection = 'single', 
              options = list(pageLength=-1,lengthChange = FALSE,scrollY = "400px",scroller = TRUE, scrollX = T,
                             search = list(regex = TRUE,caseInsensitive = FALSE),
                             dom = 'Bfrtip', buttons=c('copy','csv'),
                             selection = list(target="cell",mode="single"),
                             rownames = F,
                             server=F),
    )
  })
  
  # Render Table 2 (df2)
  output$hit <- renderDT({
    datatable(rv$df2,
              filter = list(position = 'top',clear=FALSE),
              callback=JS('$("button.buttons-copy").css("background","wheat");
                    $("button.buttons-csv").css("background","wheat");
                    return table;'),
              extensions = 'Buttons',
              #caption = 'Table can be filtered, sorted, and downloaded',
              options = list(pageLength=-1,lengthChange = FALSE,scrollY = T,scroller = TRUE, scrollX = T,
                             search = list(regex = TRUE,caseInsensitive = FALSE),
                             dom = 'Bfrtip',buttons=c('copy','csv'),
                             selection = list(target="cell",mode="single"),
                             server=F),
              rownames = F)
  })
  
  # Render Table 3 (df3)
  output$pit <- renderDT({
    datatable(rv$df3,
              filter = list(position = 'top',clear=FALSE),
              callback=JS('$("button.buttons-copy").css("background","wheat");
                    $("button.buttons-csv").css("background","wheat");
                    return table;'),
              extensions = 'Buttons',
              #caption = 'Table can be filtered, sorted, and downloaded',
              options = list(pageLength=-1,lengthChange = FALSE,scrollY = T,scroller = TRUE, scrollX = T,
                             search = list(regex = TRUE,caseInsensitive = FALSE),
                             dom = 'Bfrtip',buttons=c('copy','csv'),
                             selection = list(target="cell",mode="single"),
                             server=F),
              rownames = F)
  })
  
  # Observe the action button click to perform the transfer
  observeEvent(input$assign_button, {
    selected_name <- input$name_select
    selected_row_id <- input$row_select
    selected_pos <- ''
    if (selected_name %in% rv$df2$Name){
      selected_pos <- rv$df2$Pos[which(rv$df2$Name == selected_name)]
    }
    if (selected_name %in% rv$df3$Name){
      selected_pos <- rv$df3$P[which(rv$df3$Name == selected_name)]
    }
    # Ensure a name is selected
    if (!is.null(selected_name) && selected_name != "") {
      # 1. Add the selected Name to the chosen row in df1
      # Use an isolate() to only react to the button press, not changes in rv$df1/df2/inputs directly within this logic block before the update
      isolate({
        # Update the 'Name' column in the specific row of rv$df1
        rv$df1[rv$df1$pick_and_team == selected_row_id, "player"] <- paste0(selected_name,", ",selected_pos)
        
        # 2. update the corresponding row from df2 or df3
        rv$df2$Team[which(rv$df2$Name == selected_name)] <- rv$df1$current_team[which(rv$df1$pick_and_team == selected_row_id)]
        rv$df3$Team[which(rv$df3$Name == selected_name)] <- rv$df1$current_team[which(rv$df1$pick_and_team == selected_row_id)]

        #rv$df3 <- rv$df3 %>% filter(Name != selected_name)
      })
    }
  })
  
  #observe the save new .rda files action button
  #pass on Vancouver 13.09
  matrix$player[297] <- 'pass'
  
  observeEvent(input$save_button, {
    matrix <- rv$df1
    hit_draft <- rv$df2
    pit_draft <- rv$df3
    # Specify the objects to save (e.g., app_data()) and the file path
    save(matrix, file = "rdata/matrix.rda")
    save(hit_draft, file = "rdata/hit_draft.rda")
    save(pit_draft, file = "rdata/pit_draft.rda")
    # Optional: provide feedback to the user
    showNotification("matrix.rda, hit_draft.rda, and pit_draft.rda have been saved", type = "message")
  })
}


shinyApp(ui, server)
