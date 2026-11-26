## generate team .html files
source("scripts/preparing_data.R")

library(rmarkdown)
rjmlinfo <- read.csv('data_prep/RJMLteaminfo.csv')
teams <- sort(rjmlinfo$Team)

generate_team_reports <- function(){
  
  for (tm in teams) {
    # Define the output file name
    output_file_name <- paste0(tm, ".html")
    
    # Call rmarkdown::render()
    # Pass the parameter value to the 'params' argument as a list
    render(
      input = "team.Rmd",            # The Rmd template file
      output_file = output_file_name,    # The name for the new HTML file
      params = list(team = tm), # The parameters list
      envir = parent.frame()             # Ensures data is accessible in Rmd
    )
  }
}

generate_team_reports()

