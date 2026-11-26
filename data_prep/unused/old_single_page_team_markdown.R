# old single page team file

---
  title: "`r params$team`"
output: 
  html_document:
  toc: false
params:
  team: 'VAN'
---
  
```{css, echo = FALSE, eval=FALSE}
body .main-container {
  max-width: 100% !important;
  width: 100% !important;
  margin-left: 5px !important;
  margin-right: 5px !important;
  padding: 5px !important; /* Adjust padding as needed */
}

/* Optional: Adjust margins/padding for the body itself 
body {
  margin: 0px !important;
  padding: 0px !important;
} */
  ```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
options(knitr.kable.NA = '')
library(dplyr)
library(kableExtra)
library(htmltools)
tm = params$team
info <- read.csv('data_prep/RJMLteaminfo.csv') |> filter(Team == tm) |> select(Team,Full_team,Logo_url,Manager,Park,team_color)
#info <- scrape_info(tm) #returns vector c(team,logo_url, manager,park,position,record,color))
#page_title <- info[1]
page_title <- paste0('<span style="font-weight: bold;color:',info[1,6],'">',info[1,2], '</span>',' - ','<span style="color: black;font-size=.5em">',info[1,4], '</span>')
draft <- read.csv(paste0('data/',tm,'_draft.csv'))
hit <- read.csv(paste0('data/',tm,'_hit.csv'))
pit <- read.csv(paste0('data/',tm,'_pit.csv'))
teamPA <- sum(hit$PA)
teamGS <- sum(pit$GS)
teamBF <- sum(pit$BF)-25*teamGS
```


<div style="display: flex; align-items: center; justify-content: center; gap: 5px;"><img src=`r info[1,3]` alt="Logo" style="height: 70px;"><div style="text-align: center;font-size:1.5em; line-height: 1.4;">`r info[1,2]`<br><span style="font-size: 0.9em; font-weight: normal;">`r info[1,4]`</span></div></div>
  
  # {.tabset .tabset-pills}
  <p>
  
  ::::: {.columns}


::: {.column width="20%"}

<h3 align='center'>Draft Picks</h3>
  
  ```{r}
draft |> 
  kbl(booktabs = T,
      col.names = c('Pick','Player')) |>
  column_spec(c(1,2), background ='wheat') |> 
  kable_styling(font_size = 13,  full_width = F) |> 
  scroll_box(
    width = "100%", 
    box_css = "border: 2px solid #333; padding: 3px; border-radius: 5px;"
  )
```
:::
  
:::{.column width="3%"}

:::
  
  ::: {.column width="35%"}

<h3 align='center'>Hitters <span style="font-size: 80%; font-weight: normal; color:blue"> (Total PA = `r teamPA`)</span></h3>
  
  ```{r, results="asis"}
hit |> select(-RC27) |> mutate(AVG = AVG*1000,OBP = OBP*1000,SLG = SLG*1000, OPS = OPS*1000) |> 
  kbl(booktabs = T, 
      #col.names = c('Hitters','Positions','PA','RC27'),
      #caption = "Hitters"
  ) |> 
  kable_styling(font_size = 13, full_width = F,fixed_thead = T)|> 
  scroll_box(
    height = "500px",
    width = "100%", 
    box_css = "border: 2px solid #333; padding: 3px; border-radius: 5px;"
  ) |>
  HTML()
```

:::
  
  :::{.column width="2%"}

:::
  
  ::: {.column width="40%"}

<h3 align='center'>Pitchers <span style="font-size: 80%; font-weight: normal; color:blue"> (Total GS = `r teamGS`; Relief BF $\approx$ `r teamBF`)</span></h3>
  
  ```{r, results='asis'}
pit |> select(-RC27) |> mutate(OPS = OPS*1000) |>
  kbl(#booktabs = T,
    #col.names = c('Pitchers','GS','BF','Sdur','Rdur')
  ) |> 
  kable_styling(font_size = 13, full_width = F)|> 
  scroll_box(
    height = "500px",
    width = "100%", 
    box_css = "border: 2px solid #333; padding: 3px; border-radius: 5px;"
  ) |>
  HTML()
```

:::

  :::::
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  