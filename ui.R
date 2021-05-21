ui <- function(request){
  tagList(useShinyjs(), 
          list(tags$head(HTML('<link rel="icon", href="pmartlogo.png", 
                                 type="image/png" />'))),
          div(style="display:none",
              titlePanel(
                title="", windowTitle="pmartR"
              )
          ),
  navbarPage(title = tags$span(tags$img(src = 'pmartlogo.png', style='max-height:100%'), "pmartR"),
             id = 'top_page', theme = 'pmartR.css',
    
    #### All tab UI's are built by functions defined in ./tabs_UI/ ####
                     
    ##### UPLOAD TAB ######
    upload_UI(),
    
    ###### GROUPS TAB #######
    
    groups_UI(),
  
    #### DATA SUMMARY TAB #####
    
    data_summary_UI(),
    
    #### FILTER TAB ####
    
    filter_UI(),
    
    #### NORMALIZATION TAB ####
    
    normalization_UI(),
    
    #### Protein Rollup ####
    protein_rollup_UI(),
    
    #### ANALYSIS TAB ####
    
    analysis_UI(),
    
    #### DOWNLOAD TAB ####
    
    download_UI()
    
  ), # end Navbarpage
  
  ## Plot saving buttons
  hidden(
    div(id = "js_saveplot", style = "position:absolute;top:3px;right:16px;z-index:1000", 
          fluidRow(
            column(6, bsButton("viewplots", uiOutput('n_saved_plots'), style = "info")),
            column(6, bsButton("saveplot", "Save Last Plot", style = "info"))
          )
        )
  ),
  uiOutput("developer_buttons")
)}# shinyUI + tagList
