# Reverting
ui <- function(request) {
  tagList(
    useShinyjs(),
    add_busy_spinner(
      spin = "fading-circle", 
      position="bottom-left", 
      color = "#9146b7",
      margins = c(35,35)
      ),
    shinyjs::extendShinyjs(
      script = "lib/shinyui.js",
      functions = c(
        "isTabdisabled", # For testing purposes
        "isIconhidden", # For testing purposes
        "disableTab", # Disables a tab
        "enableTab", # Enables a tab
        "disableBtn", # Disables a button
        "toggleTabInputs" # Toggles state of inputs on a page
      )
    ),
    list(tags$head(HTML('<link rel="icon", href="pmartlogo.png", 
                                 type="image/png" />'))),
    
    div(
      id = "loading-gray-overlay",
      class = "loading-mask",
      div(class = "fadein-out busy relative-centered", style = "font-size:xx-large", "Loading app resources...")
    ), 
    
    inlineCSS('.navbar-default .navbar-brand {padding: 2px;}'),
    
    navbarPage(
      title = tags$img(id = "pmart-logo", src = "pmartlogo.png", style = "max-height:100%;cursor:pointer;"),
      windowTitle = "pmartR",
      id = "top_page", theme = "pmartR.css",
      tabPanel(
        'Welcome',
        value = "intro_panel",
        wellPanel(style='width:75%',
          includeMarkdown("www/welcome.md"),
          downloadButton("download_example_data", "Download example data"),
          downloadButton("download_example_report", "Download example report")
        )
      ),
      
      #### All tab UI's are built by functions defined in ./tabs_UI/ ####

      ##### UPLOAD TAB ######
      upload_UI(),

      ###### GROUPS TAB #######

      groups_UI(),

      ##### Reference TAB ######
      reference_UI(),
      
      #### DATA SUMMARY TAB #####

      data_summary_UI(),

      #### FILTER TAB ####

      filter_UI(),

      #### NORMALIZATION TAB ####

      normalization_UI(),

      #### Peptide-level Tabs ####
      
      #### Peptide Statistics ####
      peptide_statistics_UI(),

      #### Protein Rollup ####
      protein_rollup_UI(),

      #### statistics TAB ####

      statistics_UI(),

      #### DOWNLOAD TAB ####

      download_UI(),

      # just a help button
      tabPanel(div(style = "height:20px", uiOutput("how_use_page_UI")), value = "nav_help_options")
    ), # end Navbarpage

    ## Plot saving buttons
    div(
      style = "position:absolute;top:3px;right:16px;z-index:1100;",
      div(
        class = "inline-wrapper-1", 
        bsButton("viewplots", uiOutput("n_saved_plots"), style = "info"),
        hidden(div(id = "js_saveplot", style = "vertical-align:top",
          bsButton("saveplot", "Save last plot", style = "info")
        )),
        # Add UI if MAP is enabled
        if (MAP_ACTIVE) {
          div(id = "js_midpoint", style = "vertical-align:top", class="tooltip-wrapper",
              bsButton("exportMid", "Save and export progress", style = "success")
          )
        } else NULL
      )
    ),
    uiOutput("developer_buttons")
  )
} # shinyUI + tagList
