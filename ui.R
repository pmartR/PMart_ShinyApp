ui <- function(request) {
  tagList(
    useShinyjs(),
    list(tags$head(HTML('<link rel="icon", href="pmartlogo.png", 
                                 type="image/png" />'))),
          div(style="display:none",
              titlePanel(
                title="", windowTitle="pmartR"
              )
          ),
  navbarPage(title = tags$span(tags$img(src = 'pmartlogo.png', style='max-height:100%'), "pmartR"),
             id = 'top_page', theme = 'pmartR.css',
             
    ##### UPLOAD TAB ######
             
    tabPanel('Upload Data', class = 'collapse_page',
             fluidRow(  # begin fluidrow
               column(4, # sidebarpanel
                      bsCollapse(id = 'upload_collapse_left', open = 'datselect', multiple = TRUE, # parent collapse div
                        # upload edata sub-collapse div
                        bsCollapsePanel(div("Specify data type and upload data file", 
                                            hidden(div(id = 'ok_datselect', style = 'color:orange;float:right', icon('ok', lib='glyphicon')
                                                       )
                                                   )
                                            ), value = 'datselect',
                                        
                                      div(id = 'js_datatype', pickerInput('datatype', 'Specify molecule type',
                                                  choices = c('None'='none', 'Peptides'='pep', 'Proteins'='pro', 'Metabolites'='metab', 'Lipids'='lip')
                                                  )
                                      ),
                                      conditionalPanel("input.datatype=='lip'",
                                                       radioGroupButtons('twolipids_yn', 'Separate files for positive and negative ionization?',
                                                                         choices = c('Yes'='TRUE', 'No'='FALSE'))
                                      ),
                                      hidden(div(id = 'edata_UI_parent',
                                          uiOutput('edata_UI')
                                        )
                                      )
                                    ),
                        # specify various data info sub-collapse div
                        bsCollapsePanel(div('Specify ID Columns and Data Values', 
                                          hidden(div(id = 'ok_columnids', style = 'color:orange;float:right', icon('ok', lib='glyphicon')
                                                    )
                                                )
                                        ), value = 'columnids',
                                      conditionalPanel("input.datatype=='pep'",
                                                       radioGroupButtons('proteins_yn', 'Does your metadata file contain peptide to protein mappings?',
                                                                         choices = c('Yes'='TRUE', 'No'='FALSE')),
                                                       radioGroupButtons('labeled_yn', 'Is this labeled peptide data?',
                                                                         choices = c('Yes'='TRUE', 'No'='FALSE'), selected = 'FALSE')
                                      ),
                                      hidden(div(id = 'js_id_col',
                                                uiOutput('id_col')
                                                )
                                          ),
                                      hidden(div(id = 'js_datascale',
                                                  uiOutput('datascale_UI'),
                                                  uiOutput('transform')
                                            )
                                          ),
                                      hidden(div(id = 'js_na_symbol',
                                                textInput('na_symbol', 'What value denotes missing data?', value = NA)
                                            )
                                          ),
                                      conditionalPanel("input.labeled_yn == 'FALSE'",
                                        hidden(div(id = 'js_normalized_yn',
                                              uiOutput('normalized_UI')
                                          )
                                        )
                                      ),
                                      actionButton("BROWSER", "Activate Browser?"),
                                      div(id = 'donebutton', style = 'float:right', actionButton('done_idcols', div("I'm done specifying values", icon('ok-sign', lib='glyphicon'))))
                                    ),
                        # upload e_meta and specify protein column sub-collapse div
                        bsCollapsePanel(div("Upload Biomolecule Information", 
                                            hidden(div(id = 'ok_metadata', style = 'color:orange;float:right', icon('ok', lib='glyphicon')
                                                      )
                                            )
                        ), value = 'meta_collapse',
                                        hidden(div(id = 'js_emeta_UI',
                                                   uiOutput('emeta_UI')
                                            )
                                          ),
                                        div(id = 'js_promap',
                                                 uiOutput('promap_UI')
                                                 )
                                        )
                        ), # parent collapse
                      disabled(actionButton('makeobject', "Create omicsData object")),
                      uiOutput('warnings_upload'),
                      br(),
                      uiOutput('uploaded_data_summary')
                      ),# column 4
               column(8,
                      bsCollapse(id = 'upload_preview_collapse',
                        bsCollapsePanel('Data Preview', value = 'summary_tables',
                                      hidden(div(id = "toggle_table",
                                        div(style = 'float:left;margin-top:10px;margin-right:10px;font-weight:bold', "Display dataset:"),
                                        radioGroupButtons('which_table', choices = c('1'=1, '2'=2))
                                        )), 
                                      DTOutput('head_edata'),
                                      DTOutput('head_emeta')
                                  ),
                       bsCollapsePanel('Boxplot Preview', value = 'summary_boxplots',
                                       uiOutput('upload_boxplots')
                                       ) 
                                 ,open = "summary_tables")
              )# main_column
        )# fluidrow
    ),# tabpanel
    
    ###### GROUPS TAB #######
    
    tabPanel('Group Samples', class = 'collapse_page',
            fluidRow(
              column(4,
                     bsCollapse(id = 'groups_collapse_left', open = 'fdata_upload', multiple = TRUE, # parent collapse div
                        # file upload collapse sub-div
                       bsCollapsePanel(div('Upload Groups File', 
                                           hidden(div(id = 'ok_fdata_upload', style = 'color:orange;float:right', icon('ok', lib='glyphicon')
                                                    )
                                                 )
                                        ), value = 'fdata_upload',
                                         div(id ='upload_fdata', style = 'display:inline-block', uiOutput('fdata_UI')),
                                         div(id ='js_download_fdata', class = 'inline_top', style = 'float:right', 
                                              disabled(downloadButton('download_fdata', 'Download file template'))
                                              ),
                                         radioGroupButtons('usevizsampnames', 'Add trimmed sample names for plotting?', choices = c('Yes', 'No'), selected = 'No'),
                                         conditionalPanel(condition = "input.usevizsampnames == 'Yes'",
                                                          radioGroupButtons('customsampnames_opts', 'Trim By:', 
                                                                            choices = c('First x characters' = 'first_n', 'Range of characters' = 'range', 'Split by a character' = 'split')),
                                                          uiOutput('customsampnames_split')
                                                          )
                                         
                                       ),
                       # ID column collapse sub-div
                       bsCollapsePanel(div('Specify Main Effects and Covariates', 
                                           tipify(span(style = 'color:rgb(0,191,255)', icon("question-sign", lib = "glyphicon")), title = main_effects_text),
                                           hidden(div(id = 'ok_fdata_idcols', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))
                       ), value = 'fdata_columns',
                              hidden(div(id = 'js_fdata_id_col', uiOutput('fdata_id_col'))),
                              fluidRow(
                                column(6,
                                  uiOutput('group_col1'),
                                  uiOutput('group_col2'),
                                  uiOutput('cv_col1'),
                                  uiOutput('cv_col2')
                                ),
                                column(6,
                                   uiOutput('group_col1_2'),
                                   uiOutput('group_col2_2'),
                                   uiOutput('cv_col1_2'),
                                   uiOutput('cv_col2_2')
                                )
                              )
                       )
                      ), # parent collapse  
                     disabled(actionButton('group_designation', 'Apply Grouping')),
                     br(),
                     br(),
                     uiOutput('warnings_groups'),
                     uiOutput('grouped_data_summary')
                     ),# column 4
              column(8,
                       bsCollapse(id = 'groups_collapse_right',
                          bsCollapsePanel('Grouping File Preview', value= 'fdata_preview',
                             hidden(div(id = "toggle_fdata",
                                        div(style = 'float:left;margin-top:10px;margin-right:10px;font-weight:bold', "Display dataset:"),
                                        radioGroupButtons('which_fdata', choices = c('1'=1, '2'=2))
                             )),
                             DTOutput('fdata_table')
                          ),
                          bsCollapsePanel('Group Plots', value = 'fdata_plots',
                                          uiOutput('group_barplots')
                            
                          )
                       )
                     )
            )
    ),
  
    #### DATA SUMMARY TAB #####
    
    tabPanel('Data Summary', class = 'collapse_page',
             fluidRow(
               column(4,
                    radioGroupButtons('which_qc_plot', 'Choose a Plot Type:', 
                                      choices = c('Boxplots' = 'boxplots', 'Missing Values Barplots' = 'missingval_bar', 'Missing Values Scatterplots' = 'missingval_scatter')),
                    bsCollapse(id = 'qc_collapse', multiple = TRUE, open=c('plot_type', 'axes_opts'),
                      bsCollapsePanel('Boxplots Options', value = 'boxplot_opts',
                        tagList(
                          div('Order Boxplots By:', style = 'font-weight:bold'),
                          fluidRow(
                            column(6, uiOutput('qc_order_by')),
                            column(6, uiOutput('qc_order_by_2')) 
                          )
                        ),
                        # color selection
                        tagList(
                          div('Color Boxplots By:', style = 'font-weight:bold'),
                          fluidRow(
                            column(6, uiOutput('qc_color_by')),
                            column(6, uiOutput('qc_color_by_2')) 
                          )
                        )
                      ),
                      bsCollapsePanel('Missing Value Options', value = 'missingval_opts',
                        # By sample or by molecule if missingval plot
                        div(id = 'js_missingval_type', radioGroupButtons('missingval_type', 'Plot Missing Values By:',
                                                                         choices = c('Sample' = 'bySample', 'Biomolecule' = 'byMolecule'))),
                        div(id = 'js_qc_colors', pickerInput('qc_colors', 'Missing Values Colors', 
                                                               choices = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds","RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd","Oranges", "Greys", 
                                                                           "Greens", "GnBu", "BuPu","BuGn","Blues", "Set3", "Set2", "Set1", "Pastel2", "Pastel1", "Paired", "Dark2", "Accent", 
                                                                           "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr","PRGn", "PiYG", "BrBG")))
                        )
                    )# parent collapse  
               ),# column 4
               column(8,
                    bsCollapse(id = 'qc_collapse_main', multiple = TRUE, open = c('plots', 'axes_options'),
                               bsCollapsePanel('Plots', value = 'plots',
                                              uiOutput('qc_plots')
                                              ),
                               bsCollapsePanel('Axes Options', value = 'axes_options',
                                               uiOutput('qc_plot_options'),
                                               uiOutput('qc_apply_style')
                                               ),
                               bsCollapsePanel('Summaries', value = 'summaries',
                                               wellPanel(uiOutput('qc_data_summary'))
                                               )
                    )
               )# column 8
             )
      
    ),
    
    #### FILTER TAB ####
    
    # Filter Options
    tabPanel('Filter', class = 'collapse_page',
             column(4,
                    bsCollapse(id = 'filter_collapse', multiple = FALSE, open=c('data_filters'),
                             # biomolecule filters  
                             bsCollapsePanel(div('Biomolecule Filters',
                                                   hidden(div(id = 'ok_data_filters', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))
                               ), value = 'data_filters',
                                 # molecule filter options
                                 fluidRow(
                                   column(6,
                                    actionButton(inputId = 'add_molfilt', 
                                                 label = div('Add/Remove molecule filter', hidden(div(id = 'molfilt_exists', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))), 
                                                 width = '100%'),
                                    actionButton('plot_molfilt', 'Plot filter with current values', width = '100%')
                                   ),
                                   column(6,
                                    numericInput('mol_min_num', 'Minimum number observed', 2, step = 1)
                                   )
                                 ),
                                 
                                 hr(),
                                 # cv filter options
                                 fluidRow(
                                   column(6,
                                          actionButton(inputId = 'add_cvfilt', 
                                                       label = div('Add/Remove CV filter', hidden(div(id = 'cvfilt_exists', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))), 
                                                       width = '100%'),
                                     actionButton('plot_cvfilt', 'Plot this filter', width = '100%')
                                     ),
                                   column(6,
                                     numericInput('cv_threshold', 'Maximum CV', 150, step = 1)
                                   )
                                 ),
                                 
                                 hr(),
                                 # imd-anova filter options
                                 fluidRow(
                                   column(6,
                                          actionButton(inputId = 'add_imdanovafilt', 
                                                       label = div('Add/Remove imd-ANOVA filter', hidden(div(id = 'imdanovafilt_exists', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))), 
                                                       width = '100%'),
                                          actionButton('plot_imdanovafilt', 'Plot this filter', width = '100%')
                                   ),
                                   column(6,
                                          numericInput('min_nonmiss_anova', 'Minimum number observed to perform ANOVA', 2, step = 1),
                                          numericInput('min_nonmiss_gtest', 'Minimum number observed to perform G-test', 3, step = 1)
                                   )
                                 ),
                                 hr(),
                                 # proteomics filter
                                 div(id = 'profilt_UI',
                                     tagList(
                                       fluidRow(
                                         column(6,
                                                actionButton('add_profilt', 
                                                             label = div('Add/Remove proteomics filter', hidden(div(id = 'profilt_exists', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))), 
                                                             width = '100%'),
                                                actionButton('plot_profilt', 'Plot this filter', width = '100%')
                                         ),
                                         column(6,
                                                numericInput('min_num_peps', 'Minimum number of peptides mapped to each protein:', 2, step = 1),
                                                checkboxInput('degen_peps', 'Remove Degenerate Peptides?', TRUE)
                                         )
                                       ),
                                       hr()
                                     )
                                 )
                               
                               ),# end biomolecule filter collapse
                             
                             # sample filters
                             bsCollapsePanel(div('Sample Filters',
                                                   hidden(div(id = 'ok_meta_filters', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))
                                                    ), value = 'sample_filters',
                                   # rmd filter
                                   fluidRow(
                                     column(5,
                                            actionButton(inputId = 'add_rmdfilt', 
                                                         label = div('Add/Remove rMd filter', hidden(div(id = 'rmdfilt_exists', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))), 
                                                         width = '100%'),
                                            actionButton('plot_rmdfilt', 'Plot this filter', width = '100%')
                                     ),
                                     column(7,
                                            numericInput('pvalue_threshold', 'P-value threshold:', 0.001, step = 0.001),
                                            div(id = 'rmd_metrics_js', style = 'color:grey', pickerInput('rmd_metrics', 'Metrics to determine outliers', 
                                                        choices = c('MAD', 'Kurtosis', 'Skewness', 'Correlation'),
                                                        selected = c('MAD', 'Kurtosis', 'Skewness', 'Correlation'),
                                                        multiple = TRUE)),
                                            pickerInput('rmdfilt_plot_type', 'Plot everything or inspect certain samples?', choices = c('Plot all samples'='all', 'Select from all samples'='subset', 'Select from outliers'='outliers')),
                                            uiOutput('rmdfilt_plot_type')
                                     )
                                   ),
                                   hr(),
                                   # custom filter
                                   fluidRow(
                                     column(6,
                                            actionButton(inputId = 'add_customfilt', 
                                                         label = div('Add/Remove custom filter', hidden(div(id = 'customfilt_exists', style = 'color:orange;float:right', icon('ok', lib='glyphicon')))), 
                                                         width = '100%')
                                     ),
                                     column(6,
                                            radioGroupButtons('remove_or_keep', label = 'Remove or keep these choices?', choices = c('Remove', 'Keep'), selected = 'Remove')        
                                     )
                                   ),
                                   uiOutput('fdata_customfilt'),
                                   uiOutput('fdata_regex')
                               )
                    ),# parent collapse
                    actionButton('review_filters', 'Review and Apply Filters', width = '100%'),
                    uiOutput('warnings_filter'),
                    uiOutput('filter_data_summary')
             ),# column 4
             column(8,
                    bsCollapse(id = 'filter_plots', multiple = TRUE, open=c('filter_plots', 'axes_options'),
                            bsCollapsePanel('Visualize filters', value = 'filter_plots',
                              uiOutput('filter_dynamic_mainplot')
                            ),
                            bsCollapsePanel('Axes options', value = 'axes_options',
                              uiOutput('filter_plot_options'),
                              uiOutput('filter_apply_style')
                            )
                    )
             )# column 8
    ),
    navbarPage(
      title = tags$span(tags$img(src = "pmartlogo.png", style = "max-height:100%"), "pmartR"),
      id = "top_page", theme = "pmartR.css",

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
      div(
        id = "js_saveplot", style = "position:absolute;top:3px;right:16px;z-index:1000",
        fluidRow(
          column(6, bsButton("viewplots", uiOutput("n_saved_plots"), style = "info")),
          column(6, bsButton("saveplot", "Save Last Plot", style = "info"))
        )
      )
    ),
    uiOutput("developer_buttons")
  )
} # shinyUI + tagList
