e_data <- reactive({
  # Error handling: Need file_edata path
  req(input$file_edata$datapath)

  # Load file
  filename <- input$file_edata$datapath

  # exportTestValues(e_data = read.csv(filename, stringsAsFactors = FALSE))
  read.csv(filename, stringsAsFactors = FALSE)
})

e_data_2 <- reactive({
  # Error handling: Need file_edata path
  req(input$file_edata_2$datapath)

  # Load file
  filename <- input$file_edata_2$datapath

  # exportTestValues(e_data = read.csv(filename, stringsAsFactors = FALSE))
  read.csv(filename, stringsAsFactors = FALSE)
})

# indicator to check whether there are zeros in the data
e_data_has_zeros <- reactive({
  if(two_lipids()){
    any(e_data() == 0, na.rm = T) | any(e_data_2() == 0, na.rm = T) 
  }
  else{
    any(e_data() == 0, na.rm = T)
  }
})

# Object: Emeta column names
# Note: created when emeta is loaded/updated
emeta_cnames <- reactive({
  colnames(revals$e_meta)
})

emeta_cnames_2 <- reactive({
  colnames(revals$e_meta_2)
})

# Object: Get list of column names of Edata
# Note: created when e_data is uploaded
edata_cnames <- reactive({
  colnames(e_data())
})

edata_cnames_2 <- reactive({
  colnames(e_data_2())
})

# Object: Sample names from e_data
# Note: This object is created when e_data and edata_id are entered
sample_names <- reactive({
  setdiff(names(e_data()), input$id_col)
})

sample_names_2 <- reactive({
  setdiff(names(e_data_2()), input$id_col_2)
})
# End sample_names #


f_data_upload <- reactive({
  req(sample_names())
  col2 <- rep(NA, length(sample_names()))
  data.frame("SampleId" = sample_names(), "Var1" = col2)
}) # End fdata #

f_data_upload_2 <- reactive({
  req(sample_names_2())
  col2 <- rep(NA, length(sample_names_2()))
  data.frame("SampleId" = sample_names_2(), "Var1" = col2[0:length(sample_names_2())])
})

two_lipids <- reactive({
  input$datatype == "lip" & isTRUE(input$twolipids_yn == "TRUE")
})
