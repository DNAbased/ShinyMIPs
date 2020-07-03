#shinyMIPs

#based on MIPgen by Evan Boyle

library(shiny)
library(shinythemes)
library(shinyjs)
library(colourpicker)
library(shinyBS)
library(shinyWidgets)
library(Biostrings)
library(wordcloud)
library(wordcloud2)

#set working directory
setwd("/home/MIPgen")

source("helpers.R")

# ui ----------------------------------------------------------------------

#start of ui
ui <- tagList(tags$style(type = "text/css", "
                         .irs-bar {
                         border-top-color: #2FA4E7;
                         border-bottom-color: #2FA4E7;
                         } 
                         .irs-bar-edge {
                         border-color: #2FA4E7;
                         }
                         .irs-from, .irs-to, .irs-bar-edge, .irs-bar {
                         background: #2FA4E7;
                         }
                         .irs-single, .irs-to, .irs-bar-edge, .irs-bar {
                         background: #2FA4E7;
                         }
                         "), #close tags$style
              navbarPage(title = div(icon("diamond"), "shinyMIPs"),
                         windowTitle = "shinyMIPs", 
                         #starting active tab
                         selected = div(icon("star")," Preamble"), 
                         fluid = TRUE,
                         theme = shinytheme("cerulean"),
                         #inverse --> as the name suggests, comment-out if desired
                         #inverse = TRUE,
                         useShinyjs(),
                         tabPanel(div(icon("star"), " Preamble"), 
                                  fluidRow(
                                    column(5, align = "left",
                                           titlePanel("Preamble"),
                                           tags$br(),
                                           tags$h4("Welcome to shinyMIPs, a user interface to generate MIPs with MIPgen."),
                                           tags$h4("This program is using hg19."),
                                           tags$h4("Please note: overwriting of files is enabled at all times. I advise caution."),
                                           tags$h4("I will regularly remove old user files, so make sure to download everything you need."),
                                           imageOutput("wordcloud", 
                                                       width = "120%", 
                                                       height = "440px"),
                                           #tags$hr(),
                                           tags$h6("P.S. When talking about MIPs, what is actually meant are smMIPs."),
                                           tags$br(),
                                           bookmarkButton(label = "Bookmark", 
                                                          class = "btn-primary", 
                                                          title = "Save shinyMIPs' state to a URL to resume your work at a later time (work in progress).")
                                    ) #close column
                                  ) #close fluidrow
                         ), #close tabpanel
                         
                         tabPanel(div(icon("folder-open"), 
                                      " Project title"),
                                  titlePanel("Project title"),
                                  tags$br(),
                                  textInput("prefix", "Enter your (preferably unique) project name here (snake_case!)", 
                                            width = "350px", 
                                            placeholder = "AB_gene_XYZalpha"),
                                  bsTooltip("prefix", "This will be used for naming your files")#,
                         ), #close tabpanel
                         
                         tabPanel(div(icon("hourglass-1"), " What to cover"),
                                  tabsetPanel(id="gene_or_region",
                                              tabPanel("Gene",
                                                       titlePanel("Choose genes to be covered"),
                                                       tags$br(),
                                                       sidebarLayout(position = "right",
                                                                     sidebarPanel(
                                                                       withBusyIndicatorUI(
                                                                         actionButton("genes", 
                                                                                      "Create gene list", 
                                                                                      icon("hourglass-start"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("genes", "This creates a list of your genes on the server"),
                                                                       tags$hr(),
                                                                       withBusyIndicatorUI(
                                                                         actionButton("exons", 
                                                                                      "Convert to exons", 
                                                                                      icon("hourglass-start"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("exons", "This converts your gene list to the respective exon coordinates"),
                                                                       tags$hr(),
                                                                       withBusyIndicatorUI(
                                                                         actionButton("exons_utrs", 
                                                                                      "Convert to exons plus UTRs", 
                                                                                      icon("hourglass-start"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("exons_utrs", "This converts your gene list to the respective exon coordinates including UTRs"),
                                                                       tags$hr(),
                                                                       withBusyIndicatorUI(
                                                                         actionButton("exonsplusfive", 
                                                                                      "Increase exon sizes", 
                                                                                      icon("plus"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("exonsplusfive", "Increases your exon sizes by + 10 bp in both directions"),
                                                                       tags$hr(),
                                                                       downloadButton("DL_genes", 
                                                                                      "Download gene list", 
                                                                                      class = "btn-primary"),
                                                                       bsTooltip("DL_genes", "Contains the genes you entered on the left"),
                                                                       tags$hr(),
                                                                       downloadButton("DL_exons", 
                                                                                      "Download exon list", 
                                                                                      class = "btn-primary"),
                                                                       bsTooltip("DL_exons", "Contains the exon boundaries of your picked genes"),
                                                                       tags$hr(),
                                                                       actionButton("view_genes", 
                                                                                    "File viewer: genes", 
                                                                                    icon("align-justify"), 
                                                                                    class = "btn-primary"),
                                                                       bsTooltip("view_genes", "To check your gene list, click here"),
                                                                       tags$hr(),
                                                                       actionButton("view_exons", 
                                                                                    "File viewer: exons", 
                                                                                    icon("align-justify"), 
                                                                                    class = "btn-primary"),
                                                                       bsTooltip("view_exons", "To check your exon list, click here")
                                                                     ), #close sidebarpanel
                                                                     mainPanel(
                                                                       textAreaInput("genelist", 
                                                                                     "Paste your gene(s) here (one per line; case sensitive)", 
                                                                                     width = "350px"),
                                                                       bsTooltip("genelist", "e.g. BRCA1"),
                                                                       #this is only shown, when the program is working
                                                                       hidden(div(id = "working0",
                                                                                  tags$h3("shinyMIPs is working, please stand by.")
                                                                       )), #close hidden
                                                                       #file viewer, only shown upon button click
                                                                       hidden(div(id = "hidden_viewer1",
                                                                                  tableOutput("file_viewer1")
                                                                       )), #close hidden
                                                                       hidden(div(id = "hidden_viewer2",
                                                                                  tableOutput("file_viewer2")
                                                                       )) #close hidden
                                                                     ) #close mainpanel
                                                       ) #close sidebarlayout
                                              ), #close tabpanel
                                              tabPanel("Non-coding",
                                                       titlePanel("Choose (non-coding) regions to be covered"),
                                                       tags$br(),
                                                       sidebarLayout(position = "right",
                                                                     sidebarPanel(
                                                                       withBusyIndicatorUI(
                                                                         actionButton("regions", 
                                                                                      "Create list of regions from input", 
                                                                                      icon("hourglass-start"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("regions", "This creates a list of your regions of interest on the server (using your input on the left"),
                                                                       tags$hr(),
                                                                       withBusyIndicatorUI(
                                                                         actionButton("regions_by_file", 
                                                                                      "Create a list of regions from upload", 
                                                                                      icon("hourglass-start"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("regions_by_file", "This creates a list of your regions of interest on the server (using your uploaded file"),
                                                                       tags$hr(),
                                                                       downloadButton("DL_regions", 
                                                                                      "Download list of regions", 
                                                                                      class = "btn-primary"),
                                                                       bsTooltip("DL_regions", "Contains the regions you entered on the left"),
                                                                       tags$hr(),
                                                                       actionButton("view_regions", "File viewer: regions", 
                                                                                    icon("align-justify"), 
                                                                                    class = "btn-primary"),
                                                                       bsTooltip("view_regions", "To check your region list, click here")
                                                                     ), #close sidebarpanel
                                                                     mainPanel(
                                                                       textAreaInput("regionlist", 
                                                                                     "Paste your regions of interest here (one region per line; space-/tab-delimited; format example: chr1  1234000  1239000  region_name)", 
                                                                                     width = "350px"),
                                                                       bsTooltip("regionlist", "e.g. chr2 42209543 4238600 noncoding_region (those four strings have to be space-delimited)"),
                                                                       tags$br(),
                                                                       tags$h4("Alternative method: upload a file containing your regions."),
                                                                       fileInput("upload_regions", "Choose file to upload (following the same format as above, tab-delimited)",
                                                                                 accept = c(
                                                                                   "text/csv",
                                                                                   "text/tab-separated-values",
                                                                                   "text/plain",
                                                                                   ".csv",
                                                                                   ".tsv"
                                                                                 ) #close accept of fileinput
                                                                       ), #close fileinput
                                                                       #this is only shown, when the program is working
                                                                       hidden(div(id = "working1",
                                                                                  tags$h3("shinyMIPs is working, please stand by.")
                                                                       )), #close hidden
                                                                       #file viewer, only shown upon button click
                                                                       hidden(div(id = "hidden_viewer5",
                                                                                  tableOutput("file_viewer5")
                                                                       )) #close hidden
                                                                     ) #close mainpanel
                                                       ) #close sidebarlayout
                                              ) #close tabpanel
                                              
                                  ) #close tabsetpanel
                         ), #close tabpanel
                         
                         tabPanel(div(icon("hourglass-2"), " Create MIPs"),
                                  titlePanel("Create MIPs (advanced options for advanced users only)"),
                                  tags$br(),
                                  sidebarLayout(position = "right",
                                                sidebarPanel(
                                                  withBusyIndicatorUI(
                                                    actionButton("mips", 
                                                                 "Create MIP files", 
                                                                 icon("hourglass-half"), 
                                                                 class = "btn-primary")
                                                  ),#close withbusyindicatorui
                                                  bsTooltip("mips", "Start the MIPgen pipeline"),
                                                  tags$hr(),
                                                  actionButton("advanced", 
                                                               "Toggle advanced options", 
                                                               icon("cog"), 
                                                               class = "btn-primary"),
                                                  bsTooltip("advanced", "Seriously, watch what you are doing!"),
                                                  tags$hr(),
                                                  downloadButton("DL_picked_mips", 
                                                                 "Download MIP list", 
                                                                 class = "btn-primary"),
                                                  bsTooltip("DL_picked_mips", "Contains the MIPs picked by MIPgen"),
                                                  tags$hr(),
                                                  downloadButton("DL_snp_mips", 
                                                                 "Download SNP MIP list", 
                                                                 class = "btn-primary"),
                                                  bsTooltip("DL_snp_mips", "Contains the alternative SNP MIPs picked by MIPgen"),
                                                  tags$hr(),
                                                  downloadButton("DL_failed", 
                                                                 "Download list of failed regions", 
                                                                 class = "btn-primary"),
                                                  bsTooltip("DL_failed", "If MIPgen failed to cover any of your provided exons/regions, this file will contain the coordinates. If the file does not exist, you are good to go"),
                                                  tags$hr(),
                                                  actionButton("view_picked_mips", 
                                                               "File viewer: MIPs", 
                                                               icon("align-justify"), 
                                                               class = "btn-primary"),
                                                  bsTooltip("view_picked_mips", "To check your list of picked MIPs, click here"),
                                                  tags$hr(),
                                                  actionButton("view_snp_mips", 
                                                               "File viewer: SNP MIPs", 
                                                               icon("align-justify"), 
                                                               class = "btn-primary"),
                                                  bsTooltip("view_snp_mips", "To check your list of SNP MIPs, click here")
                                                ), #close sidebarpanel
                                                mainPanel(
                                                  fluidRow(
                                                    column(3, 
                                                           sliderInput("capture_size", 
                                                                       "Min. and max. capture size", 
                                                                       min = 100, 
                                                                       max = 600, 
                                                                       value = c(210,230)),
                                                           bsTooltip("capture_size", "Sets the range for the length of the targeting arms plus the insert"),
                                                           sliderInput("bwa_threads", 
                                                                       "Threads", 
                                                                       min = 1, 
                                                                       max = 10, 
                                                                       value = 1),
                                                           bsTooltip("bwa_threads", "Determines, how many threads are used for BWA"),
                                                           radioButtons("double_tiling", 
                                                                        "Double Tiling", 
                                                                        choices = c("off", "on")),
                                                           bsTooltip("double_tiling", "Allows you to generate MIPs on both strands for each position")
                                                    ), #close first column
                                                    #these are hidden initially
                                                    hidden(div(id= "advanced_options",
                                                               column(3,
                                                                      textInput("arm_length_sums", 
                                                                                "Min. and max. sum of arm lengths", 
                                                                                value = "40,41,42,43,44,45"),
                                                                      bsTooltip("arm_length_sums", "Sets the allowed sums of extension and ligation arm size (format: sum1,sum2,sum3,...)"),
                                                                      sliderInput("ext_min_length", 
                                                                                  "Min. length of extension arm", 
                                                                                  min = 12, 
                                                                                  max = 30, 
                                                                                  value = 16),
                                                                      bsTooltip("ext_min_length", "Rather self-explanatory"),
                                                                      sliderInput("lig_min_length", 
                                                                                  "Min. length of ligation arm", 
                                                                                  min = 12, 
                                                                                  max = 30, 
                                                                                  value = 18),
                                                                      bsTooltip("lig_min_length", "Rather self-explanatory")
                                                               ), #close second column
                                                               column(3,
                                                                      sliderInput("tag_size", 
                                                                                  "Extension arm tag size", 
                                                                                  min = 0, 
                                                                                  max = 8, 
                                                                                  value = 5),
                                                                      bsTooltip("tag_size", "Higher: less specific binding; lower: fewer unique smMIPs"),
                                                                      sliderInput("max_overlap", 
                                                                                  "Max. MIP overlap", 
                                                                                  min = 0, 
                                                                                  max = 100, 
                                                                                  value = 30),
                                                                      bsTooltip("max_overlap", "Determines the maximum overlap of target regions between two MIPs"),
                                                                      radioButtons("score_method", 
                                                                                   "Scoring method", 
                                                                                   choices = c("logistic", "mixed", "svr")),
                                                                      bsTooltip("score_method", "Logistic: fast; svr: slow (not working); mixed: as the name suggests (not working)"),
                                                                      bsAlert("score_method_alert")
                                                               ) #close third column
                                                    )) #close hidden
                                                  ), #close fluidrow
                                                  #this is only shown, when the program is working
                                                  hidden(div(id = "working2",
                                                             tags$h3("shinyMIPs is working, please stand by.")
                                                  )), #close hidden
                                                  #file viewer, only shown upon button click
                                                  hidden(div(id = "hidden_viewer3",
                                                             tableOutput("file_viewer3")
                                                  )), #close hidden
                                                  hidden(div(id = "hidden_viewer4",
                                                             tableOutput("file_viewer4")
                                                  )) #close hidden
                                                ) #close mainpanel
                                  ) #close sidebarlayout
                         ), #close tabpanel
                         
                         tabPanel(div(icon("hourglass-3"), " Generate UCSC Track"),
                                  titlePanel("MIPs to UCSC Track"),
                                  tags$br(),
                                  sidebarLayout(position = "right",
                                                sidebarPanel(
                                                  withBusyIndicatorUI(
                                                    actionButton("ucsc", 
                                                                 "Create UCSC Track", 
                                                                 icon("hourglass-end"), 
                                                                 class = "btn-primary")
                                                  ), #close withbusyindicatorui
                                                  bsTooltip("ucsc", "Uses the created MIPs to create a track for UCSC"),
                                                  tags$hr(),
                                                  downloadButton("DL_ucsc", 
                                                                 "Download UCSC Track", 
                                                                 class = "btn-primary"),
                                                  bsTooltip("DL_ucsc", "Contains a .bed track for use in UCSC.")
                                                ), #close sidebarpanel
                                                mainPanel(
                                                  fluidRow(
                                                    column(3,
                                                           textInput("ucsc_name", 
                                                                     "Name of UCSC track (snake_case!)", 
                                                                     value= "this_is_an_example"), #should be the chosen prefix
                                                           bsTooltip("ucsc_name", "This will be shown in the UCSC genome browser")
                                                    ), #close first column
                                                    column(3,
                                                           #uses the colourpicker tool
                                                           colourInput("plus_strand", 
                                                                       "Colour of plus strand", 
                                                                       value = "#2FA4E7"),
                                                           bsTooltip("plus_strand", "Changes the colour of the plus strand in your UCSC track")
                                                    ), #close second column
                                                    column(3,
                                                           #uses the colourpicker tool
                                                           colourInput("minus_strand", 
                                                                       "Colour of minus strand", 
                                                                       value = "#033C73"),
                                                           bsTooltip("minus_strand", "Changes the colour of the minus strand in your UCSC track")
                                                    ) #close third column
                                                  ), #close fluidrow
                                                  #this is only shown when the program is working
                                                  hidden(div(id = "working3",
                                                             tags$h3("shinyMIPs is working, please stand by.")
                                                  )) #close hidden
                                                ) #close mainpanel
                                  ) #close sidebarlayout
                         ), #close tabpanel
                         
                         tabPanel(div(icon("balance-scale"), " Rebalancing"),
                                  tabsetPanel(id="analysis_choice",
                                              tabPanel("Intro",
                                                       tags$br(),
                                                       tags$h4("Here you can perform some simple analyses to increase your MIPs\' performance in subsequent experiments.")
                                              ), #close tabpanel
                                              tabPanel("Grep-like",
                                                       tags$br(),
                                                       sidebarLayout(position = "right",
                                                                     sidebarPanel(
                                                                       withBusyIndicatorUI(
                                                                         actionButton("grep_input", 
                                                                                      "Read uploaded file", 
                                                                                      icon("upload"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("grep_input", "Convert uploaded file"),
                                                                       tags$hr(),
                                                                       actionButton("grep", 
                                                                                    "Start grep analysis", 
                                                                                    icon("hand-grab-o"), 
                                                                                    class = "btn-primary"),
                                                                       bsTooltip("grep", "Analyse your uploaded file using the grep-like algorithm"),
                                                                       tags$hr(),
                                                                       downloadButton("DL_grep", 
                                                                                      "Download grep results", 
                                                                                      class = "btn-primary"),
                                                                       bsTooltip("DL_grep", "Contains your grep-analysed results"),
                                                                       tags$hr(),
                                                                       actionButton("view_grep", 
                                                                                    "File viewer: grep", 
                                                                                    icon("align-justify"), 
                                                                                    class = "btn-primary"),
                                                                       bsTooltip("view_grep", "Check your grep results in a file viewer")
                                                                     ), #close sidebarpanel
                                                                     mainPanel(
                                                                       tags$h3("Work in progress..."),
                                                                       fileInput("upload_grep", "Upload your file here",
                                                                                 accept = c(
                                                                                   "text/csv",
                                                                                   "text/tab-separated-values",
                                                                                   "text/plain",
                                                                                   ".csv",
                                                                                   ".tsv"
                                                                                 ) #close accept of fileinput
                                                                       ), #close fileinput
                                                                       textInput("grep_barcode", 
                                                                                 "Enter your used barcode here"),
                                                                       bsTooltip("grep_barcode", "Used in the MIPs PCR, see primer list"),
                                                                       hidden(div(id = "working4",
                                                                                  tags$h3("shinyMIPs is working, please stand by.")
                                                                       )) #close hidden
                                                                     ) #close mainpanel
                                                       ) #close sidebarlayout
                                              ), #close tabpanel
                                              tabPanel("Align",
                                                       tags$br(),
                                                       sidebarLayout(position = "right",
                                                                     sidebarPanel(
                                                                       withBusyIndicatorUI(
                                                                         actionButton("align_input", 
                                                                                      "Read uploaded file", 
                                                                                      icon("upload"), 
                                                                                      class = "btn-primary")
                                                                       ), #close withbusyindicatorui
                                                                       bsTooltip("align_input", "Convert uploaded file"),
                                                                       tags$hr(),
                                                                       actionButton("align", 
                                                                                    "Start align analysis", 
                                                                                    icon("bars"), 
                                                                                    class = "btn-primary"),
                                                                       bsTooltip("align", "Analyse your uploaded file using the align function"),
                                                                       tags$hr(),
                                                                       downloadButton("DL_align", 
                                                                                      "Download align results", 
                                                                                      class = "btn-primary"),
                                                                       bsTooltip("DL_align", "Contains your align-analysed results"),
                                                                       tags$hr(),
                                                                       actionButton("view_align", 
                                                                                    "File viewer: align", 
                                                                                    icon("align-justify"), 
                                                                                    class = "btn-primary"),
                                                                       bsTooltip("view_align", "Check your grep results in a file viewer")
                                                                     ), #close sidebarpanel
                                                                     mainPanel(
                                                                       tags$h3("Work in progress..."),
                                                                       fileInput("upload_align", "Upload your file here",
                                                                                 accept = c(
                                                                                   "text/csv",
                                                                                   "text/tab-separated-values",
                                                                                   "text/plain",
                                                                                   ".csv",
                                                                                   ".tsv"
                                                                                 ) #close accept of fileinput
                                                                       ), #close fileinput
                                                                       textInput("align_barcode", 
                                                                                 "Enter your used barcode here"),
                                                                       bsTooltip("align_barcode", "Used in the MIPs PCR, see primer list"),
                                                                       hidden(div(id = "working5",
                                                                                  tags$h3("shinyMIPs is working, please stand by.")
                                                                       )) #close hidden
                                                                     ) #close mainpanel
                                                       ) #close sidebarlayout
                                              ) #close tabpanel
                                  ) #close tabsetpanel
                         ), #close tabpanel
                         
                         tabPanel(title = div(icon("calculator"), " Calculators"),
                                  tabsetPanel(id="calculators",
                                              tabPanel("Coverage",
                                                       tags$br(),
                                                       fluidRow(
                                                         column(3,
                                                                tags$h4("Input"),
                                                                numericInput("desired_coverage", 
                                                                             "Coverage", 
                                                                             value = 800),
                                                                bsTooltip("desired_coverage", "The coverage you want to achieve"),
                                                                sliderInput("duplicates", 
                                                                            "Duplicates", 
                                                                            min = 0, 
                                                                            max = 100, 
                                                                            post  = " %", 
                                                                            value = 10),
                                                                bsTooltip("duplicates", "The percentage of duplicates expected, default: 10%"),
                                                                numericInput("size_of_region", 
                                                                             "Region size", 
                                                                             value = 123456),
                                                                bsTooltip("size_of_region", "The size of the region to be sequenced, also see the MIPs calculator"),
                                                                numericInput("read_length", 
                                                                             "Read length", 
                                                                             value = 250),
                                                                bsTooltip("read_length", "Sequencing size in bp (default: 2x 125, so 250)"),
                                                                sliderInput("on_target", 
                                                                            "On target", 
                                                                            min = 0, 
                                                                            max = 100, 
                                                                            post  = " %", 
                                                                            value = 90),
                                                                bsTooltip("on_target", "Percentage of on-target reads expected, default: 80% to 90%"),
                                                                numericInput("sample_number", 
                                                                             "Number of samples", 
                                                                             value = 100),
                                                                bsTooltip("sample_number", "How many samples you wish to sequence"),
                                                                tags$br(),
                                                                actionButton("toggle_calc_info_1", 
                                                                             "Toggle further calculations", 
                                                                             icon("toggle-off"), 
                                                                             class = "btn-primary"),
                                                                bsTooltip("toggle_calc_info_1", "Show more information about required output etc."),
                                                                hidden(div(id = "hidden_toggle_calc_info_button",
                                                                           actionButton("toggle_calc_info_2", 
                                                                                        "Toggle further calculations", 
                                                                                        icon("toggle-on"), 
                                                                                        class = "btn-primary"),
                                                                           bsTooltip("toggle_calc_info_2", "Show less information about required output etc.")
                                                                )) #close hidden
                                                         ), #close column
                                                         column(3,
                                                                tags$h4("Output HiSeq 2500 v4 (2x 125)"),
                                                                hidden(div(id = "hidden_calc_1",
                                                                           numericInput("output_req_1", 
                                                                                        "Required output (bp)", 
                                                                                        value = 0),
                                                                           bsTooltip("output_req_1", "Total output required = region size * coverage / ((1-duplicates/100) * on target/100)"),
                                                                           numericInput("output_unit_1", 
                                                                                        "Output per unit (bp)", 
                                                                                        value = 0),
                                                                           bsTooltip("output_unit_1", "Output per unit = clusters per unit * read length"),
                                                                           numericInput("output_number_1", 
                                                                                        "Number of units", 
                                                                                        value = 0),
                                                                           bsTooltip("output_number_1", "Number of units (flow cells or lanes) = total output required / output per unit")
                                                                )), #close hidden
                                                                numericInput("output_samples_1", 
                                                                             "Samples per unit", 
                                                                             value = 0),
                                                                bsTooltip("output_samples_1", "Number of samples = output per unit / total output required"),
                                                                numericInput("output_required_1a", 
                                                                             "Required lanes", 
                                                                             value = 0),
                                                                bsTooltip("output_required_1a", "Lanes you need, to achieve your sequencing goal"),
                                                                numericInput("output_required_1b", 
                                                                             "Required flow cells", 
                                                                             value = 0),
                                                                bsTooltip("output_required_1b", "Flow cells, these lanes amount to"),
                                                                numericInput("output_price_1", 
                                                                             "Price in Euro", 
                                                                             value = 0),
                                                                bsTooltip("output_price_1", "Your costs")
                                                         ), #close column
                                                         column(3,
                                                                tags$h4("Output MiSeq v2 (2x 150)"),
                                                                hidden(div(id = "hidden_calc_2",
                                                                           numericInput("output_req_2", 
                                                                                        "Required output (bp)", 
                                                                                        value = 0),
                                                                           bsTooltip("output_req_2", "Total output required = region size * coverage / ((1-duplicates/100) * on target/100)"),
                                                                           numericInput("output_unit_2", 
                                                                                        "Output per unit (bp)", 
                                                                                        value = 0),
                                                                           bsTooltip("output_unit_2", "Output per unit = clusters per unit * read length"),
                                                                           numericInput("output_number_2", 
                                                                                        "Number of units", 
                                                                                        value = 0),
                                                                           bsTooltip("output_number_2", "Number of units (flow cells or lanes) = total output required / output per unit")
                                                                )), #close hidden
                                                                numericInput("output_samples_2", 
                                                                             "Samples per flow cell", 
                                                                             value = 0),
                                                                bsTooltip("output_samples_2", "Number of samples = output per unit / total output required"),
                                                                numericInput("output_required_2", 
                                                                             "Required flow cells", 
                                                                             value = 0),
                                                                bsTooltip("output_required_2", "Flow cells you need, to achieve your sequencing goal"),
                                                                numericInput("output_price_2", 
                                                                             "Price in Euro", 
                                                                             value = 0),
                                                                bsTooltip("output_price_2", "Your costs")
                                                         ), #close column
                                                         column(3,
                                                                tags$h4("Output MiSeq v3 (2x 300)"),
                                                                hidden(div(id = "hidden_calc_3",
                                                                           numericInput("output_req_3", 
                                                                                        "Required output (bp)", 
                                                                                        value = 0),
                                                                           bsTooltip("output_req_3", "Total output required = region size * coverage / ((1-duplicates/100) * on target/100)"),
                                                                           numericInput("output_unit_3", 
                                                                                        "Output per unit (bp)", 
                                                                                        value = 0),
                                                                           bsTooltip("output_unit_3", "Output per unit = clusters per unit * read length"),
                                                                           numericInput("output_number_3", 
                                                                                        "Number of units", 
                                                                                        value = 0),
                                                                           bsTooltip("output_number_3", "Number of units (flow cells or lanes) = total output required / output per unit")
                                                                )), #close hidden
                                                                numericInput("output_samples_3", 
                                                                             "Samples per flow cell", 
                                                                             value = 0),
                                                                bsTooltip("output_samples_3", "Number of samples = output per unit / total output required"),
                                                                numericInput("output_required_3", 
                                                                             "Required flow cells", 
                                                                             value = 0),
                                                                bsTooltip("output_required_3", "Flow cells you need, to achieve your sequencing goal"),
                                                                numericInput("output_price_3", 
                                                                             "Price in Euro", 
                                                                             value = 0),
                                                                bsTooltip("output_price_3", "Your costs")
                                                         ) #close column
                                                       ) #close fluidrow
                                              ), #close tabpanel
                                              tabPanel("MIPs",
                                                       tags$br(),
                                                       numericInput("mips_amount", 
                                                                    "Number of MIPs", 
                                                                    value = 123),
                                                       bsTooltip("mips_amount", "How many MIPs you (will) have"),
                                                       numericInput("sequencing_size", 
                                                                    "Sequencing size in bp (default: 2x 125, so 250)", 
                                                                    value = 250),
                                                       bsTooltip("sequencing_size", "Also known as read length"),
                                                       numericInput("result_mips_calculation", 
                                                                    "Result:", 
                                                                    value = 0),
                                                       bsTooltip("result_mips_calculation", "bp you need for your coverage calculation")
                                              ) #close tabpanel
                                  ) #close tabsetpanel
                         ), #close tabpanel
                         
                         tabPanel(div(icon("puzzle-piece"), " Analysis"),
                                  titlePanel("Analysis"),
                                  sidebarLayout(position = "right",
                                                sidebarPanel(
                                                  actionButton("analysis_start", 
                                                               "Analyse", 
                                                               icon("random"), 
                                                               class = "btn-primary")
                                                ), #close sidebarpanel
                                                mainPanel(
                                                  tags$br(),
                                                  tags$h4("Work in progress...")
                                                ) #close mainpanel
                                  ) #close sidebarlayout
                         ), #close tabpanel
                         
                         tabPanel(div(icon("rocket"), "Further tools"),
                                  passwordInput("pw_enter_password", "Enter password"),
                                  actionButton("pw_go", "Go", class = "btn-primary"),
                                  hidden(div(id = "pw_protect",
                                             tabsetPanel(id="Further tools",
                                                         tabPanel("Sequence grabber",
                                                                  titlePanel("Sequence grabber"),
                                                                  tags$br(),
                                                                  sidebarLayout(position = "right",
                                                                                sidebarPanel(
                                                                                  actionButton("get_sequence", 
                                                                                               "Get desired sequence", 
                                                                                               icon("keyboard-o"), 
                                                                                               class = "btn-primary"),
                                                                                  bsTooltip("get_sequence", "Print the sequence according to your entered coordinates")
                                                                                ), #close siderbarpanel
                                                                                mainPanel(
                                                                                  tags$br(),
                                                                                  textAreaInput("coordinates", 
                                                                                                "Coordinates (chrX:Y-Z):", 
                                                                                                placeholder = "chr2:5643000-5645000"),
                                                                                  bsTooltip("coordinates", "Input your coordinates here, e.g. 'chr2:5643000-5645000"),
                                                                                  tags$br(),
                                                                                  textAreaInput("sequence", 
                                                                                                "Sequence:"),
                                                                                  bsTooltip("sequence", "This is your sequence (after using the button)")
                                                                                ) #close mainpanel
                                                                  ) #close siderbarlayout
                                                         ), #close tabpanel
                                                         tabPanel("Reversecomplement",
                                                                  titlePanel("Reversecomplement"),
                                                                  tags$br(),
                                                                  sidebarLayout(position = "right",
                                                                                sidebarPanel(
                                                                                  actionButton("make_complementary", 
                                                                                               "Make reversecomplement!", 
                                                                                               icon("exchange"), 
                                                                                               class = "btn-primary"),
                                                                                  bsTooltip("make_complementary", "Creates the reversecomplementary sequence (e.g. for primers)")
                                                                                ), #close sidebarpanel
                                                                                mainPanel(
                                                                                  tags$br(),
                                                                                  textAreaInput("pre_complementary", 
                                                                                                "Your sequence (ACGT only):", 
                                                                                                placeholder = "ACGT"),
                                                                                  bsTooltip("pre_complementary", "Enter your sequence"),
                                                                                  textAreaInput("post_complementary", 
                                                                                                "Reversecomplementary sequence", 
                                                                                                placeholder = "ACGT"),
                                                                                  bsTooltip("post_complementary", "Obtain your converted sequence")
                                                                                ) #close mainpanel
                                                                  ) #close sidebarlayout
                                                         ), #close tabPanel
                                                         tabPanel("Primers",
                                                                  titlePanel("Primers"),
                                                                  tags$br(),
                                                                  sidebarLayout(position = "right",
                                                                                sidebarPanel(
                                                                                  actionButton("pick_primers", 
                                                                                               "Pick primers", 
                                                                                               icon("flask"), 
                                                                                               class = "btn-primary"),
                                                                                  bsTooltip("pick_primers", "Picks primers using your specified input")
                                                                                ), #close sidebarpanel
                                                                                mainPanel(
                                                                                  "Use", a("Primer3", href = "http://primer3.ut.ee/"), "until this is done!"
                                                                                ) #close mainpanel
                                                                  ) #close sidebarlayout
                                                         ) #close tabpanel
                                             ) #close tabsetpanel
                                  )) #close hidden
                         ), #close tabpanel
                         
                         tabPanel(div(icon("question"), " Help"),
                                  tabsetPanel(id="help",
                                              tabPanel("How to",
                                                       tags$br(),
                                                       tags$h5("Step 1: Create a prefix, your own unique identifier."),
                                                       tags$h5("Step 2: Choose, which genes or genomic regions you want to cover (i.e. sequence)."),
                                                       tags$h5("Step 3: Generate MIPs for those regions."),
                                                       tags$h5("Step 4: Create a UCSC track to check your MIPs."),
                                                       tags$h5("Step 5: Refine your MIPs."),
                                                       tags$h5("Step 6: Order your MIPs, for example from", a("IDT.", href = "https://eu.idtdna.com/site")),
                                                       tags$h5("Step 7: Perform your sequencing project."),
                                                       tags$h5("Step 8: Analyse your data."),
                                                       tags$h5("Step 9: Share your findings with the scientific community.")
                                              ), #close tabpanel
                                              tabPanel("FAQ",
                                                       tags$br(),
                                                       tags$h4("Q: How are the MIPs created?"),
                                                       tags$h5("A: Using the", a("MIPgen", href = "https://github.com/shendurelab/MIPGEN"), "pipeline. It was created by Evan Boyle and set up at our institute by Leonie Henschel. I provided the graphical user interface, which you are using right now. Furthermore, I modified some of the scripts to better suit this simplified application."),
                                                       tags$h4("Q: What is a MIP?"),
                                                       tags$h5("A: A MIP is a molecular inversion probe. See", a("this paper", href = "https://www.nature.com/nbt/journal/v21/n6/full/nbt821.html"), "by Hardenbol et al."),
                                                       tags$h4("Q: What is a smMIP?"),
                                                       tags$h5("A: A smMIP is a single molecule MIP. See", a("this paper", href = "http://genome.cshlp.org/content/23/5/843"), "by Hiatt et al."),
                                                       tags$h4("Q: What is UCSC?"),
                                                       tags$h5("A: UCSC refers to the Genome Browser by the University of California Santa Cruz. You can find it", a("here.", href = "http://genome-euro.ucsc.edu/cgi-bin/hgGateway")),
                                                       tags$h4("Q: Which genome build do you use?"),
                                                       tags$h5("A: shinyMIPs is based on hg19."),
                                                       tags$h4("Q: Anything else?"),
                                                       tags$h5("A: For the tools I also used the twoBitToFa script and the fasta_formatter from the fastx-toolkit.")
                                              ) #close tabpanel
                                  ) #close tabsetpanel
                         ), #close tabpanel
                         
                         tabPanel(div(icon("info"), " About"),
                                  tags$br(),
                                  tags$h5(id = "shinyMIPs_font", 
                                          "shinyMIPs", 
                                          align = "center"),
                                  tags$h5("2020", 
                                          align = "center")
                         ) #close tabpanel
                         
) #close navbarpage
) #close taglist

# server ------------------------------------------------------------------

server <- function(input, output, session){
  
  #wordcloud
  WORDS <- as.data.frame(read.table("./refs/wordcloud.txt", 
                                    header = FALSE))
  
  output$wordcloud <- renderPlot({
    wordcloud(words = WORDS$V1, 
              freq = WORDS$V2, 
              min.freq = 0.1, 
              random.color = TRUE, 
              colors = c("#2FA4E7", "#033C73", "#73A839"))
  }) #close renderplot
  
  #create gene list
  observeEvent(input$genes, {
    withBusyIndicatorServer("genes", {
      withProgress(message = 'Creating gene list', 
                   value = 0, {
                     shinyjs::show("working0")
                     genevariable <- input$genelist
                     write.table(genevariable, 
                                 paste("./data/", 
                                       input$prefix, 
                                       ".genes.txt", 
                                       sep = ""), 
                                 row.names = FALSE, 
                                 col.names = FALSE, 
                                 quote = FALSE)
                     system(paste("echo Gene list successfully created. \n
                                  echo Exon conversion is now available. \n
                                  echo If necessary, a list containing the genes can be downloaded on the right. \n
                                  echo ")
                     ) #close system
                     incProgress(0.3, 
                                 detail = "Reading input")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Writing list")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(1)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
    shinyjs::hide("working0", 
                  anim = TRUE, 
                  animType = "fade", 
                  time = 1.0)
  }) #close observeevent
  
  #create exons from gene list
  observeEvent(input$exons, {
    withBusyIndicatorServer("exons",{
      withProgress(message = 'Creating exon list', 
                   value = 0, {
                     shinyjs::show("working0")
                     system(paste("sh ./scripts/extract_coding_gene_exons.sh", 
                                  paste("./data/", 
                                        input$prefix, 
                                        ".genes.txt", 
                                        sep = ""), 
                                  "./refs/refGene_hg19.txt >", 
                                  paste("./data/", 
                                        input$prefix, 
                                        ".exons.bed", 
                                        sep = "")
                     )
                     ) #close system
                     system(paste("echo Exon list successfully created. \n 
                                  echo You may now continue with MIP generation. \n
                                  echo For informative purposes, the exon list can be downloaded as well. \n
                                  echo ")
                     ) #close system
                     incProgress(0.3, 
                                 detail = "Reading genes")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Finding exons")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(1)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
    shinyjs::hide("working0", 
                  anim = TRUE, 
                  animType = "fade", 
                  time = 1.0)
  }) #close observeevent
  
  #create exons plus UTRs from gene list
  observeEvent(input$exons_utrs, {
    withBusyIndicatorServer("exons_utrs", {
      withProgress(message = 'Creating exon list', 
                   value = 0, {
                     shinyjs::show("working0")
                     system(paste("sh ./scripts/extract_gene_exons_plus_utrs.sh", 
                                  paste("./data/", 
                                        input$prefix, 
                                        ".genes.txt", 
                                        sep = ""), 
                                  "./refs/refGene_hg19.txt >", 
                                  paste("./data/", 
                                        input$prefix, 
                                        ".exons.bed", 
                                        sep = "")
                     ) #close paste
                     ) #close system
                     system(paste("echo Exon list successfully created. \n 
                                  echo You may now continue with MIP generation. \n
                                  echo For informative purposes, the exon list can be downloaded as well. \n
                                  echo ")
                     ) #close system
                     incProgress(0.3, 
                                 detail = "Reading genes")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Finding exons")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(1)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
    shinyjs::hide("working0", 
                  anim = TRUE, 
                  animType = "fade", 
                  time = 1.0)
  }) #close observeevent
  
  #add 5 bp up- and downstream of each exon
  observeEvent(input$exonsplusfive, {
    withBusyIndicatorServer("exonsplusfive", {
      withProgress(message = 'Modifying exons', 
                   value = 0, {
                     temporary_exons <- read.csv(paste("./data/", 
                                                       input$prefix, 
                                                       ".exons.bed", 
                                                       sep = ""), 
                                                 sep = "\t", 
                                                 header = FALSE)
                     temporary_exons[2] <- temporary_exons[2] - 10
                     temporary_exons[3] <- temporary_exons[3] + 10
                     write.table(temporary_exons, 
                                 paste("./data/", 
                                       input$prefix, 
                                       ".exons.bed", 
                                       sep = ""), 
                                 row.names = FALSE, 
                                 col.names = FALSE, 
                                 quote = FALSE, 
                                 sep = " ")
                     shinyjs::show("working0")
                     incProgress(0.3, 
                                 detail = "Reading exons")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Calculating")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(1)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
    shinyjs::hide("working0", 
                  anim = TRUE, 
                  animType = "fade", 
                  time = 1.0)
  }) #close observeevent
  
  #download gene list
  output$DL_genes <- downloadHandler(filename = function() {(paste(input$prefix, 
                                                                   ".genes.txt", 
                                                                   sep = ""))},
                                     content = function(filename){
                                       file.copy(paste("/home/MIPgen/data/", 
                                                       input$prefix, 
                                                       ".genes.txt", 
                                                       sep = ""), 
                                                 filename)
                                     }) #close downloadhandler
  
  #download exon list
  output$DL_exons <- downloadHandler(filename = function() {(paste(input$prefix, 
                                                                   ".exons.bed", 
                                                                   sep = ""))},
                                     content = function(filename){
                                       file.copy(paste("/home/MIPgen/data/", 
                                                       input$prefix, 
                                                       ".exons.bed", 
                                                       sep = ""), 
                                                 filename)
                                     }) #close downloadhandler
  
  #create regions list
  observeEvent(input$regions, {
    withBusyIndicatorServer("regions", {
      withProgress(message = 'Creating region list', 
                   value = 0, {
                     shinyjs::show("working1")
                     regionsvariable <- input$regionlist[1]
                     write.table(regionsvariable, 
                                 paste("./data/", 
                                       input$prefix, 
                                       ".exons.bed", 
                                       sep = ""),  
                                 row.names = FALSE, 
                                 col.names = FALSE, 
                                 quote = FALSE, 
                                 sep = "\n")
                     incProgress(0.3, 
                                 detail = "Reading regions")
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Writing list")
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(0.5)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
    shinyjs::hide("working1", 
                  anim = TRUE, 
                  animType = "fade", 
                  time = 1.0)
  }) #close observeevent
  
  #create regions from uploaded file
  observeEvent(input$regions_by_file, {
    withBusyIndicatorServer("regions_by_file", {
      withProgress(message = 'Creating region list', 
                   value = 0, {
                     shinyjs::show("working1")
                     uploadedregions <- input$upload_regions
                     file.copy(uploadedregions$datapath, 
                               paste("./data/", 
                                     input$prefix, 
                                     ".exons.bed", 
                                     sep = ""), 
                               overwrite = TRUE)
                     incProgress(0.3, 
                                 detail = "Reading regions")
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Writing list")
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(0.5)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
    shinyjs::hide("working1", 
                  anim = TRUE, 
                  animType = "fade", 
                  time = 1.0)
  }) #close observeevent
  
  #download regions list
  output$DL_regions <- downloadHandler(filename = function() {(paste(input$prefix, 
                                                                     ".regions.bed", 
                                                                     sep = ""))},
                                       content = function(filename){
                                         file.copy(paste("/home/MIPgen/data/", 
                                                         input$prefix, 
                                                         ".exons.bed", 
                                                         sep = ""), 
                                                   filename)
                                       }) #close downloadhandler
  
  #create MIPs list
  observeEvent(input$mips, {
    scoring <- input$score_method
    #kill everything if score =/= logistic
    if(scoring == "svr" | scoring == "mixed") {
      createAlert(session, "score_method_alert", 
                  title = "Error", 
                  content = "At the moment, only logistic scoring is supported.", 
                  append = FALSE)
      showModal(modalDialog(
        title = "Error",
        "This was not supposed to happen.",
        easyClose = TRUE
      )) #close showmodal
    } else { #close if
      closeAlert(session, "score_method_alert")
    } #close else
    stopifnot(scoring == "logistic")
    withBusyIndicatorServer("mips", {
      withProgress(message = 'Creating MIPs', 
                   value = 0, {
                     incProgress(0.2, 
                                 detail = "Checking input")
                     shinyjs::show("working2")
                     Sys.sleep(0.5)
                     incProgress(0.2, 
                                 detail = "Working")
                     system(paste(
                       paste("./scripts/mipgen"),
                       paste("-regions_to_scan", 
                             paste("./data/", 
                                   input$prefix, 
                                   ".exons.bed", 
                                   sep = "")),
                       paste("-project_name", 
                             paste("./data/", 
                                   input$prefix, 
                                   sep = "")),
                       paste("-min_capture_size", 
                             input$capture_size[1]),
                       paste("-max_capture_size", 
                             input$capture_size[2]),
                       paste("-double_tile_strands_separately", 
                             input$double_tiling),
                       paste("-bwa_threads", input$bwa_threads),
                       paste("-arm_length_sums", 
                             input$arm_length_sums),
                       paste("-ext_min_length", 
                             input$ext_min_length),
                       paste("-lig_min_length", 
                             input$lig_min_length),
                       paste("-tag_sizes ", 
                             input$tag_size, 
                             ",0", 
                             sep = ""),
                       paste("-max_mip_overlap", 
                             input$max_overlap),
                       paste("-score_method", 
                             input$score_method),
                       paste("-bwa_genome_index ./refs/hs37d5.fa"),
                       paste("-snp_file ./refs/common_eur_wgs.vcf.gz"),
                       paste("-silent_mode on")
                     ) #close paste
                     ) #close system
                     incProgress(0.4, 
                                 detail = "Saving")
                     Sys.sleep(0.5)
                     incProgress(0.2, 
                                 detail = "Done")
                     shinyjs::hide("working2", 
                                   anim = TRUE, 
                                   animType = "fade", 
                                   time = 1.0)
                     system("echo MIP lists are now available for download. \n
                            echo ")
                   }) #close withprogress
                   }) #close withbusyindicatorserver
    }) #close observeevent
  
  #download picked mips list
  output$DL_picked_mips <- downloadHandler(filename = function() {(paste(input$prefix, 
                                                                         ".picked_mips.txt", 
                                                                         sep = ""))},
                                           content = function(filename){
                                             file.copy(paste("/home/MIPgen/data/", 
                                                             input$prefix, 
                                                             ".picked_mips.txt", 
                                                             sep = ""), 
                                                       filename)
                                           }) #close downloadhandler
  
  #download snp mips list
  output$DL_snp_mips <- downloadHandler(filename = function() {(paste(input$prefix, 
                                                                      ".snp_mips.txt", 
                                                                      sep = ""))},
                                        content = function(filename){
                                          file.copy(paste("/home/MIPgen/data/", 
                                                          input$prefix, 
                                                          ".snp_mips.txt", 
                                                          sep = ""), 
                                                    filename)
                                        }) #close downloadhandler
  
  #download failed regions list
  output$DL_failed <- downloadHandler(filename = function() {(paste(input$prefix, 
                                                                    ".coverage_failed.bed", 
                                                                    sep = ""))},
                                      content = function(filename){
                                        file.copy(paste("/home/MIPgen/data/", 
                                                        input$prefix, 
                                                        ".coverage_failed.bed", 
                                                        sep = ""), 
                                                  filename)
                                      }) #close downloadhandler
  
  #toggle advanced options
  observeEvent(input$advanced, {
    shinyjs::toggle("advanced_options")
  }) #close observeevent
  
  #create ucsc tracks
  observeEvent(input$ucsc, {
    withBusyIndicatorServer("ucsc", {
      withProgress(message = 'Creating track', 
                   value = 0, {
                     incProgress(0.3, 
                                 detail = "Reading MIPs")
                     shinyjs::show("working3")
                     system(paste("python ./scripts/generate_ucsc_track_mod_colours.py", 
                                  paste("./data/", 
                                        input$prefix, 
                                        ".picked_mips.txt", 
                                        sep = ""), 
                                  paste(input$ucsc_name), 
                                  paste(col2rgb(input$plus_strand)[1], 
                                        col2rgb(input$plus_strand)[2], 
                                        col2rgb(input$plus_strand)[3], 
                                        sep = ","), 
                                  paste(col2rgb(input$minus_strand)[1], 
                                        col2rgb(input$minus_strand)[2], 
                                        col2rgb(input$minus_strand)[3], 
                                        sep = ",")
                     ) #close paste
                     ) #close system
                     system("echo UCSC track creation completed. \n
                            echo You may now download the UCSC track file. \n
                            echo ")
                     incProgress(0.3, 
                                 detail = "Writing track")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(1)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
    shinyjs::hide("working3", 
                  anim = TRUE, 
                  animType = "fade", 
                  time = 1.0)
  }) #close observeevent
  
  #download ucsc track
  output$DL_ucsc <- downloadHandler(filename = function() {(paste(input$prefix, 
                                                                  ".picked_mips.ucsc_track.bed", 
                                                                  sep = ""))},
                                    content = function(filename){
                                      file.copy(paste("/home/MIPgen/data/", 
                                                      input$prefix, 
                                                      ".picked_mips.ucsc_track.bed", 
                                                      sep = ""), 
                                                filename)
                                    }) #close downloadhandler

  #show gene data in file viewer
  observeEvent(input$view_genes, {
    shinyjs::show("hidden_viewer1")
    shinyjs::hide("hidden_viewer2")
    shinyjs::hide("hidden_viewer3")
    shinyjs::hide("hidden_viewer4")
    shinyjs::hide("hidden_viewer5")
  }) #close observeevent
  
  output$file_viewer1 <- renderTable({
    read.csv(paste("./data/", 
                   input$prefix, 
                   ".genes.txt", 
                   sep = ""), 
             header = FALSE, 
             sep = "", 
             col.names = c("Gene_name"))
  }) #close output/rendertable
  
  #show exon data in file viewer
  observeEvent(input$view_exons, {
    shinyjs::hide("hidden_viewer1")
    shinyjs::show("hidden_viewer2")
    shinyjs::hide("hidden_viewer3")
    shinyjs::hide("hidden_viewer4")
    shinyjs::hide("hidden_viewer5")
  }) #close observeevent
  
  output$file_viewer2 <- renderTable({
    read.csv(paste("./data/", 
                   input$prefix, 
                   ".exons.bed", 
                   sep = ""), 
             header = FALSE, 
             sep = "", 
             col.names = c("Chromosome", "Exon_start", "Exon_stop", "ID"))
  }) #close output/rendertable
  
  #show mips data in file viewer
  observeEvent(input$view_picked_mips, {
    shinyjs::hide("hidden_viewer1")
    shinyjs::hide("hidden_viewer2")
    shinyjs::show("hidden_viewer3")
    shinyjs::hide("hidden_viewer4")
    shinyjs::hide("hidden_viewer5")
  }) #close observeevent
  
  output$file_viewer3 <- renderTable({
    read.csv(paste("./data/", 
                   input$prefix, 
                   ".picked_mips.txt", 
                   sep = ""), 
             header = TRUE, 
             sep = "")
  }) #close output/rendertable
  
  #show snp mips data in file viewer
  observeEvent(input$view_snp_mips, {
    shinyjs::hide("hidden_viewer1")
    shinyjs::hide("hidden_viewer2")
    shinyjs::hide("hidden_viewer3")
    shinyjs::show("hidden_viewer4")
    shinyjs::hide("hidden_viewer5")
  }) #close observeevent
  
  output$file_viewer4 <- renderTable({
    read.csv(paste("./data/", 
                   input$prefix, 
                   ".snp_mips.txt", 
                   sep = ""), 
             header = TRUE, 
             sep = "")
  }) #close output/rendertable
  
  #show region data in file viewer
  observeEvent(input$view_regions, {
    shinyjs::hide("hidden_viewer1")
    shinyjs::hide("hidden_viewer2")
    shinyjs::hide("hidden_viewer3")
    shinyjs::hide("hidden_viewer4")
    shinyjs::show("hidden_viewer5")
  }) #close observeevent
  
  output$file_viewer5 <- renderTable({
    read.csv(paste("./data/", 
                   input$prefix, 
                   ".exons.bed", 
                   sep = ""), 
             header = FALSE, 
             sep = "", 
             col.names = c("Chromosome", "Region_start", "Region_stop", "ID")) #use exon viewer?
  }) #close output/rendertable
  
  #read upload grep
  observeEvent(input$grep_input, {
    withBusyIndicatorServer("grep_input", {
      withProgress(message = "Reading input", 
                   value = 0, {
                     shinyjs::show("working4")
                     incProgress(0.3, 
                                 detail = "Reading file")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Reading file")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(1)
                     incProgress(0.1, 
                                 detail = "Done")
                     shinyjs::hide("working4", 
                                   anim = TRUE, 
                                   animType = "fade", 
                                   time = 1.0)
                   }) #close withprogress
    }) #close withbusyindicatorserver
  }) #close observeevent
  
  #read upload align
  observeEvent(input$align_input, {
    withBusyIndicatorServer("align_input", {
      withProgress(message = "Reading input", 
                   value = 0, {
                     shinyjs::show("working5")
                     incProgress(0.3, 
                                 detail = "Reading file")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Reading file")
                     Sys.sleep(1)
                     incProgress(0.3, 
                                 detail = "Saving")
                     Sys.sleep(1)
                     incProgress(0.1, 
                                 detail = "Done")
                     shinyjs::hide("working5", 
                                   anim = TRUE, 
                                   animType = "fade", 
                                   time = 1.0)
                   }) #close withprogress
    }) #close withbusyindicatorserver
  }) #close observeevent
  
  #hiseq and miseq calculations
  observe({
    #define input stuff
    coverage_var <- as.numeric(input$desired_coverage)
    duplicates_var <- as.numeric(input$duplicates)
    region_var <- as.numeric(input$size_of_region)
    read_length_var <- as.numeric(input$read_length)
    on_target_var <- as.numeric(input$on_target)
    sample_number_var <- as.numeric(input$sample_number)
    flowcell_price_hiseq_var <- as.numeric(24000) # most likely not up to date
    flowcell_price_miseq_v2_var <- as.numeric(1000) # most likely not up to date
    flowcell_price_miseq_v3_var <- as.numeric(1500) # most likely not up to date
    #output_req
    output_req_var <- coverage_var * region_var / ( (1 - duplicates_var / 100) * (on_target_var / 100) )
    updateNumericInput(session, 
                       "output_req_1", 
                       value = round(output_req_var, digits = 0))
    updateNumericInput(session, 
                       "output_req_2", 
                       value = round(output_req_var, digits = 0))
    updateNumericInput(session, 
                       "output_req_3", 
                       value = round(output_req_var, digits = 0))
    #output per unit
    output_unit_hiseq_var <- read_length_var * 249933600
    output_unit_miseq_v2_var <- read_length_var * 15000000
    output_unit_miseq_v3_var <- read_length_var * 25000000
    updateNumericInput(session, 
                       "output_unit_1", 
                       value = round(output_unit_hiseq_var, 
                                     digits = 0))
    updateNumericInput(session, 
                       "output_unit_2", 
                       value = round(output_unit_miseq_v2_var, 
                                     digits = 0))
    updateNumericInput(session, 
                       "output_unit_3", 
                       value = round(output_unit_miseq_v3_var, 
                                     digits = 0))
    #number of units
    output_number_hiseq_var <- output_req_var / output_unit_hiseq_var
    output_number_miseq_v2_var <- output_req_var / output_unit_miseq_v2_var
    output_number_miseq_v3_var <- output_req_var / output_unit_miseq_v3_var
    updateNumericInput(session, 
                       "output_number_1", 
                       value = output_number_hiseq_var)
    updateNumericInput(session, 
                       "output_number_2", 
                       value = output_number_miseq_v2_var)
    updateNumericInput(session, 
                       "output_number_3", 
                       value = output_number_miseq_v3_var)
    #samples per unit
    output_samples_hiseq_var <- floor(output_unit_hiseq_var / output_req_var)
    output_samples_miseq_v2_var <- floor(output_unit_miseq_v2_var / output_req_var)
    output_samples_miseq_v3_var <- floor(output_unit_miseq_v3_var / output_req_var)
    updateNumericInput(session, 
                       "output_samples_1", 
                       value = output_samples_hiseq_var)
    updateNumericInput(session, 
                       "output_samples_2", 
                       value = output_samples_miseq_v2_var)
    updateNumericInput(session, 
                       "output_samples_3", 
                       value = output_samples_miseq_v3_var)
    #required lanes/flow cells
    req_lanes_hiseq_var <- ceiling(sample_number_var / output_samples_hiseq_var)
    req_flowcells_hiseq_var <- req_lanes_hiseq_var / 8
    req_flowcells_miseq_v2_var <- ceiling(sample_number_var / output_samples_miseq_v2_var)
    req_flowcells_miseq_v3_var <- ceiling(sample_number_var / output_samples_miseq_v3_var)
    updateNumericInput(session, 
                       "output_required_1a", 
                       value = req_lanes_hiseq_var)
    updateNumericInput(session, 
                       "output_required_1b", 
                       value = req_flowcells_hiseq_var)
    updateNumericInput(session, 
                       "output_required_2", 
                       value = req_flowcells_miseq_v2_var)
    updateNumericInput(session, 
                       "output_required_3", 
                       value = req_flowcells_miseq_v3_var)
    #price
    price_hiseq_var <- flowcell_price_hiseq_var / 8 * req_lanes_hiseq_var
    price_miseq_v2_var <- flowcell_price_miseq_v2_var * req_flowcells_miseq_v2_var
    price_miseq_v3_var <- flowcell_price_miseq_v3_var * req_flowcells_miseq_v3_var
    updateNumericInput(session, 
                       "output_price_1", 
                       value = price_hiseq_var)
    updateNumericInput(session, 
                       "output_price_2", 
                       value = price_miseq_v2_var)
    updateNumericInput(session, 
                       "output_price_3", 
                       value = price_miseq_v3_var)
  }) #close observe
  
  #toggle hidden calculation output 1
  observeEvent(input$toggle_calc_info_1, {
    shinyjs::toggle("hidden_calc_1")
    shinyjs::toggle("hidden_calc_2")
    shinyjs::toggle("hidden_calc_3")
    shinyjs::hide("toggle_calc_info_1")
    shinyjs::toggle("hidden_toggle_calc_info_button")
  }) #close observeevent
  
  #toggle hidden calculation output 2
  observeEvent(input$toggle_calc_info_2, {
    shinyjs::toggle("hidden_calc_1")
    shinyjs::toggle("hidden_calc_2")
    shinyjs::toggle("hidden_calc_3")
    shinyjs::hide("hidden_toggle_calc_info_button")
    shinyjs::toggle("toggle_calc_info_1")
  }) #close observeevent
  
  #calculations mips, sequencing size, coverage required
  observe({
    mips_amount_var <- as.numeric(input$mips_amount)
    sequencing_size_var <- as.numeric(input$sequencing_size)
    mips_result_var <- mips_amount_var * sequencing_size_var
    updateNumericInput(session, 
                       "result_mips_calculation", 
                       value = mips_result_var)
  }) #close observe
  
  #analysis
  #button not working right now, placeholder function: alert
  observeEvent(input$analysis_start, {
    alert(text = "Work in progress...")
  }) #close observeevent
  
  #further tools
  #sequencegrabber
  observeEvent(input$get_sequence, {
    withBusyIndicatorServer("get_sequence", {
      withProgress(message = 'Obtaining sequence', 
                   value = 0, {
                     incProgress(0.3, 
                                 detail = "Reading genome")
                     coordinates_var <- input$coordinates
                     write.table(coordinates_var, 
                                 "./data/coordinates_temp.txt", 
                                 quote = FALSE, 
                                 row.names = FALSE, 
                                 col.names = FALSE)
                     system(paste(
                       "./scripts/twoBitToFa ./refs/hg19.2bit ./data/sequence_temp_before.txt -seqList=./data/coordinates_temp.txt -noMask", 
                       sep = "")
                     ) #close system
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Searching")
                     system(paste(
                       "fasta_formatter -i ./data/sequence_temp_before.txt -o ./data/sequence_temp_after.txt -t -e"
                     ) #close paste
                     ) #close system
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Writing")
                     seq_var_1 <- read.csv("./data/sequence_temp_after.txt", 
                                           header = FALSE, 
                                           sep = "\t")
                     seq_var_2 <- as.character(seq_var_1[1,2])
                     updateTextAreaInput(session, 
                                         "sequence", 
                                         value = seq_var_2)
                     Sys.sleep(0.5)
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
  }) #close observeevent
  
  #complementary
  observeEvent(input$make_complementary, {
    withBusyIndicatorServer("make_complementary", {
      withProgress(message = "Calculating", 
                   value = 0, {
                     incProgress(0.3, detail = "Reversing")
                     complementary_var_1 <- DNAString(input$pre_complementary)
                     complementary_var_2 <- reverseComplement(complementary_var_1)
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Creating complement")
                     Sys.sleep(0.5)
                     incProgress(0.3, 
                                 detail = "Writing")
                     Sys.sleep(0.5)
                     updateTextAreaInput(session, 
                                         "post_complementary", 
                                         value = as.character(complementary_var_2))
                     incProgress(0.1, 
                                 detail = "Done")
                   }) #close withprogress
    }) #close withbusyindicatorserver
  }) #close observeevent
  
  #password
  observeEvent(input$pw_go, {
    pw_temp <<- input$pw_enter_password
    if(pw_temp=="password") { # for testing purposes only
      shinyjs::show("pw_protect")
      shinyjs::hide("pw_enter_password")
      shinyjs::hide("pw_go")}
  }) #close observeevent
  
  session$onSessionEnded(stopApp)
  
  } #close function
#end of server

enableBookmarking(store = "url")

shinyApp(ui = ui, 
         server = server)