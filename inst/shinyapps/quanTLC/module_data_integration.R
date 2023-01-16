library(shinyBS)


data_integration_ui = function(id){
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
                   uiOutput(NS(id,"Integration_ui_1"))
      ),
      
      mainPanel(width=9,
                numericInput(NS(id,"Integration_plot_chrom_select"),"Selection of the track",1),
                column(6,
                       plotOutput(NS(id,"Integration_plot_1"),click = NS(id,"click_Integration_plot_1"),height = "200px"),
                       plotOutput(NS(id,"Integration_plot_2"),click = NS(id,"click_Integration_plot_2"),height = "200px")
                ),
                column(6,
                       plotOutput(NS(id,"Integration_plot_3"),click = NS(id,"click_Integration_plot_3"),height = "200px"),
                       plotOutput(NS(id,"Integration_plot_4"),click = NS(id,"click_Integration_plot_4"),height = "200px")
                )
                
      )
    ),
    sidebarLayout(
      sidebarPanel(width=6,
                   rHandsontableOutput(NS(id,"Stat_batch")),hr(),
                   uiOutput(NS(id,"Stat_column")),
                   actionButton(NS(id,"Stat_remove_all"),"Remove all selected peaks",icon=icon("remove")),
                   actionButton(NS(id,"Stat_remove_last"),"Remove last selected peak",icon=icon("remove")),
                   verbatimTextOutput(NS(id,"Stat_summary"))
      ),
      mainPanel(width=6,
                uiOutput(NS(id,"Stat_plot_select")),
                plotOutput(NS(id,"Stat_plot"))
      )
    )
  )
}



data_integration_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$Integration_ui_1 = renderUI({
      tagList(
        h4("Peak integration"),
        helpText(   a("Help for this feature",target="_blank",
                      href="https://www.rdocumentation.org/packages/pracma/versions/1.9.9/topics/findpeaks")
        ),
        numericInput(NS(id,"Integration_nups"),"Minimum number of increasing steps before a peak is reached",10,min=1),
        numericInput(NS(id,"Integration_ndowns"),"Minimum number of decreasing steps after the peak",10,min=1),
        numericInput(NS(id,"Integration_minpeakheight"),"The minimum (absolute) height a peak has to have to be recognized as such",0.01,min=0),
        actionButton(NS(id,"Integration_action_auto"),"Perform automatic integration",icon=icon("flask")),
        actionButton(NS(id,"Integration_show"), "Show peak list",icon = icon("edit")),hr(),
        bsModal("IntegrationModal", "Peak list", NS(id,"Integration_show"), size = "large",
                dataTableOutput(NS(id,"PeakList"))
        ),
        h4("Peak selection"),
        numericInput(NS(id,"Integration_hrf_tol"),"hRF range [pixel]",5),
        bsTooltip(NS(id,"Integration_hrf_tol"),"Windows size in pixel for peak selection."),
        checkboxInput(NS(id,"Integration_area_height"),"Use peak height",F)
        
      )
    })
    
    outputOptions(output, "Integration_ui_1", suspendWhenHidden = FALSE)
    
    observeEvent(input$Integration_action_auto,{
      if(is.null(reac$preprocessed)){
        shinyalert("Error!", "Preprocessed the chromatograms.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }else{
        reac$model=list()
        reac$Integration = list(
          Integration_nups = input$Integration_nups,
          Integration_ndowns = input$Integration_ndowns,
          Integration_minpeakheight = input$Integration_minpeakheight,
          # Integration_npeaks = input$Integration_npeaks,
          PeakList= data.frame() ## colnames() = c("Track","Channel","Start","End","Max","hRF","Height","Area")
        )
        reac$batch = reac$batch[,1:3]
        width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
        Zf = reac$dimension["Migration front [mm]",]
        dist.bas = reac$dimension["Distance to lower edge [mm]",]
        
        for(channel in seq(4)){
          for(i in seq(nrow(reac$preprocessed))){
            m = findpeaks(reac$preprocessed[i,,channel],
                          nups = input$Integration_nups, ndowns = input$Integration_ndowns,
                          zero = "0", peakpat = NULL, minpeakheight = input$Integration_minpeakheight,
                          minpeakdistance = 1, threshold = 0, npeaks = 50, sortstr = FALSE)
            ## colnames(reac$Integration$PeakList) = c("Track","Channel","Start","End","Max","hRF","Height","Area")
            if(!is.null(m)){
              for(j in seq_len(nrow(m))){
                reac$Integration$PeakList = rbind(
                  reac$Integration$PeakList,
                  c(i,channel,
                    f.index_to_hrf(m[j,4],width,dist.bas,Zf,reac$preprocessed),
                    f.index_to_hrf(m[j,3],width,dist.bas,Zf,reac$preprocessed),
                    f.index_to_hrf(m[j,2],width,dist.bas,Zf,reac$preprocessed),
                    m[j,1],sum(reac$preprocessed[i,m[j,4]:m[j,3],channel]),
                    m[j,4],m[j,3],m[j,2])
                )
              }
            }
          }
        }
        if(nrow(reac$Integration$PeakList)==0){
          shinyalert("Warning!", "No peak found, try changing the preprocessing or integration options.", type = "warning", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
        }else{
          colnames(reac$Integration$PeakList) = c("Track","Channel","Start hRf","End hRf","hRf","Height","Area","Start","End","Max")
        }
        reac$Integration$PeakList = reac$Integration$PeakList[reac$Integration$PeakList$hRf > 0 & reac$Integration$PeakList$hRf < 100,]
      }
      
    })
    output$PeakList = renderDataTable({
      validate(need(nrow(reac$Integration$PeakList) > 0,"Perform the integration."))
      d = reac$Integration$PeakList[,1:7]
      d$Channel = c("red","green","blue","gray")[d$Channel]
      d
    })
    
    sapply(1:4,function(channel){
      output[[paste0('Integration_plot_',channel)]] = renderPlot({
        # channel = 2
        validate(need(!is.null(reac$preprocessed),"Preprocess the video densitograms."),
                 need(input$Integration_plot_chrom_select > 0 && input$Integration_plot_chrom_select <= reac$nbr.band,"Wrong track selection."))
        width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
        Zf = reac$dimension["Migration front [mm]",]
        dist.bas = reac$dimension["Distance to lower edge [mm]",]
        par(mar=c(2.5,2.5,2,0),mgp=c(1.5,0.5,0))
        f.plot.array(reac$preprocessed,id = as.numeric(input$Integration_plot_chrom_select),
                     hauteur = width,Zf = Zf,dist.bas = dist.bas,reconstruct = F,channel = channel,main=c("Red channel","Green channel","Blue channel","Grayscale")[channel])
        for(i in seq_len(nrow(reac$Integration$PeakList))){
          if(reac$Integration$PeakList$Channel[i] == channel && reac$Integration$PeakList$Track[i] == input$Integration_plot_chrom_select){
            abline(v=reac$Integration$PeakList[i,8:10],col=c("green","red","black"))
          }
        }
        abline(h=0)
      })
    })
    
    observeEvent(input$click_Integration_plot_1,{
      reac$Integration_selection = list(channel = 1,x = round(input$click_Integration_plot_1$x))
    })
    observeEvent(input$click_Integration_plot_2,{
      reac$Integration_selection = list(channel = 2,x = round(input$click_Integration_plot_2$x))
    })
    observeEvent(input$click_Integration_plot_3,{
      reac$Integration_selection = list(channel = 3,x = round(input$click_Integration_plot_3$x))
    })
    observeEvent(input$click_Integration_plot_4,{
      reac$Integration_selection = list(channel = 4,x = round(input$click_Integration_plot_4$x))
    })
    
    
    observeEvent(reac$Integration_selection,{
      # validate(need(nrow(reac$Integration$PeakList)>0,"Perform the integration first"))
      if(nrow(reac$Integration$PeakList)==0){
        shinyalert("Error!", "Perform the integration first.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }else{
        width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
        Zf = reac$dimension["Migration front [mm]",]
        dist.bas = reac$dimension["Distance to lower edge [mm]",]
        truc = rep(0,nrow(reac$preprocessed))
        rows = c()
        for(i in seq(nrow(reac$preprocessed))){
          df = reac$Integration$PeakList[reac$Integration$PeakList$Channel == reac$Integration_selection$channel & reac$Integration$PeakList$Track == i,]
          row = which.min(abs(df$Max - reac$Integration_selection$x))
          if(length(row) > 0){
            rows = c(rows,df$hRf[row])
            if(abs(df$hRf[row] - f.index_to_hrf(reac$Integration_selection$x,width,dist.bas,Zf,reac$preprocessed)) <= input$Integration_hrf_tol){
              truc[i] = df[row,if(input$Integration_area_height){"Height"}else{"Area"}]
            }
          }
        }
        # c(red=1,green=2,blue=3,gray=4)
        compound = paste0(if(input$Integration_area_height){"Height "}else{"Area "},c("red","green","blue","gray")[reac$Integration_selection$channel] ," - hRF ",round(mean(rows))," [AU]")
        batch = hot_to_r(input$Stat_batch)
        if(compound %in% colnames(batch)){
          shinyalert("Error!", "Peak already selected.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
        }else if(0 %in% truc[batch$Standard]){
          shinyalert("Error!", "Peak not found in at least one standard.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
          # }else if(sum(batch[,"Standard"]) < 4){
          #   shinyalert("Error!", "no calibration with 3 standards.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
        }else{
          reac$batch = cbind(batch,truc)
          
          data = data.frame(x=reac$batch[reac$batch[,"Standard"],"Quantity [AU]"],y=truc[reac$batch[,"Standard"]])
          reac$model[[compound]] = calibrate(y~x, data, test.higher.orders = T,
                                             max.order = if(sum(batch[,"Standard"]) < 4){1}else{2}, 
                                             p.crit = 0.05, 
                                             F.test = "partial", method = "qr", model = T)
          truc = inversePredictCalibrate(reac$model[[compound]],truc)[,2] %>% round(4)
          reac$batch = cbind(reac$batch,truc)
          colnames(reac$batch)[(ncol(reac$batch)-1):(ncol(reac$batch))] = c(compound,paste0("Prediction ",compound))
        }
      }
    })
    
    output$Stat_batch = renderRHandsontable({
      validate(need(!is.null(reac$extracted),"Extract the chromatograms."))
      reac$batch$`Quantity [AU]` = as.numeric(reac$batch$`Quantity [AU]`)
      reac$batch$Track = as.character(reac$batch$Track)
      truc = rhandsontable(reac$batch,rowHeaders = NULL) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      if(ncol(reac$batch) >3){
        truc %>% hot_col(colnames(reac$batch), readOnly = TRUE)
      }else{
        truc
      }
    })
    output$Stat_plot_select = renderUI({
      validate(need(!is.null(reac$batch),"Extract the Chromatograms."))
      validate(need(ncol(reac$batch) > 3,"Select at least one peak."))
      choices = colnames(reac$batch)[seq(from=4,by=2,length.out = (ncol(reac$batch)-3)/2)]
      selectizeInput(ns("Stat_plot_select"),"Select peak",choices = choices,selected=choices[length(choices)])
    })
    output$Stat_plot = renderPlot({
      validate(need(length(reac$model) > 0,"Select at least one peak."))
      validate(need(!is.null(input$Stat_plot_select),"Not ready yet."))
      data = data.frame(x=reac$batch[,"Quantity [AU]"],y=reac$batch[,input$Stat_plot_select])
      data$x[!reac$batch$Standard] = reac$batch[,paste0("Prediction ",input$Stat_plot_select)][!reac$batch$Standard]
      plot(x = data$x,y=data$y,xlab = "Quantity [AU]",ylab = "Intensity [AU]",pch = 4,col=(!reac$batch$Standard)+1,main=input$Stat_plot_select)
      timevalues <- seq(min(data$y), max(data$y), by = abs(min(data$y) - max(data$y))/100)
      pred <- inversePredictCalibrate(reac$model[[input$Stat_plot_select]],timevalues)[,2]
      lines(pred,timevalues)
    })
    output$Stat_summary = renderPrint({
      validate(need(length(reac$model) > 0,"Select at least one peak."))
      validate(need(!is.null(input$Stat_plot_select),"Not ready yet."))
      model = reac$model[[input$Stat_plot_select]]
      print(summary(model))
      if(is.null(model$model$`I(x^2)`)){
        truc = coef(summary(model))
        cat(paste0("LOD: ",round(abs(3.3*truc[1,2]/truc[2,1]),4)," [AU]\n\n"))
        cat(paste0("LOQ: ",round(abs(10*truc[1,2]/truc[2,1]),4)," [AU]"))
      }else{
        cat("LOD and LOQ not available for quadratic models")
      }
    })
    
    observeEvent(input$Stat_remove_all,{
      if(is.null(reac$batch)){
        shinyalert("Error!", "Extract the chromatograms.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }else if(ncol(reac$batch) > 3){
        reac$model = list()
        reac$batch = reac$batch[,1:3]
      }else{
        shinyalert("Error!", "No peak to remove.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }
    })
    observeEvent(input$Stat_remove_last,{
      if(is.null(reac$batch)){
        shinyalert("Error!", "Extract the chromatograms.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }else if(ncol(reac$batch) > 3){
        reac$model[[length(reac$model)]] = NULL
        reac$batch = reac$batch[,1:(ncol(reac$batch)-2)]
      }else{
        shinyalert("Error!", "No peak to remove.", type = "error", closeOnEsc = TRUE,closeOnClickOutside = TRUE)
      }
    })
  })
}