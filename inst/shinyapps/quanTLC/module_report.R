library(shinyBS)
library(rmarkdown)

report_ui = function(id){
  tabPanel("Report",
           sidebarLayout(
             sidebarPanel(width=6,
                          checkboxGroupInput(NS(id,"Report_options"),"Include in report",
                                             choices = c(
                                               "Chromatogram"
                                               ,"Dimension table"
                                               ,"Preprocessing options"
                                               ,"Integration options"
                                               ,"Video-densitograms"
                                               ,"Peak list"
                                               ,"Peak list for each sample"
                                               ,"Regression results"
                                             ),
                                             selected = c(
                                               "Chromatogram"
                                               ,"Dimension table"
                                               ,"Preprocessing options"
                                               ,"Integration options"
                                               ,"Video-densitograms"
                                               ,"Peak list"
                                               ,"Peak list for each sample"
                                               ,"Regression results"
                                             )),
                          radioButtons(NS(id,'reportformat'), 'Document format', c('PDF', 'HTML', "MS Word"='Word'),
                                       inline = TRUE),
                          downloadButton(NS(id,'downloadReport'),label = "Report"),
                          hr(),
                          downloadButton(NS(id,"downloadCheckpoint"),label = "Save data"),
                          p("Upload this Rdata file instead of the chromatogram"),
                          hr(),
                          downloadButton(NS(id,"downloadChrom"),label = "Export as CSV format")
             ),
             mainPanel()
           )
  )
}



report_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste("quanTLC", sep = '.', switch(
          input$reportformat, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      content = function(file) {
        src <- normalizePath('report.Rmd')
        out <- rmarkdown::render('report.Rmd', switch(
          input$reportformat,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )
    output$downloadChrom <- downloadHandler(
      filename = 'quanTLC.zip',
      content = function(file) {
        fs <- c()
        channel <- c(red=1,green=2,blue=3,gray=4)
        for(i in names(channel)){
          path <- paste0("before_preprocess_",i,'.csv')
          fs <- c(fs,path)
          truc = reac$extracted[,dim(reac$extracted)[2]:1,channel[i]]
          width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
          Zf = reac$dimension["Migration front [mm]",]
          dist.bas = reac$dimension["Distance to lower edge [mm]",]
          colnames(truc) = paste0(round(seq(-dist.bas/(Zf-dist.bas),(width-dist.bas)/(Zf-dist.bas),length.out=dim(truc)[2]),3))
          write.csv(truc,file=path,row.names = T)    ##F,col.names = T)
        }
        for(i in names(channel)){
          path <- paste0("after_preprocess_",i,'.csv')
          fs <- c(fs,path)
          truc = reac$preprocessed[,dim(reac$preprocessed)[2]:1,channel[i]]
          width = reac$dimension["Plate width [mm]",];if(reac$double){width=0.5*width}
          Zf = reac$dimension["Migration front [mm]",]
          dist.bas = reac$dimension["Distance to lower edge [mm]",]
          colnames(truc) = paste0(round(seq(-dist.bas/(Zf-dist.bas),(width-dist.bas)/(Zf-dist.bas),length.out=dim(truc)[2]),3))
          write.csv(truc,file=path,row.names = T)    ##F,col.names = T)
        }
        path <- paste0('PeakList.csv')
        fs <- c(fs,path)
        truc = reac$Integration$PeakList
        write.csv(truc,file=path,row.names = F)   ##,col.names = T)
        
        path <- paste0('batch.csv')
        fs <- c(fs,path)
        truc = reac$batch
        write.csv(truc,file=path,row.names = F)   ##,col.names = T)
        
        tempFile <- tempfile(fileext = ".zip")
        zip(zipfile=tempFile, files=fs)
        file.rename(tempFile, file)
      },
      contentType = "application/zip"
    )
    output$downloadCheckpoint <- downloadHandler(
      filename = "quanTLC.Rdata",
      content = function(con) {
        assign('data',list(image = reac$image,
                           image.name = reac$image.name,
                           batch = reac$batch,
                           dimension = reac$dimension,
                           convention=reac$convention,
                           double = reac$double,
                           nbr.band=reac$nbr.band,
                           extracted = reac$extracted,
                           Preprocess.order = reac$Preprocess.order,
                           Preprocess.options = reac$Preprocess.options,
                           preprocessed = reac$preprocessed,
                           Integration = reac$Integration,
                           model=reac$model
        ))
        save(list='data',file=con)
      }
    )
  })
}
