help_ui = function(id){
  tabPanel("Help",
         helpText(   a("Instruction for local installation",target="_blank",
                       href="https://github.com/DimitriF/quantlc")
         ),
         h4("PDF manual"),
         downloadButton(NS(id,"pdf_manual"),label = "PDF"),
         h4("Video manual"),
         HTML('<video src="out.mp4" controls/>')
)
}

help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$pdf_manual <- downloadHandler(
      filename = "quanTLC_manual.pdf",
      content = function(file) {
        file.copy('www/Instruction/quanTLC_manual.pdf', file)
      }
    )
  })
}