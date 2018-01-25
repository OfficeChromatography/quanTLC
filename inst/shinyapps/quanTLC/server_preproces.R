output$Preprocess_ui_1 = renderUI({
  column(12,
         column(4,
                # h4("Median filter"),
                # numericInput('preprocess.medianfilter','Half-size of the filtering window',3),
                h4('Gamma correction'),
                numericInput('preprocess.gammacorrection','Value',2),
                h4("Smoothing"),
                helpText(   a("Help for this feature",target="_blank",
                              href="https://www.rdocumentation.org/packages/prospectr/versions/0.1.3/topics/savitzkyGolay?")
                ),
                helpText(   a("Wikipedia link",target="_blank",
                              href="https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter")
                ),
                numericInput("window.size","Size of the window",15,min=3,max=NA,step=2),
                numericInput("poly.order","Polynomial order",1),
                numericInput("diff.order","Differentiation order",0)
         ),
         column(4,
                h4("Baseline"),
                helpText(   a("Help for this feature",target="_blank",
                              href="http://cran.r-project.org/web/packages/baseline/baseline.pdf")
                ),
                selectizeInput("baseline", "Type of baseline", choices=c("als","fillPeaks","irls","lowpass","medianWindow","modpolyfit","peakDetection","rollingBall"),select=NULL),
                conditionalPanel(condition="input.baseline=='als'",
                                 numericInput("lambda.1","lambda: 2nd derivative constraint",5),
                                 numericInput("p","p: weighting of positive residuals",0.05),
                                 numericInput("maxit.1","maxit: maximum number of iterations",20)
                ),
                conditionalPanel(condition="input.baseline=='fillPeaks'",
                                 numericInput("lambda.2","lambda: 2nd derivative constraint for primary smoothing",6),
                                 numericInput("hwi","hwi: half width of local windows",100),
                                 numericInput("it","it: number of iterations in suppression loop",10),
                                 numericInput("int","int: number of buckets to divide spectra into",200)
                ),
                conditionalPanel(condition="input.baseline=='irls'",
                                 numericInput("lambda1","lambda1: 2nd derivative constraint for primary smoothing",5),
                                 numericInput("lambda2","lambda2: 2nd derivative constraint for secondary smoothing",9),
                                 numericInput("maxit.2","maxit: maximum number of iterations",200),
                                 numericInput("wi","wi: weighting of positive residuals",0.05)
                ),
                conditionalPanel(condition="input.baseline=='lowpass'",
                                 numericInput("steep","steep: steepness of filter curve",2),
                                 numericInput("half","half: half way point of filter curve",5)
                ),
                conditionalPanel(condition="input.baseline=='medianWindow'",
                                 numericInput("hwm","hwm: window half width for local medians",300),
                                 numericInput("hws","hws: window half width for local smoothing",5),
                                 checkboxInput("end","end: original endpoint handling",F)
                ),
                conditionalPanel(condition="input.baseline=='modpolyfit'",
                                 numericInput("degree","degree: degree of polynomial",4),
                                 numericInput("tol","tol: tolerance of difference between iterations",0.001),
                                 numericInput("rep","rep: maximum number of iterations",100)
                ),
                conditionalPanel(condition="input.baseline=='peakDetection'",
                                 numericInput("left","left: smallest window size for peak widths",30),
                                 numericInput("right","right: largest window size for peak widths",300),
                                 numericInput("lwin","lwin: Smallest window size for minimums and medians in peak removed spectra",50),
                                 numericInput("rwin","rwin: Largest window size for minimums and medians in peak removed spectra",50),
                                 numericInput("snminimum","snminimum: Minimum signal to noise ratio for accepting peaks",10)
                ),
                conditionalPanel(condition="input.baseline=='rollingBall'",
                                 numericInput("wm","wm: Width of local window for minimization/maximization",200),
                                 numericInput("ws","ws: Width of local window for smoothing",200)
                )
         ),
         column(4,
                h4("Warping"),
                helpText(   a("Wikipedia link",target="_blank",
                              href="https://en.wikipedia.org/wiki/Dynamic_time_warping")
                ),
                selectizeInput("warpmethod","Warping method",choices=(c("ptw",'dtw')),selected="ptw"),
                conditionalPanel(condition="input.warpmethod=='ptw'",
                                 helpText(   a("Help for this feature",target="_blank",
                                               href="https://www.rdocumentation.org/packages/ptw/versions/1.9-11/topics/ptw")
                                 ),
                                 numericInput('ptw.warp.ref',"track of reference",1)
                ),
                conditionalPanel(condition="input.warpmethod=='dtw'",
                                 helpText(   a("Help for this feature",target="_blank",
                                               href="https://www.rdocumentation.org/packages/dtw/versions/1.18-1/topics/dtw?")
                                 ),
                                 numericInput('dtw.warp.ref',"track of reference",1),
                                 checkboxInput('dtw.split','Do the alignment on the 4 channels separately.',F)
                )
         )
  )
})
