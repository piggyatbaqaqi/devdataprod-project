shinyUI(pageWithSidebar(
  headerPanel("Project Gutenberg Distributed Proofreading ocrdiff2 small dataset"),
  sidebarPanel(
    h2("Changes"),
    checkboxInput("all", "Select All", value = TRUE),
    uiOutput("changeSelector")
  ),
  mainPanel(
    p("This is data from the ", a(href="http://www.pgdp.net", "PGDP"),
      " ",
      a(href="http://www.pgdp.net/wiki/Confidence_in_Page_analysis",
        "Confidence in Page Project"),
      ". Each point represents a project, a complete book, done by the PGDP team. Checking a change type on the left adds the ratio of that change versus the number of words in the book to an overall metric. The graphs compare this metric between successive rounds of proofreading, three proofreading rounds, P1, P2, P3, and two formatting rounds, F1 and F2."),
    p("Try removing NEWLINE_SPLIT, TOTAL_MISMATCH, and XXLARGE_DIFF which all have pathological cases."),
    p("This page prepared by La Monte Henry Piggy Yarroll for ",
      a(href="https://class.coursera.org/devdataprod-016/", "Developing Data Products"),
      " taught by Brian Caffo, PhD, Jeff Leek, PhD, and Roger D. Peng, PhD, of the Johns Hopkins Bloomberg School of Public Health, on Coursera."),
    h2("P1 vs P2"),
    plotOutput('p1Plot'),
    h2("P2 vs P3"),
    plotOutput('p2Plot'),
    h2("P3 vs F1"),
    plotOutput('p3Plot'),
    h2("F1 vs F2"),
    plotOutput('f1Plot')
  )
))
