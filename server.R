library(reshape2)
library(ggplot2)

# The source data file contains proofreader data so is considered sensitive.
# The aggregate reduces the file from 40M to 6M.
if (file.exists('ocrdiff2_metrics_per_page.csv')) {
    d <- read.csv('ocrdiff2_metrics_per_page.csv')
    proofing <- aggregate(cbind(count, size) ~ project + round + change, d, sum)
    proofing$ratio <- proofing$count / proofing$size
    write.csv(proofing, "proofing.csv")
} else {
    proofing <- read.csv("proofing.csv")
}
proofing.wide <- dcast(proofing, project + round ~ change, value.var="ratio", fun.aggregate=sum)
p1.projects <- proofing$project[proofing$round=="P1"]
p2.projects <- proofing$project[proofing$round=="P2"]
p3.projects <- proofing$project[proofing$round=="P3"]
f1.projects <- proofing$project[proofing$round=="F1"]
f2.projects <- proofing$project[proofing$round=="F2"]
complete.p.projects <-  intersect(intersect(p1.projects, p2.projects), p3.projects)
complete.f1.projects <- intersect(complete.p.projects, f1.projects)
complete.f2.projects <- intersect(complete.f1.projects, f2.projects)

extract.project <- function(projects) {
    retval <- proofing.wide[is.element(proofing.wide$project, projects), ]
    retval <- retval[order(retval$project), ]
    retval
}

complete.p.proofing <- extract.project(complete.p.projects)
complete.f1.proofing <- extract.project(complete.f1.projects)
complete.f2.proofing <- extract.project(complete.f2.projects)
changes <- as.character(unique(proofing$change))

make.metric <- function(pdata)
    function(round, changes) {
    if (length(changes) == 0) {
        retval <- rep(0, sum(pdata$round == round))
    } else {
        if (length(changes) == 1) {
            retval <- pdata[pdata$round == round, changes]
        } else {
            retval <- apply(pdata[pdata$round == round, changes], 1, sum)
        }
    }
    retval
}

metric.p <- make.metric(complete.p.proofing)
metric.f1 <- make.metric(complete.f1.proofing)
metric.f2 <- make.metric(complete.f2.proofing)

shinyServer(
  function(input, output, session) {
    observe({
        updateCheckboxGroupInput(
            session, 'changes', choices = changes,
            selected = if (input$all) changes
        )
    })
    output$changeSelector <- renderUI({
        checkboxGroupInput("changes", NULL, changes)
    })
    metrics.p <- reactive({
        data.frame(project=complete.p.proofing$project[complete.p.proofing$round == "P1"],
                   p1=metric.p("P1", input$changes),
                   p2=metric.p("P2", input$changes),
                   p3=metric.p("P3", input$changes))
        })
    metrics.f1 <- reactive({
        data.frame(project=complete.f1.proofing$project[complete.f1.proofing$round == "P3"],
                   p3=metric.f1("P3", input$changes),
                   f1=metric.f1("F1", input$changes))
    })
    metrics.f2 <- reactive({
        data.frame(project=complete.f2.proofing$project[complete.f2.proofing$round == "F1"],
                   f1=metric.f2("F1", input$changes),
                   f2=metric.f2("F2", input$changes))
    })
    output$p1Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics.p()$p1+epsilon, metrics.p()$p2+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("P1 changes/word") + ylab("P2 changes/word")
    })
    output$p2Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics.p()$p2+epsilon, metrics.p()$p3+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("P2 changes/word") + ylab("P3 changes/word")
    })
    output$p3Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics.f1()$p3+epsilon, metrics.f1()$f1+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("P3 changes/word") + ylab("F1 changes/word")
    })
    output$f1Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics.f2()$f1+epsilon, metrics.f2()$f2+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("F1 changes/word") + ylab("F2 changes/word")
    })
  }
)
