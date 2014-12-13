library(UsingR)
library(reshape2)
library(ggplot2)

# d <- read.csv('ocrdiff2_metrics_per_page.csv')
# d$ratio <- d$count / d$size
# reduced <- aggregate(cbind(count, size) ~ project + round + change, d, sum)
# reduced <- aggregate(cbind(count, size) ~ project + round + change, d, sum)
# write.csv(reduced, "proofing.csv")
proofing <- read.csv("proofing.csv")
proofing.wide <- dcast(proofing, project + round ~ change, value.var="ratio", fun.aggregate=sum)
p1.projects <- proofing$project[proofing$round=="P1"]
p2.projects <- proofing$project[proofing$round=="P2"]
p3.projects <- proofing$project[proofing$round=="P3"]
f1.projects <- proofing$project[proofing$round=="F1"]
f2.projects <- proofing$project[proofing$round=="F2"]
complete.projects <- intersect(intersect(intersect(intersect(p1.projects, p2.projects), p3.projects), f1.projects), f2.projects)
complete.proofing <- proofing.wide[is.element(proofing.wide$project, complete.projects), ]
complete.proofing <- complete.proofing[order(complete.proofing$project),]
changes <- as.character(unique(proofing$change))

metric <- function(round, changes) {
    if (length(changes) == 0) {
        retval <- rep(0, sum(complete.proofing$round == round))
    } else {
        if (length(changes) == 1) {
            retval <- complete.proofing[complete.proofing$round == round, changes]
        } else {
            retval <- apply(complete.proofing[complete.proofing$round == round, changes], 1, sum)
        }
    }
    retval
}

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
    metrics <- reactive({
        data.frame(project=complete.proofing$project[complete.proofing$round == "P1"],
                   p1=metric("P1", input$changes),
                   p2=metric("P2", input$changes),
                   p3=metric("P3", input$changes),
                   f1=metric("F1", input$changes),
                   f2=metric("F2", input$changes))
        })
    output$p1Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics()$p1+epsilon, metrics()$p2+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("P1 changes/word") + ylab("P2 changes/word")
    })
    output$p2Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics()$p2+epsilon, metrics()$p3+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("P2 changes/word") + ylab("P3 changes/word")
    })
    output$p3Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics()$p3+epsilon, metrics()$f1+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("P3 changes/word") + ylab("F1 changes/word")
    })
    output$f1Plot <- renderPlot({
        epsilon=1e-06
        qplot(metrics()$f1+epsilon, metrics()$f2+epsilon, geom=c("point", "smooth")) +
            scale_x_log10() + scale_y_log10() +
            xlab("F1 changes/word") + ylab("F2 changes/word")
    })
  }
)
