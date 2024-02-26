library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)
library(RColorBrewer)
library(targets)

setwd("..")

pooled_connection_ls_models_weighted_summary <- tar_read(pooled_connection_ls_models_weighted_summary)
pooled_connection_swbs_models_weighted_summary <- tar_read(pooled_connection_swbs_models_weighted_summary)
pooled_connection_anx_models_weighted_summary <- tar_read(pooled_connection_anx_models_weighted_summary)
pooled_connection_cesd_models_weighted_summary <- tar_read(pooled_connection_cesd_models_weighted_summary)
pooled_connection_sh_models_weighted_summary <- tar_read(pooled_connection_sh_models_weighted_summary)
pooled_connection_paykel_models_weighted_summary <- tar_read(pooled_connection_paykel_models_weighted_summary)

pooled_connection_ls_models_relevelled_weighted_summary <- tar_read(pooled_connection_ls_models_relevelled_weighted_summary)
pooled_connection_swbs_models_relevelled_weighted_summary <- tar_read(pooled_connection_swbs_models_relevelled_weighted_summary)
pooled_connection_anx_models_relevelled_weighted_summary <- tar_read(pooled_connection_anx_models_relevelled_weighted_summary)
pooled_connection_cesd_models_relevelled_weighted_summary <- tar_read(pooled_connection_cesd_models_relevelled_weighted_summary)
pooled_connection_sh_models_relevelled_weighted_summary <- tar_read(pooled_connection_sh_models_relevelled_weighted_summary)
pooled_connection_paykel_models_relevelled_weighted_summary <- tar_read(pooled_connection_paykel_models_relevelled_weighted_summary)

pooled_internet_ls_models_weighted_summary <- tar_read(pooled_internet_ls_models_weighted_summary)
pooled_internet_swbs_models_weighted_summary <- tar_read(pooled_internet_swbs_models_weighted_summary)
pooled_internet_anx_models_weighted_summary <- tar_read(pooled_internet_anx_models_weighted_summary)
pooled_internet_cesd_models_weighted_summary <- tar_read(pooled_internet_cesd_models_weighted_summary)
pooled_internet_sh_models_weighted_summary <- tar_read(pooled_internet_sh_models_weighted_summary)
pooled_internet_paykel_models_weighted_summary <- tar_read(pooled_internet_paykel_models_weighted_summary)

connection_ls_models_weighted_summary <- tar_read(connection_ls_models_weighted_summary)
connection_swbs_models_weighted_summary <- tar_read(connection_swbs_models_weighted_summary)
connection_anx_models_weighted_summary <- tar_read(connection_anx_models_weighted_summary)
connection_cesd_models_weighted_summary <- tar_read(connection_cesd_models_weighted_summary)
connection_sh_models_weighted_summary <- tar_read(connection_sh_models_weighted_summary)
connection_paykel_models_weighted_summary <- tar_read(connection_paykel_models_weighted_summary)

connection_ls_models_relevelled_weighted_summary <- tar_read(connection_ls_models_relevelled_weighted_summary)
connection_swbs_models_relevelled_weighted_summary <- tar_read(connection_swbs_models_relevelled_weighted_summary)
connection_anx_models_relevelled_weighted_summary <- tar_read(connection_anx_models_relevelled_weighted_summary)
connection_cesd_models_relevelled_weighted_summary <- tar_read(connection_cesd_models_relevelled_weighted_summary)
connection_sh_models_relevelled_weighted_summary <- tar_read(connection_sh_models_relevelled_weighted_summary)
connection_paykel_models_relevelled_weighted_summary <- tar_read(connection_paykel_models_relevelled_weighted_summary)

internet_ls_models_weighted_summary <- tar_read(internet_ls_models_weighted_summary)
internet_swbs_models_weighted_summary <- tar_read(internet_swbs_models_weighted_summary)
internet_anx_models_weighted_summary <- tar_read(internet_anx_models_weighted_summary)
internet_cesd_models_weighted_summary <- tar_read(internet_cesd_models_weighted_summary)
internet_sh_models_weighted_summary <- tar_read(internet_sh_models_weighted_summary)
internet_paykel_models_weighted_summary <- tar_read(internet_paykel_models_weighted_summary)

ui <- fluidPage(
    titlePanel("Results dashboard"),

    sidebarLayout(
        sidebarPanel(
            selectInput(
                "country",
                "Country:",
                c("All countries" = "pooled", "Cambodia", "Ethiopia", "Indonesia", "Kenya", "Malaysia", "Mozambique", "Namibia", "Philippines", "Thailand", "Uganda", "Vietnam")
            ),

            selectInput(
                "analysis",
                "Analysis:",
                c('Lockdown and connection ("no lockdown" reference group)' = "connection", 'Lockdown and connection ("lockdown, disconnected" reference group)' = "connection_relevelled", "Lockdown and internet use" = "internet")
            ),

            selectInput(
                "indicator",
                "Indicator:",
                c("Life satisfaction" = "ls", "CW-SWBS" = "swbs", "Anxiety" = "anx", "CESD-R" = "cesd")
            ),

            selectInput(
                "group",
                "Show coefficients for:",
                c("None", "All participants", "Boys", "Girls", "Rural", "Urban", "Rural boys", "Urban boys", "Rural girls", "Urban girls")
            )
        ),

        mainPanel(
            plotOutput("barplot"),
            tableOutput("coefs")
        )
    )
)

server <- function(input, output){
    output$barplot <- renderPlot({
        summary_list <- c("All participants", "Boys", "Girls", "Rural", "Urban", "Rural boys", "Urban boys", "Rural girls", "Urban girls")

        if(input$country == "pooled"){
            if(input$analysis == "connection"){
                plotData <- get(paste("pooled_connection", input$indicator, "models_weighted_summary", sep = "_"))
            } else if(input$analysis == "connection_relevelled"){
                plotData <- get(paste("pooled_connection", input$indicator, "models_relevelled_weighted_summary", sep = "_"))
            } else if(input$analysis == "internet"){
                plotData <- get(paste("pooled_internet", input$indicator, "models_weighted_summary", sep = "_"))
            }
        } else {
            if(input$analysis == "connection"){
                plotData <- get(paste("connection", input$indicator, "models_weighted_summary", sep = "_"))
            } else if(input$analysis == "connection_relevelled"){
                plotData <- get(paste("connection", input$indicator, "models_relevelled_weighted_summary", sep = "_"))
            } else if(input$analysis == "internet"){
                plotData <- get(paste("internet", input$indicator, "models_weighted_summary", sep = "_"))
            }
        }

        plotData %<>% 
            reduce(rbind) %>%
            mutate(term = case_when(
                term == "dv_covid_statusLockdown, connected" ~ "Lockdown, connected",
                term == "dv_covid_statusLockdown, disconnected" ~ "Lockdown, disconnected",
                term == "dv_covid_statusNo lockdown" ~ "No lockdown",
                term == "B1" ~ "Internet use",
                term == "CO2" ~ "Lockdown",
                term == "B1:CO2" ~ "Interaction"
            ))

        if(input$country != "pooled"){
            plotData %<>% filter(country == input$country)
        }

        if(input$analysis %in% c("connection", "connection_relevelled")){
            plotData$group <- rep(summary_list, each = 2)
        } else if(input$analysis == "internet"){
            plotData$group <- rep(summary_list, each = 3)
        }

        ggplot(plotData, aes(x = term, y = estimate, fill = group, ymin = estimate + qt(0.025, df) * std.error, ymax = estimate + qt(0.975, df) * std.error)) +
            geom_col(position = "dodge") +
            geom_errorbar(position = position_dodge(width = 0.9), width = 0.5) + 
            labs(x = "Term", y = "Coefficient", fill = "Group") +
            scale_fill_brewer(palette = "Paired") +
            theme_bw()
    })

    output$coefs <- renderTable({
        summary_list <- c("All participants", "Boys", "Girls", "Rural", "Urban", "Rural boys", "Urban boys", "Rural girls", "Urban girls")

        if(input$group != "None"){
            if(input$country == "pooled"){
                if(input$analysis == "connection"){
                    plotData <- get(paste("pooled_connection", input$indicator, "models_weighted_summary", sep = "_"))
                } else if(input$analysis == "connection_relevelled"){
                    plotData <- get(paste("pooled_connection", input$indicator, "models_relevelled_weighted_summary", sep = "_"))
                } else if(input$analysis == "internet"){
                    plotData <- get(paste("pooled_internet", input$indicator, "models_weighted_summary", sep = "_"))
                }
            } else {
                if(input$analysis == "connection"){
                    plotData <- get(paste("connection", input$indicator, "models_weighted_summary", sep = "_"))
                } else if(input$analysis == "connection_relevelled"){
                    plotData <- get(paste("connection", input$indicator, "models_relevelled_weighted_summary", sep = "_"))
                } else if(input$analysis == "internet"){
                    plotData <- get(paste("internet", input$indicator, "models_weighted_summary", sep = "_"))
                }
            }

            plotData %<>% 
                reduce(rbind) %>%
                mutate(term = case_when(
                    term == "dv_covid_statusLockdown, connected" ~ "Lockdown, connected",
                    term == "dv_covid_statusLockdown, disconnected" ~ "Lockdown, disconnected",
                    term == "dv_covid_statusNo lockdown" ~ "No lockdown",
                    term == "B1" ~ "Internet use",
                    term == "CO2" ~ "Lockdown",
                    term == "B1:CO2" ~ "Interaction"
                ))

            if(input$country != "pooled"){
                plotData %<>% filter(country == input$country)
            }

            if(input$analysis %in% c("connection", "connection_relevelled")){
                plotData$group <- rep(summary_list, each = 2)
            } else if(input$analysis == "internet"){
                plotData$group <- rep(summary_list, each = 3)
            }

            plotData %>% filter(group == input$group) %>% select(term, estimate, p.value) %>% rename("Term" = term, "\u03B2" = estimate, "p" = p.value)
        }
    }, digits = 3)
}

shinyApp(ui = ui, server = server)