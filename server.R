#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
require(scales)
library(stringr)
library(readr)
library(dplyr)
library(rgdal)

options(shiny.trace = TRUE)

#### import data ####
sparsity_data <- read.csv("data/sparsity_data_2020.csv")

#### Define server logic ####
shinyServer(function(input, output) {
    #### Define reactive variables ####
    react_data = reactive({
        sparsity_data %>%
            mutate(# weighted_ADM = ADM + (ADM * input$single_weight),
                new_aid = input$single_weight * sr,
                new_aid_pp = new_aid / ADM)
        # weighted_sr = (as.numeric(gsub("[^0-9|^.]", "", input$single_weight))*weighted_ADM))
        
        
        # ,
        #
        # weighted_srpp = weighted_sr/ADM,
        # difference_sr = weighted_sr - sr_BEP,
        # diference_sr_pct = difference_sr/sr_BEP,
        # weighted_tr = weighted_sr + lr_BEP,
        # weighted_tr_pp = weighted_tr/ADM,
        # difference_tr = weighted_sr = slrBEP,
        # difference_tr_pp = difference_tr/ADM)
    })
    
    output$weighted_sr <- reactive({
        dollar(sum(react_data()$weighted_sr, na.rm = TRUE))
    })
    
    output$new_aid_total <- reactive({
        dollar(sum(react_data()$new_aid, na.rm = TRUE))
    })
    
    # output$table1 <- DT::renderDataTable(DT::datatable({
    #     data <- sparsity_data
    #     data
    # }))
    
    tableData <- reactive({
        react_data() %>%
            filter(ADM_persqmile <= input$enroll_psm)
    })
    prettyData <- reactive({
        tableData() %>%
            rename(
                District = District,
                Enrollment = ADM,
                `New Aid ($)` = new_aid,
                `New Aid Per Person ($)` = new_aid_pp
            )
    })
    percentVARs <- reactive({
        x <- prettyData() %>%
            select(contains("%")) ## any variable with % will be formated as a percent
        colnames(x)
    })
    
    currencyVARs <- reactive({
        x <- prettyData() %>%
            select(contains("$"))  ## any variable without these things will be a $
        colnames(x)
    })
    
    roundVARS = reactive({
        x = prettyData() %>%
            select(matches('Enrollment')) ## any variable with these will be an integer
        colnames(x)
    })
    
    prettyTableData <- reactive({
        prettyData() %>% select(c(
            'District',
            'Enrollment',
            'New Aid Per Person ($)',
            'New Aid ($)'
        ))
    })
    output$prettyTable <- DT::renderDataTable({
        datatable(
            prettyTableData(),
            rownames = FALSE,
            options = list(
                paging = FALSE,
                scrollY = "700px",
                scrollX = TRUE,
                scrollCollapse = TRUE
            )
        ) %>%
            formatCurrency(
                currencyVARs(),
                digits = 0,
                currency = '$',
                mark = ','
            ) %>%
        formatCurrency(roundVARS(), currency = "", mark = ',', digits = 0)
    })
    # rownames = FALSE,
    # options = list(paging = FALSE, scrollY = "700px", scrollX = TRUE, scrollCollapse = TRUE)) %>%
    # formatPercentage(percentVARs(), digits = 0) %>% # format percents
    # formatCurrency(currencyVARs(), digits = 0, currency = "$", mark = ",") %>% #format dollars
    # formatCurrency(roundVARS(), currency = "", digits = 0) %>% #format numbers
    # formatStyle(names(tableData()), color = JS("value < 0 ? 'red' : 'black'")))
    # output$table2 <- DT::renderDataTable(
    #     prettyData() %>%
    #         select(on_of(c("")))
    # )
    #     DT::datatable({
    #     data <- tableData
    #     data
    # }))
    
    output$budget <- reactive({
        dollar(sum(sparsity_data$sr_BEP, na.rm = TRUE))
    })
    
    output$balance <- reactive({
        dollar(sum(sparsity_data$sr_BEP) - sum(sparsity_data$sr, na.rm = TRUE))
        # dollar(sum(react_data()$weighted_sr)-sum(react_data()$sr_BEP, na.rm = TRUE))
    })
    # prettyTableData <- reactive({
    #     prettyData() %>%
    #         select(all_of(c("District")))
    # })
    
    ## DATA TABLE
    # defining the table data as a subset of prettyDat
    # tableData <- reactive({
    #     prettyData() %>%
    #         select(one_of(c("District", input$tableCols)))
    # })
    #
    # # rendering the table output
    # output$tbl <- DT::renderDataTable({
    #
    #     datatable(tableData(), rownames = FALSE,
    #               options = list(paging = FALSE, scrollY = "700px", scrollX = TRUE, scrollCollapse = TRUE)) %>%
    #         formatPercentage(percentVARs(), digits = 0) %>% # format percents
    #         formatCurrency(currencyVARs(), digits = 0, currency = "$", mark = ",") %>% #format dollars
    #         formatCurrency(roundVARS(), currency = "", digits = 0) %>% #format numbers
    #         formatStyle(names(tableData()), color = JS("value < 0 ? 'red' : 'black'"))
    #
    # })
    
    # output$table1 <- DT::renderDataTable(
    #     data <- tableData()
    #     # , rownames = FALSE,
    #     #           options = list(paging = FALSE, scrollY = "700px", scrollX = TRUE, scrollCollapse = TRUE)) %>%
    #     #     formatPercentage(percentVARs(), digits = 0) %>% # format percents
    #     #     formatCurrency(currencyVARs(), digits = 0, currency = "$", mark = ",") %>% #format dollars
    #     #     formatCurrency(roundVARS(), currency = "", digits = 0) %>% #format numbers
    #     #     formatStyle(names(tableData()), color = JS("value < 0 ? 'red' : 'black'"))
    # )
    
})