library(dplyr)
library(reshape2)
library(lpSolve)
library(RColorBrewer)
library(ggplot2)
library(shiny)
library(queueing)
library(xtable)
#library(Rglpk)
#library(Rsymphony)

# Reactive Data
# results.df
# shifts.df
# solutions.df
#
# Output:
# loadplot
# mean_Wq
# excess_capacity
# sched_text

shinyServer(function(input, output) {


# MMC Queue Server Calculations -------------------------------------------


    #Construct the shifts based on inputs
    results.df <- reactive({
      waiting_time <- input$waiting_time
      service_rate <- input$service_rate
      arrivals <- unlist(read.csv(input$file$datapath,header=F))
      Weekday <- factor(c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                        levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))
      Hour = factor(c('0:00','0:30','1:00','1:30','2:00','2:30','3:00','3:30','4:00','4:30',
                      '5:00','5:30','6:00','6:30','7:00','7:30','8:00','8:30','9:00','9:30',
                      '10:00','10:30','11:00','11:30','12:00','12:30','13:00','13:30','14:00',
                      '14:30','15:00','15:30','16:00','16:30','17:00','17:30','18:00','18:30',
                      '19:00','19:30','20:00','20:30','21:00','21:30','22:00','22:30','23:00',
                      '23:30'),levels = c('0:00','0:30','1:00','1:30','2:00','2:30','3:00','3:30','4:00','4:30',
                                    '5:00','5:30','6:00','6:30','7:00','7:30','8:00','8:30','9:00','9:30',
                                    '10:00','10:30','11:00','11:30','12:00','12:30','13:00','13:30','14:00',
                                    '14:30','15:00','15:30','16:00','16:30','17:00','17:30','18:00','18:30',
                                    '19:00','19:30','20:00','20:30','21:00','21:30','22:00','22:30','23:00',
                                    '23:30'))

      opt_servers <- vector(mode="integer",length(arrivals))
      Wq <- vector(mode="numeric",length(arrivals))
      i <- 1
      #calculate optimal servers
      for (arrival in arrivals) {

        min_servers <- ceiling(arrival/service_rate)
        opt_server <- min_servers
        mmc_Wq <- waiting_time

        repeat {
          mmc_input <- NewInput.MMC(arrival,service_rate,c=opt_server)
          mmc <- QueueingModel.i_MMC(mmc_input)
          mmc_Wq <- Wq(mmc)
          if (mmc_Wq < waiting_time) break
          opt_server <- opt_server + 1
        }

        opt_servers[i] <- opt_server
        Wq[i] <- mmc_Wq
        i <- i + 1
      }
      data.frame(Weekday=rep(Weekday,each=24),Hour=rep(0:23,7),Arrival_Rate = arrivals,Servers = opt_servers,Wq)
    })


# Shift matrix -------------------------------------------------



    shifts.df <- reactive({
        span <- 168

        if(is.null(input$file))     return(NULL)
        if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

        shift.length <- as.numeric(input$shift.length)



        #construct shift matrices
        shift.build <- c()
        shifts <- c()
        for (i in shift.length) {

            shift1 <- c(rep(1,i),rep(0,span-i))
            shift.build <- shift1
            for (j in 1:(span-1)) {
                shift1 <- c(shift1[length(shift1)],shift1[1:(length(shift1)-1)])
                shift.build <- cbind(shift.build,shift1)
            }
            shifts <- cbind(shifts,shift.build)
        }
        rm(i)
        dimnames(shifts) <- NULL

        shifts

    })


# Optimization MIP ---------------------------------------------------------



    solutions.df <- reactive({


        span <- 168
        if(is.null(input$file))     return(NULL)
        if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)


        shift.length <- as.numeric(input$shift.length)
        load <- results.df()$Servers
        starts <- as.numeric(c(input$start1,input$start2,input$start3,input$start4,
                               input$start5,input$start6,input$start7))
        shifts <- shifts.df()
        #find columns that will not be turned off
        select <- rep(0:(length(shift.length)-1)*span,each=length(starts)) + starts

        shifts[,-select] <- 0

        #Construct LP matrices
        #
        #Objective:     min X+ + X-
        #Constraints:   X+ - X- +X = Offered Load
        #               X,X-,X+ >= 0
        #
        #
        #LHS =     [Shifts][diag 1][diag -1]
        #          [        diag 1         ]
        #

        lhs <- rbind(cbind(shifts,diag(1,nrow(shifts)),diag(-1,nrow(shifts))),
                     diag(1,ncol(shifts)+2*nrow(shifts)))

        oper <- c(rep('==',nrow(shifts)),rep('>=',ncol(lhs)))

        rhs <- c(load,rep(0,nrow(lhs)-span))

        obj <- c(rep(0,ncol(shifts)),rep(1,ncol(lhs)-ncol(shifts)))

        int <- 1:ncol(shifts)

        #Solve LP
        lp.model <- lpSolve::lp(direction = 'min',obj,lhs,oper,rhs,transpose.constraints = T,int.vec = int)
        #lp.model <- Rglpk_solve_LP(obj,lhs,oper,rhs,types=rep("I",ncol(shifts)))
        #lp.model <- Rsymphony_solve_LP(obj,lhs,oper,rhs,types=rep("I",ncol(shifts)),first_feasible = FALSE,write_lp=TRUE)
        lp.model$solution[1:ncol(shifts)]

    })


# Server Optimization Plot -------------------------------------------------



    output$loadplot <- renderPlot({


        span <- 168
        if(is.null(input$file))     return(NULL)
        if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

        #pass in derived variables and matrices
        results <- results.df()
        solutions <- solutions.df()
        shifts <- shifts.df()


        #Develop Load vs. Scheduled Capacity Plot
        Optimal_Servers <- colSums(solutions * t(shifts))
        plot.input <- data.frame(select(results,Weekday,Hour,MMC_Servers=Servers),Optimal_Servers) %>%
          melt(id.vars=c("Weekday","Hour"),variable.name="Type",value.name="Value")
        colors <- brewer.pal(12,'Set3')[c(5,12)]

        ggplot(plot.input,aes(xmin=Hour,xmax=rep(1:24,14),ymin=0,ymax=Value,fill=Type)) +
        geom_rect(alpha=.3) +
        facet_grid(Weekday ~ .,scales="free") + theme(text=element_text(size = 20)) +
        theme_bw() + labs(title="Daily Comparison of Optimal Solution vs. MMC Queueing Theory Calculation") +
        scale_x_continuous(name="Hour",breaks=0:24) +
        scale_fill_manual(name="",label=c("MMC Calculated Servers","Optimal Server Solution"),values=colors)

    })


# Mean Waiting Time in Queue -----------------------------------------


#     output$mean_Wq <- renderText({
#       if(is.null(input$file))     return(NULL)
#       #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)
#
#
#       arrivals <- results.df()$Arrival_Rate
#       solutions <- solutions.df()
#       shifts <- shifts.df()
#       sched.cap <- rowSums(t(solutions * t(shifts)))
#       service_rate <- input$service_rate
#
#       Wq <- mapply(function(x,y) NewInput.MMC(x,service_rate,y) %>%
#                      QueueingModel.i_MMC() %>% Wq(),arrivals,sched.cap)
#       results <- data.frame(Arrival_Rate = arrivals,sched.cap,Wq)
#
#       paste('The average waiting time in queue is ',
#            sprintf("%.2f",as.numeric(summarize(results,sum(Wq*Arrival_Rate)/sum(Arrival_Rate)))))
#     })


# Optimized Absolute Error ---------------------------------------------------------

    output$absolute_error <- renderText({
      if(is.null(input$file))     return(NULL)
      #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

      results <- results.df()
      solutions <- solutions.df()
      shifts <- shifts.df()
      sched.cap <- colSums(solutions * t(shifts))


      paste('The absolute error is',sum(abs(sched.cap-results[['Servers']])))
  })

# Optimal Number of man-hours ---------------------------------------------------------

    output$optimal_schedules <- renderText({
      if(is.null(input$file))     return(NULL)
      #if(length(unlist(read.csv(input$file$datapath,header = F))) != span)    return(NULL)

      span <- 168
      solutions <- solutions.df()
      shifts <- shifts.df()
      shift.length <- as.numeric(input$shift.length)
      wd.list <- factor(c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                        levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

      shift.times <- data.frame(Weekday = unlist(lapply(wd.list,function(x) rep(x,24))),
                                Shift.Start = paste0(0:23,":00"),
                                Shift.Length = sort(rep(shift.length,span)))
      schedule <- cbind(shift.times[solutions > 0,],N = solutions[solutions > 0])

      paste('The optimal number of worker hours is',sum(schedule[['Shift.Length']]*schedule[['N']]))
  })


# Table Schedules ---------------------------------------------------------

    output$schedtext <- renderTable({

        if(is.null(input$file))     return(NULL)

        span <- 168
        solutions <- solutions.df()
        shifts <- shifts.df()
        shift.length <- as.numeric(input$shift.length)
        wd.list <- factor(c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'),
                          levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))

        shift.times <- data.frame(Weekday = unlist(lapply(wd.list,function(x) rep(x,24))),
                                  Shift.Start = paste0(0:23,":00"),
                                  Shift.Length = sort(rep(shift.length,span)))

        schedule <- cbind(shift.times[solutions > 0,],N = solutions[solutions > 0])
        schedule <- schedule[order(schedule$Weekday),]
        row.names(schedule) <- NULL
        xtable(schedule,type = 'html')



    })

    output$table <- renderTable({
      if(is.null(input$file))     return(NULL)

      results <- results.df()
      solutions <- solutions.df()
      shifts <- shifts.df()
      service_rate <- input$service_rate

      Optimal_Servers <- colSums(solutions * t(shifts))

#       Optimal_Wq <- mapply(function(x,y) NewInput.MMC(x,service_rate,y) %>%
#                      QueueingModel.i_MMC() %>% Wq(),results[['Arrival_Rate']],Optimal_Servers)

      output_table <- data.frame(results,Optimal_Servers,row.names = NULL)

      xtable(output_table,type='html')
    })

})



