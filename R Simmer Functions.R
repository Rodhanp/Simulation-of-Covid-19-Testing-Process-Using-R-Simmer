# R Simmer Packages & Functions
# Online - https://pastebin.com/raw/8W0WuEUF

# R Simmer Packages & Functions

pkgs = c("simmer","simmer.plot", "simmer.bricks","data.table") 
check = pkgs %in% installed.packages()
if(!check[1]) install.packages("simmer")
if(!check[2]) install.packages("simmer.plot")
if(!check[3]) install.packages("simmer.bricks")
if(!check[4]) install.packages("data.table")
rm(pkgs, check)
library(simmer)
library(simmer.plot)
library(simmer.bricks)
library(data.table)

# User Defined Function To Log Both Resource & Activity Name (Using Attributes) 
VisitStep <- function(activity, duration = 0, resource = "0", n = 0) {
  t1 = trajectory() %>% set_attribute(activity, 0)     # enter the queue
  t2 = trajectory() %>% seize(resource, n)             # seize the resource
  t3 = trajectory() %>% set_attribute(activity, 1)     # begin the activity
  t4 = trajectory() %>% timeout(duration)              # activity duration
  t5 = trajectory() %>% release(resource, n)           # release the resource
  t6 = trajectory() %>% set_attribute(activity, 2)     # leave the activity
  if(resource == "0") return(join(t1,t3,t4,t6)) 
  return(join(t1,t2,t3,t4,t5,t6)) 
}

# Number of Arrivals Complete, Throughput, PCE, Histogram of Process Time, Flow Time
SimSummary <- function(x) {
  id = unique(x[Activity == "Exit"]$ID)
  x = x[ID %in% id]
  y = x[Activity == "Exit"]
  TH = length(id)/(max(y$Finish)-min(y$Finish))
  x[, ProcTime := Finish - Begin]
  x[, WaitTime := Begin - Arrival]
  x[, FlowTime := Finish - Arrival]
  x = x[, .(ProcTime = sum(ProcTime), 
            WaitTime = sum(WaitTime),
            FlowTime = sum(FlowTime)),
        by = ID]
  cat("\n",
      paste("Number of Arrivals Processed :", length(id)), "\n",
      paste("Avg. Throughput              :", round(TH,2)), "\n",
      paste("Process Cycle Efficiency     :", round(sum(x$ProcTime)/sum(x$FlowTime),2)), "\n","\n")
  
  p = data.frame(time = x$ProcTime); p$name = "ProcTime"
  f = data.frame(time = x$FlowTime); f$name = "FlowTime"
  ggplot(rbind(p,f), aes(time, fill = name)) + geom_density(alpha = 0.4)
}

# Step Plot for Arrivals & Departures
EventLogPlot <- function(x) {
  xentr = x[Activity == "Enter"]
  xexit = x[Activity == "Exit"]
  n = trunc(max(xexit$Finish))+1
  dt = data.frame(time = 1:n)
  dt$arrivals = 0; dt$departures = 0
  for(i in 0:n) {
    dt$arrivals[i] = nrow(xentr[Arrival <= i])
    dt$departures[i] = nrow(xexit[Finish <= i])
  }
  ggplot(dt, aes(time)) +
    geom_step(aes(y = arrivals), colour="blue") +
    geom_step(aes(y = departures), colour="red")    
}

# Dumbbell Chart for Arrivals & Departures
EventLogPlot2 <- function(x) {
  id = unique(x[Activity == "Exit"]$ID)
  x = x[ID %in% id]
  x = x[, .(Arr = min(Arrival), 
            Dep = max(Finish)),
        by = ID]
  FlowTime = x$Dep - x$Arr
  n = length(unique(x$ID))
  xmax = (trunc(max(x$Dep)/10)+1)*10
  plot(x$Dep, col = "white",
       xlab = "Time",
       ylab = "Arrivals",
       yaxt = "n",
       xlim = c(0,xmax),
       ylim = c(0,n+1))
  for(i in 1:n) {
    j = (n+1-i)
    lines(c(x$Arr[i],x$Dep[i]),c(j,j), col = "grey50")
    points(x$Arr[i], j, pch = 20, cex = 0.8, col = "blue")
    points(x$Dep[i], j, pch = 15, cex = 0.6, col = "maroon")
  }
  return(FlowTime)
}

# Dumbbell Plot Given Environment Via Arrivals Table
ArrivalsPlot <- function(env) {
  x = data.table(get_mon_arrivals(env, per_resource = TRUE, ongoing = FALSE))
  x = x[order(start_time)]
  x = x[, .(Arr = min(start_time), 
            Dep = max(end_time)),
        by = name]
  n = length(unique(x$name))
  xmax = (trunc(max(x$Dep)/10)+1)*10
  plot(x$Dep, col = "white",
       xlab = "Time",
       ylab = "Arrivals",
       yaxt = "n",
       xlim = c(0,xmax),
       ylim = c(0,n+1))
  for(i in 1:n) {
    j = (n+1-i)
    lines(c(x$Arr[i],x$Dep[i]),c(j,j), col = "grey50")
    points(x$Arr[i], j, pch = 20, cex = 0.8, col = "blue")
    points(x$Dep[i], j, pch = 15, cex = 0.6, col = "maroon")
  }
}

Sys.setenv(TZ = 'GMT')

cat("\014")