# SIMULATION OF COVID-19 TESTING PROCESS USING R-STUDIO

source("R Simmer Functions.R")



# *********************************************************
# Create Statistical Variables
#          Required For Simulation
# *********************************************************

# Parameters Required For Simulation.
Arrival = function() {return(rexp(1, 1/2.5))}          # Inter Arrival Time - Exponential Distribution
RegTime = function() {return(runif(1, 0.5, 1.5))}      # Registration Time - Uniform Distribution
SwabTime = function() {return(rnorm(1, 2, 1/3))}       # Swab Taking Time - Normal Distribution
CleanTime = function() {return(rnorm(1, 2, 0.33))}     # Bay Cleaning Time - Normal Distribution
TestTime = function() {return(rnorm(1, 1.5, 0.25))}    # Test Process Time - Normal Distribution
SimTime = 240


# How Statistical Variables Work In R
rexp(5, 1/3)
mean(rexp(1000, 1/3))
hist(rexp(1000, 1/3))
help(rexp)

# Task 1 Exercise - Generate Normal Distribution 
#                   (Mean = 0, Std Dev = 1) 
rnorm(5,0,1)
hist(rnorm(500,0,1))
# Display 5 Values, Plot Histogram (N = 500)


# *********************************************************
# Define Trajectory, Resources, Arrivals
# *********************************************************

# A. Define Process Trajectory
t = trajectory() %>%
  
  # Registration Step
  seize("Registrar", 1) %>%  
  timeout(RegTime) %>%
  release("Registrar",1) %>%
  
  # Pickup Test Kit
  seize("Test Staff", 1) %>%   
  timeout(0.5) %>%
  release("Test Staff", 1) %>% 
  
  # Take Swab, Return Swab to Test Staff & Clean Bay Steps
  seize("Bay", 1) %>%  
  timeout(SwabTime) %>%
  seize("Test Staff", 1) %>%
  timeout(0.5) %>%
  release("Test Staff", 1) %>%
  timeout(CleanTime) %>%
  release("Bay",1) 

# Print & Plot Trajectory
print(t)
plot(t, verbose = TRUE)

# B. Define Resources & Arrivals

# Create Simulation Environment
env <- simmer()

# Define Resources
env %>% add_resource("Registrar", 1)
env %>% add_resource("Bay", 4)
env %>% add_resource("Test Staff", 1)

# Generate Arrivals
env %>% add_generator("Student", t, Arrival)

# *********************************************************
# Run Simulation, Store & Display Results
# *********************************************************

# C. Run Simulation
env %>% run(until = SimTime)

# D. Store Results
resources = get_mon_resources(env)
arrivals = get_mon_arrivals(env, per_resource = TRUE)

# Display Arrivals Table
head(arrivals)

# Display path of single individual
subset(arrivals, name == "Student8")

# Task 3 Exercise:
#   A. Store "arrivals" Results With 
#             'per_resource = FALSE'.
#   B. Plot Histogram of Flow Time
resources = get_mon_resources(env)
head(arrivals)
arrivals = get_mon_arrivals(env, per_resource = FALSE)
hist(arrivals$end_time - arrivals$start_time)

# *********************************************************
#Plot Charts & Interpret Results
# *********************************************************

# Plot Dumbbell Chart of Arrivals (Custom Function)
ArrivalsPlot(env)

# Plot Utilisation Chart
plot(resources, metric = "utilization", c("Registrar", "Test Staff", "Bay") )

# Plot Usage Chart (Average)
plot(resources, metric = "usage", "Registrar")

# Plot Usage Chart (Instantaneous) 
plot(resources, metric = "usage", "Registrar", items = "server", steps = TRUE)

# Task 4 Exercise
#      Plot the Usage of "Bay" 
#      With Steps = TRUE
plot(resources, metric="usage", "Bay", steps = TRUE )

plot(resources, metric="usage", "Bay", items = "server", steps = TRUE )
# *********************************************************
# Advanced Trajectory - Parallel Paths
# *********************************************************

# Reset Simulation 
reset(env)

# Advanced Trajectory - Parallel Paths 
# Use "Visit" instead of Seize, Timeout, Release

t1 = trajectory() %>%
  
  # Registration Step
  visit("Registrar", RegTime, 1) %>%
  
  # Pickup Test Kit
  visit("Test Staff", 0.5, 1) %>%
  
  # Take Swab (Note - Bay Not Released)
  seize("Bay", 1) %>%  
  timeout(SwabTime) %>%
  
  # Return Swab
  visit("Test Staff", 0.5, 1) 

# Here the path splits into parallel routes

t2 = trajectory() %>%
  
  # Clean Bay
  timeout(CleanTime) %>%
  
  # Release Bay (seized in t1)
  release("Bay",1) 

t3 = trajectory() %>%
  # Perform Test
  visit("Test Staff", TestTime, 1) 


# Join Trajectories
t = trajectory() %>%
  
  # path before the split
  join(t1) %>%
  
  # split into parallel paths t2, t3
  clone(n = 2, t2, t3) %>%
  
  # join parallel paths 
  synchronize(wait = TRUE) 

print(t)
plot(t, verbose = TRUE)


# Create Environment, Add Resources, Generator (Arrivals)
env <- simmer() %>%
  add_resource("Registrar", 1)  %>%
  add_resource("Bay", 4)   %>%
  add_resource("Test Staff", 1) %>%
  add_generator("Student", t, Arrival)

# Run Simulation
env %>% run(until = SimTime)

# Plot Dumbbell Chart of Arrivals (Custom Function)
ArrivalsPlot(env)

# Store Results
resources = get_mon_resources(env)
arrivals = get_mon_arrivals(env, per_resource = TRUE)
arrivals2 = get_mon_arrivals(env)
subset(arrivals, name == "Student0")

# Plot Utilisation Chart
plot(resources, metric = "utilization", c("Registrar", "Test Staff", "Bay") )

# Plot Flow Time
plot(arrivals2, metric="flow_time")

# Discuss Flow Time Results
hist(arrivals2$activity_time)
head(arrivals2)

# Task 5 Exercise: 
#        Plot the Waiting Time of Arrivals 
#        Use metric = "waiting_time"
plot(arrivals2, metric = "waiting_time")
# *********************************************************
# Log Activity Names - "VisitStep" Function
# *********************************************************

# Reset Simulation 
reset(env)

# Student Trajectory - Alternative Approach (Easier)
# "VisitStep" is a user defined function 
# VisitStep(Activity_Name, Duration, Resource, Res_Qty)
# VisitStep(Activity_Name, Duration)

t1 = join(
  VisitStep("Enter", 0),           # Always use "Enter" step
  VisitStep("Register", RegTime, "Registrar", 1),
  VisitStep("Pick TestKit", 0.5, "Test Staff", 1),
  trajectory () %>% seize("Bay", 1),
  VisitStep("Take Swab", SwabTime),
  VisitStep("Return Swab", 0.5, "Test Staff", 1)
)

t2 = join(
  VisitStep("Clean Bay", CleanTime),
  trajectory () %>% release("Bay", 1)
)

t3 = VisitStep("Perform Test", TestTime, "Test Staff", 1)


# Join Trajectories
t = trajectory() %>%
  
  # path before the split
  join(t1) %>%
  
  # split into parallel paths t2, t3
  clone(n = 2, t2, t3) %>%
  
  # join parallel paths 
  synchronize(wait = TRUE) %>%
 
  join(VisitStep("Exit", 0))         # Always use "Exit" step

# ** Use "mon = 2" in add_generator function to log Activity Names **

# Create Environment, Add Resources, Generator (Arrivals)
env <- simmer() %>%
  add_resource("Registrar", 1)  %>%
  add_resource("Bay", 4)   %>%
  add_resource("Test Staff", 1) %>%
  add_generator("Student", t, Arrival, mon = 2) # mon = 2 to log attributes

# Run Simulation
env %>% run(until = SimTime)

# Plot Dumbbell Chart of Arrivals (Custom Function)
ArrivalsPlot(env)

# Store Results
resources = get_mon_resources(env)
arrivals = get_mon_arrivals(env, per_resource = TRUE)
arrivals2 = get_mon_arrivals(env, per_resource = FALSE)

# Plot Utilisation Chart
plot(resources, metric = "utilization", c("Registrar", "Test Staff", "Bay") )

# Plot Flow Time (Better Method Below)
plot(arrivals, metric="flow_time")


# Create Event Log With Activity Names
eventlog = dcast(data.table(get_mon_attributes(env)), 
                 name + key ~ value, value.var = "time",
                 fun.aggregate = sum)
names(eventlog) = c("ID","Activity", "Arrival","Begin","Finish")
eventlog = eventlog[order(Arrival)]
print(eventlog)

# Compare EventLog with 'arrivals' table
eventlog[ID == "Student15"]
subset(arrivals, name == "Student15")


# Simulation Summary & Histogram
SimSummary(eventlog)

# Step Plot of Arrivals & Departures
EventLogPlot(eventlog)

# Dumbbell Plot of Arrivals & Departures
# Also outputs FlowTime
EventLogPlot2(eventlog)

# Plot Flow Time 
FlowTime = EventLogPlot2(eventlog)
plot(FlowTime, type = "l", col = "blue")

# Task 6 Exercise: 
#        Plot Histogram of Flow Time 
hist(FlowTime)

# End of Guided Project


