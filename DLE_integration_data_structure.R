#' Class DLE.dimension
#'
#' @description class representing each dimension of DLE
#'
#' @details Each dimension needs to inherit DLE.dimension to have dimension-specific methods or fields.

DLE.dimension <- setRefClass("DLE.dimension", 
                             
            fields = list(
              # DLS indicator of this dimension
              indicator = "character",   # e.g. pkm/yr or kcal/day
              
              # Threshold values defining DLS
              thres     = "data.frame",  # by [grp, region] : 'grp' - dimension-specific sub-group (e.g. urban/rural or adult/child)
              
              # Identified gap below the threshold. It can be population-average gap by group
              gap       = "data.frame",  # by [grp, region] 
              
              # How we fill the gap over years
              rollout   = "data.frame",  # by [grp, region, year] 
            
              # Indirect final energy intensity by unit of indigrpor (MJ/kcal)
              tei.OP    = "data.frame",  # by [grp, region] : long format (may have elec/non-elec breakdown)
              tei.CON   = "data.frame",
              
              # Derived DLE pathway of corresponding scenario
              DLE.tot="numeric"
              ),
            
            # Many of these methods need to be customized (through inheritance) to individual dimensions, while some already defined below will be standard.
            methods = list(
              
              #' @description
              #' Derive DLS thresholds from data if not pre-determined
              DeriveThreshold = function() {
                print("DeriveThreshold: Base")
              },
              
              #' @description
              #' Identify DLS gap if not given
              IdentifyGap = function() {
                print("IdentifyGap: Base")
              },
              
              #' @description
              #' Derive (or import) final energy intensities from data if not pre-determined
              DeriveEnergyIntensity = function() {
              },
              
              #' @description
              #' Map the results to MESSAGE regions
              AggregateRegion = function() {
              },
              
              #' @description
              #' Construct rollout scenario based on identified gap and other assumptions.
              #' This can be coded differently for different scenarios.
              ConstructRolloutScenario = function(scen) {
                
                if (scen=="BAU") {
                  print("ConstructRolloutScenario: BAU")
                }
                else if (scen=="ACCEL") {
                  print("ConstructRolloutScenario: ACCEL")
                }
                else if (scen=="ACCEL.LCT") {
                  print("ConstructRolloutScenario: ACCEL.LCT")
                }
                else {  # A dummy case (linearly filling the gap)
                  yr.now <- as.integer(format(Sys.Date(), "%Y"))
                  yr.target <- 2030  # This can be specified in each scenario too.
                  n.yr <- yr.target - yr.now
                  
                  inc <- gap$val/n.yr #seq(0, gap, gap/n.yr)
                  
                  rollout <<- apply(inc, 1, function(x) {    } )
                }
              },
              
              #' @description
              #' Update certain parameters
              UpdateParameter = function(par, val) {
                .self$par <<- val   # placeholder
              },
              
              #' @description
              #' (Re)Calculate energy values
              UpdateDLE = function() {
                DLE.tot <<- (tei.OP+tei.CON) * rollout  # placeholder
              }
              
              )
            )


#' Class DLE.dimension.health
#'
#' @description subclass for health dimension (inheriting DLE.dimension)

DLE.dimension.health <- setRefClass("DLE.dimension.health", 
                                    
            contains = "DLE.dimension", # inherit the DLE.dimension class
              
            methods = list(
              
              #' @description
              #' Overwrite the same-named function in the superclass to be specific to this dimension
              IdentifyGap = function() {
                print("IdentifyGap: Health")
                
                gap <<- thres %>% mutate(val = 0.05*val)
              }
            )
)
                             

#' Class DLE.scenario
#'
#' @description class representing one DLE scenario 
#'
#' @details This class includes all DLS dimensions.

                                    
DLE.scenario <- setRefClass("DLE.scenario", 
            fields=list(
              
              #' @field scenario Name of the scenario (e.g. BAU, ACCEL, LCT, HIGH, etc.)
              scenario = "character",
              
              #' @field dims List of all DLS dimensions
              dims = "list",
              # list(
              #   Housing    ="DLE.dimension", # Can be long or wide 
              #   Clothing   ="DLE.dimension",
              #   Food       ="DLE.dimension",
              #   Health     ="DLE.dimension",
              #   Education  ="DLE.dimension",
              #   Water      ="DLE.dimension",
              #   Sanitation ="DLE.dimension",
              #   Cooking    ="DLE.dimension",
              #   TV         ="DLE.dimension",
              #   Fridge     ="DLE.dimension",
              #   AC         ="DLE.dimension",
              #   Cellphone  ="DLE.dimension",
              #   Road       ="DLE.dimension",
              #   Mobility   ="DLE.dimension"  # Will be car/bus/train/etc later
              # ),
              
              # Aggregate sectors (OP/CON)
              Shelter      = "data.frame",
              Mobility.all = "data.frame",
              Appliance    = "data.frame",
              Food         = "data.frame"
            ),
            
            methods = list(
              
              #' @description
              #' Set up rollout trajectory for a given scenario
              SetupRollout = function() {
                # The scenario-specific rollout can be taken care of in each dimension.
                lapply(dims, function(x) {x$ConstructRolloutScenario(scenario)})
              },
              
              #' @description
              #' Aggregate some dimensions in case there's a need.
              AggregateDimensions = function() {
                Shelter <<- Housing$DLE.tot + Clothing$DLE.tot
                Appliance <<- Cooking$DLE.tot + TV$DLE.tot + Fridge$DLE.tot + AC$DLE.tot
              },
              
              #' @description
              #' Summarize all the dimensions' DLE
              SumDLE = function() {
                DLE <- lapply(dims, function(x) {x$UpdateDLE()})
                sum(DLE)
              },
              
              #' @description
              #' Perform a sensitivity test by changing a variable in a certain dimension
              #' @param dim DLE dimensions
              #' @param var input variable to test
              #' @param val changes to the variable
              SensitivityTest = function(dim, var, val) {
              },
              
              #' @description
              #' Plot whatever
              PlotDLE = function() {
              }
            )
)
              
            
