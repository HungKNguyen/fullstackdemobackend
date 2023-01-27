#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

cat("Loading Libraries ")
library(plumber)
library(tidyverse)
library(jsonlite)
library(plm)
library(httpuv)
cat("- Done\n")

cat("Loading data block ")
geodata <- readRDS("geodata.rds")
data_block <- readRDS("data_block.rds")
cat("- Done\n")

level_options <- list(
  level = c("World", "Region", "Income Group", "Country"),
  code = c("world", "region", "incomegroup", "country")
)

country_options <- unique(select(data_block, country, iso3)) %>% arrange(country)

region_options <- unique(select(data_block, region))

incomegroup_options <- unique(select(data_block, incomegroup)) %>% filter(!is.na(incomegroup))

indicator_options <- list(
  indicator = c("GDP per capita, PPP", " Natural Log GDP per capial, PPP",
              "GNI Per Capita, PPP",
              "Human Development Index", "Life expectancy",
              "Expected Years of Schooling", "Mean Years of Schooling",
              "Coefficient of human inequality",
              "Inequality in life expectancy", "Inequality in education",
              "Inequality in income",
              "Carbon dioxide emissions per capita (production) (tonnes)"),
  code = c("gdp_pcap_ppp", "ln_gdp_pcap_ppp", "gnipc", "hdi", "le",
            "eys", "mys", "coef_ineq", "ineq_le", "ineq_edu",
            "ineq_inc", "co2_prod")
)

model_options <- data.frame(
  model = c("Pooled OLS", "Fixed Effect", "Random Effect"),
  code = c("pooling", "within", "random")
)


get_regression <- function(model, dependent, independents) {
  f <- as.formula(paste(dependent, "~", paste(independents, collapse="+")))
  m <- plm(f, data_block, model = model, index = c("iso3"))
  return(paste(capture.output(summary(m)), collapse="\n"))
}

get_data <- function(level, include_members_or_groups, include_years,
                     year_range, include_indicators, metadata,
                     aggregate_funs) {
  # Init data
  data <- data_block

  # Filter by members or groups
  countries = c()
  regions = c()
  inc_groups = c()
  if (!is.null(include_members_or_groups)) {
    for (mem in include_members_or_groups) {
      if (mem %in% data_block$region) {
        regions <- append(regions, mem)
      } else if (mem %in% data_block$incomegroup) {
        inc_groups <- append(inc_groups, mem)
      } else if (mem %in% data_block$iso3) {
        countries <- append(countries, mem)
      }
    }
    data <- data %>%
      filter(iso3 %in% countries | region %in% regions | incomegroup %in% inc_groups)
  }

  # Filter by year range or years points
  if(!is.null(year_range)) {
    data <- data %>%
      filter(between(year, year_range[1], year_range[2]))
  } else if (!is.null(include_years)) {
    data <- data %>%
      filter(year %in% include_years)
  }

  # Select relevant variables
  variables <- c("iso3", "year", "incomegroup", "country", "region")
  variables <- append(variables, include_indicators)
  data <- data %>%
    select(variables)

  # Aggregation if needed
  funcs = list()
  if (level %in% level_options$code & level != "country") {
    for (func_name in aggregate_funs) {
      #funcs <- append(funcs, list({{func_name}} = eval(parse(text = func_name))))
      funcs[[func_name]] = eval(parse(text = func_name))
    }
  }

  #mean instead of funcs due to simple use case
  if (level == "region") {
    data <- data %>%
      group_by(region, year) %>%
      summarise_at(include_indicators, mean, na.rm = TRUE)
    # if (length(include_indicators) == 1) {
    #   var = include_indicators[1]
    #   data <- data %>% rename(var = mean)
    # }
  } else if (level == "incomegroup") {
    data <- data %>%
      group_by(incomegroup, year) %>%
      summarise_at(include_indicators, mean, na.rm = TRUE)
  }

  # Check if remove metadata
  if (level == "country" & !metadata) {
    data <- data %>%
      select(-c("incomegroup", "region"))
  }

  # Return data
  return(data)
}

#* @apiTitle Full stack Demo API
#* @apiDescription API for data visualization project

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
  toJSON(list(msg = paste0("The message is: '", msg, "'")))
}


#* Return the sum of two numbers
#* @post /sum
function(req) {
  return(as.numeric(req$body$a) + as.numeric(req$body$b))
}

#* Return all countries in the dataset
#* @get /countries
#* @serializer json list(dataframe = "columns")
function() {
  country_options
}

#* Return all regions in the dataset
#* @get /regions
#* @serializer json list(dataframe = "columns")
function() {
  region_options
}

#* Return all income groups in the dataset
#* @get /incomegroups
#* @serializer json list(dataframe = "columns")
function() {
  incomegroup_options
}

#* Return all indicators in the dataset
#* @get /indicators
#* @serializer json list(dataframe = "columns")
function() {
  indicator_options
}

#* Return all indicators in the dataset
#* @get /model
#* @serializer json list(dataframe = "columns", auto_unbox = TRUE)
function() {
  model_options
}

#* Return geometry data in the dataset
#* @get /map_data
function(res) {
  res$setHeader("Content-Type", "application/json")
  res$body <- geodata
  res
  #FROM_GeoJson(sf_geojson(geodata))
}

#* Data query
#* @post /data
#* @serializer json list(dataframe = "columns")
function(req) {
  params <- list(
    level = "country",
    include_members_or_groups = NULL,
    include_years = NULL,
    year_range = NULL,
    include_indicators = c("gdp_pcap_ppp"),
    metadata = FALSE,
    aggregate_funs = c("mean")
  )
  if ("level" %in% names(req$body)) {
    params$level = req$body$level
  }
  if ("include_members_or_groups" %in% names(req$body)) {
    params$include_members_or_groups = req$body$include_members_or_groups
  }
  if ("include_years" %in% names(req$body)) {
    params$include_years = req$body$include_years
  }
  if ("year_range" %in% names(req$body)) {
    params$year_range = req$body$year_range
  }
  if ("include_indicators" %in% names(req$body)) {
    params$include_indicators = req$body$include_indicators
  }
  if ("metadata" %in% names(req$body)) {
    params$metadata = req$body$metadata
  }
  if ("aggregate_funs" %in% names(req$body)) {
    params$aggregate_funs = req$body$aggregate_funs
  }
  get_data(params$level, params$include_members_or_groups,
           params$include_years, params$year_range,
           params$include_indicators, params$metadata,
           params$aggregate_funs)
}

#* Regression query
#* @post /regression
#* @serializer json list(dataframe = "columns")
function(req) {
  params <- list(
    model = NULL,
    dependent = NULL,
    independents = c()
  )
  if ("model" %in% names(req$body)) {
    params$model = req$body$model
  } else {
    return(
      list(
        body = "Error: Missing model specification"
      )
    )
  }
  if ("dependent" %in% names(req$body)) {
    params$dependent = req$body$dependent
  } else {
    return(
      list(
        body = "Error: Missing dependent variabel"
      )
    )
  }
  if ("independents" %in% names(req$body)) {
    params$independents = req$body$independents
  }
  output <- get_regression(params$model, params$dependent, params$independents)
}

#* @filter cors
cors <- function(req, res) {
  # safe_domains <- c("http://localhost:3000/",
  #                   "https://hungknguyen.github.io/fullstackdemo/")
  # if (any(grepl(pattern = paste0(safe_domains,collapse="|"), req$HTTP_REFERER,ignore.case=T))) {
  if(TRUE) {
    res$setHeader("Access-Control-Allow-Origin", sub("/$","",req$HTTP_REFERER)) #Have to remove last slash, for some reason

    if (req$REQUEST_METHOD == "OPTIONS") {
      res$setHeader("Access-Control-Allow-Methods","GET,HEAD,PUT,PATCH,POST,DELETE") #This is how node.js does it
      res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
      res$status <- 200
      return(list())
    } else {
      plumber::forward()
    }
  } else {
    plumber::forward()
  }
}

#* @get /
#* @serializer json list(dataframe = "columns")
function(){
  list(a=2)
}








