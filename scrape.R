library('rvest')
library('RJSONIO')
library('rmongodb')
library('base64')
library('png')

## STATIC DATA

all_recepies <- "http://www.een.be/sites/een.be/modules/custom/recipe/iframes/recepten.php";
recepie_endpoint <- "http://www.een.be/programmas/dagelijkse-kost/recepten/";
host <- "http://www.een.be";
postfix_persons <- "?&personen=";
mongo <- NULL
mongo.dbname <- 'hackathon-mongo'
mongo.collectionname <- 'recipes'
recipes.list <- list()

## FUNCTIONS

# returns string w/o leading or trailing whitespace
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# returns without brackets
trim.brackets <- function(x) gsub("\\((\\s+)*\\w+(\\s+|\\w+|\\,|\\.|\\-|\\_|\\'|\\/|\\+|\\%)*\\)", "", x)

mongoConnect <- function() {
  mongo <<- mongo.create(db=mongo.dbname)
  if(mongo.is.connected(mongo) == TRUE) {
    collections <- mongo.get.database.collections(mongo, mongo.dbname)
    if (!(paste(mongo.dbname, mongo.collectionname, sep=".") %in% collections )) {
      stop("no recipes collection in mongodb")
    } else {
      return(TRUE)
    }
  } else {
    stop("no mongo connection established")
  }
  return(FALSE)
}

mongoDisconnect <- function() {
  if(mongo.is.connected(mongo) == TRUE) {
    mongo.destroy(mongo)
    return(TRUE)
  }
  return(FALSE)
}

mongoInsertRecipes <- function(recipes) {
  if(mongo.is.connected(mongo) == FALSE) {
    mongoConnect()
  }
  icoll <- paste(mongo.dbname, mongo.collectionname, sep=".")
  mongo.insert.batch(mongo, icoll, recipes)
}

getFullUrl <- function(link) {
  return(paste0(host, link))
}

getMeasurements <- function() {
  measurements <- c(
    'g',
    'dg',
    'cg',
    'mg',
    'kg',
    'l',
    'dl',
    'cl',
    'ml',
    'centimeter',
    'bakje',
    'blaadje',
    'builtjes',
    'beetje',
    'blaadjes',
    'blik',
    'blikken',
    'bussel',
    'bussels',
    'busseltjes',
    'bolletjes',
    'bolletje',
    'bol',
    'delen',
    'dikke',
    'doosje',
    'dopje',
    'druppels',
    'druppel',
    'eetlepel',
    'eetlepels',
    'extra',
    'eierdopje',
    'eierdopjes',
    'fles',
    'flesje',
    'flessen',
    'fijne',
    'grote',
    'groene',
    'gerookte',
    'gefrituurde',
    'gezouten',
    'grof',
    'hele',
    'harde',
    'halve',
    'hardgekookte',
    'halfharde',
    'kleine',
    'klein',
    'koffielepel',
    'koffielepels',
    'kopje',
    'kopjes',
    'klont',
    'klontje',
    'klontjes',
    'krop',
    'kroppen',
    'kwartjes',
    'loskokende',
    'lapjes',
    'lepels',
    'lepeltje',
    'middelgrote',
    'neutrale',
    'ongesneden',
    '(ontpitte)',
    'pakje',
    'potjes',
    'plakjes',
    'plakken',
    'pikante',
    'rode',
    'roze',
    'ronde',
    'repen',
    'rijpe',
    'stukje',
    'sneetje',
    'sneetjes',
    'stengel',
    'stengels',
    'stronkje',
    'stronkjes',
    'soeplepel',
    'soeplepels',
    'schijfjes',
    'snuif',
    'snuifje',
    'scheut',
    'scheutje',
    'scheutjes',
    'stam',
    'stammen',
    'stevige',
    'scheutje',
    'sprieten',
    'snuifje',
    'suifje',
    'snuif',
    'stukken',
    'teen',
    'tenen',
    'teentje',
    'teentjes',
    'theelepel',
    'toefje',
    'takje',
    'takjes',
    'takken',
    'tros',
    'trojse',
    'van',
    'vel',
    'vellen',
    'verse',
    'verkruimelde',
    'witte',
    'zachte',
    'zoete',
    'zoute',
    'zwarte',
    'zure',
    'zakje',
    'zongedroogde'
  )
  return(measurements)
}

# ingredients of each recepie
getIngredients <- function(html) {
  nodes.ingredients <- html_nodes(html, ".recipe-ingredients .item-list ul li")
  ingredients <- html_text(nodes.ingredients)
  return(ingredients)
}

getImage <- function(html, name) {
  node.img <- html_nodes(html, ".recipe-description img")
  link <- html_attr(node.img, "src")
  # z <- tempfile()
  # download.file(link, z, mode = "wb")
  # base64.uri <- img(z, Rd = FALSE, alt = name)
  # file.remove(z)
  
  return(link)
}

getSummary <- function(html) {
  nodes.summary <- html_node(html, ".recipe-quick-summary")
  return(html_text(nodes.summary))
}

getDescription <- function(html) {
  nodes.description <- html_nodes(html, ".recipe-description p")
  description.texts <- trim(html_text(nodes.description))
  description <- paste(description.texts[lapply(description.texts, nchar) > 30], collapse=' ')
  return(description)
}

getExtra <- function(html) {
  nodes.description.list <- html_nodes(html, ".recipe-description ul li")
  extra <- trim(html_text(nodes.description.list))
  return(extra)
}

analyseIngredientLine <- function(ingredient.line) {
  ingredient.splitted <- strsplit(trim(trim.brackets(ingredient.line)), "\\s+")[[1]]
  measurements <- getMeasurements()
  mylist <- list()
  my.unknown.list <- list()
  
  ingredients.measure.locations <- ingredient.splitted %in% measurements
  
  if(sum(ingredients.measure.locations) > 0) {
    max.pos <- max(which(ingredients.measure.locations))
    min.pos <- min(which(ingredients.measure.locations))
    if (max.pos < length(ingredients.measure.locations)) {
      not.measurements.location <- which(!(ingredients.measure.locations))
      after.measurements <- ingredient.splitted[not.measurements.location[not.measurements.location > max.pos]]
      before.measurements <- ingredient.splitted[not.measurements.location[not.measurements.location < min.pos]]
      real.ingredient <- paste(after.measurements, collapse = ' ')
      measure <- ingredient.splitted[max.pos]
      quantity <- paste(before.measurements, collapse = ' ')
      mylist <- c(mylist, list(quantity = as.character(quantity), measure = as.character(measure), ingredient = as.character(real.ingredient)))
    }
  } else {
    # unknown ingredient line (no measurement in line)
    if (length(ingredient.splitted) == 1) {
      mylist <- c(mylist, list(quantity = NULL, measure = NULL, ingredient = as.character(ingredient.splitted[1])))
    }
    else if (length(ingredient.splitted) == 2) {
      mylist <- c(mylist, list(quantity = as.character(ingredient.splitted[1]), measure = NULL, ingredient = as.character(ingredient.splitted[2])))
    } else if (!is.na(as.numeric(ingredient.splitted[1]))) {
      rest <- paste(ingredient.splitted[-1], collapse = ' ')
      mylist <- c(mylist, list(quantity = as.character(ingredient.splitted[1]), measure = NULL, ingredient = rest ))
    } else {
      my.unknown.list <- c(my.unknown.list, as.character(paste(ingredient.splitted, collapse = ' ')))
    }
  }
  
  return(list(structured = mylist, unstructured = my.unknown.list))
}

analyseRecipeFull <- function(link, name) {
  url <- getFullUrl(link)
  html <- read_html(url)
  
  ingredients <- getIngredients(html)
  ingredients.data <- lapply(ingredients, analyseIngredientLine)
  img.data <- getImage(html, name)
  summary.data <- getSummary(html)
  description.data <- getDescription(html)
  extra.data <- getExtra(html)
  return(list(name=name, url=getFullUrl(link), image=img.data, summary=summary.data, description=description.data, extra=extra.data, ingredients=ingredients.data))
}

## RUN

main <- function() {
  all_recepies_html <- read_html(all_recepies)
  nodes_all_recepies <- html_nodes(all_recepies_html, ".recipe a")
  texts <- html_text(nodes_all_recepies)
  links <- sapply(html_attrs(nodes_all_recepies), unname)
  
  for (i in 1:length(links)) {
    recipe <- analyseRecipeFull(links[i], texts[i])
    recipes.list <- c(recipes.list, recipe)
    mongoInsertRecipes(list(mongo.bson.from.list(recipe)))
    print(paste(as.character(i), texts[i], "done."))
  }
  # recipe <- analyseRecipe(links[1], texts[1])
  # mongoInsertRecipes(recipes.list)
  mongoDisconnect()
}






