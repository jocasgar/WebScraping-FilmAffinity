library(xml2)
library(rvest)
library(stringr)
library(dplyr)

# Colocamos la id del usuario y el número de páginas disponibles en tus votaciones
id = "7848930"
pages = 9

# Generamos el data frame donde guardaremos todos los datos
#df = as.data.frame(matrix(data=NA,ncol=5,nrow=pages*50))
df <- NULL

#Generamos la url del usuario
url = paste("https://www.filmaffinity.com/en/userratings.php?user_id=",id,sep="")

for(i in 1:pages) {
  url2 = paste(url,"&p=",i,"&orderby=4",sep="")
  rawinfo = read_html(url2)
  # Read titles, directors, ratings
  titles = html_nodes(rawinfo, ".mc-title")
  directors = html_nodes(rawinfo, ".mc-director")
  ratings = html_nodes(rawinfo, ".ur-mr-rat-img")
  castings = html_nodes(rawinfo, ".mc-cast")
  notasFA = html_nodes(rawinfo, ".avgrat-box")
  notasFA = html_text(notasFA)
  numeroVotos = html_nodes(rawinfo,".ratcount-box")
  numeroVotos = html_text(numeroVotos)
  
  for(j in 1:length(directors)) {
    
    # Guardamos el título de la película
    title = xml_attrs(xml_child(titles[[j]], 1))[["title"]]
    title = sub(",","",title)
    
    # Guardamos el director
    director = tryCatch({
      xml_attrs(xml_child(xml_child(xml_child(directors[[j]], 1), 1), 1))[["title"]]},
      error = function(e){message(paste("Error en el director, posible documental en "),j,
                                  sep=" ")
        director = "NULL"})
    
    # País de la película
    pais = xml_attrs(xml_child(titles[[j]],2))[["title"]]
    
    # Año de la película
    htmlPelicula <- read_html(xml_attrs(xml_child(titles[[j]],1))[["href"]])
    contents <- xml_contents(htmlPelicula)
    stryear <- strsplit(xml_attrs(xml_child(contents[[1]],9))[["content"]]," ")
    stryear <- as.vector(stryear[[1]])
    year <- stryear[length(stryear)]
    year <- str_sub(year,2,5)
    
    # Género de la película
    str <- strsplit(xml_attrs(xml_child(contents[[1]], 10))[["content"]], " ")
    str <- as.vector(str[[1]])
    if(length(which(grepl("Genre:",str))) > 0){
      genero <- str[grep("Genre:",str)+1]
    } else { 
      genero <- "Ningún género encontrado"
    }
    
    # Actores de la película
    casting <- tryCatch({
      do.call(rbind,lapply(1:xml_length(xml_child(castings[[j]],1)), function(y){
            xml_attrs(xml_child(xml_child(xml_child(castings[[j]], 1), y), 1))[["title"]]
      }))}, error = function(e){message("No se ha encontrado actores")
      casting = "No Actores"})
    
    
    
    # User Rating
    rating = sub("/imgs/myratings/","",xml_attrs(xml_child(ratings[[j]], 1))[["src"]])
    rating = sub(".png","",rating)
    rating = sub("_","",rating)
    rating = as.integer(rating)
    
    # FA rating
    notaFA = notasFA[j]
    
    # Número de Votos 
    nvotos = numeroVotos[j]
    
    if(length(casting) > 1){
      df2 <- do.call(rbind,lapply(1:nrow(casting),function(x){
        c(title,director,pais,year,genero,casting[x],rating,notaFA,nvotos)
      }))
      #pelicula <- c(title,director,rating,pais)
      df <- rbind(df,df2)
      #df[(i-1)*30+j,] = c(title,director,rating,pais)
    } else df <- rbind(df,c(title,director,pais,year,genero,casting,rating,notaFA,nvotos))
  }
}

# Change name of the columns and clean the data frame
colnames(df) = c("Title","Directors","País","Año","Género","Actores","UserRating"
                 ,"FArating","NúmeroVotos")
df = as.data.frame(df)

#Save into .csv file
write.table(df,"PeliculasFA.csv",sep = ";",dec = ".",row.names = FALSE)


