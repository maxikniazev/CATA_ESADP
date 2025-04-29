library(ade4)
library(FactoMineR)
library(factoextra)
library(readxl)
library(plotly)
library(grid)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(shiny)
library(DescTools)
library(rlang)
library(rstatix)
library(kableExtra)
library(RColorBrewer)

library(pairwise)
library(rcompanion)
library(multcompView)

# Interfaz de usuario
ui <- fluidPage(
    # Application title
    titlePanel(
        tags$div(
            tags$img(src = "Logosens.png", height = "70px"),  # Cambia el tamaño si es necesario
            "Check-All-That-Applies (CATA)"
        )
    ),
    
    sidebarLayout(
        sidebarPanel(
            # Campo de contraseña
            passwordInput("password", "Ingrese la contraseña:"),
            
            # Mostrar el resto del contenido solo si la contraseña es correcta
            conditionalPanel(
                condition = "input.password == 'ESADP.UdelaR'",  # Condición para que aparezca el contenido
                
                # Mensaje para el usuario
                helpText("Sube un archivo Excel (.xlsx) que contenga una hoja 'CATA' con los datos del ensayo Check-All-That-Applies según el Tutorial"),
                
                # Botón para ver o descargar el tutorial en PDF
                actionButton("tutorial_button", "Tutorial"),
                br(),  # Espacio entre el botón y el input de archivo
                
                # Campo para subir el archivo (solo .xlsx)
                fileInput("datafile", "Sube tu archivo de datos (Solo Excel)", 
                          accept = c(".xlsx")),
                # Cuadro de texto para cos2max_value
                numericInput("cos2min_value", "Valor de cos2min", value = 0.67, min = 0, max = 1),
                
                # Define the checkbox input for intensity control
                checkboxInput("intensidad", "Desactivar Alpha", value = FALSE)
                
            )
        ),
        
        mainPanel(
            #Cambiar esto
            textOutput("Bienvenida"),
            tableOutput("pruebaQ"),
            textOutput("contingenciaTexto"),
            tableOutput("contingenciaTabla"),
            verbatimTextOutput("resultadoChi"),
            textOutput("textoChi"),
            plotOutput("screePlot"),
            textOutput("explicacionCos2"),
            tableOutput("cos2FilasTable"),
            tableOutput("cos2ColumnasTable"),
            plotOutput("principalPlot"),
            plotOutput("penaltyLifts")
            
        )
    )
)

# Lógica del servidor
server <- function(input, output) {
    
    observeEvent(input$tutorial_button, {
        # Enlace a un archivo PDF almacenado en la carpeta www
        showModal(modalDialog(
            title = "Tutorial de CATA",
            tags$a(href = "tutorialCATA.pdf", target = "_blank", "Descargar Tutorial"),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    datos_CATA <- reactive({
        req(input$datafile)  # Asegura que el archivo ha sido subido
        
        # Cargar el archivo .xlsx
        ext <- tools::file_ext(input$datafile$name)
        if (ext == "xlsx") {
            # Cargar las hojas del archivo
            datos_CATA <- read_excel(input$datafile$datapath, sheet = "CATA")
            
            return(datos_CATA)
        } else {
            return(NULL)
        }
    })
    
    #Colores
    colores_paleta <- brewer.pal(n = 8, name = "Paired")
    myblue <- colores_paleta[2]
    myred <- colores_paleta[6]
    mypink <- colores_paleta[5]
    
    output$Bienvenida <- renderText({
        req(input$datafile)  # Asegurarse de que el archivo ha sido cargado
        datos <- read_excel(input$datafile$datapath, sheet = "CATA", col_names = FALSE)  # Leer el archivo correctamente
        
        # Verificar si la primera celda es "ATRIBUTO"
        if (datos[1, 1] == "ParticipantCode") {
            return("Datos correctamente ingresados, se muestra a continuación el resultado de la prueba Q de Cochran para cada atributo.")
        } else {
            return("Hubo un error en el formato de datos, revisar el TUTORIAL.")
        }
    })
    
    output$pruebaQ <- renderTable({
        req(datos_CATA())
        
        df <- datos_CATA()
        
        # Preparar un dataframe para almacenar los resultados
        results <- data.frame(
            Atributo = colnames(df[, 4:ncol(df)]),
            p_valor = rep(NA, ncol(df) - 3)
        )
        
        # Realizar la prueba Q de Cochran para cada atributo
        for (i in seq_along(results$Atributo)) {
            # Extrayendo los datos relevantes
            mydata <- data.frame(
                outcome = df[, i + 3],
                treatment = df$Sample.Name,
                participant = df$ParticipantCode
            )
            colnames(mydata) <- c("outcome", "treatment", "participant")
            
            # Aplicando la prueba Q de Cochran
            test_result <- cochran_qtest(mydata, outcome ~ treatment | participant)
            
            # Almacenando los p-valores en el dataframe de resultados
            results$p_valor[i] <- format(round(test_result$p, 3), nsmall = 3)
        } 
        
        #Agregar % de seleccion para cada muestra en cada atributo al df results
        cantidad_participantes <- length(unique(df$ParticipantCode))
        
        df_grouped <- df %>%
            select(-c(ParticipantCode, Aceptabilidad)) %>%
            group_by(Sample.Name) %>%
            summarize(across(everything(), ~ round(sum(.) / cantidad_participantes, 3))) %>%
            t() %>%
            as.data.frame()
        
        # Asignar nombres de columnas y eliminar la primera fila
        colnames(df_grouped) <- df_grouped[1, ]
        df_grouped <- df_grouped[-1, ]  # Eliminar la primera fila
        
        # Crear columna 'Atributo' con los nombres de fila
        df_grouped$Atributo <- rownames(df_grouped)
        
        # Realizar el left join con 'results'
        results_joined <- results %>%
            left_join(df_grouped, by = "Atributo")
        orden_muestras <- results_joined %>% select(-c(1:2)) %>% colnames()
        #=============================
        ###Pairwise de McNemar:
        library(pairwise)
        library(rcompanion)
        library(multcompView)
        
        for (k in 1:nrow(results_joined)){
            #extraer un atributo del df
            mydata <- data.frame( #Una iteración del for, para i:=1
                outcome = df[, k+3], #arranca en el 4, es el primer atributo
                treatment = df[, 2],
                participant = df[, 1]
            )
            #Hacemos pairwise McNemar 
            colnames(mydata) <- c("outcome", "treatment", "participant")
            pw <- pairwise_mcnemar_test(mydata, outcome ~ treatment | participant)
            
            #Agarremos el atributo k de results_joined
            proportions_att <- results_joined[k,-c(1,2)] #quedarse con la primera fila (primer atributo)
            proportions_att <- rbind(proportions_att,colnames(proportions_att))
            rownames(proportions_att) <- c("propor","Muestra")
            proportions_att <- proportions_att %>% t() %>% 
                as.data.frame() %>%
                arrange(desc(propor))#Formatear como columna el primer atributo
            
            #=====generemos los grupos
            
            # Crear columnas para almacenar los números de las muestras (i y j)
            proportions_att$muestra_inicial <- NA
            proportions_att$muestra_final <- NA
            
            # Ciclo para recorrer cada muestra, incluyendo la última muestra
            for (i in 1:nrow(proportions_att)) {
                
                # Asignar el número de muestra inicial (el índice i)
                proportions_att$muestra_inicial[i] <- i
                
                # Si es la última muestra
                if (i == nrow(proportions_att)) {
                    # Asignar 5 si no tiene más muestras con las cuales compararse
                    proportions_att$muestra_final[i] <- nrow(proportions_att) + 1
                    next
                }
                
                # Comparar la muestra i con todas las muestras siguientes
                for (j in (i + 1):nrow(proportions_att)) {
                    
                    # Obtener el valor de p.adj desde el data frame pw
                    p_adj_value <- pw %>%
                        filter((group1 == proportions_att$Muestra[i] & group2 == proportions_att$Muestra[j]) | 
                                   (group1 == proportions_att$Muestra[j] & group2 == proportions_att$Muestra[i])) %>%
                        select(p.adj) %>%
                        pull()
                    
                    # Si se encuentra una diferencia significativa (p < 0.05)
                    if (p_adj_value < 0.05) {
                        # Guardar el índice j donde se encontró la diferencia significativa
                        proportions_att$muestra_final[i] <- j
                        break  # Salimos del ciclo interno, ya que encontramos una diferencia significativa
                    }
                }
                
                # Si no se encontró ninguna diferencia significativa, asignar muestra_final como 5
                if (is.na(proportions_att$muestra_final[i])) {
                    proportions_att$muestra_final[i] <- nrow(proportions_att) + 1  # Uno más que el total
                }
            }
            
            
            # Inicializar la primera letra
            letra_inicial <- "a"
            
            # Ciclo para recorrer cada muestra y asignar letras
            for (i in 1:nrow(proportions_att)) {
                
                # Si no es la primera fila, verificar si debemos cambiar la letra
                if (i > 1 && proportions_att$muestra_final[i - 1] < proportions_att$muestra_final[i]) {
                    # Cambiar la letra si muestra_final de la fila actual es mayor que la anterior
                    letra_inicial <- letters[which(letters == letra_inicial) + 1]
                }
                
                # Asignar el grupo actual entre muestra_inicial y muestra_final
                for (j in proportions_att$muestra_inicial[i]:(proportions_att$muestra_final[i] - 1)) {
                    proportions_att[j, i + 4] <- letra_inicial  # Asignar la letra en la columna correspondiente
                }
            }
            
            
            
            # Crear una nueva columna para almacenar la concatenación de propor con las letras
            proportions_att$concatenado <- NA
            
            # Ciclo para recorrer cada fila
            for (i in 1:nrow(proportions_att)) {
                
                # Iniciar la concatenación con el valor de propor
                concatenado <- as.character(proportions_att$propor[i])
                
                # Inicializar un vector para acumular las letras únicas
                letras_acumuladas <- ""
                
                # Ciclo para recorrer las columnas de letras (V5 en adelante)
                for (j in (5):ncol(proportions_att)) {
                    
                    # Verificar que no haya NA en la columna y si la columna tiene un valor
                    if (!is.na(proportions_att[i, j]) && proportions_att[i, j] != "") {
                        # Solo añadir la letra si no es igual a la última letra añadida
                        if (!grepl(proportions_att[i, j], letras_acumuladas)) {
                            letras_acumuladas <- paste0(letras_acumuladas, proportions_att[i, j])
                        }
                    }
                }
                
                # Si se han acumulado letras, añadirlas entre paréntesis a la concatenación
                if (letras_acumuladas != "") {
                    concatenado <- paste0(concatenado, "(", letras_acumuladas, ")")
                }
                
                # Asignar el valor concatenado a la nueva columna
                proportions_att$concatenado[i] <- concatenado
            }
            
            
            # Ordenar el data frame 'proportions_att' según el vector 'orden_muestras'
            fila_en_Q <- proportions_att %>%
                slice(match(orden_muestras, Muestra)) %>%  # Ordenar utilizando 'orden_muestras'
                select(Muestra, concatenado) %>%  # Seleccionar las columnas 'Muestra' y 'concatenado'
                t() %>%  # Transponer el data frame
                as.data.frame()  # Convertir de nuevo a data frame
            
            results_joined[k,3:ncol(results_joined)] <- fila_en_Q[2,] #sumar fila k (itera el 1)
            
        }
        
        return(results_joined)
        
        
        
        
        
    }, rownames = FALSE,
    digits = 3)
    
    
    output$contingenciaTexto <- renderText({
        req(contingency_table())
        "A continuación se muestra la tabla de contingencia y el resultado del test de Chi Cuadrado"
    })
    
    #Mostrar tabla de contingencia
    contingency_table <- reactive({
        req(datos_CATA())
        contingency_table <- datos_CATA() %>% 
            select(c(2, 4:ncol(datos_CATA()))) %>%  # Seleccionar la columna 2 (Sample.Name) y las demás columnas numéricas
            group_by(Sample.Name) %>%     # Agrupar por Sample.Name
            summarize(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
            data.frame(., row.names = "Sample.Name")
    })
    
    output$contingenciaTabla <- renderTable({
        req(contingency_table())
        return(contingency_table())
    }, digits = 0,
    rownames = TRUE)
    
    #Test Chi Sq
    # Variable reactiva para almacenar el resultado del chisq.test
    Chi_pp <- reactive({
        req(contingency_table())  # Asegurarse de que los datos están disponibles
        chisq.test(contingency_table())  # Realizar el test de chi-cuadrado
    })
    
    # Renderizar el resultado del chisq.test
    output$resultadoChi <- renderPrint({
        print(Chi_pp())  # Mostrar el resultado del test
    })
    
    # Renderizar un texto basado en el valor p del test de chi-cuadrado
    output$textoChi <- renderText({
        # Extraer el valor p del resultado del test
        if (Chi_pp()$p.value < 0.05) {
            paste0("p-valor = ", round(Chi_pp()$p.value, 4), " < 0.05. Las filas y las columnas de la tabla son independientes según el test de Chi Cuadrado.")
        } else {
            paste0("p-valor = ", round(Chi_pp()$p.value, 4), " > 0.05. No existe evidencia estadística suficiente para rechazar la hipótesis nula, por lo tanto, no se puede asegurar la independencia de filas y columnas.")
        }
    })
    
    #Analisis de correspondencia
    ACS <- reactive({
        req(contingency_table())
        CA(contingency_table(), graph = FALSE)
    })
    
    #Gráfica del porcentaje de varianza explicado
    output$screePlot <- renderPlot({
        
        if (ACS()$eig[1,2] < 80){
            limsup <- 80
        } else {
            limsup <- ACS()$eig[1,2]*1.2
        }
        
        fviz_screeplot(ACS(), addlabels = TRUE, ylim = c(0, limsup))+ggtitle("Scree plot")+
            ylab("Porcentaje de varianza explicado") + xlab("Ejes")
    })
    
    output$explicacionCos2 <- renderText({
        req(ACS())
        "A continuación se presentan las tablas de Coseno Cuadrado para filas y columnas. Las filas bien representadas estarán ploteadas más abajo. Se puede cambiar el valor de coseno cuadrado minimo por defecto (aumenta el ruido) en el panel lateral. Todas las muestras están ploteadas por defecto independientemente de su Cos2."
    })
    
    #Tablas de cos2
    output$cos2FilasTable <- renderTable({
        cosenos_fila <- ACS()$row$cos2[,1:2] %>% as.data.frame(.) %>%
            mutate(SumaCos2 = `Dim 1` + `Dim 2`) %>% 
            mutate(`Bien Representado` = SumaCos2 >= input$cos2min_value) %>%
            mutate(Muestra = rownames(.)) %>%
            select(Muestra, everything()) %>%
            arrange(desc(SumaCos2), decreasing = TRUE)
        return(cosenos_fila)
    },digits = 2)
    
    output$cos2ColumnasTable <- renderTable({
        cosenos_columna <- ACS()$col$cos2[,1:2] %>% as.data.frame(.) %>%
            mutate(SumaCos2 = `Dim 1` + `Dim 2`) %>% 
            mutate(`Bien Representado` = SumaCos2 >= input$cos2min_value) %>%
            mutate(Atributo = rownames(.)) %>%
            select(Atributo, everything()) %>%
            arrange(desc(SumaCos2), decreasing = TRUE)
        return(cosenos_columna)
    },digits = 2)
    
    output$principalPlot <- renderPlot({
        
        #Extraer pesos de las dimensiones
        Peso_dim1 <- round(ACS()$eig[1,2],2)
        Peso_dim2 <- round(ACS()$eig[2,2],2)
        
        #Cosenos cuadrados
        cosenos_fila <- ACS()$row$cos2[,1:2] %>% as.data.frame(.) %>%
            mutate(SumaCos2 = `Dim 1` + `Dim 2`) %>% 
            mutate(BienRepresentado = SumaCos2 >= input$cos2min_value) %>%
            mutate(Muestra = rownames(.))
        cosenos_columna <- ACS()$col$cos2[,1:2] %>% as.data.frame(.) %>%
            mutate(SumaCos2 = `Dim 1` + `Dim 2`) %>% 
            mutate(BienRepresentado = SumaCos2 >= input$cos2min_value) %>%
            mutate(Atributo = rownames(.))
        
        
        #Coordenadas de fila
        coordenadas.fila <- ACS()$row$coord %>%
            as.data.frame(.) %>%
            mutate(Muestra = rownames(.)) %>%
            left_join(cosenos_fila, by = "Muestra")
        coordenadas.columna <- ACS()$col$coord %>%
            as.data.frame(.) %>%
            mutate(Atributo = rownames(.)) %>%
            left_join(cosenos_columna, by = "Atributo") %>%
            filter(BienRepresentado == TRUE)
        
        
        #Maximos de ejes
        x_max <- max(abs(coordenadas.fila$`Dim 1.x`), abs(coordenadas.columna$`Dim 1.x`))
        y_max <- max(abs(coordenadas.fila$`Dim 2.x`), abs(coordenadas.columna$`Dim 2.x`))
        
        masde3muestras <- length(coordenadas.fila$Muestra) > 3
        
        # Definir el valor de alpha condicionalmente
        # Detectar si SumaCos2 es un vector constante con todos los valores iguales a 1
        alpha_fila <- if (input$intensidad || !masde3muestras) {
            NA  # Sin transparencia (plano)
        } else {
            coordenadas.fila$SumaCos2  # Aplicar alpha basado en SumaCos2
        }
        
        alpha_columna <- if (input$intensidad || !masde3muestras) {
            NA  # Sin transparencia (plano)
        } else {
            coordenadas.columna$SumaCos2  # Aplicar alpha basado en SumaCos2
        }
            
        ggplot() +
            #atributos en rojo
            geom_point(data = coordenadas.fila, aes(x = `Dim 1.x`, 
                                                    y = `Dim 2.x`,
                                                    alpha = alpha_fila),
                       color = myred,
                       show.legend = FALSE) +
            geom_text_repel(data = coordenadas.fila, aes(x = `Dim 1.x`, 
                                                         y = `Dim 2.x`,
                                                         alpha = alpha_fila,
                                                         label = Muestra),
                            color = myred,
                            size = 4,
                            max.overlaps = 10,
                            force = 5,
                            show.legend = FALSE) +
            #graficar muestras en azul
            geom_point(data = coordenadas.columna, aes(x = `Dim 1.x`, 
                                                       y = `Dim 2.x`,
                                                       alpha = alpha_columna),
                       color = myblue,
                       show.legend = FALSE) +
            geom_text_repel(data = coordenadas.columna, aes(x = `Dim 1.x`, 
                                                            y = `Dim 2.x`,
                                                            alpha = alpha_columna,
                                                            label = Atributo),
                            color = myblue,
                            size = 4,
                            max.overlaps = 10,
                            force = 2,
                            show.legend = FALSE) +
            # Líneas guía en los ejes
            geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
            geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
            # Etiquetas y título
            labs(x = paste("F1", Peso_dim1, "%"), 
                 y = paste("F2", Peso_dim2, "%"), 
                 title = paste("Biplot (Ejes F1 y F2:", 
                               sum(Peso_dim1, Peso_dim2), "%)")) +
            # Tema minimalista
            theme_classic() +
            theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
            xlim(-x_max * 1.1, x_max * 1.1) + 
            ylim(-y_max * 1.1, y_max * 1.1)
        
        
    })
    
    #---- Penalty lifts
    
    output$penaltyLifts <- renderPlot({
        req(datos_CATA())
        
        
        df <- datos_CATA()
        
        # Preparar un data frame vacío para almacenar los resultados
        penalty_lifts <- data.frame(Atributo = character(), PL = numeric(), stringsAsFactors = FALSE)
        
        for (i in 4:ncol(df)) {
            # Asignar el nombre de la columna actual a la variable 'atributo'
            atributo_name <- names(df)[i]
            
            # Calcular la media de Aceptabilidad cuando el atributo es 1
            A1 <- df %>%
                select(Aceptabilidad, all_of(atributo_name)) %>%
                filter(!!sym(atributo_name) == 1) %>%
                summarise(mean_Aceptabilidad = mean(Aceptabilidad)) %>%
                pull(mean_Aceptabilidad)
            
            # Calcular la media de Aceptabilidad cuando el atributo es 0
            A0 <- df %>%
                select(Aceptabilidad, all_of(atributo_name)) %>%
                filter(!!sym(atributo_name) == 0) %>%
                summarise(mean_Aceptabilidad = mean(Aceptabilidad)) %>%
                pull(mean_Aceptabilidad)
            
            # Calcular el Penalty Lift
            PL <- A1 - A0
            
            # Agregar el resultado al data frame de resultados
            penalty_lifts <- rbind(penalty_lifts, data.frame(Atributo = atributo_name, PL = PL))
        }
    
        
        # Asumiendo que 'penalty_lifts' es tu data frame que contiene las columnas 'Atributo' y 'PL'.
        
        # Ordena el data frame por el valor de PL de mayor a menor
        penalty_lifts <- penalty_lifts[order(penalty_lifts$PL, decreasing = TRUE), ]
        
        # Crea el gráfico de barras
       pl <-  ggplot(penalty_lifts, aes(x = reorder(Atributo, PL), y = PL, fill = PL > 0)) +
            geom_bar(stat = "identity", position = "identity") +
            coord_flip() +  # Hace que las barras sean horizontales
            scale_fill_manual(values = c("brown2", "darkolivegreen3")) +  # Colores para los valores negativos y positivos
            labs(x = "Atributo", y = "Penalización en aceptabilidad", fill = "Penalty Lift") +
            ggtitle("Gráfico de Penalización de Aceptabilidad") +
            theme_minimal() +
            theme(legend.position = "none") +  # Ocultar la leyenda
            geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Línea en y=0
     
       return(pl)   
    })
    
}

shinyApp(ui = ui, server = server)
