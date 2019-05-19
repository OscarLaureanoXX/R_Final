# Oscar Laureano A00819139
# Aldo Cervantes A01039261

# Librerías a utilizar
library(tidyverse)

########################################################################################################
######################################## LIMPIEZA DE DATOS #############################################
########################################################################################################

# I.  Los archivos contienen información del 2005 al 2017. Sin embargo, para este estudio utilizarás la
# información de los últimos 5 años disponibles (2013 a 2017). Realiza los filtros necesarios en
# ambos archivos para contar con los datos sólo de los años 2013 a 2017.

# Leyendo csvs
accident <- read.csv("Finale/Accident_Information.csv")
vehicle <- read.csv("Finale/Vehicle_Information.csv")

# Filtrando los años
vehicle <- vehicle %>% filter(Year >= 2013 & Year <= 2017)
accident <- accident %>% filter(Year >= 2013 & Year <= 2017)

# II. En la tabla de vehículos existe una variable que indica el tipo con una gran cantidad de
# categorías. Para poder manejar de mejor manera los datos, realiza agrupaciones necesarias para
# obtener las categorías de: car, motocycle, goods vehicle, buses and others.

# A. Motorcycle over 500cc, Motorcycle 125cc or under, etc, se agrupan en la misma categoría
# de Motorcycle.

# Creando filtro para identificar renglones 
filtro_motos <- str_detect(vehicle$Vehicle_Type, '^Motorcycle')
# Agregando un nuevo factor
levels(vehicle$Vehicle_Type) <- c(levels(vehicle$Vehicle_Type), 'Motorcycle')
# Sustituyendo el factor
vehicle[filtro_motos ,'Vehicle_Type'] <- 'Motorcycle' ; 


# B. Buses, Coach y Minibus (todos los transporte de pasajeros) se agrupan en Buses.

# Creando filtro para identificar renglones 
filtro_buses <- str_detect(vehicle$Vehicle_Type,'^Bus')
filtro_minibus <- str_detect(vehicle$Vehicle_Type,'^Minibus')
# Agregando un nuevo factor
levels(vehicle$Vehicle_Type) <- c(levels(vehicle$Vehicle_Type), 'Buses')
# Sustituyendo el factor
vehicle[filtro_buses ,'Vehicle_Type'] <- 'Buses' ;
vehicle[filtro_minibus ,'Vehicle_Type'] <- 'Buses' ;


# C. Los vehículos de carga (Goods vehicles over 10 ton, etc.) se agrupan todos en una
# categoría Goods Vehicle.

# Creando filtro para identificar renglones
filtro_carga <- str_detect(vehicle$Vehicle_Type, '^Good')
filtro_van <- str_detect(vehicle$Vehicle_Type, '^Van')
# Agregando un nuevo factor
levels(vehicle$Vehicle_Type) <- c(levels(vehicle$Vehicle_Type), 'Goods Vehicle')
# Sustituyendo el factor
vehicle[filtro_carga,'Vehicle_Type'] <- 'Goods Vehicle' ;
vehicle[filtro_van,'Vehicle_Type'] <- 'Goods Vehicle' ;

# levels(droplevels(vehicle$Vehicle_Type))

# III. La variable Weather_Conditions en la tabla de Accidentes también tiene un gran número de
# categorías. Agrupa las categorías relacionadas con lluvia (rain no winds y rain high winds) y de
# nieve (snow no wind, snow high winds).

# Creando filtros para identificar renglones
filtro_rain <- str_detect(accident$Weather_Conditions, '^Rain')
filtro_snow <- str_detect(accident$Weather_Conditions, '^Snow')
# Agregando nuevos factores
levels(accident$Weather_Conditions) <- c(levels(accident$Weather_Conditions), 'Rain', 'Snow')
# Sustituyendo factores
accident[filtro_rain,'Weather_Conditions'] <- 'Rain' ;
accident[filtro_snow,'Weather_Conditions'] <- 'Snow' ;

# levels(droplevels(accident$Weather_Conditions))

########################################################################################################
######################################## PREGUNTAS/GRÁFICAS ############################################
########################################################################################################

# 1.- ¿Cuál es la tendencia del número de accidentes ocurridos en los últimos 5 años (aumenta, disminuye, se queda igual)?
pregunta1 <- accident %>% group_by(Year) %>% summarise(n = n())

graph1 <- ggplot(data = pregunta1) + 
            geom_point(mapping = aes(x = Year, y = n), size = 3, color = "red") +
            geom_smooth(mapping = aes(x = Year, y = n), size = 2) +
            labs(title = 'Accidentes por año', x = 'Año', y = 'Accidentes') + 
            ylim(100000, 150000)
graph1


# 2.- ¿Ha variado la proporción de accidentes graves, no graves y fatales en los últimos años? 
# ¿Cuántos fueron causados por mujeres y cuántos por hombres? Una opcion es hacer un group_by year y sex_of_driver

# Join de todo
All <- accident %>% inner_join(vehicle, by = 'Accident_Index')

pregunta2Hombres <- All %>% filter(Sex_of_Driver == 'Male') %>% group_by(Year.x,Accident_Severity) %>% summarise(n = n())
pregunta2Mujeres <- All %>% filter(Sex_of_Driver == 'Female') %>% group_by(Year.x,Accident_Severity) %>% summarise(n = n())

graph2Hombres <- ggplot(data = pregunta2Hombres, aes(x = Year.x, y = n, group = Accident_Severity)) +
                    geom_col(aes(fill = Accident_Severity), position = "dodge") +
                    geom_text(
                        aes(label = n, y = n + 0.05),
                        position = position_dodge(0.9),
                        vjust = 0
                        ) +
                    ggtitle('Tipos de Accidentes por Año HOMBRES', subtitle = waiver()) + 
                    ylab('Porcentaje') +
                    xlab('Año') +
                    ylim(0, 150000)
# graph2Hombres

graph2Mujeres <- ggplot(data = pregunta2Mujeres, aes(x = Year.x, y = n, group = Accident_Severity)) +
                    geom_col(aes(fill = Accident_Severity), position = "dodge") +
                    geom_text(
                    aes(label = n, y = n + 0.05),
                    position = position_dodge(0.9),
                    vjust = 0
                    ) +
                    ggtitle('Tipos de Accidentes por Año MUJERES', subtitle = waiver()) + 
                    ylab('Porcentaje') +
                    xlab('Año') +
                    ylim(0, 150000)
# graph2Mujeres

# 3.- Con base en los datos de los últimos 5 años, ¿cuál es el número más probable de afectados que pueda haber en un accidente?

pregunta3 <- accident %>% group_by(Year) %>% summarise(average = mean(Number_of_Casualties))

graph3 <- ggplot(data = pregunta3) +
            geom_line(mapping = aes(x = Year, y = average), size = 2, color = "red") +
            labs(title = 'Promedio de Afectados en Accidentes', x = 'Año', y = 'Afectados promedio') +
            ylim(1,2) +
            annotate("text", x = 2013:2017, y = 1.3 , label = c('1.32','1.33','1.33','1,33','1.32'))
# graph3

# 4. ¿Cómo se comparan las edades de cada tipo de vehículo? Genera diagramas de caja y bigote y genera conclusiones. 
#Puedes usar: outlier.shape = NA para eliminar outliers y cambiar los limites

pregunta4 <- vehicle %>% group_by(Vehicle_Type,Age_of_Vehicle) %>% filter(Vehicle_Type != 'Data missing or out of range' & Vehicle_Type != 'Tram' & Vehicle_Type != 'Ridden horse' & Vehicle_Type != 'Pedal cycle')


graph4 <- ggplot(data = pregunta4) + 
            geom_boxplot(outlier.shape = NA, mapping = aes(x = Vehicle_Type, y = Age_of_Vehicle), fill = '#679EF7') +
            ylim(0,30) +
            coord_flip() +
            labs(title = 'Edades de distintos Tipos de Vehículos', x = 'Tipo de Vehículo', y = 'Edad')
# graph4

# 5. Comparar la capacidad de combustible los diferentes tipos de vehículo (carro, motocicleta, autobuses)
#Una opcion es hacer un group_by vehicle_type y engine_capacity
pregunta5 <- vehicle %>% group_by(Vehicle_Type, Engine_Capacity_.CC.)  %>% filter(Vehicle_Type != 'Data missing or out of range' & Vehicle_Type != 'Tram' & Vehicle_Type != 'Ridden horse' & Vehicle_Type != 'Pedal cycle' & Vehicle_Type != 'Mobility scooter')

graph5 <- ggplot(data = pregunta5) + 
            geom_boxplot(outlier.shape = NA, mapping = aes(x = Vehicle_Type, y = Engine_Capacity_.CC.), fill = '#679EF7') +
            ylim(0,15000) +
            coord_flip() +
            labs(title = 'Capacidad de Combustible por tipo de Vehículo', x = 'Tipo de Vehículo', y = 'CC')
# graph5

# 6. ¿Ha habido cambios en los últimos años en la proporción de accidentes que han ocurrido cuando el pavimento está seco o mojado? 
# ¿Hay más accidentes cuando el pavimento está mojado?

#Una opcion es hacer un group_by Road_Surface_Conditions y year
pregunta6 <- accident %>% group_by(Road_Surface_Conditions,Year) %>% filter(Road_Surface_Conditions != 'Data missing or out of range') %>% summarise(n = n())

graph6 <- ggplot(data = pregunta6, aes(x = Year, y = n, group = Road_Surface_Conditions)) +
            geom_col(aes(fill = Road_Surface_Conditions), position = "dodge") +
            geom_text(
                aes(label = n, y = n + 0.05),
                position = position_dodge(0.9),
                vjust = 0
                ) +
            ggtitle('Condiciones del Camino en Accidentes por Año', subtitle = waiver()) + 
            ylab('Cantidad') +
            xlab('Año') +
            ylim(0, 105000)
# graph6

# 7. Compara el efecto que tiene la luz y el pavimento sobre los accidentes. ¿Habrá algún efecto de la luz y 
# la condición del pavimento que hace más propicios los accidentes?

pregunta7 <- accident  %>% filter(Road_Surface_Conditions != 'Data missing or out of range' & Light_Conditions != 'Data missing or out of range') %>% group_by(Light_Conditions,Road_Surface_Conditions) %>% summarise(n = n())

graph7 <- ggplot(data = pregunta7, aes(x = Light_Conditions, y = n, group = Road_Surface_Conditions)) +
            geom_col(aes(fill = Road_Surface_Conditions), position = "dodge") +
            geom_text(
                aes(label = n, y = n + 0.05),
                position = position_dodge(0.9),
                vjust = 0
                ) +
            ggtitle('Condiciones del Camino en Accidentes por Año', subtitle = waiver()) + 
            ylab('Cantidad') +
            xlab('Condición del Camino') +
            ylim(0,390000)

# graph7

# 8. Cuál es la relación que hay entre la severidad del accidente y las condiciones del clima?

pregunta8 <- accident %>% filter(Weather_Conditions != 'Data missing or out of range' & Accident_Severity != 'Data missing or out of range') %>% group_by(Accident_Severity,Weather_Conditions) %>% summarise(n = n())

graph8 <- ggplot(data = pregunta8, aes(x = Weather_Conditions, y = n, group = Accident_Severity)) +
            geom_col(aes(fill = Accident_Severity), position = "dodge") +
            geom_text(
                aes(label = n, y = n + 0.05),
                position = position_dodge(0.9),
                vjust = 0
                ) +
            ggtitle('Condiciones del Clima en Severidad de Accidentes', subtitle = waiver()) + 
            ylab('Cantidad') +
            xlab('Condición del Clima') +
            ylim(0,90000)
# graph8

# 9. ¿Hay alguna relación entre el número de vehículos involucrados y el número de lesionados que hubo? 
# Escoge datos de un año en particular. ¿En esta relación, hubo algún impacto de las condiciones del pavimento o del clima?

#Esta seria con la grafica de puntitos
pregunta9_1 <- accident %>% filter(Year == 2017) %>% group_by(Number_of_Vehicles,Number_of_Casualties)

graph9_1 <- ggplot(data = pregunta9_1) + 
                geom_point(mapping = aes(x = Number_of_Vehicles, y = Number_of_Casualties)) +
                geom_smooth(mapping = aes(x = Number_of_Vehicles, y = Number_of_Casualties)) +
                ggtitle('Afectados por Número de Vehículos en 2017', subtitle = waiver()) + 
                ylab('Afectados') +
                xlab('Cantidad de Vehículos')

#graph9_1

pregunta9_2 <-  accident %>% filter(Year == 2017 & Weather_Conditions != 'Data missing or out of range') %>% group_by(Number_of_Vehicles,Number_of_Casualties,Weather_Conditions)
graph9_2 <- ggplot(data = pregunta9_2, aes(x = Number_of_Vehicles, y = Number_of_Casualties, group = Weather_Conditions)) +
                geom_col(aes(fill = Weather_Conditions), position = "dodge") +
                ggtitle('Afectados por Número de Vehículos en 2017', subtitle = waiver()) + 
                ylab('Afectados') +
                xlab('Cantidad de Vehículos')
#graph9_2

# 10 . ¿Hay alguna relación entre el número de lesionados, el límite de velocidad y las condiciones del pavimento en el último año (2017)? 

pregunta10 <- accident %>% filter(Year == 2017 & Road_Surface_Conditions != 'Data missing or out of range') %>% group_by(Road_Surface_Conditions,Speed_limit,Number_of_Casualties) 

graph10 <- ggplot(data = pregunta10, aes(x = Road_Surface_Conditions, y = Number_of_Casualties, group = Speed_limit)) +
            geom_col(aes(fill = Speed_limit), position = "dodge") +
            ggtitle('Lesionados por Condiciones de Camino y Velocidad', subtitle = waiver()) + 
            ylab('Lesionados') +
            xlab('Condición del Camino') 
# graph10

# 11. Basándose en los datos de los últimos 5 años disponibles (2013-2017), 
# ¿Cuántos accidentes ocurren en promedio por cada día de la semana? ¿Cuáles son los días en los que hay más accidentes? 

pregunta11 <- accident %>% group_by(Day_of_Week) %>% summarise(n = n()) %>% mutate( Promedio = n / 5)


graph11 <- ggplot(data = pregunta11) +
            geom_bar(mapping = aes(x = Day_of_Week, y = Promedio, fill = Day_of_Week), stat = "identity") +
            ggtitle('Cantidad de Accidentes promedio entre 2013 y 2017 por dia de la semana', subtitle = waiver()) + 
            geom_text(mapping = aes(label = Promedio, y = Promedio + 500, x = Day_of_Week)) +
            ylab('Accidentes promedio') +
            xlab('Día de la Semana') 
# graph11

# 12. Del promedio de accidentes por día de la pregunta anterior, ¿cuáles fueron las principales razones por 
# las que los conductores estaban viajando (Journey purpose of driver)?

# Join de todo
All <- accident %>% inner_join(vehicle, by = 'Accident_Index')

pregunta12 <- All %>% group_by(Day_of_Week,Journey_Purpose_of_Driver) %>% summarise(n = n())

graph12 <- ggplot(data = pregunta12, aes(x = Day_of_Week, y = n, group = Journey_Purpose_of_Driver)) +
                geom_col(aes(fill = Journey_Purpose_of_Driver), position = "dodge") +
                ggtitle('Razones de Accidentes por Día de la Semana', subtitle = waiver()) + 
                ylab('Accidentes') +
                xlab('Día de la Semana')#+
                #ylim(0, 25500)
# graph12

# 13. ¿Cuáles con las maniobras más populares al momento del accidente?

pregunta13 <- vehicle %>% group_by(Vehicle_Manoeuvre) %>% summarise(n = n())

graph13 <- ggplot(data = pregunta13) +
            geom_bar(mapping = aes(x = Vehicle_Manoeuvre, y = n, fill = Vehicle_Manoeuvre), stat = "identity") +
            geom_text(mapping = aes(label = n, y = n + 1000, x = Vehicle_Manoeuvre)) +
            coord_flip() +
            ggtitle('Maniobras Populares', subtitle = waiver()) + 
            xlab('Maniobras') +
            ylim(0,400000) +
            ylab('Cantidad')   
# graph13

# 14. Cuál es la distribución de las edades de los conductores que estuvieron involucrados en algún accidente? ¿Hay alguna tendencia? 

pregunta14 <- vehicle %>% filter(Age_Band_of_Driver != 'Data missing or out of range' & Age_Band_of_Driver != '0 - 5' & Age_Band_of_Driver != '6 - 10') %>% group_by(Age_Band_of_Driver) %>% summarise(n = n())

graph14 <- ggplot(data = pregunta14) +
            geom_bar(mapping = aes(x = Age_Band_of_Driver, y = n, fill = Age_Band_of_Driver), stat = "identity") +
            ggtitle('Cantidad de Accidentes por Edades', subtitle = waiver()) + 
            geom_text(mapping = aes(label = n, y = n + 500, x = Age_Band_of_Driver)) +
            ylab('Accidentes') +
            xlab('Edades') 
graph14

# 15 . En el último año, ¿cuál podría haber sido causa de los accidentes: 
# las condiciones del clima, las condiciones de luz, las condiciones del pavimento, 
# objetos en la vialidad (Carriageway hazard), el control en el crucero (semáforo, ceder el paso, sin control, etc.)? 

pregunta15clima <- accident %>% filter(Year == 2017) %>% group_by(Weather_Conditions) %>% summarise(n = n())
pregunta15luz <- accident %>% filter(Year == 2017) %>% group_by(Light_Conditions) %>% summarise(n = n())
pregunta15pavimento <- accident %>% filter(Year == 2017) %>% group_by(Road_Surface_Conditions) %>% summarise(n = n())
pregunta15vialidad <- accident %>% filter(Year == 2017) %>% group_by(Carriageway_Hazards) %>% summarise(n = n())
pregunta15crucero <- accident %>% filter(Year == 2017) %>% group_by(Junction_Control) %>% summarise(n = n())