# 02-tarea

Esta tarea debe ser resuelta en GitHub Class Room. Para acceder a la  [Tarea N�2 hacer click aqu�](https://classroom.github.com/g/CzMiL4Gs). Si a�n tienes dudas de c�mo interactuar con [GitHub Class Roomm revisa el pr�ctico](../example/01-practico)


## Objetivo

La tarea N�2 consiste en la creaci�n de un script en R que les permita construir datos limpios y procesados que servir�n de "input" para los an�lisis exploratorios que se reporten de los datos procesados (tarea N�3). El ejercicio que deben realizar se basa en un caso simulado de "trabajo" que les han solicitado

### Instrucciones

En su nuevo trabajo como investigadores/as les han encargado hacer un estudio sobre el efecto de la pandemia sobre el empleo. Para ello, les han solicitado a ustedes la recopilaci�n y procesamiento de una serie de datos relevantes para iniciar el estudio. De manera preliminar, le solicitan los siguientes puntos

1. Debe ocupar la Encuesta Nacional de Empleo (ENE) del Instituto Nacional de Estad�sticas, particularmente el trimestre *Enero-Febrero-Marzo* del a�o 2019 y del 2021. 

2. Las variables que m�s interesan para el informe son: condici�n de actividad econ�mica general (*CAE*), no general (*Condici�n de actividad*) y las horas de trabajo de la actividad principal (*modulo C*)

3. Tambi�n le han indicado que hay ciertas variables sociodemogr�ficas de inter�s como el nivel educacional (*CINE*), edad y sexo 

### Instrucciones espec�ficas

En su equipo de investigaci�n quieren realizar los siguientes an�lisis, para lo cu�l usted debe construir un data set procesado llamado *datos_proc.RData* que permita realizar estos an�lisis de manera debida

0. Los an�lisis solo deben considerar a las personas en edad de trabajar seg�n las definiciones del Instituto Nacional de Estad�stica. 

1. Un an�lisis de la distribuci�n de sociodemogr�ficos de nivel educacional, edad y sexo. Le han solicitado que el nivel educacional est� solo clasificado en "Superior completa", "Media y b�sica completa" y "B�sica o menos incompleta", que la edad est� clasificada en tramos de "18 a 39 a�os", "40 a 64 a�os" y "65 y m�s"; sexo debe estar codificada como "Mujer" y "Hombre". 

2. Un an�lisis de la distribuci�n de la condici�n de actividad econ�mica, situaci�n de empleo y las suma de horas trabajadas en la actividad principal para la muestra del 2019 y 2020.

3. Una persona del equipo sospecha de la variable de condici�n de actividad que no es general (*activ*). Por ello, le piden que cree una nueva variable llamada *"cae_corregido"* en base a la definici�n que se hace de esta variable en el manual metodol�gico.

**Atenci�n**: la ejecuci�n concreta de los an�lisis ser�n parte de la tarea N�3. Por ahora usted debe preocuparse de: 

1. Construir un Rproject ordenado seg�n los criterios establecidos en el curso (02-tarea.Rproject)

2. Construir un script llamado *procesamiento-datos.R* que est� alojado en la carpeta que corresponda. Este script debe tener un orden y utilizar los comentarios (#) para hacer referencia a los procesos realizados

3. Cargar los paquetes y datos necesarios para el procesamiento

4. Filtrar, seleccionar y crear las variables indicadas (no olvides crear el �ndice sumativo)

5. Una vez limpios los datos del 2019 y del 2020, unir los datos en un �nico objeto llamado "datos20202019"

6. Guardar los datos procesados del 2019, 2020 y la uni�n de ambos en un archivo llamado *datos_proc.RData*. 

7. Bonus: si incorporan un README.md con la explicaci�n de d�nde sacaron los datos e informaci�n metodol�gica, ser� bonificado con 0,4 d�cimas.


## Formato

- Recuerden que puede ser realizada en pareja, la misma durante todo el semestre. 

- La tarea se puede entregar hasta el d�a viernes 17 de septiembre 23.59


## Recursos

-  ["R para ciencia de datos - Como usar RMarkdown"](https://es.r4ds.hadley.nz/r-markdown.html) 

- Recursos
  - [Recursos del curso](https://learn-r-uah.netlify.app/resource/) 
  - [Tutorial dplyr](https://www.youtube.com/watch?v=APzU10EMMjg)

-   [Sesi�n N�2](/content/02-content)
-   [Sesi�n N�3](/content/03-content)
-   [Sesi�n N�4](/content/04-content)
-   [Sesi�n N�5](/content/05-content)

-   [Pr�ctico N�2](/example/02-practico)
-   [Pr�ctico N�3](/example/03-practico)
-   [Pr�ctico N�4](/example/04-practico)
-   [Pr�ctico N�5](/example/05-practico)



