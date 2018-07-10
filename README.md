# elecciones-catalanas-21d
Este script genera los resultados de las elecciones catalanas del 21D que usamos en el [interactivo de Politibot](https://politibot.io/asi-ha-votado-cataluna/).

## Datos
El archivo [resultados_2017.csv](https://github.com/politibot/elecciones-catalanas-21d/blob/master/resultados_2017.csv) contiene los resultados limpios.

## Fuentes
Los datos están organizados en estas tres carpetas:

- `results` resultados en 2017 y 2015
- `shp` contornos municipales que usamos en el mapa
- `src` datos para cruzar con los resultados

Los resultados vienen directamente del feed de datos en tiempo real de la Generalitat de Catalunya al que tienen acceso los medios en la noche electoral. Es un formato de texto plano que suelen usar los recuentos hechos por Indra.

También adjuntamos los resultados de 2015 para poder hacer comparaciones y los contornos de los municipios que usamos en el mapa electoral.

Esto es lo que cruzamos a nivel municipal:

- [Población de 2 años y más según conocimiento del catalán](https://www.idescat.cat/pub/?id=censph&n=17&by=mun)
- [Población de 16 años y más según nivel educativo](https://www.idescat.cat/pub/?id=censph&n=15&by=mun)
- [Población según lugar de nacimiento por CCAA](https://www.idescat.cat/pub/?id=censph&n=12&by=mun)
- Edad mediana calculada a partir de la [estadística del padrón continuo](http://www.ine.es/dynt3/inebase/es/index.htm?type=pcaxis&file=pcaxis&path=%2Ft20%2Fe245%2Fp05%2F%2Fa2016)
- [Renta bruta media en los municipios de más de 1.000 habitantes](http://www.agenciatributaria.es/AEAT/Contenidos_Comunes/La_Agencia_Tributaria/Estadisticas/Publicaciones/sites/irpfmunicipios/2015/jrubik1ba3b6ffb879f0b4654305cde4f7da3038a346e9.html)
- Densidad de población calculada en QGIS a partir de datos del [INE](http://ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177011&menu=resultados&idp=1254734710990)
