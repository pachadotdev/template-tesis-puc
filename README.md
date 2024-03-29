# Plantilla para escribir tesis en LaTeX (puctesis)

# Acerca de

Esta plantilla es uno de los resultados de mi propia tesis y está diseñada de acuerdo a los requerimientos de la Pontificia Universidad Católica de Chile para presentar los trabajos de tesis. 

Este trabajo se basa en la plantilla que aparece en http://mtorrest.sitios.ing.uc.cl/downloads.htm#puctesis y que fue diseñada por Miguel Torres.

La idea de este trabajo es simplificar la tarea a otros tesistas de modo que se concentren en el contenido y no en pasar en constantes combates con Latex. Este repositorio se puede descargar en zip y editar directamente.

En esta plantilla incluyo:
* Todos los paquetes de uso frecuente para insertar ecuaciones, tablas, imágenes, diagramas vectorizados, etc.
* Ejemplos de cómo insertar tablas, imágenes, notas al pie, comentarios, etc y cómo citar papers o libros en Latex

Como complemento a esta plantilla también tengo un manual express de Latex (revisado) en base a las guías que hice para mis ayudantías: https://github.com/pachamaltese/tutorial-express-latex

# Uso

* Basta con abrir tesis-rmarkdown.Rmd en RStudio y compilar el pdf con todos los capítulos, apéndices y bibliografía clikeando el botón "knit". 
* Los nuevos capítulos y apéndices se pueden ir agregando en capitulos/ siguiendo el ejemplo de los dos capítulos de demostración que incluí.
* Para incluir bibliografía hay que insertarla en el archivo bibliografía.bib

# Desventajas

* El encabezado del Departamento de Estadística se edita en puctesis.csl, no en tesis-markdown.Rmd
* El resumen del trabajo se edita directamente en formato-puc.tex,  no en tesis-markdown.Rmd.
* No he podido encontrar forma de desactivar el índice de figuras cuando no hay figuras.
* La bibliografía parte con un número 10, se puede borrar desde el tex que genera RStudio y crear el pdf nuevamente

# Problemas

Si tienes problemas para generar el pdf:

1. Instala [TinyTex](https://yihui.org/tinytex/)
2. Reinicia
3. Genera el PDF desde RStudio

# Licencia

CC0 1.0 Universal.
