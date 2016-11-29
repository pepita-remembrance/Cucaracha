# Cucaracha

Este proyecto contiene el el parser, analizador semántico, interperte y compilador del lenguaje Cucaracha.

### Arquitectura

 - Se utilizó [ANTLRv4](http://www.antlr.org/) para generar el parser, a partir de la gramática definida en [CucarachaGrammar](/src/main/antlr/CucarachaGrammar.g4)
 - Todo lo demás se programó usando Scala y Gradle como herramienta de build.
 - Se utilizó un [Visitor](/src/main/scala/ar/edu/unq/parse/tp1/ast/ASTifier.scala) para, a partir del árbol de derivación que retorna el parser, [generar un AST](/src/main/scala/ar/edu/unq/parse/tp1/ast/ASTTree.scala)
 - El [analizador semántico](/src/main/scala/ar/edu/unq/parse/tp1/semantics/SemanticChecker.scala) define las reglas de chequeo.
 - Los nodos del AST que sean expresiones, tienen la capacidad de chequear (e inferir) sus tipos.
 - Los nodos del AST que sean sentencias, tienen la capacidad de realizar un chequeo de tipos sobre sus nodos hijos.
 - Se corren tests sobre todos los casos especificados en `/src/test/test_cucaracha`

### Setup & test

 - Requerimiento: Java JDK debe estar instalada y seteada la variable `JAVA_HOME`
 - Windows: ejecutar `gradlew.bat spec`
 - Linux: ejecutar `chmod +x ./gradlew && ./gradlew spec`
 
No debería ser necesario instalar nada más. Los scripts descargan automáticamente todas las dependencias necesarias.
 
### Interpretar un archivo

  - Windows: ejecutar `gradlew.bat run -Din="./ruta/al/archivo"`
  - Linux: ejecutar `./gradlew run -Din="./ruta/al/archivo"`

Ejemplos:
  - Windows: `gradlew.bat run -Din="./src/test/tests_cucaracha/test01.input"`
  - Linux: `./gradlew run -Din="./src/test/tests_cucaracha/test01.input"`

Tambien se puede modificar el texto de la clase Run (archivo CucaApp.scala) e invocar run sin un archivo especifico.

### Compilar un archivo

  - Windows: ejecutar `gradlew.bat compile`
  - Linux: ejecutar `./gradlew compile`

Adicionalmente se proveen tres opciones extra:

- `-Dformat`
Permite especificar para que plataforma se desea generar el codigo.
En caso de no utilizarse esta opcion, el programa generara assembler para el sistema operativo en el que esta ejecutandose.

  - Generar para Windows: `-Dformat=win64`
  - Generar para Linux: `-Dformat=elf64`

- `-Din`
Permite especificar un archivo desde donde leer el codigo a compilar.
En caso de no utilizarse esta opcion, el programa compilara el texto de la clase Compile (archivo CucaApp.scala).

  - Ejemplo: `-Din="./src/test/tests_cucaracha/test01.input"`

- `-Dout`
Permite especificar el archivo donde escribir el codigo generado. Crea o sobreescribe el archivo en caso de ser necesario.
En caso de no utilizarse esta opcion, el programa mostrara el codigo generado en la consola.

  - Ejemplo: `-Dout="./src/test/tests_cucaracha/test01.asm"`
