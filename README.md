# Cucaracha

Este proyecto contiene el el parser y analizador semántico del lenguaje Cucaracha.

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
 
### Ejecutar un archivo

  - Windows: ejecutar `gradlew.bat run -Dtarget="./ruta/al/archivo"`
  - Linux: ejecutar `./gradlew run -Dtarget="./ruta/al/archivo"`

  Ejemplos:
  - Windows: `gradlew.bat run -Dtarget="./src/test/tests_cucaracha/test01.input"`
  - Linux: `./gradlew run -Dtarget="./src/test/tests_cucaracha/test01.input"`

  Tambien se puede modificar el texto en CucaApp.scala e invocar run sin argumentos.

### Compilar un archivo

  - Windows: ejecutar `gradlew.bat assemble -Dtarget="./ruta/al/archivo"`
  - Linux: ejecutar `./gradlew assemble -Dtarget="./ruta/al/archivo"`

  Ejemplos:
  - Windows: `gradlew.bat compile -Dtarget="./src/test/tests_cucaracha/test01.input"`
  - Linux: `./gradlew compile -Dtarget="./src/test/tests_cucaracha/test01.input"`

  Tambien se puede modificar el texto en CucaApp.scala e invocar compile sin argumentos.