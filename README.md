# Haskell draft beers

En una fiesta, se cuenta con 3 barriles de cerveza de diferentes capacidades. Los barriles están conectados entre
sí mediante tubos que permiten transferir cerveza de uno a otro. Además, cada barril tiene una salida que
permite servir cerveza directamente en vasos.

Su objetivo como especialista en programación funcional es diseñar un programa que permita determinar
cuántos litros de cerveza deben agregarse entre los barriles para servir exactamente n vasos de cerveza desde
cualquiera de las salidas

## Estructura del proyecto
```{html}
haskell-draft-beers/
├── bin/                # Ejecutable
├── docs/               # Enunciado y documentación
├── iface/              # archivos de interfaz
├── obj/                # Archivos objeto
├── src/                # Código fuente
│   ├── main.hs         # Punto de entrada del programa
│   └── ...             # Otros módulos
├── tests/              # Archivos de pruebas
│   └── ...             # Casos de prueba
├── Makefile            # Archivo makefile (constructor)
└── README.md           # Este archivo
```
> [!Note]
> Al culminar el proyecto todos los archivos que tengan el **codigo fuente** seran unidos en un unico archivo, la razón de esta estructura es simplemente para un mejor desarrollo.

## Constructor del proyecto

Para lograr compilar el poryecto, haremos uso de un archivo **makefile** el cual permite la construcción de archivos ejecutables programados en lenguajes compilados. Esta es una herramienta para realizar el build del proyecto.

> [!Warning]
> Si estas en windows, recomiendo usar la consola **Git bash**. Puede llegar a fallar alguno de los comandos que se usaron en el makefile si se usa **poweshell** o la **cmd** de windows.

- Windows
    ```{powershell}
        mingw32-make 
    ```
    Este comando hara la construcción del ejecutable **draftbeers.exe**.

- Linux
    ```{bash}
        make
    ```
    Asi se utiliza en linux.

> [!Note]
> **make** es la herramienta que se utiliza para ejecutar archivos **makefile**. En windows este viene junto con la instalación de **C/C++**. Y si estas en linux este viene junto con el entorno **Unix**. Otra cosa, el makefile tiene implementada una regla **execute** para facilitar la ejecución programa (Revisar el makefile para ver mas reglas o comentarios).

# Documentación
El objetivo es diseñar con programación funcional un programa  que permita determinar cuántos litros de cerveza deben agregarse entre los barriles para servir exactamente $n$ vasos de cerveza desde cualquiera de las salidas.

### 1 - Inicialización de barriles
```{haskell}
validateBarrel :: Barrel -> Bool
validateBarrel (cap, curr) = cap > 0 && curr >= 0 && curr <= cap

initialBarrels :: Barrel -> Barrel -> Barrel -> (Barrel, Barrel, Barrel)
initialBarrels a b c = (validate a, validate b, validate c)
  where
    validate (cap, curr)
      | validateBarrel (cap, curr) = (cap, curr)
      | cap < 0                    = (0,0)
      | curr > cap                 = (cap, cap)
      | curr < 0                   = (cap,0)                
      | otherwise                  = (cap, 0)

```
La funcion `validateBarrels` verifica si un barril, representado como una tupla (cap, curr) de capacidad y contenido actual, cumple con las restricciones necesarias para ser considerado válido, que la capacidad sea mayor a 0, que la cantidad actual sea mayor igual a cero, y que esta misma sea menor igual a la capacidad del barril.

`initialBarrels` crea una terna de barriles validados, asegurando que cada barril cumpla con las restricciones definidas por `validateBarrel`. Los barriles inválidos se ajustan para garantizar valores válidos de capacidad y contenido, preservando las cantidades válidas cuando sea posible.

### 2 - Existe solución
```{haskell}
iSolution :: (Barrel, Barrel, Barrel) -> Int -> Bool
iSolution (barrelA, barrelB, barrelC) n
    | n < 0     = False
    | not (validateBarrel barrelA) || not (validateBarrel barrelB) || not (validateBarrel barrelC) = False
    | otherwise = 
        let (capA, currA) = barrelA
            (capB, currB) = barrelB
            (capC, currC) = barrelC
        in (capA >= n && currA >= n) || (capB >= n && currB >= n) || (capC >= n && currC >= n)

```
`iSolution` determina si es posible obtener una cantidad exacta de cerveza n en al menos uno de los tres barriles proporcionados, considerando sus capacidades y contenidos actuales. La función verifica que los barriles sean válidos y que al menos uno de ellos tenga la capacidad y el contenido suficientes para contener la cantidad n.

### 3 - Añadir cerveza
```{haskell}
    addBeer :: Int -> Barrel -> (Barrel, Int)
    addBeer n (cap, curr)
        | n <= 0 = ((cap, curr), 0)
        | otherwise = let newCurr = min (curr + n) cap
                  in ((cap, newCurr), n - (newCurr - curr))

```
Esta rutina `addBeer` realiza la funcion de añadir cerveza a un barril, primero valida que la cantidad a añadir sea valida, luego calcular el total añadido, el desbordamiento y retorna una tupla con el barril actualizado y un entero que representa el desbordamiento.

### 4 - Mejor solución
```{haskell}
    fromAToC :: (Barrel, Barrel, Barrel) -> Int -> Int -> (Int, (Barrel, Barrel, Barrel))
    fromAToC (a, b, c) n contador
        | iSolution (a, b, c) n = (contador, (a, b, c))
        | otherwise =
            let (nuevoA, desbordeA) = addBeer 1 a
                (nuevoB, desbordeB) = addBeer desbordeA b
                (nuevoC, _)         = addBeer desbordeB c
            in fromAToC (nuevoA, nuevoB, nuevoC) n (contador + 1)

    fromCToA :: (Barrel, Barrel, Barrel) -> Int -> Int -> (Int, (Barrel, Barrel, Barrel))
    fromCToA (a, b, c) n contador
        | iSolution (a, b, c) n = (contador, (a, b, c))
        | otherwise =
            let (nuevoC, desbordeC) = addBeer 1 c
                (nuevoB, desbordeB) = addBeer desbordeC b
                (nuevoA, _)         = addBeer desbordeB a
            in fromCToA (nuevoA, nuevoB, nuevoC) n (contador + 1)

    findBestSolution :: Int -> (Barrel, Barrel, Barrel) -> (Int, (Barrel, Barrel, Barrel))
    findBestSolution n (a, b, c)
        | n <= 0 || not (isSatisfied a b c n) = (0, (a, b, c))
        | otherwise =
            let -- Camino agregando a 'a'
                (addedFromA, stateA) = fromAToC (a, b, c) n 0

                -- Camino agregando a 'c'
                (addedFromC, stateC) = fromCToA (a, b, c) n 0

                -- Lista de soluciones válidas
                solutions = [(addedFromA, stateA), (addedFromC, stateC)]
            in 
                minimum solutions -- Elige la de menor cantidad agregada

```
Esta rutina `findBestSolution` realiza la función de encontrar la cantidad de cerveza óptima que debe agregarse en los barriles, con el fin de alcanzar una cantidad específica de vasos de cerveza en uno de los tres barriles.

Esta verifica primero si la cantidad de vasos a llenar es valida y si hay algun barril que pueda llenar los $N$ vasos solicitados utilizando `isSatisfied`. Si se cumple lo anterior, entonces porcede con la busqueda. Este llama a dos funciones `fromAtoC` y `fromCtoA`, estas realizan el procesos de llenar los barriles de un $1L$ en $1L$ recursivamente, hasta lograr encontrar un barrill que pueda satisfacer los $N$ vasos. un caso para llenar desde el barril $A$ al $C$ y vicersa. Estas funciones internamente se apoyan de las funciones `iSolution` para la condición de parada y `addBeer` para agregar los litros. Al encontrar las posibles soluciones, tomamos el caso con menor ceversa agregada posible utilizando `minimum`, La expresión `minimum xs` funciona porque Haskell compara las tuplas por su primer elemento (en este caso, `addedFromA` o `addedFromC`), ya que la lista soluciones es de la forma `[(addedFromA, stateA)]`.