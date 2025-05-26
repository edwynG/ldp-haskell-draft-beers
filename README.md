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
iSolution (a, b, c) n
  | n < 0 = False
  | otherwise = checkBarrel a || checkBarrel b || checkBarrel c
  where
    checkBarrel (cap, curr) = validateBarrel (cap, curr) && curr >= n
```
`iSolution` determina si es posible obtener una cantidad exacta de cerveza n en al menos uno de los tres barriles proporcionados, considerando sus capacidades y contenidos actuales. La función verifica que al menos uno de los barriles sea válido y que tenga la capacidad y el contenido suficientes para contener la cantidad n.

### 3 - Añadir cerveza
```{haskell}
addBeer :: Int -> Barrel -> (Barrel, Int)
addBeer n (cap, curr)
    | n <= 0 = ((cap, curr), 0)
    | otherwise = let newCurr = min (curr + n) cap
                in ((cap, newCurr), n - (newCurr - curr))

```
La función `addBeer` es fundamental para simular el proceso de agregar cerveza a un barril dentro del sistema. Su funcionamiento inicia validando que la cantidad de cerveza a añadir sea positiva; si la cantidad es cero o negativa, simplemente retorna el barril sin cambios y un desbordamiento de cero. En caso contrario, la función calcula el nuevo contenido del barril sumando la cantidad actual con la cantidad a agregar, pero asegurándose de que no se exceda la capacidad máxima del barril. Para esto, utiliza la función `min` para limitar el contenido al valor de la capacidad.

Posteriormente, `addBeer` determina si hubo desbordamiento. El desbordamiento se calcula como la diferencia entre la cantidad que se intentó agregar y la cantidad que efectivamente pudo ser almacenada en el barril. Finalmente, la función retorna una tupla: el primer elemento es el barril actualizado (con su nueva cantidad de cerveza, que nunca excede la capacidad), y el segundo elemento es la cantidad de cerveza que no pudo ser almacenada y, por tanto, representa el desbordamiento. Esta lógica permite modelar de manera precisa el comportamiento físico de los barriles y es clave para el manejo correcto de los flujos de cerveza entre los diferentes recipientes en el sistema.

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

-- Desborda desde A hacia B, luego el desborde de B se reparte
overflowFromA :: (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
overflowFromA (a, b, c) =
    let (a', overflowA) = if snd a > fst a then ( (fst a, fst a), snd a - fst a ) else (a, 0)
        (b', overflowB) = if snd b + overflowA > fst b
                        then ( (fst b, fst b), snd b + overflowA - fst b )
                        else ( (fst b, snd b + overflowA), 0 )
    in overflowFromB (a', b', c) overflowB

-- El desborde de B va al barril vecino con menor cantidad actual
overflowFromC :: (Barrel, Barrel, Barrel) -> (Barrel, Barrel, Barrel)
overflowFromC (a, b, c) =
    let (c', overflowC) = if snd c > fst c then ( (fst c, fst c), snd c - fst c ) else (c, 0)
        (b', overflowB) = if snd b + overflowC > fst b
                        then ( (fst b, fst b), snd b + overflowC - fst b )
                        else ( (fst b, snd b + overflowC), 0 )
    in overflowFromB (a, b', c') overflowB

-- El desborde de B va al barril vecino con menor cantidad actual
overflowFromB :: (Barrel, Barrel, Barrel) -> Int -> (Barrel, Barrel, Barrel)
overflowFromB (a, b, c) overflowB
    | overflowB <= 0 = (a, b, c)
    | snd a <= snd c && snd a < fst a =
        let spaceA = fst a - snd a
            transferA = min overflowB spaceA
            a' = (fst a, snd a + transferA)
            lost = overflowB - transferA
        in (a', b, c) -- Si quieres llevar la cuenta de lo perdido, puedes devolverlo también
    | snd c < fst c =
        let spaceC = fst c - snd c
            transferC = min overflowB spaceC
            c' = (fst c, snd c + transferC)
            lost = overflowB - transferC
        in (a, b, c')
    | otherwise = (a, b, c) -- Todo el desborde se pierde

-- | Determinar la cantidad de cerveza óptima que debe agregarse en los barriles para poder llenar n vasos desde un barril
findBestSolution :: Int -> (Barrel, Barrel, Barrel) -> (Int, (Barrel, Barrel, Barrel))
findBestSolution n (a, b, c)
    | n < 0 || not (isSatisfied a b c n) = (0, (a, b, c))
    | otherwise =
        let 
            (a', b', c') = overflowFromA (a, b, c)
            (a'', b'', c'') = overflowFromC (a', b', c')

            -- Camino agregando a 'a'
            (addedFromA, stateA) = fromAToC (a'', b'', c'') n 0

            -- Camino agregando a 'c'
            (addedFromC, stateC) = fromCToA (a'', b'', c'') n 0

            -- Lista de soluciones válidas
            solutions = [(addedFromA, stateA), (addedFromC, stateC)]
        in 
            minimum solutions -- Elige la de menor cantidad agregada

```
La función `findBestSolution` busca la cantidad mínima de cerveza que debe agregarse a los barriles para poder servir exactamente una cantidad específica de vasos desde alguno de ellos. Primero, valida si la cantidad de vasos solicitada es posible utilizando la función isSatisfied, que comprueba si algún barril puede satisfacer la demanda con su capacidad actual. Si es así, la función procede a explorar las posibles formas de agregar cerveza.

Para encontrar la solución óptima, `findBestSolution` utiliza dos caminos principales: agregar cerveza desde el barril A hacia el C y desde el barril C hacia el A. Estas rutas se implementan mediante las funciones internas `fromAToC` y `fromCToA`, que agregan cerveza de litro en litro al barril de inicio y simulan el proceso de desborde entre barriles.

El manejo de los desbordes se realiza con las funciones `overflowFromA` y `overflowFromC`. Cuando se agrega cerveza a A y este se desborda, `overflowFromA` transfiere el exceso a B, y si B también se desborda, su exceso se reparte al barril vecino con menor cantidad actual (A o C) o se pierde si ambos están llenos. De manera análoga, `overflowFromC` gestiona el desborde cuando se agrega cerveza a C, transfiriendo el exceso a B y aplicando la misma lógica de distribución del desborde.

Durante este proceso, las funciones internas utilizan `addBeer` para simular el agregado de cerveza y el posible desborde, y iSolution para verificar si ya es posible servir la cantidad deseada de vasos. El proceso se repite recursivamente hasta alcanzar una configuración válida.

Finalmente, `findBestSolution` compara las soluciones obtenidas por ambos caminos y selecciona la que requiere la menor cantidad de cerveza agregada usando la función `minimum`. Como las soluciones se representan como tuplas donde el primer elemento es la cantidad de litros agregados, `minimum` garantiza que se elija la estrategia más eficiente. Así, la función asegura siempre retornar la forma óptima de servir la cantidad exacta de vasos solicitados, considerando la dinámica realista de los desbordes entre barriles.