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