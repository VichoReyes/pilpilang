# La idea

La idea es que el compilador funcione con las siguientes fases:

1. Parsear
2. Opcionalmente: Comunicarse con la base de datos para tipar automáticamente las columnas en declaraciones de recurso/actor.
3. Hacer type-checking de las reglas, convertir estructuras a una representación más rica.
4. Emitir reglas con estructura `CREATE POLICY`
5. Listo!

# Comandos útiles

Correr el programa (aunque por el momento el programa es más para hacer pruebas que algo útil):

```bash
cabal run pilpilang # muestra AST de cada línea
cabal run pilpilang -- test/examples/twitter.pilpil # compila el programa
```

Correr los tests (obsoleto):

```bash
cabal test
# o, si uno quiere ver el output incluso de los casos exitosos:
cabal test --test-show-details=streaming
```

Para experimentar con las funciones, resulta útil GHCi:

```bash
cabal repl # carga la librería
cabal repl pilpilang-test # carga la test suite
```

## Por qué haskell?

Dicen que es el mejor lenguaje para escribir compiladores, ya que los compiladores son simplemente funciones, y Haskell es un lenguaje funcional, diseñado para crear y manipular funciones fácilmente.
Hay buen soporte de librerías para hacer _parsing_ (megaparsec) y las mónadas son una abstracción útil para arrastrar los cambios que se le hacen al árbol de sintaxis.

# Troubleshooting

Lo que estás haciendo es usar cabal, instalado por ghcup. De hecho, si hay algún problema de que falta ghc, lo que hay que hacer es configurarlo con ghcup:

```
ghcup set ghc [versión]
```

## LLVM

También hubo un problema con LLVM. No sé si vuelva a pasar, pero se solucionó:

1. Instalando la versión 12 específicamente: `brew install llvm@12`
2. Instalando GHC con un comando horrible: `OPT=/opt/homebrew/opt/llvm@12/bin/opt LLC=/opt/homebrew/opt/llvm@12/bin/llc ghcup install ghc 8.10.7 --force`

