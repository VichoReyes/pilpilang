# La idea

La idea es que el compilador funcione con las siguientes fases:

1. Parsear todo (done-ish)
2. **TODO:** Comunicarse con la base de datos para tipar _solo las columnas en declaraciones de recurso/actor_.
3. Hacer type-checking
4. ???
5. Profit

# Comandos útiles

Correr el programa (aunque por el momento el programa es más para hacer pruebas que algo útil):

```bash
cabal run pilpilang -- [argumentos]
# ejemplos
cabal run pilpilang -- AST test/examples/twitter.pilpil
```

Correr los tests:

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

Dicen que es el mejor lenguaje para escribir compiladores, ya que los compiladores son simplemente funciones. Además, cuándo volveré a tener la oportunidad de usarlo?

# Troubleshooting

Lo que estás haciendo es usar cabal, instalado por ghcup. De hecho, si hay algún problema de que falta ghc, lo que hay que hacer es configurarlo con ghcup:

```
ghcup set ghc [versión]
```

## LLVM

También hubo un problema con LLVM. No sé si vuelva a pasar, pero se solucionó:

1. Instalando la versión 12 específicamente: `brew install llvm@12`
2. Instalando GHC con un comando horrible: `OPT=/opt/homebrew/opt/llvm@12/bin/opt LLC=/opt/homebrew/opt/llvm@12/bin/llc ghcup install ghc 8.10.7 --force`

Tengo miedo, mamá
