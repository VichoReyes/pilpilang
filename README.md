# Cómo ejecutar?

```
cabal run polar-parse
```

Lo que estás haciendo es usar cabal, instalado por ghcup. De hecho, si hay algún problema de que falta ghc, lo que hay que hacer es configurarlo con ghcup:

```
ghcup set ghc [versión]
```

## LLVM

También hubo un problema con LLVM. No sé si vuelva a pasar, pero se solucionó:

1. Instalando la versión 12 específicamente: `brew install llvm@12`
2. Instalando GHC con un comando horrible: `OPT=/opt/homebrew/opt/llvm@12/bin/opt LLC=/opt/homebrew/opt/llvm@12/bin/llc ghcup install ghc 8.10.7 --force`

Tengo miedo, mamá

# Motivación?

Hay alguna?

Decidí usar haskell porque no podía ser que me limitara una cosa tan irrelevante. Es el mejor lenguaje para hacer compiladores, y uno que hasta cierto punto ya conozco. Quiero (al menos) tener un argumento para decir "este lenguaje es malo".

Chigual lo podría haber hecho en Elixir.