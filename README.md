# Compiler_coursework
Open command line terminal with a path to folder containing Haskel files
run "ghc Main.hs -o mtc" in the command line
This should generate .hi and .o for each corresponding Haskell file
To run, input "./mtc 'file_name'.mt" to compile program "./mtc 'file_name'.tam" to run compiled program

MT program reseved words:
- let
- in
- var
- fun
- Integer
- Boolean
- if
- then
- else
- while
- do
- getint
- begin
- end

MT grammar :
program ::= let declarations in command

declaration ::= var identifier : type
              | var identifier : type := expr
              | fun identifier ( vardecls ) : type = expr

type ::= Integer 
        | Boolean

vardecl ::= identifier : type

vardecls ::= vardecls’
            | (empty symbol)
            
vardecls’ ::= vardecl , vardecls’ 
             | vardecl

command ::= identifier := expr
                       | if expr then command else command
                       | while expr do command
                       | getint ( identifier )
                       | printint ( expr )
                       | begin commands end

commands ::= command | command ; commands
