/-
TreePPL Compiler command line
-/

-- (vsenderov, 2923-06-16 I don't remeber what are all the includes for any more;
-- perhaps we should test and see if we need all of them)
include "sys.mc"

include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"
include "mexpr/generate-json-serializers.mc"

include "treeppl-to-coreppl/compile.mc"

include "coreppl::dppl-arg.mc" -- inherit cmd-line opts from cppl
include "coreppl::parser.mc"




-- Command line menu for TreePPL
let tpplMenu = lam. join [
  "Usage: tpplc program.tppl [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]

mexpr

-- Use the arg.mc library to parse arguments
let result = argParse defaultArgs config in
match result with ParseOK r then
  let options: Options = r.options in
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (tpplMenu ());
    if gti (length r.strings) 1 then exit 1 else exit 0
  else
    match r.strings with [filename] in
    compileTpplToExecutable filename options
else argPrintError result
