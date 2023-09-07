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
include "coreppl::coreppl-to-rootppl/compile.mc"
include "coreppl::build.mc"
include "coreppl::parser.mc"

--lang CorePPLUsage =
--  CPPLBackcompat + LoadRuntime +
--  ImportanceSamplingMethod + BPFMethod + APFMethod +
--  LightweightMCMCMethod  + NaiveMCMCMethod + TraceMCMCMethod +
--	PIMHMethod + ProjMatchPprint
--end

-- (vsenderov, 2023-06-16 The CPPL language as defined in the cppl command line)
lang CPPLLang =
  MExprAst + MExprCompile + TransformDist + MExprEliminateDuplicateCode +
  MExprSubstitute + MExprPPL + GenerateJsonSerializers

  -- Check if a CorePPL program uses infer
  sem hasInfer =
  | expr -> hasInferH false expr

  sem hasInferH acc =
  | TmInfer _ -> true
  | expr -> sfold_Expr_Expr hasInferH acc expr

end

lang TreePPLThings = TreePPLAst + TreePPLCompile
  + LowerProjMatch + ProjMatchTypeCheck + ProjMatchPprint
end

-- Command line menu for TreePPL
let tpplMenu = lam. join [
  "Usage: tpplc program.tppl out.mc [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]

mexpr

-- (vsenderov, 2023-06-16 Changed this to CPPLLang as it is in the cppl command line)
-- use CorePPLUsage in
use CPPLLang in

-- Use the arg.mc library to parse arguments
let result = argParse default config in
match result with ParseOK r in
  let options: Options = r.options in
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (tpplMenu ());
    if gti (length r.strings) 1 then exit 1 else exit 0
  else
    match r.strings with [filename] in
    -- (vsenderov, 2023-06-16) until here the logic follows the cppl command line
    -- The data is an mcore file; that can be parsed with the bootparser
    -- However, now filename is a tppl program so it has to be parsed with TreePPLThings
    let content = readFile filename in
    use TreePPLThings in
    match parseTreePPLExn filename content with  file in
    let corePplAst: Expr = compile file in

    -- printLn (mexprPPLToString corePplAst);

    let prog: Expr = typeCheck corePplAst in
    let prog: Expr = lowerProj prog in

    let prog =  mexprCpplCompile options false prog in
    buildMExpr options prog

    -- let outName = sysTempFileMake () in
    -- writeFile outName (use MExpr in concat "mexpr\n" (mexprPPLToString prog));

    -- -- NOTE(2023-08-16,dlunde): Makes it possible to use the --output-mc cppl command line flag to output the compiled _CorePPL_ program
    -- (if options.outputMc then
    --   sysCopyFile outName (concat options.output ".mc"); ()
    -- else ());

    -- let msg = "Compilation from generated CorePPL code failed" in
    -- runCommandWithError
    --   ["cppl",
    --    "--output", options.output,
    --    outName] msg;

    -- sysDeleteFile outName;

    -- ()

