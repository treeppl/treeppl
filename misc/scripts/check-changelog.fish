#!/usr/bin/env fish

# === Library stuff ===

source (path resolve (status dirname))/repo-utils.fish

set -g repoNames
set -g repoUrls

function prsSince --argument-names repoName date
    set -l dir "$repoDir/other-repositories/$repoName"
    cd $dir
    git log --since=$date --grep="[ (]#[0-9]\+\b" --format=%s\
        | sed "s!^.*\?\(#[0-9]\+\).*!$repoName\1!"
end

function addRepo --argument-names repoName url
    set -ag repoNames $repoName
    set -ag repoUrls $url
    ensureRepo $repoName $url
end

function unmentionedPRs --argument-names date
    set -l files $argv[2..]

    set -l allPRs
    for repoName in $repoNames
        set -a allPRs (prsSince $repoName $date)
    end

    set -l allPRs (string join \n -- $allPRs | sort | uniq)
    set -l mentioned (grep '[a-zA-Z0-9-]\+#[0-9]\+' --only-matching --no-filename $files | sort | uniq)
    comm -2 -3 (string join \n $allPRs | psub) (string join \n $mentioned | psub)
end

function setKV --argument-names key value
    if not begin; echo $value | jq -Rs fromjson &>/dev/null; end
        # NOTE(vipa, 2025-07-21): Not valid json, quote it
        set value '"'(string join \n -- $value | jq -Rs '.')'"'
    end
    set -l cache $repoDir/other-repositories/cache.json
    if test -f $cache
        set -l result (jq -c '.["'$key'"] = '$value < $cache)
        echo $result > $cache
    else
        echo '{"'$key'":'$value'}' > $cache
    end
end

function getKV --argument-names key
    set -l cache $repoDir/other-repositories/cache.json
    jq -r '.["'$key'"]' < $cache | string collect
end

function getBody --argument-names pr
    if set -gq online
        string match --entire --regex --quiet '(?<repoName>[a-zA-Z0-9-]+)#(?<prNum>[0-9]+)' $pr
        or begin; echo "match 1 failed" >&2; exit 1; end
        set -l url $repoUrls[(contains --index $repoName $repoNames)]
        or begin; echo "contains failed" >&2; exit 1; end
        string match --entire --regex --quiet '[a-z]://(?<repo>.*?)\.git' $url
        or begin; echo "match 2 failed" >&2; exit 1; end
        set -l body (gh pr view --json body --jq .body --repo $repo $prNum | tr -d '\r' | jq -Rs '.')
        setKV $pr $body
        string collect $body
    else
        getKV $pr
    end
end

set -g headings
function collectBullets --argument-names pr
    set -l lines (string split \n -- (getBody $pr))
    if set -l idx (contains --index -- "## TreePPL Release Notes" $lines)
        for line in $lines[(math $idx + 1)..]
            switch $line
                case "###*"
                    set -l headingVar (string escape --style=var -- $line)
                    if not contains $headingVar $headings
                        set -a headings $headingVar
                    end
                    set -f currentHeading $headingVar
                case "##*" "---"
                    break
                case "- *" "\* *"
                    set -f addedBullet
                    set -q currentHeading || continue
                    set -ga $currentHeading "- ($pr) $(string match --regex --groups-only '. *(.*)' -- $line)"
            end
        end
        if not set -qf addedBullet
            echo $pr\t"(had changelog with no bullets)"
        end
    else
        echo $pr
    end
end

function showBullets
    for heading in $headings
        if not count $$heading >/dev/null
            continue
        end
        string unescape --style=var $heading
        string join \n -- $$heading
        echo
    end
end

function splitOffBullets
    for pr in $argv
        collectBullets $pr
    end
end

function mkGHLinkSed --argument-names i
    echo -- "-e"
    echo -n "s@"
    echo -n $repoNames[$i]"#\([0-9]\+\)@"
    echo -n '['$repoNames[$i]'#\1]('(echo $repoUrls[$i] | sed 's/.git$//')'/pull/\1)@'
    echo 'g'
end

function formatLinks
    set -l cmd
    sed (for i in (seq (count $repoNames)); mkGHLinkSed $i; end)
end


# === Application ===

set -l startDate 2026-03-13

if contains -- --offline $argv
    set -ge online
end

set -l extraPRs (string match --invert --entire -- --offline $argv)

addRepo miking https://github.com/miking-lang/miking.git
addRepo dppl https://github.com/miking-lang/miking-dppl.git

echo 
splitOffBullets (begin; unmentionedPRs $startDate $repoDir/../CHANGELOG.md $repoDir/scripts/(status basename); string join \n -- $extraPRs; end) | formatLinks
echo
showBullets | formatLinks


# These are to be ignored:
# dppl#224
# dppl#226 (changes to how tests are run. Relevant for developers, but not for users?)
# dppl#227 (flake lock)
# dppl#232 (rename to avoid conflict with hoas.mc)
# dppl#233 (debug flag in cppl only)
# dppl#235 (recognize .cppl files in loader)
# miking#975 (docgen)
# miking#980 (invariant checking in --debug-phases)
# miking#982 (javascript test fixes)
# miking#983 (filesystem utilities)
# miking#985 (test-spec.mc)
# miking#987 (update ocamlformat)
# miking#988 (eqset.mc updates)
# miking#989 (subseqFindIdx function)
# miking#990 (fix test-spec.mc with tup with _many_ arguments)
# miking#991 (bump github action)
# miking#992 (shallow.mc introduce fewer functions)
# miking#993 (effects.mc test parallelism fix)
# miking#994 (test-spec.mc --watch ignore hidden files)
# treeppl#142 (add invariants to tpplc. Maybe relevant to developers, but not for users?)
# treeppl#144 (moved documentation of hostrep into repo, same as the others)
# treeppl#145 (v0.3)
# treeppl#146 (test-spec.mc. Relevant for developers, but not for users?)
# treeppl#152 (make sure --debug-mcmc is used in tests)
# treeppl-python#17 (v0.3)
# treeppl-python#19 (add import for trees.tppl)
# treeppl-python#20 (move import to lib/trees.tppl)
# treeppl-python#21 (remove import of lib/trees.tppl)
# dppl#237 (dppl typechecker, not used for tppl stuff)
# miking#995 (backend stuff, used in the graph translation)
# miking#996 (backend stuff, used in the graph translation)
# miking#997 (ast-to-json cases)
# miking#998 (free variables bugfix, used in graph translation)
# treeppl#154 (ast-to-json cases)
# treeppl#160 (release v0.4)
# treeppl-python#22 (release v0.4)
# treepplr#1 (Not entirely sure, but before the big remake)
# treepplr#9 (Not entirely sure, but before the big remake)
# treeppl#167 (Less precise tests)
# treeppl#168 (record gnu awk dependency in tests)
# treeppl#170 miking#1005 dppl#248 (loader update, only minor internal changes)
# dppl#244 (User relative paths in includes in coreppl instead of `coreppl::`)
# dppl#247 miking#1004 (Unmatched pattern change via `never`)
# miking#1000 (new parser in miking)
# miking#1001 (more flexible running of the test suite)
# miking#1002 (`exec` in miking)
# miking#1003 (nicer runing of tup, with exec instead of xargs)
# miking#1008 (nicer printing when running tests with make)
# miking#1009 (eliminate duplicate code bug, don't remember what the impact was from tppl point of view)
# treepplr#11 (new trepplr api, I _think_ this has been talked about elsewhere)
# treepplr#29 (fix to make it run in the test suite)
# miking#1014, dppl#250 (nix flake)
# miking#1007 (new parser in miking)
# miking#1013 (new interpreter in miking)
# miking#1015 (transpose function in miking)
# miking#1018 (fix `--tag` in `make` mode)
# miking#1019 (generalize type of `mapEq`)
# treeppl-python#24 (release 0.5)


# == stuff for the new graph, eventually ==
# miking#1016 (pass that tracks instantiations in the AST)
# miking#1017 (peval graph, v1)
