# Shared helpers for downloading external repositories and keeping them
# up to date. Repositories are cloned into other-repositories/<name> at
# the project root (which is git-ignored), and left in place between runs
# so subsequent calls only need to pull.

set -g repoDir (path resolve (status dirname)/..)

set -g online yes

function ensureRepo --argument-names repoName url
    set -l dir "$repoDir/other-repositories/$repoName"
    if not test -d $dir
        git clone $url $dir || exit 1
    end
    cd $dir
    set -qg online && begin; git pull --force || exit 1; end
end
