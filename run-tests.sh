#!/bin/sh -e

if [ -n "$TRAVIS" ]; then
    cd "$(dirname "$0")"

    ECUKES_EMACS=${EMACS:-$(which emacs)}
    export ECUKES_EMACS

    echo "*** Emacs version ***"
    echo "ECUKES_EMACS = $ECUKES_EMACS"
    "$ECUKES_EMACS" --version
    echo
fi

cask exec ert-runner -L . -L test "$@"
