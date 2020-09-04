#!/usr/bin/env sh

set -e
set pipefail

getTarget() {
    case $(uname) in
        "Darwin") OS="darwin";;
        "FreeBSD") OS="freebsd";;
        "Linux") OS="linux";;
    esac
    case $(uname -m) in
        "armv7l") ARCH="arm";;
        *) ARCH="$(uname -m)";;
    esac
    echo "$ARCH-$OS-dist.tar.gz"
}

addBin() {

    printf 'export PATH=$HOME/.local/bin:$PATH' >> "$HOME"/.bashrc
    export PATH=$HOME/.local/bin:$PATH

}

main() {

    checkpoint=$(pwd)

    latest="$(curl -s https://github.com/vmchale/dickinson/releases/latest/ | cut -d'"' -f2 | rev | cut -d'/' -f1 | rev)"
    dist=$(getTarget)

    dest=$(mktemp -d)

    if command -v wget > /dev/null ; then
        wget https://github.com/vmchale/dickinson/releases/download/"$latest"/"$dist" -O "$dest"/"$dist"
    else
        curl -L https://github.com/vmchale/dickinson/releases/download/"$latest"/"$dist" -o "$dest"/"$dist"
    fi

    cd "$dest"
    tar xvf "$dist"
    cd language-dickinson-*
    make install

    cd "$checkpoint"

    case :$PATH: in
        *:$HOME/.local/bin:*) ;;
        *) addBin ;;
    esac

}

main
