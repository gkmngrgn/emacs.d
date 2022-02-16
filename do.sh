#!/usr/bin/env bash

print_help() {
    echo "Subcommands:"
    echo "  > install       Install Emacs with default parameters."
}

check_prerequisites() {
    sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
    sudo apt update -y
    sudo apt install -y autoconf         \
                        make             \
                        checkinstall     \
                        pkg-config       \
                        texinfo          \
                        libgif-dev       \
                        libgtk-3-dev     \
                        libgnutls28-dev  \
                        libncurses5-dev  \
                        libjansson-dev   \
                        libgccjit-10-dev \
                        libxpm-dev       \
                        gcc-10           \
                        g++-10           \
                        zlib1g-dev

    branch="master"
    if [ ! -d "emacs" ]; then
        git clone -b $branch --single-branch --depth=1 https://git.savannah.gnu.org/git/emacs.git emacs
    else
        cd emacs
        git fetch --all
        git merge origin/$branch
        cd ..
    fi
}

build_and_install_emacs() {
    cd emacs
    ./autogen.sh
    ./configure --with-native-compilation --with-x-toolkit=gtk3
    make -j$(nproc)
    sudo make install
    cd ..
}


install() {
    check_prerequisites
    build_and_install_emacs
}

if [ -z ${1} ]
then
    print_help
else
    ${@}
fi
