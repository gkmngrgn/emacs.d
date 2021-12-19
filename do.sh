#!/usr/bin/env bash

print_help() {
    echo "Subcommands:"
    echo "  > install       Install Emacs with default parameters."
    echo "  > install_nox   Install Emacs without GUI support."
}

check_prerequisites() {
    sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
    sudo apt update -y
    sudo apt install -y autoconf         \
                        make             \
                        checkinstall     \
                        pkg-config       \
                        texinfo          \
                        libgnutls28-dev  \
                        libncurses5-dev  \
                        libjansson-dev   \
                        libgccjit-10-dev \
                        gcc-10           \
                        g++-10           \
                        zlib1g-dev

    if [ ! -d "emacs" ]; then
        git clone -b emacs-28 --single-branch --depth=1 https://git.savannah.gnu.org/git/emacs.git emacs
    else
        cd emacs
        git fetch --all
        git merge origin/emacs-28
        cd ..
    fi
}

build_and_install_emacs() {
    params="--prefix=$HOME/.local --with-native-compilation"
    if [[ ${1} == '--enable-x' ]]; then
        params="
            $params                 \
            --with-x-toolkit=gtk3   \
            --with-xpm=ifavailable  \
            --with-jpeg=ifavailable \
            --with-gif=ifavailable  \
            --with-tiff=ifavailable "
    else
        params="
            $params             \
            --without-x-toolkit \
            --without-xft       \
            --without-x         "
    fi

    cd emacs
    ./autogen.sh
    ./configure $params
    make -j$(nproc)
    make install
    cd ..
}


install() {
    check_prerequisites
    sudo apt install -y libgtk-3-dev
    build_and_install_emacs --enable-x
}


install_nox() {
    check_prerequisites
    build_and_install_emacs
}


if [ -z ${1} ]
then
    print_help
else
    ${@}
fi
