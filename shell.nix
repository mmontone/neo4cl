with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "neo4cl";

    buildInputs = [
        # Lisp env
        pkgs.openssl
        pkgs.sbcl
    ];

    env = buildEnv {
        name = name;
        paths = buildInputs;
    };

    LD_LIBRARY_PATH = lib.makeLibraryPath [
        pkgs.openssl
    ];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix neo4cl] \\w\\$\\[\\033[00m\\] '";
}
