{ callPackage }:

callPackage (builtins.fetchTarball {
  url = https://github.com/nix-community/nix-doom-emacs/archive/master.tar.gz;
}) {
  doomPrivateDir = ./.;
}
