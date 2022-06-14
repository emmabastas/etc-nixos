{ callPackage, multimarkdown }:

callPackage (builtins.fetchTarball {
  url = https://github.com/nix-community/nix-doom-emacs/archive/master.tar.gz;
}) {
  doomPrivateDir = ./.;

  # Used for previews in markdown mode
  extraPackages = [ multimarkdown ];

  extraConfig = ''
    (setq markdown-command "${multimarkdown}/bin/multimarkdown")
  '';
}
