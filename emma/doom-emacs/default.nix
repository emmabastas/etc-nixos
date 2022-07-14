{ callPackage
, multimarkdown
, hunspellWithDicts
, hunspellDicts
}:

let
  hunspell = hunspellWithDicts ( with hunspellDicts; [ sv-se  en-us ] );
in

callPackage (builtins.fetchTarball {
  url = https://github.com/nix-community/nix-doom-emacs/archive/master.tar.gz;
}) {
  doomPrivateDir = ./.;

  extraPackages = [
    multimarkdown   # Previews in markdown mode
    #hunspell        # Used by the `spell` module
  ];

  extraConfig = ''
    (setq exec-path (append exec-path '("${multimarkdown}/bin")))
    ;(setq exec-path (append exec-path '("${hunspell}/bin")))
  '';
}
