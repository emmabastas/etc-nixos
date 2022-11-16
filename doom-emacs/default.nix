{ callPackage
, multimarkdown
, hunspellWithDicts
, hunspellDicts
}:

let
  hunspell = hunspellWithDicts ( with hunspellDicts; [ sv-se  en-us ] );
in

callPackage (builtins.fetchTarball {
  url = https://github.com/nix-community/nix-doom-emacs/archive/c38ccd08345f58001cac2c2578e71d3f29b59bc0.tar.gz;
  sha256 = "1k4wxn84hgxmr2ddbmybnqqih07r22d3qfyx5rjl9fzw2p5kkxhk";
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
