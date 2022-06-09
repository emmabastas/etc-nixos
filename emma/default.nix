{ pkgs, ... }:

let
  callPackage = pkgs.callPackage;

  vim = (callPackage ./vim.nix {});
in
{
  home.packages = [
    (callPackage ./st {})
    (callPackage ./doom-emacs {})
    (callPackage /home/emma/nixpkgs/pkgs/tools/security/spectre-cli {})
    vim
    pkgs.firefox
    pkgs.chromium
    pkgs.megacmd
    (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
  programs = {
    ssh.enable = true;
    bash.enable = true;
    git = {
      enable = true;
      userName = "emmabastas";
      userEmail = "emma.bastas@protonmail.com";
      extraConfig = {
        core.editor = "${vim.out}/bin/vim";
        init.defaultBranch = "main";
      };
      ignores = [ "*.swp" ];
    };
  };
  home.file.".config/i3/config".source = ./i3.conf;
  fonts.fontconfig.enable = true;
}
