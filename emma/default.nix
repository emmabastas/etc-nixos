{ pkgs, ... }:

let
  vim = (pkgs.callPackage ./vim.nix {});
in
{
  home.packages = with pkgs; [
    (callPackage ./st {})
    vim
    (callPackage ./doom-emacs {})
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    pkgs.firefox
    chromium
    (callPackage /home/emma/nixpkgs/pkgs/tools/security/spectre-cli {})
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
  home.file.".config/i3/config".text = builtins.readFile ./i3.conf;
  fonts.fontconfig.enable = true;
}
