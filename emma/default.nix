{ pkgs, ... }:

{
  home.packages = with pkgs; [
    (callPackage ./st {})
    (callPackage ./vim.nix {})
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    pkgs.firefox
    (callPackage /home/emma/nixpkgs/pkgs/tools/security/spectre-cli {})
  ];
  programs = {
    ssh.enable = true;
    bash.enable = true;
    git = {
      enable = true;
      userName = "emmabastas";
      userEmail = "emma.bastas@protonmail.com";
      extraConfig.init.defaultBranch = "main";
    };
  };
  home.file.".config/i3/config".text = builtins.readFile ./i3.conf;
  fonts.fontconfig.enable = true;
}
