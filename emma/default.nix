{ pkgs, emacs }:

let
  callPackage = pkgs.callPackage;

  spectre-cli = (callPackage /home/emma/nixpkgs/pkgs/tools/security/spectre-cli {});

  vim = (callPackage ./vim.nix {});

  direnv = pkgs.direnv;

  firefox = pkgs.firefox;

  applicationScript = cmd: {
    text = ''
      #!/bin/sh
      (${cmd} &)
      kill $(expr $PPID - 1)
    '';
    executable = true;
  };
in
{
  home.packages = [
    (callPackage ./st {})
    spectre-cli
    vim
    direnv
    firefox
    pkgs.chromium
    pkgs.megacmd
    (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
  programs = {
    ssh.enable = true;
    bash = {
      enable = true;
      bashrcExtra = ''
        export PATH=$HOME/bin:$PATH
        eval "$(${direnv}/bin/direnv hook bash)"
      '';
    };
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
  home.shellAliases = {
    spectre = ''SPECTRE_USERNAME="emmabastas" ${spectre-cli}/bin/spectre -q'';
    spectre_ = ''${spectre-cli}/bin/spectre -q'';
  };
  home.file = {
    ".config/i3/config".source = ./i3.conf;
    ".config/nix/nix.conf".text = ''experimental-features = nix-command flakes'';
    "bin/emacs" = applicationScript "${emacs}/bin/emacsclient -cn $@";
    "bin/firefox" = applicationScript "${firefox}/bin/firefox $@";
  };
  fonts.fontconfig.enable = true;
}
