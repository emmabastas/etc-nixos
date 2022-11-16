{ pkgs, ... }:

let
  callPackage = pkgs.callPackage;

  spectre-cli = (callPackage ./spectre-cli.nix {});

  vim = (callPackage ./vim.nix {});

  direnv = pkgs.direnv;

  firefox = pkgs.firefox;

  chromium = pkgs.chromium;

  shellScript = cmd: {
    text = ''
      #!/bin/sh
      ${cmd}
    '';
    executable = true;
  };

  applicationScript = cmd: shellScript ''
    (${cmd} &)
    kill $(expr $PPID - 1)
  '';
in
{
  home.packages = [
    (callPackage ./st {})
    spectre-cli
    vim
    direnv
    firefox
    pkgs.megacmd
    pkgs.signal-desktop
    pkgs.feh
    pkgs.gimp
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
    #doom-emacs = {
    #  enable = true;
    #  doomPrivateDir = ./doom-emacs;
    #  extraPackages = with pkgs; [
    #    graphviz # Used by org-roam to render notes as a graph
    #  ];
    #  extraConfig = with pkgs; ''
    #    (setq org-roam-graph-executable "${graphviz.out}/bin/dot")
    #  '';
    #};
  };
  #services.emacs = {
  #  enable = true;
  #};
  home.shellAliases = {
    spectre = ''SPECTRE_USERNAME="emmabastas" ${spectre-cli}/bin/spectre -q'';
    spectre_ = ''${spectre-cli}/bin/spectre -q'';
  };
  home.file = {
    ".config/i3/config".source = ./i3.conf;
    ".config/nix/nix.conf".text = ''experimental-features = nix-command flakes'';
    "bin/emacs" = applicationScript "emacsclient -cn $@";
    "bin/emacs-debug" = shellScript "emacs-28.1 -l /etc/nixos/emma/doom-emacs/config.el $@";
    "bin/firefox" = applicationScript "${firefox}/bin/firefox $@";
    "bin/chromium" = applicationScript "${chromium}/bin/chromium $@";
  };
  fonts.fontconfig.enable = true;
}
