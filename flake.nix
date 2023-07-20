{
  description = "emmabastas system configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
  };

  outputs = { self, nixpkgs, home-manager, nix-doom-emacs, ... }:
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
    };

    lib = nixpkgs.lib;

    utils = (import ./utils.nix) lib;

  in {
    nixosConfigurations = {
      acomputer = lib.nixosSystem {
        inherit system;

        modules = [
          home-manager.nixosModules.home-manager {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.emma =
              let
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
              lib.mkMerge [
                nix-doom-emacs.hmModule
                ({ pkgs, ... }: utils.recursiveMerge [
                  {
                    home.stateVersion = "21.11";
                  }
                  {
                    programs.doom-emacs = {
                      doomPrivateDir = pkgs.linkFarm "doom-config" [
                        { name = "config.el"; path = ./doom-emacs/config.el; }
                        { name = "init.el";   path = ./doom-emacs/init.el; }
                        # Should *not* fail because we're building our straight environment
                        # using the doomPackageDir, not the doomPrivateDir.
                        {
                          name = "packages.el";
                          path = pkgs.writeText "packages.el" "(package! not-a-valid-package)";
                        }
                      ];
                      doomPackageDir = pkgs.linkFarm "doom-config" [
                        # straight needs a (possibly empty) `config.el` file to build
                        { name = "config.el";   path = pkgs.emptyFile; }
                        { name = "init.el";     path = ./doom-emacs/init.el; }
                        { name = "packages.el"; path = ./doom-emacs/packages.el; }
                      ];
                    };
                  }
                  {
                    programs.doom-emacs.enable = true;
                    services.emacs.enable = true;
                  }
                  {
                    home.file = {
                      "bin/emacs" = applicationScript "emacsclient -cn $@";
                      "bin/emacs-debug" = shellScript "emacs-28.1 -l /home/emma/etc-nixos/doom-emacs/config.el $@";
                    };
                  }
                  {
                    programs.doom-emacs.extraPackages = [ pkgs.graphviz ];
                  }
                  {
                    programs.doom-emacs.extraPackages = [ pkgs.unzip ];
                  }
                  {
                    home.packages = [ pkgs.nodePackages.pyright ];
                  }
                  {
                    home.packages = [ pkgs.megacmd ];
                  }
                  {
                    home.packages = [ pkgs.mullvad ];
                  }
                  {
                    home.packages = [ pkgs.gimp ];
                  }
                  {
                    home.packages = [ pkgs.feh ];
                  }
                  {
                    home.packages = [ pkgs.tealdeer ];
                  }
                  {
                    home.packages = [ pkgs.zip pkgs.unzip ];
                  }
                  {
                    home.packages = [ (pkgs.callPackage ./webwork-flashcard {}) ];
                  }
                  {
                    home.packages = [
                      pkgs.ghc
                      pkgs.haskell-language-server
                    ];
                  }
                  {
                    home.packages = [ pkgs.spectre-cli ];
                    home.shellAliases = {
                      spectre = ''SPECTRE_USERNAME="emmabastas" ${pkgs.spectre-cli}/bin/spectre -q'';
                      spectre_ = ''${pkgs.spectre-cli}/bin/spectre -q'';
                    };
                  }
                  (
                  let
                    firefox = pkgs.firefox;
                  in
                  {
                    home.packages = [ firefox ];
                    home.file."bin/firefox" = applicationScript "${firefox}/bin/firefox $@";
                  }
                  )
                  {
                    home.packages = [ (pkgs.callPackage ./vim-cli.nix {}) ];
                  }
                  {
                    home.packages = [ pkgs.direnv ];
                  }
                  {
                    home.packages = [ (pkgs.callPackage ./st {}) ];
                  }
                  {
                    home.packages = [ (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; }) ];
                    fonts.fontconfig.enable = true;
                  }
                  {
                    programs.ssh.enable = true;
                  }
                  {
                    programs.bash = {
                      enable = true;
                      bashrcExtra = ''
                        export PATH=$HOME/bin:$PATH
                        eval "$(${pkgs.direnv}/bin/direnv hook bash)"
                      '';
                    };
                  }
                  {
                    programs.git = {
                      enable = true;
                      userName = "emmabastas";
                      userEmail = "emma.bastas@protonmail.com";
                      extraConfig = {
                        core.editor = "${(pkgs.callPackage ./vim-cli.nix {}).out}/bin/vim";
                        init.defaultBranch = "main";
                      };
                      ignores = [ "*.swp" ];
                    };
                  }
                  {
                    home.file.".config/i3/config".source = ./i3.conf;
                  }
                  {
                    home.file.".config/nix/nix.conf".text = ''experimental-features = nix-command flakes'';
                  }
                  {
                    programs.doom-emacs = {
                      extraConfig = ''
                        (setq org-roam-graph-executable "${pkgs.graphviz.out}/bin/dot")
                        (setq nov-unzip-program "${pkgs.unzip.out}/bin/unzip")
                      '';
                    };
                  }
                ])
              ];
              users.foo =
              let
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
              lib.mkMerge [
                nix-doom-emacs.hmModule
                ({ pkgs, ... }: utils.recursiveMerge [
                  {
                    home.stateVersion = "21.11";
                  }
                  {
                    };
                  }
                  {
                    home.packages = [ pkgs.spectre-cli ];
                  }
                  {
                    programs.git = {
                      enable = true;
                      extraConfig = {
                        core.editor = "${(pkgs.callPackage ./vim-cli.nix {}).out}/bin/vim";
                        init.defaultBranch = "main";
                      };
                      ignores = [ "*.swp" ];
                    };
                  }
                  {
                    home.packages = [ pkgs.mullvad-browser ];
                    home.file."bin/mullvad-browser" = applicationScript "${pkgs.mullvad-browser}/bin/mullvad-browser $@";
                  }
                  {
                    programs.doom-emacs = {
                      doomPrivateDir = pkgs.linkFarm "doom-config" [
                        { name = "config.el"; path = ./doom-emacs/config.el; }
                        { name = "init.el";   path = ./doom-emacs/init.el; }
                        # Should *not* fail because we're building our straight environment
                        # using the doomPackageDir, not the doomPrivateDir.
                        {
                          name = "packages.el";
                          path = pkgs.writeText "packages.el" "(package! not-a-valid-package)";
                        }
                      ];
                      doomPackageDir = pkgs.linkFarm "doom-config" [
                        # straight needs a (possibly empty) `config.el` file to build
                        { name = "config.el";   path = pkgs.emptyFile; }
                        { name = "init.el";     path = ./doom-emacs/init.el; }
                        { name = "packages.el"; path = ./doom-emacs/packages.el; }
                      ];
                    };
                  }
                  {
                    programs.doom-emacs.enable = true;
                    services.emacs.enable = true;
                  }
                  {
                    home.file = {
                      "bin/emacs" = applicationScript "emacsclient -cn $@";
                      "bin/emacs-debug" = shellScript "emacs-28.1 -l /home/emma/etc-nixos/doom-emacs/config.el $@";
                    };
                  }
                  {
                    home.packages = [ pkgs.nodePackages.pyright ];
                  }
                  {
                    home.packages = [ pkgs.mullvad ];
                  }
                  {
                    home.packages = [ pkgs.tealdeer ];
                  }
                  {
                    home.packages = [ pkgs.zip pkgs.unzip ];
                  }
                  {
                    home.packages = [
                      pkgs.ghc
                      pkgs.haskell-language-server
                    ];
                  }
                  {
                    home.packages = [ (pkgs.callPackage ./vim-cli.nix {}) ];
                  }
                  {
                    home.packages = [ pkgs.direnv ];
                  }
                  {
                    home.packages = [ (pkgs.callPackage ./st {}) ];
                  }
                  {
                    home.packages = [ (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; }) ];
                    fonts.fontconfig.enable = true;
                  }
                  {
                    programs.ssh.enable = true;
                  }
                  {
                    programs.bash = {
                      enable = true;
                      bashrcExtra = ''
                        export PATH=$HOME/bin:$PATH
                        eval "$(${pkgs.direnv}/bin/direnv hook bash)"
                      '';
                    };
                  }
                  {
                    home.file.".config/i3/config".source = ./i3.conf;
                  }
                  {
                    home.file.".config/nix/nix.conf".text = ''experimental-features = nix-command flakes'';
                  }
                  {
                    programs.doom-emacs = {
                      extraConfig = ''
                      '';
                    };
                  }
                ])
              ];
            };
          }
          ({ config, pkgs, ... }: (utils.recursiveMerge [
            {
              imports = [ utils.hardwareConfig ];

              users.users.emma = {
                isNormalUser = true;
                extraGroups = [ "wheel" ];
              };

              users.users.foo = {
                isNormalUser = true;
                extraGroups = [ "wheel" ];
              };

              system.stateVersion = "21.11";
            }
            {
              # Use the systemd-boot EFI boot loader.
              boot.loader.systemd-boot.enable = true;
              boot.loader.efi.canTouchEfiVariables = true;
            }
            {
              networking.networkmanager.enable = true;
            
              # The global useDHCP flag is deprecated, therefore explicitly set to false here.
              # Per-interface useDHCP will be mandatory in the future, so this generated config
              # replicates the default behaviour.
              networking.useDHCP = false;
              networking.interfaces.enp3s0.useDHCP = true;
              networking.interfaces.wlp2s0.useDHCP = true;
            
              # Open ports in the firewall.
              # networking.firewall.allowedTCPPorts = [ ... ];
              # networking.firewall.allowedUDPPorts = [ ... ];
              # Or disable the firewall altogether.
              networking.firewall.enable = false;
            }
            {
              users.users.emma.extraGroups = [ "networkmanager" ];
            }
            {
              services.mullvad-vpn.enable = true;
            }
            {
              i18n.defaultLocale = "en_US.UTF-8";
              console.keyMap = "sv-latin1";
              services.xserver.layout = "se";
              time.timeZone = "Europe/Stockholm";
            }
            {
              networking.hostName = "acomputer";
            }
            {
              console = {
                earlySetup = true;
                font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
                packages = with pkgs; [ terminus_font ];
              };
            }
            {
              services.xserver = {
                videoDrivers = [ "modesetting" ];
                dpi = 220;
              };
            }
            {
              services.xserver = {
                desktopManager.xterm.enable = false;
            
                displayManager = {
                  defaultSession = "none+i3";
                };
            
                windowManager.i3 = {
                  enable = true;
                };
              };
            }
            {
              environment.systemPackages = [ pkgs.vim ];
            }
            {
              environment.systemPackages = [ pkgs.brightnessctl ];
            }
            {
              #nixpkgs.config.allowUnfree = true;
              #environment.systemPackages = [ pkgs.mathematica ];
            }
            {
              #nixpkgs.config.allowUnfree = true;
              #services.teamviewer.enable = true;
            }
            {
              sound.enable = true;
              hardware.pulseaudio.enable = true;
            }
            {
              services.xserver.libinput.enable = true;
            }
            {
              services.xserver = {
                enable = true;
                autorun = true;
              };
            }
            {
              services.openssh.enable = true;
            }
            
          ]))
        ];
      };
    };
  };
}
