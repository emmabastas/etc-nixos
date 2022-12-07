{
  description = "emmabastas system configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
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
      nixos = lib.nixosSystem {
        inherit system;

        modules = [
          home-manager.nixosModules.home-manager {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.emma = lib.mkMerge [
                nix-doom-emacs.hmModule
                ({ pkgs, ... }: utils.recursiveMerge [
                  ((import ./emma) { pkgs = pkgs; })
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
                    programs.doom-emacs.extraPackages = [ pkgs.graphviz ];
                  }
                  {
                    programs.doom-emacs.extraPackages = [ pkgs.ditaa ];
                  }
                  {
                    programs.doom-emacs.extraPackages = [ pkgs.megacmd ];
                  }
                  {
                    programs.doom-emacs = {
                      extraConfig = ''
                        (setq org-roam-graph-executable "${pkgs.graphviz.out}/bin/dot")
                        ;(add-hook! 'org-babel-before-execute-hook
                                   ;(lambda () (setq org-ditaa-jar-path "${pkgs.ditaa.out}/lib/ditaa.jar")))
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
              i18n.defaultLocale = "en_US.UTF-8";
              console.keyMap = "sv-latin1";
              services.xserver.layout = "se";
              time.timeZone = "Europe/Stockholm";
            }
            {
              networking.hostName = "nixos";
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
                useGlamor = true;
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
              nixpkgs.config.allowUnfree = true;
              environment.systemPackages = [ pkgs.mathematica ];
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
            
            {
              # This value determines the NixOS release from which the default
              # settings for stateful data, like file locations and database versions
              # on your system were taken. It‘s perfectly fine and recommended to leave
              # this value at the release version of the first install of this system.
              # Before changing this value read the documentation for this option
              # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
              system.stateVersion = "21.11"; # Did you read the comment?
            }
          ]))
        ];
      };
    };
  };
}
