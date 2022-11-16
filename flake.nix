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
                (import ./emma)
              ];
            };
          }
          ({ config, pkgs, ... }: (utils.recursiveMerge [
            {
              imports = [ utils.hardwareConfig ];
            }
            {
              users.users.emma = {
                isNormalUser = true;
                extraGroups = [ "wheel" "networkmanager" ];
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
