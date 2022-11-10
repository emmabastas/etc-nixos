{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];



  ########
  # Boot #
  ########

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;



  ##############
  # Networking #
  ##############

  networking.hostName = "nixos";
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


  ##############################
  # Localization, timezone etc #
  ##############################

  time.timeZone = "Europe/Stockholm";
  i18n.defaultLocale = "en_US.UTF-8";


  #######
  # TTY #
  #######

  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    packages = with pkgs; [ terminus_font ];
    keyMap = "sv-latin1";
  };


  #######
  # X11 #
  #######

  services.xserver = {
    enable = true;
    autorun = true;
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
    dpi = 220;
    layout = "se";

    desktopManager.xterm.enable = false;

    displayManager = {
      defaultSession = "none+i3";
    };

    windowManager.i3 = {
      enable = true;
    };
  };

  # These options could help if I experience graphics issues, the
  # per would be lower though
  # services.xserver.videoDrivers = [ "intel" ];
  # services.xserver.deviceSection = ''
  #   Option "DRI" "2"
  #   Option "TearFree" "true"
  # '';


  #########
  # Sound #
  #########

  sound.enable = true;
  hardware.pulseaudio.enable = true;


  ############
  # Touchpad #
  ############

  services.xserver.libinput.enable = true;


  ##############
  # Teamviewer #
  ##############

  nixpkgs.config.allowUnfree = true;
  services.teamviewer.enable = true;


  ###################
  # System packages #
  ###################

  environment.systemPackages = with pkgs; [
    # brightnesscta s +10%
    # brightnessctl s 10%-
    brightnessctl
    vim
  ];


  #########
  # Users #
  #########

  users.users.emma = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  #fonts.fontDir.enable = true;
  #fonts.fonts = with pkgs; [
  #  (nerdfonts.override { fonts = [ "FiraCode" ]; })
  #];


  ########
  # Misc #
  ########

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
