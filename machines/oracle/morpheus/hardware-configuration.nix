{ modulesPath, ... }:
{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
    device = "nodev";
  };
  fileSystems."/boot" = { device = "/dev/disk/by-uuid/61E0-20B8"; fsType = "vfat"; };
  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = { device = "/dev/sda3"; fsType = "xfs"; };
}
