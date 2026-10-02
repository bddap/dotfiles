# Framework Laptop 16 (Ryzen AI 300) with the NVIDIA GPU module: the
# nixos-hardware profile with the open kernel module and PRIME render
# offload (the AMD iGPU drives the panel; NVIDIA renders on request).
# Import from nix/nixos/local/default.nix.
#
# The profile's PRIME bus IDs are defaults; expansion cards and drives can
# shift them. If `lspci | grep -E "VGA|3D|Display"` disagrees, set
# hardware.nvidia.prime.{nvidiaBusId,amdgpuBusId} in local/ (hex c2 is
# "PCI:194:0:0").
let sources = import ../nix/sources.nix;
in {
  imports = [
    (sources.nixos-hardware + "/framework/16-inch/amd-ai-300-series/nvidia")
  ];
}
