-- Helpful docs:
--   - https://venam.nixers.net/blog/unix/2021/06/23/pipewire-under-the-hood.html
--   - https://gitlab.freedesktop.org/pipewire/wireplumber/-/blob/f0166d6b3c6ac1c4d6351dc7a732d06117cd9b60/tests/examples/get-default-sink-volume.lua

rule = {
  matches = {
    {
      { "node.name", "equals", "alsa_output.pci-0000_00_1f.3.analog-stereo" },
      { "media.class", "equals", "Audio/Sink" },
    },
  },
  apply_properties = {
    ["node.description"] = "laptop speakers",
  },
}
table.insert(alsa_monitor.rules, rule)
rule = {
  matches = {
    {
      { "node.name", "equals", "alsa_input.pci-0000_00_1f.3.analog-stereo" },
      { "media.class", "equals", "Audio/Source" },
    },
  },
  apply_properties = {
    ["node.description"] = "laptop mic",
  },
}
table.insert(alsa_monitor.rules, rule)


rule = {
  matches = {
    {
      { "node.name", "equals", "alsa_output.usb-Kingston_HyperX_Cloud_Flight_S_000000000001-00.analog-stereo" },
      { "media.class", "equals", "Audio/Sink" },
    },
  },
  apply_properties = {
    ["node.description"] = "wireless headphones",
  },
}
table.insert(alsa_monitor.rules, rule)


rule = {
  matches = {
    {
      { "node.name", "equals", "alsa_input.usb-Kingston_HyperX_Cloud_Flight_S_000000000001-00.mono-fallback" },
      { "media.class", "equals", "Audio/Source" },
    },
  },
  apply_properties = {
    ["node.description"] = "wireless headphones",
  },
}
table.insert(alsa_monitor.rules, rule)


load_script("set-defaults.lua")
