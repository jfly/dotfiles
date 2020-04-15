import re
import subprocess
from dataclasses import dataclass
from typing import Dict, List, Set

from .types import BluetoothDevice, MacAddress


@dataclass
class RegistryNode:
    subkeys: Set[str]
    value_by_name: Dict[str, bytes]

def _run_registry_commands(cmds):
    stdin = "\n".join(cmds + ['q']) + '\n'
    stdout = subprocess.check_output("inspect-windows-registry.sh", input=stdin, stderr=subprocess.DEVNULL, text=True)
    return stdout

def _de_hex_dump(hex_dump):
    # Parse the various pieces of a hex dump. The hex dumps look something like this:
    #
    # :00000  62 79 30 A8 BB 8F A0 85 41 D3 F6 0F E5 C9 26 23 by0.....A.....&#
    # :00010  20 42 6C 75 65 74 6F 6F 74 68 20 4B 65 79 62 6F  Bluetooth Keybo
    # ...

    data_bytes = bytes()
    prefix = ":00000  "
    suffix = " by0.....A.....&#"
    for line in hex_dump.split("\n"):
        hex_pairs = line[len(prefix):-len(suffix)].split()
        data_bytes += bytes(int(hex_pair, 16) for hex_pair in hex_pairs)
    return data_bytes

def _read_registry_value(path, value_name) -> bytes:
    stdout = _run_registry_commands([f"cd {path}", f"hex {value_name}"])

    # The output of hex looks something like this:
    #
    # ...
    # (...)\BTHPORT\Parameters\Keys\f894c22fa97b> Value <28c13ccc04de> of type REG_BINARY (3), data length 16 [0x10]
    # :00000  62 79 30 A8 BB 8F A0 85 41 D3 F6 0F E5 C9 26 23 by0.....A.....&#

    registry_value_re = re.compile(fr"""Value <{value_name}> of type.*, data length (?P<data_length>\d+).*
(?P<data_hex_dump>(.+\n)+)""")
    match = registry_value_re.search(stdout)
    assert match is not None
    data_length = int(match.group(1))
    data = _de_hex_dump(match.group('data_hex_dump'))
    assert len(data) == data_length

    return data

def _read_registry_path(path) -> RegistryNode:
    stdout = _run_registry_commands([f"cd {path}", "dir"])

    # The output of dir looks something like this:
    #
    # ...
    # (...)\BTHPORT\Parameters\Keys\f894c22fa97b> Node has 1 subkeys and 5 values
    #   key name
    #   <eff6de0b1412>
    #   size     type              value name             [value if type DWORD]
    #     16  3 REG_BINARY         <28c13ccc04de>
    #     16  3 REG_BINARY         <dcaf68e16c36>
    #     16  3 REG_BINARY         <MasterIRK>
    #     16  3 REG_BINARY         <2811a5368333>
    #     16  3 REG_BINARY         <907f610383fc>
    # ...

    registry_dir_re = re.compile(r"""Node has \d+ subkeys and \d+ values
  key name
(?P<subkeys>(.|\n)*)
  size     type              value name             \[value if type DWORD\]
(?P<values>(?:.+\n)+)
""")
    match = registry_dir_re.search(stdout)
    assert match is not None

    subkeys = {
        subkey.strip().lstrip("<").rstrip(">")
        for subkey in match.group('subkeys').split("\n")
    }

    value_by_name = {}
    for value_line in match.group('values').split("\n"):
        # Skip empty lines.
        if not value_line.strip():
            continue
        # Parsing the second row in this output:
        #   size     type              value name             [value if type DWORD]
        #     16  3 REG_BINARY         <28c13ccc04de>         ....
        value_re = re.compile(r" *(?P<size>\d+) *(?P<type_short>\S+) *(?P<type_long>\S+) .*<(?P<value_name>[^>]+)>.*")
        match = value_re.match(value_line)
        value_name = match.group('value_name')

        value_by_name[value_name] = _read_registry_value(path, value_name)

    return RegistryNode(
        subkeys=subkeys,
        value_by_name=value_by_name,
    )

def _get_device_name(mac_address):
    registry_path = fr"ControlSet001\Service\BTHPORT\Parameters\Devices\{mac_address.format(caps=False, separator='')}"
    registry_node = _read_registry_path(registry_path)
    return registry_node.value_by_name['Name']

def get_devices(adapter: MacAddress) -> List[BluetoothDevice]:
    registry_path = fr"ControlSet001\Service\BTHPORT\Parameters\Keys\{adapter.format(caps=False, separator='')}"
    registry_node = _read_registry_path(registry_path)

    devices = []
    for value_name, value in registry_node.value_by_name.items():
        if not MacAddress.is_valid_mac_address(value_name):
            continue

        mac_address = MacAddress(value_name)
        device = BluetoothDevice(
            mac_address=mac_address,
            link_key=value,
            description=_get_device_name(mac_address),
        )
        devices.append(device)

    return devices
