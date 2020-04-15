import configparser
import os
from typing import List

from .types import BluetoothDevice, MacAddress
from .util import chunkify

BLUETOOTH_DIR = "/var/lib/bluetooth"

def get_local_bluetooth_adapters() -> List[MacAddress]:
    """
    Finds all locally attached bluetooth adapters. Returns their MAC addresses.
    """
    return [MacAddress(f) for f in os.listdir(BLUETOOTH_DIR)]

def to_bytes(hex_str: str):
    assert len(hex_str) % 2 == 0
    return bytes(int("".join(hexed), 16) for hexed in chunkify(hex_str, 2))

def get_devices(adapter: MacAddress) -> BluetoothDevice:
    adapter_dir = os.path.join(BLUETOOTH_DIR, adapter.format(caps=True, separator=":"))
    devices = []
    for mac_address in os.listdir(adapter_dir):
        if not MacAddress.is_valid_mac_address(mac_address):
            continue

        config = configparser.ConfigParser()
        with open(os.path.join(adapter_dir, mac_address, "info"), "r") as f:
            config.read_file(f)

        link_key = None
        if 'LinkKey' in config:
            link_key_section = config['LinkKey']
            link_key = to_bytes(link_key_section['key'])

        description = None
        if 'General' in config:
            general_section = config['General']
            description = general_section['Name']

        devices.append(BluetoothDevice(
            mac_address=MacAddress(mac_address),
            link_key=link_key,
            description=description,
        ))

    return devices
