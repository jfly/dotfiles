from dataclasses import dataclass

from .util import chunkify

HEX_CHARS = set("0123456789ABCDEF")

@dataclass(frozen=True)
class MacAddress:
    mac_address: str

    def __init__(self, mac_address: str):
        object.__setattr__(self, 'mac_address', self.normalize_mac_address(mac_address))

    @classmethod
    def normalize_mac_address(cls, mac_address):
        assert cls.is_valid_mac_address(mac_address)
        normalized = mac_address.replace(":", "").upper()
        return normalized

    @classmethod
    def is_valid_mac_address(cls, mac_address):
        normalized = mac_address.replace(":", "").upper()
        if len(normalized) != 12:
            return False

        if any(ch not in HEX_CHARS for ch in normalized):
            return False

        return True

    def format(self, caps: bool, separator: str):
        formatted = self.mac_address
        if caps:
            formatted = formatted.upper()
        else:
            formatted = formatted.lower()

        formatted = separator.join("".join(pair) for pair in chunkify(formatted, 2))
        return formatted

@dataclass
class BluetoothDevice:
    mac_address: MacAddress
    link_key: bytes
    description: str
