#!/usr/bin/env python

import ast
import sys
import uuid

clip = sys.stdin.read().strip()
if any(clip.startswith(prefix) for prefix in ['b"', '"', "'", "b'"]):
    bs = ast.literal_eval(clip)
    parsed = uuid.UUID(bytes=bs)
else:
    clip = clip.replace("0x", "")
    parsed = uuid.UUID(hex=clip)

pretty = str(parsed)
formats = [
    pretty,
    pretty.replace("-", ""),
    pretty.upper(),
    pretty.upper().replace("-", ""),
    repr(parsed.bytes),
    "0x" + pretty.upper().replace("-", ""),
]

print("\n".join(formats))
