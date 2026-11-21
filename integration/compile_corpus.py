#!/usr/bin/env python3
"""Compile and execute generated corpus decoders with the active OCaml toolchain."""
import argparse
from pathlib import Path
import subprocess

p = argparse.ArgumentParser(description=__doc__)
p.add_argument("artifacts", type=Path)
p.add_argument("--build-dir", type=Path, default=Path("_build"))
a = p.parse_args()
folder = a.artifacts.resolve()
runtime = a.build_dir.resolve() / "default/lib/sqlkit"
compiler = ["ocamlfind", "ocamlc", "-package", "ptime,uuidm,decimal,yojson",
            "-I", str(runtime / ".sqlkit.objs/byte"), "-I", str(folder)]
for source in ["same_fields.mli", "same_fields.ml", "corpus.mli", "corpus.ml", "decode_checks.ml"]:
    subprocess.run(compiler + ["-c", str(folder / source)], check=True, timeout=300)
exe = folder / "decode_checks.exe"
subprocess.run(compiler + ["-linkpkg", str(runtime / "sqlkit.cma"),
                          str(folder / "same_fields.cmo"), str(folder / "corpus.cmo"), str(folder / "decode_checks.cmo"),
                          "-o", str(exe)], check=True, timeout=300)
subprocess.run([str(exe)], check=True, timeout=120)
