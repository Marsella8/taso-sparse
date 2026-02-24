from __future__ import annotations

import importlib.util
import subprocess
from pathlib import Path

import modal

APP_NAME = "taso-generate-substitution-sexpr"
TASO_REPO = "https://github.com/jiazhihao/TASO.git"
RULES_OUTPUT_PATH = Path("data/substitution_rules.txt")

image = (
    modal.Image.debian_slim(python_version="3.11")
    .apt_install("git", "g++", "protobuf-compiler", "libprotobuf-dev")
    .pip_install("protobuf>=4.25.0")
)

app = modal.App(APP_NAME)


def _run(cmd: list[str], cwd: Path) -> None:
    subprocess.run(cmd, cwd=str(cwd), check=True)


def _load_rules_pb2(module_path: Path):
    spec = importlib.util.spec_from_file_location("rules_pb2_generated", module_path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Failed to load protobuf module at {module_path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _rules_to_text(rules) -> str:
    chunks: list[str] = []
    for idx, rule in enumerate(rules.rule):
        chunks.append(f"rule {idx}")
        chunks.append(str(rule).rstrip())
    return "\n\n".join(chunks) + "\n"


def save_rules(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


@app.function(image=image, timeout=60 * 60, cpu=8, memory=65536)
def generate_substitution_sexpr() -> tuple[str, int]:
    workdir = Path("/root/work")
    repo = workdir / "TASO"
    gen = repo / "src" / "generator"
    xflow_ops = repo / "include" / "xflow" / "ops.h"

    workdir.mkdir(parents=True, exist_ok=True)
    _run(["git", "clone", TASO_REPO, str(repo)], cwd=workdir)
    if not xflow_ops.exists():
        xflow_ops.parent.mkdir(parents=True, exist_ok=True)
        xflow_ops.write_text(
            '#pragma once\n#include "taso/ops.h"\nnamespace XFlow = taso;\n',
            encoding="utf-8",
        )
    generator_cc = gen / "generator.cc"
    src = generator_cc.read_text(encoding="utf-8")
    src = src.replace("if (depth >= 3) return;", "if (depth >= 4) return;")
    generator_cc.write_text(src, encoding="utf-8")

    _run(["protoc", "--cpp_out=src/generator", "--proto_path=src/core", "src/core/rules.proto"], cwd=repo)
    _run(["protoc", "--python_out=src/core", "--proto_path=src/core", "src/core/rules.proto"], cwd=repo)

    _run(
        [
            "g++",
            "generator.cc",
            "rules.pb.cc",
            "-o",
            "generator",
            "-I../../include",
            "-I/usr/local/include",
            "-L/usr/local/lib",
            "-lprotobuf",
            "-std=c++11",
            "-pthread",
            "-O2",
        ],
        cwd=gen,
    )

    log_path = gen / "generator.log"
    with log_path.open("w", encoding="utf-8") as log:
        subprocess.run(["./generator"], cwd=str(gen), check=True, stdout=log, stderr=subprocess.STDOUT)
    rules_pb2 = _load_rules_pb2(repo / "src" / "core" / "rules_pb2.py")

    rules = rules_pb2.RuleCollection()
    pb_path = gen / "graph_subst.pb"
    rules.ParseFromString(pb_path.read_bytes())

    return _rules_to_text(rules), len(rules.rule)


@app.local_entrypoint()
def main() -> None:
    rules_text, rule_count = generate_substitution_sexpr.remote()

    repo_root = Path(__file__).resolve().parents[2]
    rules_out_path = repo_root / RULES_OUTPUT_PATH
    save_rules(rules_out_path, rules_text)

    print(f"Wrote {rule_count} rules to {rules_out_path}")
