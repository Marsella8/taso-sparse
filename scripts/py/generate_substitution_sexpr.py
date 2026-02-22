from __future__ import annotations

import importlib.util
import subprocess
from dataclasses import dataclass
from pathlib import Path

import modal

APP_NAME = "taso-generate-substitution-sexpr"
TASO_REPO = "https://github.com/jiazhihao/TASO.git"
OUTPUT_PATH = Path("data/substitutions.sexp")

# PMParameter ids from include/xflow/ops.h at TASO commit 90150c0.
PM_KERNEL_H = 4
PM_KERNEL_W = 5
PM_STRIDE_H = 6
PM_STRIDE_W = 7
PM_PAD = 8
PM_ACTI = 9
PM_AXIS = 11

SCALAR_INPUT_OP_IDS = {-20}

image = (
    modal.Image.debian_slim(python_version="3.11")
    .apt_install("git", "g++", "protobuf-compiler", "libprotobuf-dev")
    .pip_install("protobuf>=4.25.0")
)

app = modal.App(APP_NAME)


@dataclass(frozen=True)
class TensorRef:
    opId: int
    tsId: int


def _run(cmd: list[str], cwd: Path) -> None:
    subprocess.run(cmd, cwd=str(cwd), check=True)


def _load_rules_pb2(module_path: Path):
    spec = importlib.util.spec_from_file_location("rules_pb2_generated", module_path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Failed to load protobuf module at {module_path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _params(op) -> dict[int, int]:
    return {p.key: p.value for p in op.para}


def _pad_mode(v: int) -> str:
    match v:
        case 0:  # PD_MODE_SAME
            return "same"
        case 1:  # PD_MODE_VALID
            return "valid"
        case _:
            raise ValueError(f"Unsupported pad mode: {v}")


def _acti_mode(v: int) -> str:
    match v:
        case 0:  # AC_MODE_NONE
            return "none"
        case 2:  # AC_MODE_RELU
            return "relu"
        case 1:  # AC_MODE_SIGMOID
            return "sigmoid"
        case 3:  # AC_MODE_TANH
            return "tanh"
        case _:
            raise ValueError(f"Unsupported activation mode: {v}")


def _axis_expr(v: int) -> str:
    return f"(axis {v})"


def _kernel_expr(h: int, w: int) -> str:
    return f"(kernel2d {h} {w})"


def _stride_expr(h: int, w: int) -> str:
    return f"(stride2d {h} {w})"


def _tensor_var(op_id: int) -> str:
    return f"(var t{-op_id} tensor)"


def _scalar_var(op_id: int) -> str:
    return f"(var s{-op_id} scalar)"


def _scalar_term(ref: TensorRef, ops) -> str:
    if ref.opId < 0:
        return _scalar_var(ref.opId)
    raise ValueError(f"Expected scalar input tensor, found opId={ref.opId}, tsId={ref.tsId}")


def _tensor_expr(ref: TensorRef, ops) -> str:
    if ref.opId < 0:
        if ref.opId in SCALAR_INPUT_OP_IDS:
            raise ValueError(f"Scalar input used in tensor position: opId={ref.opId}")
        return _tensor_var(ref.opId)

    op = ops[ref.opId]
    p = _params(op)

    match op.type:
        case 3:  # OP_CONV2D
            s = _stride_expr(p[PM_STRIDE_H], p[PM_STRIDE_W])
            pad = _pad_mode(p[PM_PAD])
            act = _acti_mode(p[PM_ACTI])
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            y = _tensor_expr(TensorRef(op.input[1].opId, op.input[1].tsId), ops)
            return f"(conv2d {s} {pad} {act} {x} {y})"

        case 6:  # OP_POOL2D_MAX
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            s = _stride_expr(p[PM_STRIDE_H], p[PM_STRIDE_W])
            pad = _pad_mode(p[PM_PAD])
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            return f"(pool2d-max {k} {s} {pad} {x})"

        case 7:  # OP_POOL2D_AVG
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            s = _stride_expr(p[PM_STRIDE_H], p[PM_STRIDE_W])
            pad = _pad_mode(p[PM_PAD])
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            return f"(pool2d-avg {k} {s} {pad} {x})"

        case 8:  # OP_RELU
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            return f"(relu {x})"

        case 12:  # OP_CONCAT
            a = _axis_expr(p[PM_AXIS])
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            y = _tensor_expr(TensorRef(op.input[1].opId, op.input[1].tsId), ops)
            return f"(concat {a} {x} {y})"

        case 13:  # OP_SPLIT
            a = _axis_expr(p[PM_AXIS])
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            match ref.tsId:
                case 0:
                    return f"(split0 {a} {x})"
                case 1:
                    return f"(split1 {a} {x})"
                case _:
                    raise ValueError(f"Unexpected split output index: {ref.tsId}")

        case 15:  # OP_TRANSPOSE
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            return f"(transpose {x})"

        case 16:  # OP_EW_ADD
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            y = _tensor_expr(TensorRef(op.input[1].opId, op.input[1].tsId), ops)
            return f"(ewadd {x} {y})"

        case 17:  # OP_EW_MUL
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            y = _tensor_expr(TensorRef(op.input[1].opId, op.input[1].tsId), ops)
            return f"(ewmul {x} {y})"

        case 18:  # OP_MATMUL
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            y = _tensor_expr(TensorRef(op.input[1].opId, op.input[1].tsId), ops)
            return f"(matmul {x} {y})"

        case 19:  # OP_MUL
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            s = _scalar_term(TensorRef(op.input[1].opId, op.input[1].tsId), ops)
            return f"(mul {x} {s})"

        case 20:  # OP_ENLARGE
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            x = _tensor_expr(TensorRef(op.input[0].opId, op.input[0].tsId), ops)
            return f"(enlarge {k} {x})"

        case 22:  # OP_CONSTANT_IMM
            return "(const-imm)"

        case 23:  # OP_CONSTANT_ICONV
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            return f"(const-iconv {k})"

        case 24:  # OP_CONSTANT_ONE
            return "(const-one)"

        case 25:  # OP_CONSTANT_POOL
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            return f"(const-pool {k})"

        case _:  # Unsupported
            raise ValueError(f"Unsupported op type in substitution serializer: {op.type}")


def _rule_to_eq(rule) -> str:
    mapped = rule.mappedOutput[0]
    lhs = _tensor_expr(TensorRef(mapped.srcOpId, mapped.srcTsId), rule.srcOp)
    rhs = _tensor_expr(TensorRef(mapped.dstOpId, mapped.dstTsId), rule.dstOp)
    return f"(eq {lhs} {rhs})"


@app.function(image=image, timeout=60 * 60, cpu=8, memory=65536)
def generate_substitution_sexpr() -> tuple[str, int, int]:
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

    lines: list[str] = []
    for rule in rules.rule:
        lines.append(_rule_to_eq(rule))

    return "\n".join(lines) + "\n", len(rules.rule), len(lines)


@app.local_entrypoint()
def main() -> None:
    sexpr_text, generated_count, kept_count = generate_substitution_sexpr.remote()

    repo_root = Path(__file__).resolve().parents[2]
    out_path = repo_root / OUTPUT_PATH
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(sexpr_text, encoding="utf-8")

    print(f"Wrote {kept_count} substitutions (from {generated_count} generated rules) to {out_path}")
