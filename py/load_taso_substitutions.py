from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path

RULES_PATH = Path("taso/substitutions/substitution_rules.txt")
OUTPUT_PATH = Path("taso/substitutions/substitutions.sexp")

PM_KERNEL_H = 4
PM_KERNEL_W = 5
PM_STRIDE_H = 6
PM_STRIDE_W = 7
PM_PAD = 8
PM_ACTI = 9
PM_AXIS = 11

SCALAR_INPUT_OP_IDS = {-20}


@dataclass
class Input:
    opId: int
    tsId: int


@dataclass
class Para:
    key: int
    value: int


@dataclass
class Op:
    type: int
    input: list[Input] = field(default_factory=list)
    para: list[Para] = field(default_factory=list)


@dataclass
class MappedOutput:
    srcOpId: int
    dstOpId: int
    srcTsId: int
    dstTsId: int


@dataclass
class Rule:
    srcOp: list[Op] = field(default_factory=list)
    dstOp: list[Op] = field(default_factory=list)
    mappedOutput: list[MappedOutput] = field(default_factory=list)


# --- parser ---


def _parse_int_fields(block: str) -> dict[str, int]:
    return {m.group(1): int(m.group(2)) for m in re.finditer(r"(\w+):\s*(-?\d+)", block)}


def _parse_op(block: str) -> Op:
    fields = _parse_int_fields(block)
    inputs = [
        Input(opId=int(m.group(1)), tsId=int(m.group(2)))
        for m in re.finditer(r"input\s*\{[^}]*opId:\s*(-?\d+)[^}]*tsId:\s*(\d+)[^}]*\}", block)
    ]
    paras = [
        Para(key=int(m.group(1)), value=int(m.group(2)))
        for m in re.finditer(r"para\s*\{[^}]*key:\s*(\d+)[^}]*value:\s*(\d+)[^}]*\}", block)
    ]
    return Op(type=fields["type"], input=inputs, para=paras)


def _parse_mapped_output(block: str) -> MappedOutput:
    f = _parse_int_fields(block)
    return MappedOutput(srcOpId=f["srcOpId"], dstOpId=f["dstOpId"], srcTsId=f["srcTsId"], dstTsId=f["dstTsId"])


def _parse_rule(text: str) -> Rule:
    rule = Rule()
    for m in re.finditer(r"(srcOp|dstOp|mappedOutput)\s*\{", text):
        tag = m.group(1)
        start = m.end()
        depth = 1
        pos = start
        while depth > 0:
            if text[pos] == "{":
                depth += 1
            elif text[pos] == "}":
                depth -= 1
            pos += 1
        block = text[start : pos - 1]
        match tag:
            case "srcOp":
                rule.srcOp.append(_parse_op(block))
            case "dstOp":
                rule.dstOp.append(_parse_op(block))
            case "mappedOutput":
                rule.mappedOutput.append(_parse_mapped_output(block))
    return rule


def _parse_rules(text: str) -> list[Rule]:
    chunks = re.split(r"^rule \d+\s*$", text, flags=re.MULTILINE)
    return [_parse_rule(chunk) for chunk in chunks if chunk.strip()]


# --- helpers ---


def _params(op: Op) -> dict[int, int]:
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


# --- conversion ---


def _assign_names(ops: list[Op], prefix: str) -> dict[tuple[int, int], str]:
    lookup: dict[tuple[int, int], str] = {}
    counter = 0
    for i, op in enumerate(ops):
        if op.type == 13:  # OP_SPLIT
            lookup[(i, 0)] = f"{prefix}{counter}"
            lookup[(i, 1)] = f"{prefix}{counter + 1}"
            counter += 2
        else:
            lookup[(i, 0)] = f"{prefix}{counter}"
            counter += 1
    return lookup


def _tensor_ref(ref: Input, lookup: dict[tuple[int, int], str]) -> str:
    if ref.opId < 0:
        return f"(tensor t{-ref.opId})"
    return f"(tensor {lookup[(ref.opId, ref.tsId)]})"


def _scalar_ref(ref: Input) -> str:
    return f"(scalar s{-ref.opId})"


def _op_expr(op: Op, lookup: dict[tuple[int, int], str]) -> str:
    p = _params(op)

    match op.type:
        case 3:  # OP_CONV2D
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            s = _stride_expr(p[PM_STRIDE_H], p[PM_STRIDE_W])
            pad = _pad_mode(p[PM_PAD])
            act = _acti_mode(p[PM_ACTI])
            x = _tensor_ref(op.input[0], lookup)
            y = _tensor_ref(op.input[1], lookup)
            return f"(conv2d {k} {s} {pad} {act} {x} {y})"

        case 6:  # OP_POOL2D_MAX
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            s = _stride_expr(p[PM_STRIDE_H], p[PM_STRIDE_W])
            pad = _pad_mode(p[PM_PAD])
            x = _tensor_ref(op.input[0], lookup)
            return f"(pool2d-max {k} {s} {pad} {x})"

        case 7:  # OP_POOL2D_AVG
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            s = _stride_expr(p[PM_STRIDE_H], p[PM_STRIDE_W])
            pad = _pad_mode(p[PM_PAD])
            x = _tensor_ref(op.input[0], lookup)
            return f"(pool2d-avg {k} {s} {pad} {x})"

        case 8:  # OP_RELU
            x = _tensor_ref(op.input[0], lookup)
            return f"(relu {x})"

        case 12:  # OP_CONCAT
            a = _axis_expr(p[PM_AXIS])
            x = _tensor_ref(op.input[0], lookup)
            y = _tensor_ref(op.input[1], lookup)
            return f"(concat {a} {x} {y})"

        case 15:  # OP_TRANSPOSE
            x = _tensor_ref(op.input[0], lookup)
            return f"(transpose {x})"

        case 16:  # OP_EW_ADD
            x = _tensor_ref(op.input[0], lookup)
            y = _tensor_ref(op.input[1], lookup)
            return f"(ewadd {x} {y})"

        case 17:  # OP_EW_MUL
            x = _tensor_ref(op.input[0], lookup)
            y = _tensor_ref(op.input[1], lookup)
            return f"(ewmul {x} {y})"

        case 18:  # OP_MATMUL
            x = _tensor_ref(op.input[0], lookup)
            y = _tensor_ref(op.input[1], lookup)
            return f"(matmul {x} {y})"

        case 19:  # OP_MUL
            x = _tensor_ref(op.input[0], lookup)
            s = _scalar_ref(op.input[1])
            return f"(mul {x} {s})"

        case 20:  # OP_ENLARGE
            k = _kernel_expr(p[PM_KERNEL_H], p[PM_KERNEL_W])
            x = _tensor_ref(op.input[0], lookup)
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

        case _:
            raise ValueError(f"Unsupported op type: {op.type}")


def _graph_sexpr(ops: list[Op], prefix: str) -> tuple[str, dict[tuple[int, int], str]]:
    lookup = _assign_names(ops, prefix)
    assts: list[str] = []
    for i, op in enumerate(ops):
        if op.type == 13:  # OP_SPLIT
            p = _params(op)
            a = _axis_expr(p[PM_AXIS])
            x = _tensor_ref(op.input[0], lookup)
            assts.append(f"(asst (tensor {lookup[(i, 0)]}) (split0 {a} {x}))")
            assts.append(f"(asst (tensor {lookup[(i, 1)]}) (split1 {a} {x}))")
        else:
            expr = _op_expr(op, lookup)
            assts.append(f"(asst (tensor {lookup[(i, 0)]}) {expr})")
    if not assts:
        return "(graph)", lookup
    return "(graph " + " ".join(assts) + ")", lookup


def _free_inputs(ops: list[Op]) -> set[int]:
    ids: set[int] = set()
    for op in ops:
        for inp in op.input:
            if inp.opId < 0:
                ids.add(inp.opId)
    return ids


def _rule_to_rewrite(rule: Rule) -> str:
    src_graph, src_lookup = _graph_sexpr(rule.srcOp, "s")
    dst_graph, dst_lookup = _graph_sexpr(rule.dstOp, "d")

    shared = _free_inputs(rule.srcOp) & _free_inputs(rule.dstOp)
    tensor_ids = sorted([x for x in shared if x not in SCALAR_INPUT_OP_IDS], key=lambda x: -x)

    input_pairs: list[str] = []
    for op_id in tensor_ids:
        n = f"t{-op_id}"
        input_pairs.append(f"((tensor {n}) (tensor {n}))")
    input_bimap = "(bimap " + " ".join(input_pairs) + ")"

    output_pairs: list[str] = []
    for mo in rule.mappedOutput:
        src_name = src_lookup[(mo.srcOpId, mo.srcTsId)]
        dst_name = dst_lookup[(mo.dstOpId, mo.dstTsId)]
        output_pairs.append(f"((tensor {src_name}) (tensor {dst_name}))")
    output_bimap = "(bimap " + " ".join(output_pairs) + ")"

    return f"(substitution {src_graph} {dst_graph} {input_bimap} {output_bimap})"


# --- val ---

OP_NAMES: dict[int, str] = {
    3: "conv2d", 6: "pool2d-max", 7: "pool2d-avg", 8: "relu",
    12: "concat", 13: "split", 15: "transpose", 16: "ewadd",
    17: "ewmul", 18: "matmul", 19: "mul", 20: "enlarge",
    22: "const-imm", 23: "const-iconv", 24: "const-one", 25: "const-pool",
}


def _expected_asst_count(ops: list[Op]) -> int:
    return sum(2 if op.type == 13 else 1 for op in ops)


def _check_rule(idx: int, rule: Rule) -> None:
    src_lookup = _assign_names(rule.srcOp, "s")
    dst_lookup = _assign_names(rule.dstOp, "d")

    src_free = _free_inputs(rule.srcOp)
    dst_free = _free_inputs(rule.dstOp)
    shared = src_free & dst_free

    src_assts = _expected_asst_count(rule.srcOp)
    dst_assts = _expected_asst_count(rule.dstOp)
    if src_assts != len(src_lookup):
        raise ValueError(f"rule {idx}: src lookup size {len(src_lookup)} != expected {src_assts}")
    if dst_assts != len(dst_lookup):
        raise ValueError(f"rule {idx}: dst lookup size {len(dst_lookup)} != expected {dst_assts}")

    for mo in rule.mappedOutput:
        if (mo.srcOpId, mo.srcTsId) not in src_lookup:
            raise ValueError(f"rule {idx}: mappedOutput src ({mo.srcOpId},{mo.srcTsId}) not in src lookup")
        if (mo.dstOpId, mo.dstTsId) not in dst_lookup:
            raise ValueError(f"rule {idx}: mappedOutput dst ({mo.dstOpId},{mo.dstTsId}) not in dst lookup")

    for op in rule.srcOp + rule.dstOp:
        if op.type not in OP_NAMES:
            raise ValueError(f"rule {idx}: unknown op type {op.type}")
        for inp in op.input:
            if inp.opId >= 0:
                continue
            if inp.opId not in shared and inp.opId in dst_free and inp.opId in src_free:
                raise ValueError(f"rule {idx}: free input {inp.opId} in both sides but not in shared")


# --- main ---


def main() -> None:
    repo_root = Path(__file__).resolve().parents[1]
    text = (repo_root / RULES_PATH).read_text(encoding="utf-8")
    rules = _parse_rules(text)

    for i, rule in enumerate(rules):
        _check_rule(i, rule)

    lines: list[str] = []
    seen: set[str] = set()
    for rule in rules:
        rewrite = _rule_to_rewrite(rule)
        if rewrite not in seen:
            seen.add(rewrite)
            lines.append(rewrite)

    out = repo_root / OUTPUT_PATH
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Checked {len(rules)} rules, wrote {len(lines)} substitutions to {out}")


if __name__ == "__main__":
    main()
