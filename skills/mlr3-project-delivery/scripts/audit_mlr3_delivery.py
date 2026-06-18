#!/usr/bin/env python3
import argparse
import csv
from pathlib import Path


def status(ok, label, detail=""):
    tag = "OK" if ok else "MISSING"
    suffix = f" - {detail}" if detail else ""
    print(f"{tag}\t{label}{suffix}")


def warn(label, detail=""):
    suffix = f" - {detail}" if detail else ""
    print(f"WARN\t{label}{suffix}")


def count_rows(path):
    if not path.exists():
        return 0
    with path.open(newline="", encoding="utf-8", errors="replace") as handle:
        reader = csv.reader(handle, delimiter="\t")
        return max(sum(1 for _ in reader) - 1, 0)


def has_doc(docs, predicate):
    return any(predicate(p.name) for p in docs)


def main():
    parser = argparse.ArgumentParser(description="Audit mlr3 project delivery structure.")
    parser.add_argument("--project", required=True, help="Project root directory.")
    parser.add_argument("--result", required=True, help="Formal result directory.")
    parser.add_argument("--expected-branches", type=int, default=None)
    parser.add_argument("--expected-models-per-branch", type=int, default=None)
    args = parser.parse_args()

    project = Path(args.project)
    result = Path(args.result)

    status(project.exists(), "project root", str(project))
    status(result.exists(), "formal result root", str(result))
    status((result / "RESULT_INTERPRETATION_MANUAL.md").is_file(), "RESULT_INTERPRETATION_MANUAL.md")

    for rel in [
        "README.md",
        "docs/README.md",
        "scripts/README.md",
    ]:
        status((project / rel).is_file(), rel)

    docs = list((project / "docs").glob("*.md")) if (project / "docs").is_dir() else []
    consolidated_docs = [
        "01_建模设计与脚本来源.md",
        "02_复现与参数.md",
        "03_结果读取与技术汇总.md",
        "04_结果解释边界与Methods.md",
    ]
    has_consolidated = all((project / "docs" / name).is_file() for name in consolidated_docs)
    has_legacy = (
        (project / "docs" / "FORMAL_REPRODUCTION.md").is_file()
        and (project / "docs" / "METHODS_FOR_MANUSCRIPT_CN_EN.md").is_file()
        and has_doc(docs, lambda name: "建模计划" in name)
        and has_doc(docs, lambda name: "参数设置教程" in name)
        and has_doc(docs, lambda name: "Step2" in name)
    )
    doc_mode = "consolidated" if has_consolidated else "legacy" if has_legacy else "missing"
    status(has_consolidated or has_legacy, "docs core delivery set", doc_mode)
    for name in consolidated_docs:
        if has_consolidated:
            status((project / "docs" / name).is_file(), f"docs/{name}")

    input_dirs = [p for p in project.glob("v0.2.2_inputs*") if p.is_dir()]
    status(bool(input_dirs), "input directory v0.2.2_inputs*")
    for idir in input_dirs:
        status((idir / "README.md").is_file(), f"{idir.name}/README.md")
        status((idir / "input_summary.tsv").is_file(), f"{idir.name}/input_summary.tsv")

    step1_dirs = sorted(result.glob("run_model_*_step1"))
    step2_dirs = sorted(result.glob("run_model_*_step2"))
    status(bool(step1_dirs), "Step1 directories", str(len(step1_dirs)))
    status(bool(step2_dirs), "Step2 directories", str(len(step2_dirs)))

    if args.expected_branches is not None:
        status(len(step2_dirs) == args.expected_branches, "expected Step2 branch count", f"{len(step2_dirs)}/{args.expected_branches}")

    for step2 in step2_dirs:
        for fname in ["efs.RData", "model_performance_comparison.xlsx", "analysis_flowchart.md"]:
            status((step2 / fname).is_file(), f"{step2.name}/{fname}")

    perf = result / "01_model_performance_summary"
    status(perf.is_dir(), "01_model_performance_summary/")
    if perf.is_dir():
        status((perf / "README.md").is_file(), "01_model_performance_summary/README.md")
        status(any(perf.glob("*.md")), "01_model_performance_summary/*.md")
        status(any(perf.glob("*.xlsx")), "01_model_performance_summary/*.xlsx")
        all_tsv = list(perf.glob("*all*model*.tsv")) + list(perf.glob("all_model_metrics.tsv"))
        status(bool(all_tsv), "01_model_performance_summary all-model TSV")
        for p in all_tsv[:1]:
            rows = count_rows(p)
            detail = f"{p.name}: {rows} rows"
            if args.expected_branches and args.expected_models_per_branch:
                expected = args.expected_branches * args.expected_models_per_branch
                status(rows == expected, "all-model metric row count", f"{rows}/{expected}")
            else:
                print(f"OK\tall-model metric row count - {detail}")

    tech = result / "02_step2_technical_summary"
    if tech.is_dir():
        status((tech / "README.md").is_file(), "02_step2_technical_summary/README.md")
        status((tech / "step2_technical_summary.tsv").is_file(), "02_step2_technical_summary/step2_technical_summary.tsv")
        status((tech / "step2_best_hyperparameters.tsv").is_file(), "02_step2_technical_summary/step2_best_hyperparameters.tsv")
        status((tech / "step2_technical_summary.xlsx").is_file(), "02_step2_technical_summary/step2_technical_summary.xlsx")
    else:
        warn("02_step2_technical_summary/", "recommended for 20260403_lilei-style delivery")

    shap = result / "03_shap_output_summary"
    if shap.is_dir():
        status((shap / "SHAP_OUTPUT_SUMMARY.md").is_file(), "03_shap_output_summary/SHAP_OUTPUT_SUMMARY.md")
        status((shap / "shap_status_summary.tsv").is_file(), "03_shap_output_summary/shap_status_summary.tsv")
        status((shap / "shap_model_files.tsv").is_file(), "03_shap_output_summary/shap_model_files.tsv")
        status((shap / "shap_output_summary.xlsx").is_file(), "03_shap_output_summary/shap_output_summary.xlsx")
    else:
        warn("03_shap_output_summary/", "missing or SHAP not run")

    shap_status = list(result.glob("run_model_*_step2/step3_shap*/step3_shap_status.tsv"))
    if shap_status:
        status(True, "per-branch SHAP status files", str(len(shap_status)))
    else:
        warn("per-branch SHAP status files", "none found")


if __name__ == "__main__":
    main()
