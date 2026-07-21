"""Strict IfcOpenShell schema/WHERE-rule gate for generated IFC files."""

import json
import sys

import ifcopenshell.validate


def validate(path: str) -> list[dict]:
    logger = ifcopenshell.validate.json_logger()
    ifcopenshell.validate.validate(path, logger, express_rules=True)
    return [statement for statement in logger.statements if statement.get("level") == "error"]


def main(paths: list[str]) -> int:
    failures = []
    for path in paths:
        errors = validate(path)
        print(json.dumps({"file": path, "errors": len(errors)}, ensure_ascii=False))
        failures.extend({"file": path, **error} for error in errors)
    if failures:
        print(json.dumps({"failures": failures}, ensure_ascii=False, default=str))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
