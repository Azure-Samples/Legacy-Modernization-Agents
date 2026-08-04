import os
from collections.abc import Mapping


def required_environment_variable(
    name: str,
    environment: Mapping[str, str] | None = None,
) -> str:
    values = os.environ if environment is None else environment
    value = values.get(name)
    if not value:
        raise RuntimeError(
            f"{name} is required. Copy .env.example to .env and set a strong password."
        )
    return value
