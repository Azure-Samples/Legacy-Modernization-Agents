import uuid
from pathlib import Path


def source_relative_path(file_path: str, source_dir: str) -> str:
    """Return a container-portable path relative to the mounted source root."""
    return Path(file_path).relative_to(Path(source_dir)).as_posix()


def scoped_graph_id(run_id: int, program: str, upstream_id: object) -> str:
    """Create a stable graph identity scoped to one run and source program."""
    value = f"{run_id}/{program}/{upstream_id}"
    return str(uuid.uuid5(uuid.NAMESPACE_URL, value))


def artifact_source_path(
    artifact_path: str,
    output_dir: str,
    source_files: list[str],
) -> str | None:
    """Resolve a REKT artifact to one unambiguous source-relative path."""
    relative_artifact = Path(artifact_path).relative_to(Path(output_dir))
    candidates: list[str] = []

    for index, part in enumerate(relative_artifact.parts[:-1]):
        if part.endswith(".report"):
            report_name = part.removesuffix(".report")
            candidates.append(
                Path(*relative_artifact.parts[:index], report_name).as_posix()
            )

    artifact_name = relative_artifact.stem
    for prefix in (
        "flow-ast-",
        "cfg-",
        "control-flow-",
        "data-structure-",
        "data_structure-",
    ):
        if artifact_name.lower().startswith(prefix):
            artifact_name = artifact_name[len(prefix):]
            break
    candidates.append(artifact_name)

    normalized_sources = [Path(source).as_posix() for source in source_files]
    for candidate in candidates:
        if candidate in normalized_sources:
            return candidate

    for candidate in candidates:
        matches = [
            source
            for source in normalized_sources
            if Path(source).name == Path(candidate).name
        ]
        if len(matches) == 1:
            return matches[0]

    return None
