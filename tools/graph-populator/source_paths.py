from pathlib import Path


def source_relative_path(file_path: str, source_dir: str) -> str:
    """Return a container-portable path relative to the mounted source root."""
    return Path(file_path).relative_to(Path(source_dir)).as_posix()
