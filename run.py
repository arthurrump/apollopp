import argparse
import os
import shutil
import subprocess
import sys
import tempfile

from pathlib import Path

from pre_process import pde_to_java

def main(lang: str, mode: str, criteria: str, target: str, output: str | None):
    with tempfile.TemporaryDirectory() as tmpdir:
        projects_dir = Path(tmpdir, "projects")

        print("Copying target to temporary directory.")
        if mode == "single":
            shutil.copytree(target, projects_dir / "single")
        else:
            shutil.copytree(target, projects_dir)

        if lang == "processing":
            print("Building Processing projects.")
            for proj in os.listdir(projects_dir):
                if not pde_to_java.buildProject(projects_dir, proj):
                    if mode == "single":
                        print("Building Processing project failed. Aborting.", file=sys.stderr)
                        return 1
                    else:
                        print(f"Building Processing project {proj} failed. Ignoring project.", file=sys.stderr)
                        shutil.rmtree(projects_dir / proj)
                        continue
                shutil.copytree(projects_dir / proj / "build" / "source", projects_dir / proj / "source")

        print("Running extractor.")
        extractor_proc = subprocess.run(
            ["rascal", "Processing" if lang == "processing" else "Java", "file://" + str(projects_dir)],
            cwd=Path(__file__).parent / "extractor",
            capture_output=True,
            text=True)
        print(extractor_proc.stderr, file=sys.stderr, end=None)
        if extractor_proc.returncode != 0:
            print("Extractor failed", file=sys.stderr)
            print(extractor_proc.stdout, file=sys.stderr)
            return 1

        print("Running graph matcher.")
        gm_proc = subprocess.run(
            ["dotnet", "run", "--project", str(Path(__file__).parent / "graphmatcher" / "src"),
             "--criteria", criteria,
             "--targets", str(projects_dir),
             "run"] + ([] if output == None else ["--output", output]),
            capture_output=True,
            text=True)
        print(gm_proc.stderr, file=sys.stderr, end=None)
        print(gm_proc.stdout, end=None)
        if gm_proc.returncode != 0:
            print("Graph matcher failed", file=sys.stderr)
            return 1
    
    return 0

if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-l", "--language",
        choices=["java", "processing"],
        required=True,
        help="Programming language used in the project(s)")

    parser.add_argument(
        "-m", "--mode",
        choices=["single", "s", "batch", "b"],
        required=True,
        help="Whether to run on a single (s) project or in batch (b) mode on multiple projects")

    parser.add_argument(
        "-c", "--criteria",
        required=True,
        help="Path to a directory containing criteria rules")

    parser.add_argument(
        "-t", "--target",
        required=True,
        help="Target directory containing the project or a set of projects")

    parser.add_argument(
        "-o", "--output",
        help="Directory to write output to, rather than stdout")

    args = parser.parse_args()

    main(args.language, args.mode, args.criteria, args.target, args.output)
