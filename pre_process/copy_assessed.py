import json
import os
import shutil
import sys

from os import path

submissionsRoot = sys.argv[1]

dest = path.normpath(path.join(submissionsRoot, "..", "assessed"))
print(f"Copying assessed submissions to {dest}")
os.mkdir(dest)

for subm in os.listdir(submissionsRoot):
    with open(path.join(submissionsRoot, subm, "assessment.json"), "r") as assessmentFile:
        assessment = json.load(assessmentFile)
    if(len(assessment) > 0 and len(assessment[0]["rubrics"]) > 0):
        os.mkdir(path.join(dest, subm))
        shutil.copy(path.join(submissionsRoot, subm, "assessment.json"), path.join(dest, subm))
        shutil.copytree(path.join(submissionsRoot, subm, "build", "source"), path.join(dest, subm, "source"))
