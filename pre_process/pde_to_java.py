import os
import shutil
import subprocess
import sys

from os import path

submissionsRoot = sys.argv[1]

def findPdeRoot(searchRoot):
    items = os.listdir(searchRoot)
    for item in items:
        if path.isfile(path.join(searchRoot, item)) and item.endswith(".pde"):
            return searchRoot
    for item in items:
        if path.isdir(path.join(searchRoot, item)):
            result = findPdeRoot(path.join(searchRoot, item))
            if result != None:
                return result
    return None

def isValidProcessingProject(pdeRoot):
    return path.basename(pdeRoot) + ".pde" in os.listdir(pdeRoot)

def findMainPde(pdeRoot):
    items = os.listdir(pdeRoot)
    for item in items:
        if path.isfile(path.join(pdeRoot, item)) and item.endswith(".pde"):
            with open(path.join(pdeRoot, item), "r") as f:
                lines = f.readlines()
            if any([ line.strip().startswith("void setup()") for line in lines ]) and \
               any([ line.strip().startswith("void draw()") for line in lines ]):
                return item

for subm in os.listdir(submissionsRoot):
    print(f"\nBuilding {subm}")
    pdeRoot = findPdeRoot(path.join(submissionsRoot, subm))
    if pdeRoot != None:
        if not isValidProcessingProject(pdeRoot):
            print("Invalid Processing project, attempting a fix")
            mainPde = findMainPde(pdeRoot)
            if mainPde != None:
                print(f"Found main file {mainPde}")
                sketchName = path.splitext(mainPde)[0]
                if len(sketchName) > 30:
                    print(f"Renaming long sketch name {sketchName} to EndAssignment")
                    os.rename(path.join(pdeRoot, mainPde), path.join(pdeRoot, "EndAssignment.pde"))
                    sketchName = "EndAssignment"
                newPdeRoot = path.join(submissionsRoot, subm, sketchName)
                try: 
                    os.mkdir(newPdeRoot)
                except FileExistsError:
                    pass # Ignore if the folder already exists
                print(f"Moving files to {newPdeRoot}")
                for item in os.listdir(pdeRoot):
                    if item != "assessment.json" and item != sketchName:
                        shutil.move(path.join(pdeRoot, item), path.join(newPdeRoot, item))
                pdeRoot = newPdeRoot
            else:
                print(f"Invalid Processing project {subm}, unable to detect main file")
                continue
        else:
            sketchName = path.basename(pdeRoot)
            if len(sketchName) > 30:
                print(f"Renaming long sketch name {sketchName} to EndAssignment")
                os.rename(path.join(pdeRoot, sketchName + ".pde"), path.join(pdeRoot, "EndAssignment.pde"))
                sketchName = "EndAssignment"
                newPdeRoot = path.join(submissionsRoot, subm, sketchName)
                try: 
                    os.mkdir(newPdeRoot)
                except FileExistsError:
                    pass # Ignore if the folder already exists
                print(f"Moving files to {newPdeRoot}")
                for item in os.listdir(pdeRoot):
                    if item != "assessment.json" and item != sketchName:
                        shutil.move(path.join(pdeRoot, item), path.join(newPdeRoot, item))
                pdeRoot = newPdeRoot

        buildProc = subprocess.run(["processing-java", "--sketch=" + path.abspath(pdeRoot), "--output=" + path.abspath(path.join(submissionsRoot, subm, "build")), "--force", "--build"], capture_output = True, text = True)
        print(f"processing-java exited with code {buildProc.returncode} for project {subm}")
        print(buildProc.stderr)
        print(buildProc.stdout)
    else:
        print(f"Invalid Processing project {subm}, unable to find .pde root")
