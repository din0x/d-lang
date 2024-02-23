import os
import subprocess

FILE = "test"

if not os.path.exists(FILE):
    print(f"Creating file: {FILE}")
    open(FILE, "w")

subprocess.run(["cargo", "r", "--", FILE])