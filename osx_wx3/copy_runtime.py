import os
import shutil

SOURCE_DIR = '../deploy/runtime'
TARGET_DIR = 'SAM.app/Contents/runtime'

if os.path.exists(TARGET_DIR):
    shutil.rmtree(TARGET_DIR)

shutil.copytree(SOURCE_DIR, TARGET_DIR, ignore=shutil.ignore_patterns('.svn'))

SOURCE_DIR = '../deploy/solar_resource'
TARGET_DIR = 'SAM.app/Contents/solar_resource'

if os.path.exists(TARGET_DIR):
    shutil.rmtree(TARGET_DIR)

shutil.copytree(SOURCE_DIR, TARGET_DIR, ignore=shutil.ignore_patterns('.svn'))

SOURCE_DIR = '../deploy/wind_resource'
TARGET_DIR = 'SAM.app/Contents/wind_resource'

if os.path.exists(TARGET_DIR):
    shutil.rmtree(TARGET_DIR)

shutil.copytree(SOURCE_DIR, TARGET_DIR, ignore=shutil.ignore_patterns('.svn'))

SOURCE_DIR = '../deploy/libraries'
TARGET_DIR = 'SAM.app/Contents/libraries'

if os.path.exists(TARGET_DIR):
    shutil.rmtree(TARGET_DIR)

shutil.copytree(SOURCE_DIR, TARGET_DIR, ignore=shutil.ignore_patterns('.svn'))
