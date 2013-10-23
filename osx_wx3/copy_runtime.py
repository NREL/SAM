import os
import shutil

SOURCE_DIR = '../deploy/runtime'
TARGET_DIR = 'SAM.app/Contents/runtime'

if os.path.exists(TARGET_DIR):
    shutil.rmtree(TARGET_DIR)

shutil.copytree(SOURCE_DIR, TARGET_DIR, ignore=shutil.ignore_patterns('.svn'))

#SOURCE_DIR = '../../deploy/samples'
#TARGET_DIR = 'SAM.app/Contents/MacOS/samples'

#if os.path.exists(TARGET_DIR):
#    shutil.rmtree(TARGET_DIR)

#shutil.copytree(SOURCE_DIR, TARGET_DIR, ignore=shutil.ignore_patterns('.svn'))

#SOURCE_DIR = '../../deploy/weather'
#TARGET_DIR = 'SAM.app/Contents/MacOS/weather'

#if os.path.exists(TARGET_DIR):
#    shutil.rmtree(TARGET_DIR)

#shutil.copytree(SOURCE_DIR, TARGET_DIR, ignore=shutil.ignore_patterns('.svn'))
