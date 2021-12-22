import json
import os
import tempfile
import requests
import tarfile
import shutil
from collections.abc import Iterable
from collections import OrderedDict

#################################################################################################################
#
# This file compares the SSC Compute Modules input variables and the SAM defaults for these input variables
# between the current branch and the previous SAM Release. This helps track which variables and defaults have
# changed between SAM, SDK and PySAM releases to make it easier to notify users during version upgrade.
#
# The output JSON contains 3 nested dictionaries: 
#    1. 'Cmods with new defaults files'
#           Cmods listed here have new default configuration files in the new Release. This may occur with a new
#           SAM configuration, or if an existing SAM configuration added or switched a cmod, e.g. PVWatts using 
#           v8 instead of v7
#    2. 'Cmods with removed defaults files'
#           Cmods listed here have deleted default configuration files in the new Release. This may occur with a
#           deleted SAM configuration.
#    3. 'Cmods with modified variables'
#           There are three categories here: 'Added variables', 'Removed variables' and 'Type changed'.
#    4. 'Configs with modified defaults'
#           This is an option that records the default values that have changed. For variables whose lengths are
#           greater than or equal to 8760, the value changes are only reported for the first index to save space.
#
# Note: Since this file uses the Default files to search for variable changes, and Default files do not exist
#    for cmods not in a SAM configuration (e.g. modules providing helper functions or older versions, listed 
#    here: https://nrel-pysam.readthedocs.io/en/master/Models.html#other-modules-names-and-descriptions), for 
#    these cmods, variable changes will not be tracked by this current method. 
#
#################################################################################################################

#
# Download the previous SAM Release from Github's Release API
# Set to 0 to compare with the latest release; gets the previous release if 1
#
previous_release = 1
resp = requests.get("https://api.github.com/repos/NREL/sam/releases").json()
old_release = resp[previous_release]['tarball_url']
print(f"Comparing Current branch's SSC Variables with Release `{resp[previous_release]['name']}` published at {resp[previous_release]['created_at']}")

tmpdir = tempfile.TemporaryDirectory()
print(f'Using temporary dir {tmpdir.name}')
with requests.get(old_release, stream = True) as File:
    # stream = true is required by the iter_content below
    sam_old_file = os.path.join(tmpdir.name, "sam_old")
    with open(sam_old_file, 'wb') as fd:
        for chunk in File.iter_content(chunk_size=128):
            fd.write(chunk)

with tarfile.open(sam_old_file, "r:gz") as tf:
    tf.extractall(tmpdir.name)
    # To save the extracted file in directory of choice with same name as downloaded file.
    file_list_old = []
    for tarinfo in tf:
        if "defaults" in tarinfo.name and os.path.splitext(tarinfo.name)[1] == '.json':
            file_list_old.append(tarinfo.name)

sam_path = os.environ.get('SAMNTDIR')
api_path = os.path.join(sam_path, "api", "api_autogen", "library", "defaults")
file_list_new = []

for root, dirs, files in os.walk(api_path):
    for file in files:
        file_list_new.append(root + os.sep + file)

outfile_dict = {}

#
# Get changed default files
#

defaults_new = set([os.path.splitext(os.path.basename(i))[0] for i in file_list_new])
defaults_old = set([os.path.splitext(os.path.basename(i))[0] for i in file_list_old])

newly_added_defaults_dict = {}
for n in defaults_new - defaults_old:
    cmod, config = n.split('_')
    if cmod not in newly_added_defaults_dict.keys():
        newly_added_defaults_dict[cmod] = []
    newly_added_defaults_dict[cmod].append(config)
for k, v in newly_added_defaults_dict.items():
    v.sort()
outfile_dict["Cmods with new defaults files"] = newly_added_defaults_dict

newly_removed_defaults_dict = {}
for n in defaults_old - defaults_new:
    cmod, config = n.split('_')
    if cmod not in newly_removed_defaults_dict.keys():
        newly_removed_defaults_dict[cmod] = []
    newly_removed_defaults_dict[cmod].append(config)
for k, v in newly_removed_defaults_dict.items():
    v.sort()
newly_removed_defaults_dict.keys()

outfile_dict["Cmods with removed defaults files"] = newly_removed_defaults_dict

#
# Get variable changes
#

def get_flat_dict(defaults_json):
    output = {}
    for k, v in defaults_json.items():
        if type(v) is dict:
            output.update(get_flat_dict(v))
        else:
            if isinstance(v, Iterable):
                if min(v) == 0 and max(v) == 0:
                    v = [0]
            output[k] = v
    return output

def file_to_flat_dict(filename):
    with open(filename, 'r') as f:
        defaults_json = json.load(f)
        return get_flat_dict(defaults_json)

new_defaults_data = {}
for f in file_list_new:
    file_name = os.path.split(f)[1]
    new_defaults_data[file_name] = file_to_flat_dict(f)

old_defaults_data = {}
for f in file_list_old:
    f = os.path.join(tmpdir.name, f)
    file_name = os.path.split(f)[1]
    old_defaults_data[file_name] = file_to_flat_dict(f)

outfile_dict["Cmods with modified variables"] = {}
outfile_dict["Configs with modified defaults"] = {}

for k, new_data in new_defaults_data.items():
    if k in old_defaults_data:
        name = k.split('_')[0]
        config_name = k.split('.')[0]
        old_data = old_defaults_data[k]
        
        # First, compare sets
        new_key_set = set(new_data.keys())
        old_key_set = set(old_data.keys())
        
        addtl_keys = new_key_set.difference(old_key_set)
        
        if (len(addtl_keys) > 0):
            if name not in outfile_dict["Cmods with modified variables"].keys():
                outfile_dict["Cmods with modified variables"][name] = {'Added variables': []}
            for ak in addtl_keys:
                if ak not in outfile_dict["Cmods with modified variables"][name]['Added variables']:
                    outfile_dict["Cmods with modified variables"][name]['Added variables'].append(ak)
            
        removed_keys = old_key_set.difference(new_key_set)
        
        if (len(removed_keys) > 0):
            if name not in outfile_dict["Cmods with modified variables"].keys():
                outfile_dict["Cmods with modified variables"][name] = {'Removed variables': []}
            if 'Removed variables' not in outfile_dict["Cmods with modified variables"][name].keys():
                outfile_dict["Cmods with modified variables"][name]['Removed variables'] = []
            for rk in removed_keys:
                if rk not in outfile_dict["Cmods with modified variables"][name]['Removed variables']:
                    outfile_dict["Cmods with modified variables"][name]['Removed variables'].append(rk)
                
        all_keys = new_key_set.intersection(old_key_set)
        changed_keys = {}
        for key in all_keys:
            v_new = new_data[key]
            v_old = old_data[key]
            
            if type(v_new) != type(v_old):
                if not isinstance(v_new, (int, float)) or not isinstance(v_old, (int, float)):
                    if name not in outfile_dict["Cmods with modified defaults files"].keys():
                        outfile_dict["Cmods with modified variables"][name] = {'Type changed': []}
                    if key not in [r[0] for r in outfile_dict["Cmods with modified variables"][name]['Type changed']]:
                        outfile_dict["Cmods with modified variables"][name]['Type changed'] = [key, str(type(v_old)), str(type(v_new))]
            if v_new != v_old:
                if isinstance(v_new, Iterable) and isinstance(v_old, Iterable):
                    if len(v_new) + len(v_old) >= 8760 * 2:
                        changed_keys[key] = [">=8760 truncated to 0 index", v_new[0], v_old[0]]
                    else:
                        changed_keys[key] = [v_old, v_new]
                else:
                    changed_keys[key] = [v_old, v_new]
        if len(changed_keys) > 0:
            if name not in outfile_dict["Configs with modified defaults"].keys():
                outfile_dict["Configs with modified defaults"][config_name] = {}
            outfile_dict["Configs with modified defaults"][config_name]['Changed variables'] = changed_keys
        # sort and order
        if name not in outfile_dict["Cmods with modified variables"]:
            continue
        if 'Removed variables' in outfile_dict["Cmods with modified variables"][name].keys():
            outfile_dict["Cmods with modified variables"][name]['Removed variables'].sort()
        if 'Added variables' in outfile_dict["Cmods with modified variables"][name].keys():
            outfile_dict["Cmods with modified variables"][name]['Added variables'].sort()
        if 'Type changed' in outfile_dict["Cmods with modified variables"][name].keys():
            outfile_dict["Cmods with modified variables"][name]['Type changed'].sort()
            
outfile_dict["Cmods with modified variables"] = OrderedDict(sorted(outfile_dict["Cmods with modified variables"].items()))
outfile_dict["Configs with modified defaults"] = OrderedDict(sorted(outfile_dict["Configs with modified defaults"].items()))

#
# Export, print and clean up
#

outfile_name = "version_diffs.json"

with open(outfile_name, "w") as out_file:
    json.dump(outfile_dict, out_file, indent=4)
print(json.dumps(outfile_dict, indent=4))

shutil.rmtree(tmpdir.name)
