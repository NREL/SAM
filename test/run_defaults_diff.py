from PySAM.PySSC import PySSC
import requests
from collections import OrderedDict
from collections.abc import Iterable
import tempfile
import os
import sys
import json
import tarfile
from ctypes import *


"""
      This Script compares the SSC input variables and the defaults for these input variables
      between the current branch and the last release. This helps track which variables
      and defaults have changed between releases to make it easier to notify users during version upgrade.
      Below are lists for each Compute Module:

          1. New Default configurations

             Cmods have new default configuration files in the new Release. This may occur with a new
             SAM configuration, or if an existing SAM configuration added or switched a cmod, e.g. PVWatts using 
             v8 instead of v7

          2. Removed Default configurations

             Cmods have deleted default configuration files in the new Release. This may occur with a deleted SAM
             configuration.

          3. Modified Variables: New variables, Removed variables and Type-changed variables

          4. Modified Default Values

             This is records the default values that have changed. For variables whose lengths are greater than or
             equal to 8760, the value changes are only reported for the first index to save space.
          
      To Use: Provide as arguments to the script
          1. Path to the old ssc library
          2. Path to the new ssc library
"""


if len(sys.argv) < 3:
    raise RuntimeError("Please provide the path to the old Release's ssc library and the path to the new library")
old_ssc = sys.argv[1]
if not os.path.exists(old_ssc):
    raise RuntimeError("Path to old Release's ssc library was invalid")
new_ssc = sys.argv[2]
if not os.path.exists(new_ssc):
    raise RuntimeError("Path to old Release's ssc library was invalid")
print(f"Comparing SSC libraries from old at {old_ssc} to new at {new_ssc}")


########################################################################################
#
# Download the SAM repo tagged for the previous Release to compare defaults
#
########################################################################################

previous_release = 0    # set to 1 to compare against previous before last
resp = requests.get("https://api.github.com/repos/NREL/sam/releases").json()
old_release = resp[previous_release]['tarball_url']

print(
    f"Comparing Current branch's SSC Defaults with Release `{resp[previous_release]['name']}` published at "
    f"{resp[previous_release]['created_at']}")

tmpdir = tempfile.TemporaryDirectory()
print(f'Using temporary dir {tmpdir.name}')
with requests.get(old_release, stream=True) as File:
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

########################################################################################
#
# Get changed default files
#
########################################################################################

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

outfile_dict["Configs with modified defaults"] = {}

for k, new_data in new_defaults_data.items():
    if k in old_defaults_data:
        name = k.split('_')[0]
        config_name = k.split('.')[0]
        old_data = old_defaults_data[k]

        # First, compare sets
        new_key_set = set(new_data.keys())
        old_key_set = set(old_data.keys())

        all_keys = new_key_set.intersection(old_key_set)
        changed_keys = {}
        for key in all_keys:
            v_new = new_data[key]
            v_old = old_data[key]

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

# Organize

doc_dict = OrderedDict()
cmods = set(outfile_dict['Cmods with new defaults files'].keys())
cmods = cmods.union(set(outfile_dict['Cmods with removed defaults files'].keys()))
cmods = list(cmods)
cmods.sort()

for c in cmods:
    doc_dict[c] = {}
    if c in outfile_dict['Cmods with new defaults files'].keys():
        doc_dict[c]['new_defaults'] = outfile_dict['Cmods with new defaults files'][c]
    if c in outfile_dict['Cmods with removed defaults files'].keys():
        doc_dict[c]['del_defaults'] = outfile_dict['Cmods with removed defaults files'][c]
    def_dict = {}
    for k, v in outfile_dict['Configs with modified defaults'].items():
        if c in k:
            def_dict[k] = v['Changed variables']
    if len(def_dict) > 0:
        doc_dict[c]['mod_defaults'] = def_dict
    if len(doc_dict[c]) == 0:
        doc_dict.pop(c)

########################################################################################
#
# Get variable changes using PySSC to query Compute Modules' interfaces
#
########################################################################################


def get_var_dict():
    cmod_variables = {}
    i = 0
    while ssc.module_entry(i):
        name = ssc.entry_name(ssc.module_entry(i))
        mod = ssc.module_create(name)
        j = 0
        cmod_variables[name.decode("utf-8")] = {}
        while ssc.module_var_info(mod, j):
            var = ssc.module_var_info(mod, j)
            vname = ssc.info_name(var).decode("utf-8")
            vtype = ssc.info_var_type(var)
            if vtype == 1 or vtype == 3:
                cmod_variables[name.decode("utf-8")][vname] = ssc.info_data_type(var)
            j += 1
        i += 1
    return cmod_variables


ssc = PySSC()
ssc.pdll = CDLL(new_ssc)
new_cmod_variables = get_var_dict()


ssc = PySSC(old_ssc)
ssc.pdll = CDLL(old_ssc)
old_cmod_variables = get_var_dict()

cmod_int = set(new_cmod_variables.keys()).intersection(set(old_cmod_variables.keys()))

for name in cmod_int:
    pysam_name = "".join([s.capitalize() for s in name.split('_')])
    mod_variables = dict()

    new_key_set = set(new_cmod_variables[name].keys())
    old_key_set = set(old_cmod_variables[name].keys())

    addtl_keys = new_key_set.difference(old_key_set)

    if len(addtl_keys) > 0:
        addtl_keys = list(addtl_keys)
        addtl_keys.sort()
        mod_variables['Added variables'] = addtl_keys

    removed_keys = old_key_set.difference(new_key_set)

    if len(removed_keys) > 0:
        removed_keys = list(removed_keys)
        removed_keys.sort()
        mod_variables['Removed variables'] = removed_keys

    all_keys = new_key_set.intersection(old_key_set)
    changed_keys = {}
    for key in all_keys:
        v_new = new_cmod_variables[name][key]
        v_old = old_cmod_variables[name][key]

        if v_new != v_old:
            if 'Type changed' not in mod_variables.keys():
                mod_variables['Type changed'] = []
            mod_variables['Type changed'].append(key)
    if 'Type changed' in mod_variables.keys():
        mod_variables['Type changed'].sort()
    if len(mod_variables) > 0:
        if pysam_name not in doc_dict.keys():
            doc_dict[pysam_name] = {'mod_variables': mod_variables}
        else:
            doc_dict[pysam_name].update({'mod_variables': mod_variables})

doc_dict = OrderedDict(sorted(doc_dict.items()))

with open('version_diff.json', 'w') as f:
    json.dump(doc_dict, f, indent=4)
