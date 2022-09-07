from PySAM.PySSC import PySSC
import requests
from collections import OrderedDict
from collections.abc import Iterable
import tempfile
import os
import glob
from pathlib import Path
import json
import tarfile
import subprocess
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

resp = requests.get("https://api.github.com/repos/NREL/sam/releases").json()

# Get the previous SAM release
previous_release = 0

print(f"Comparing Current Branch's SSC Variables and SAM Defaults to those from Release {resp[previous_release]['tag_name']}")

release_desp = resp[previous_release]['body']
linux_download_url = release_desp.split("Linux Download: ")[1].split("\r")[0]

tmpdir = tempfile.TemporaryDirectory()
print(f'Downloading {linux_download_url} to temporary dir {tmpdir.name}')
with requests.get(linux_download_url, stream=True) as File:
    # stream = true is required by the iter_content below
    sam_old_exec = os.path.join(tmpdir.name, "sam_exec_old")
    with open(sam_old_exec, 'wb') as fd:
        for chunk in File.iter_content(chunk_size=128):
            fd.write(chunk)

cmd = ". | sh " + sam_old_exec
ps = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
output = str(ps.communicate()[0])

installations_dirs = Path(output.split('installing to: ')[1].split(' ...')[0])
print(f"Installed to {installations_dirs}")

old_ssc = installations_dirs / "linux_64" / 'ssc.so'

ssc_dir = Path(os.environ.get("SSCDIR"))
new_ssc = glob.glob(str(ssc_dir / "build" / "ssc" / "*ssc.so"))[0]

if not new_ssc.exists():
    raise EnvironmentError


########################################################################################
#
# Get the SAM Defaults from the Downloaded Released and the Current Branch
#
########################################################################################

old_release = resp[previous_release]['tarball_url']

print(
    f"Comparing Current branch's SSC Defaults with SSC Released in `{resp[previous_release]['name']}` published at "
    f"{resp[previous_release]['created_at']}")

with requests.get(old_release, stream=True) as File:
    # stream = true is required by the iter_content below
    sam_old_file = os.path.join(tmpdir.name, "sam_old.tar.gz")
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

print(
    f"Comparing Current branch's SAM Defaults at {api_path} with old Defaults at {sam_old_file}")

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
    try:
        file_name = os.path.split(f)[1]
        new_defaults_data[file_name] = file_to_flat_dict(f)
    except:
        raise RuntimeError(f"Error reading defaults data from {file_name}")

old_defaults_data = {}
for f in file_list_old:
    try:
        f = os.path.join(tmpdir.name, f)
        file_name = os.path.split(f)[1]
        old_defaults_data[file_name] = file_to_flat_dict(f)
    except:
        raise RuntimeError(f"Error reading defaults data from {file_name}")

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


print("Getting variable changes using PySSC to query Compute Modules")


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

print(f"Exporting file to {os.path.join(sam_path, 'version_diff.json')}")

with open(os.path.join(sam_path, 'version_diff.json'), 'w') as f:
    json.dump(doc_dict, f, indent=4)
